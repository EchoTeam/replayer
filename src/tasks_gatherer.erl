-module(tasks_gatherer).

-export([
    merge_logs/2
]).

-define(TARGET, merger_target).

-type task_type() :: disk_log | csv_log.
-type task() :: {Type :: task_type(), FileName :: string()}.

% tasks_gatherer:merge_logs([{disk_log, "./submit_requests.0.log"}, {disk_log, "./users_update_requests.0.log"},{csv_log, "./search_requests.0.log"}], "./merged.log").
-spec merge_logs(From :: [task()], To :: string()) -> ok | {error, Why :: string()}.
merge_logs(Tasks, FileName) ->
    Files = [ F || {_, F} <- Tasks ],
    io:format("The following logs are going to be merged into the ~p file:~n~p~n", [FileName, Files]),
    case confirm() of
        true -> 
            try merge_logs_ll(Tasks, FileName) 
            catch C:R ->
                io:format("ERROR: failed due to error:~p~nStack:~p~n", [{C,R}, erlang:get_stacktrace()]),
                {error, lists:flatten(io_lib:format("~p", [{C,R}]))}
            end;
        _ -> 
            ok
    end.

confirm() ->
    case io:fread("Are you sure want to continue? (yY/nN)> ", "~1s") of
        {ok, ["y"]} -> true;
        {ok, ["Y"]} -> true;
        {ok, [_]} -> false
    end.

merge_logs_ll(Tasks, FileName) ->
    OpenedTasks = open_tasks(Tasks),
    ok = open_target_file(FileName),
    merge(OpenedTasks),
    close_target_file(),
    ok.

open_target_file(File) ->
    case disk_log:open([{name, ?TARGET}, {file, File}, {repair, truncate}]) of
        {ok, ?TARGET} -> nop;
        Error -> throw({open_target_file, Error})
    end,
    ok.

close_target_file() ->
    disk_log:close(?TARGET).

open_tasks(Tasks) ->
    lists:map(fun
            ({disk_log, File}) -> disk_log_reader(File);
            ({csv_log, File}) -> csv_log_reader(File)
        end, Tasks).

% reader returns a list of item + continuation function, which should be run
% every time for the next portion of items.
% {[item()]
-type continuation_fun() :: fun(() -> eof | {continuation_fun(), [any()]}).
-spec disk_log_reader(File :: string()) -> {continuation_fun(), [any()]}.
-spec csv_log_reader(File :: string()) -> {continuation_fun(), [any()]}.

disk_log_reader(File) -> 
    disk_log:open([{name, File}, {file, File}, {mode, read_only}]),
    Fun = disk_log_reader_fun(File, start),
    Fun().

disk_log_reader_fun(Name, Continuation) ->
    fun() ->
            case disk_log:chunk(Name, Continuation) of
                {Continuation2, Items} -> 
                    Requests = lists:map(fun disk_log_item_process/1, Items),
                    {disk_log_reader_fun(Name, Continuation2), Requests};
                eof -> disk_log:close(Name), eof;
                Error -> throw({"Error while reading log", Name, Error})
            end
    end.

csv_log_reader(File) -> 
    {ok, FH} = file:open(File, [read, binary]),
    Fun = csv_log_reader_fun({File, FH}, undefined),
    Fun().

            
csv_log_reader_fun({Name, FH}, _Cont) ->
    fun() ->
            case io:get_line(FH, '') of
                {error, Error} -> throw({"Error while reading log", Name, Error});
                eof -> file:close(FH), eof;
                "\n" -> csv_log_reader_fun({Name, FH}, undefined);
                "\r\n" -> csv_log_reader_fun({Name, FH}, undefined);
                Data -> 
                    Items = string:tokens(strip(binary_to_list(Data)), [$;]),
                    Request = csv_log_item_process(Items),
                    %io:format("Request:~p~n", [Request]),
                    {csv_log_reader_fun({Name, FH}, undefined), [Request]}
            end
    end.

-spec disk_log_item_process({string(), erlang:timestamp(), binary()}) -> replayer_utils:request().
disk_log_item_process({Endpoint, Ts, Data}) ->
    {match, [E]} = re:run(Endpoint, "/get/api/(.*)", [{capture, [1], list}]),
    Url = "http://api.aboutecho.com/" ++ E,
    {post, Ts, Url, Data}.

% see https://github.com/lkiesow/erlang-datetime/blob/master/datetime.erl for the help
-spec csv_log_item_process([string()]) -> replayer_utils:request().
csv_log_item_process([DateTimeStr,Endpoint]) ->
    {ok, [MonStr, Day, Year, Hour, Min, Sec, MS], []} = io_lib:fread("~3s ~d, ~d ~d:~d:~d.~d", DateTimeStr),
    Mon = month(MonStr),
    Secs  = calendar:datetime_to_gregorian_seconds( {{Year,Mon,Day},{Hour,Min,Sec}}) - 
                calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Ts = {Secs div 1000000, Secs rem 1000000, MS},
    Url = "http://api.aboutecho.com" ++ Endpoint,
    {get, Ts, Url}.

month("Jan") -> 1;
month("Feb") -> 2;
month("Mar") -> 3;
month("Apr") -> 4;
month("May") -> 5;
month("Jun") -> 6;
month("Jul") -> 7;
month("Aug") -> 8;
month("Sep") -> 9;
month("Oct") -> 10;
month("Nov") -> 11;
month("Dec") -> 12.

strip(Str) ->
    lists:foldl(fun(Char, Acc) -> string:strip(Acc, both, Char) end, Str, [$\r, $\n]).

merge([]) -> ok;
merge([{_,[R|_]} | _ ] = Handlers) -> 
    MinIdx = min_idx(
        1, 
        {1, replayer_utils:request_timestamp(R)}, 
        Handlers),
    %io:format("MinIdx:~p Handlers:~p~n", [MinIdx, Handlers]),
    {Request, NewHandlers} = get_and_read_next(MinIdx, Handlers),
    %io:format("Request: ~p~n", [element(2,Request)]),
    disk_log:log(?TARGET, Request),
    merge(NewHandlers).

min_idx(_CurIdx, {MinIdx, _Min}, []) -> MinIdx;
min_idx(CurIdx, {_MinIdx, Min} = M, [{_, [R|_]} | Rest]) ->
    case replayer_utils:request_timestamp(R) of 
        Cur when Cur < Min -> min_idx(CurIdx+1, {CurIdx, Cur}, Rest);
        _ -> min_idx(CurIdx+1, M, Rest)
    end.

get_and_read_next(Idx, Handlers) ->
    {_, Request, NewHandlers} = lists:foldl(fun
        ({ContFun, [R|Rs]}, {N, _, Acc}) when N == Idx ->  % found handler with the minimum el
            NewAcc = case Rs of   % check if we have more items in the chunk
                [] -> 
                    %io:format("Handlers:~p~nIdx=~p~nRs=~p~n", [Handlers, Idx, Rs]),
                    case ContFun() of   % no more elements read. Need next chunk
                        eof -> Acc;  % end of file, nothing to pass through
                        {_ContFun2, _NextRs} = El -> [El|Acc]
                    end;
                _ -> [{ContFun,Rs}|Acc]   % have enough elements from the previos chunk to continue working
            end,
            %io:format("Acc:~p~nNewAcc:~p~n", [Acc, NewAcc]),
            {N+1, R, NewAcc};
        (El, {N, R, Acc}) -> {N+1, R, [El|Acc]}
    end, {1, undefined, []}, Handlers),
    {Request, NewHandlers}.
