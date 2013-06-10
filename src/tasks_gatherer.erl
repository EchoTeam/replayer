-module(tasks_gatherer).

-export([merge_logs/2]).

-define(TARGET, merger_target).

-type task_type() :: disk_log | csv_log.
-type task() :: {Type :: task_type(), FileName :: string()}.
-type request() :: 
    {get,  Ts :: erlang:timestamp(), URL :: string()} |
    {post, Ts :: erlang:timestamp(), URL :: string(), Body :: binary()}.


request_timestamp({get, Ts, _}) -> Ts;
request_timestamp({post, Ts, _, _}) -> Ts.


-spec merge_logs(From :: [task()], To :: string()) -> ok | {error, Why :: string()}.
merge_logs(Tasks, FileName) ->
    Files = [ F || {_, F} <- Tasks ],
    io:format("The following logs are going to be merged into the ~p file:~n~p~n", [FileName, Files]),
    case confirm() of
        true -> 
            try merge_logs_ll(Tasks, FileName) 
            catch C:R ->
                io:format("ERROR: failed due to error:~p~n", [{C,R}]),
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
    ok.

open_target_file(File) ->
    {ok, ?TARGET} = disk_log:open({name, ?TARGET}, {file, File}, {repair, truncate}),
    ok.


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
    {disk_log_reader_fun(File, start), []}.

disk_log_reader_fun(Name, Continuation) ->
    fun() ->
            case disk_log:chunk(Name, Continuation) of
                {Continuation2, Items} -> 
                    Requests = lists:map(fun disk_log_item_process/1, Items),
                    {disk_log_reader_fun(Name, Continuation2), Requests};
                eof -> eof;
                Error -> throw({"Error while reading log", Name, Error})
            end
    end.

csv_log_reader(File) -> 
    {ok, FH} = file:open(File, [read, binary]),
    {csv_log_reader_fun({File, FH}, undefined), []}.

            
csv_log_reader_fun({Name, FH}, _Cont) ->
    fun() ->
            case io:get_line(FH, '') of
                {error, Error} -> throw({"Error while reading log", Name, Error});
                eof -> eof;
                "\n" -> csv_log_reader_fun({Name, FH}, undefined);
                "\r\n" -> csv_log_reader_fun({Name, FH}, undefined);
                Data -> 
                    Items = string:tokens(strip(binary_to_list(Data)), [$;]),
                    Requests = lists:map(fun csv_log_item_process/1, Items),
                    {csv_log_reader_fun({Name, FH}, undefined), Requests}
            end
    end.

-spec disk_log_item_process({string(), erlang:timestamp(), binary()}) -> request().
disk_log_item_process({Endpoint, Ts, Data}) ->
    {match, [E]} = re:run(Endpoint, "/get/api/(.*)", [{capture, [1], list}]),
    Url = "http://api.aboutecho.com/" ++ E,
    {post, Ts, Url, Data}.

-spec csv_log_item_process([string()]) -> request().
csv_log_item_process([Ts,Url]) ->
    {get, Ts, Url}.

strip(Str) ->
    lists:foldl(fun(Char, Acc) -> string:strip(Acc, Char) end, Str, [$\r, $\n]).

merge([{_,[R|_]} | _ ] = Handlers) -> 
    MinIdx = min_idx(1, request_timestamp(R), Handlers),
    {Request, NewHandlers} = get_and_read_next(MinIdx, Handlers),
    disk_log:log(?TARGET, Request),
    merge(NewHandlers),
    ok.

min_idx(Idx, Min, [{_, [R|_]} | Rest]) ->
    case request_timestamp(R) of 
        Cur when Cur < Min -> min_idx(Idx+1, Cur, Rest);
        _ -> min_idx(Idx+1, Min, Rest)
    end.

get_and_read_next(Idx, Handlers) ->
    {_, Request, NewHandlers} = lists:foldl(fun
        ({ContFun, [R|Rs]}, {N, _, Acc}) when N == Idx ->  % found handler with the minimum el
            NewAcc = case Rs of   % check if we have more items in the chunk
                [] -> 
                    case ContFun() of   % no more elements read. Need next chunk
                        eof -> Acc;  % end of file, nothing to pass through
                        {_ContFun2, _NextRs} = El -> [El|Acc]
                    end;
                _ -> [{ContFun,Rs}|Acc]   % have enough elements from the previos chunk to continue working
            end,
            {N+1, R, NewAcc};
        (El, {N, R, Acc}) -> {N+1, R, [El|Acc]}
    end, {1, undefined, []}, Handlers),
    {Request, NewHandlers}.
