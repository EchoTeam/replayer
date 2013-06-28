-module(tasks_gatherer).

-export([
    merge_logs/2,
    log_map/3
]).

-define(TARGET, merger_target).

-type task_type() :: disk_log | csv_log.
-type task() :: {Type :: task_type(), FileName :: string()}.

% tasks_gatherer:merge_logs(
%               [
%                   {disk_log, "./test/submit_requests.log"},
%                   {disk_log, "./test/users_requests.log"},
%                   {disk_log, "./test/search_requests.log"},
%                   {disk_log, "./test/mux_requests.log"},
%                   {http_log, "/tmp/coser.1000.log"}
%               ],
%               "./test/merged.log"
%           ).
-spec merge_logs(From :: [task()], To :: string()) -> ok | {error, Why :: string()}.
merge_logs(Tasks, FileName) ->
    Files = [ F || {_, F} <- Tasks ],
    io:format("The following logs are going to be merged into the ~p file:~n~p~n", [FileName, Files]),
    case confirm() of
        true -> 
            try merge_logs_ll(Tasks, FileName) 
            catch C:R ->
                io:format("ERROR: failed due to error:~p~nStack:~p~n",
                                            [{C,R}, erlang:get_stacktrace()]),
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
    case disk_log:open([{name, ?TARGET}, {file, File}]) of
        {ok, ?TARGET} -> nop;
        {repaired, ?TARGET, _, _} -> nop;
        Error -> throw({open_target_file, Error})
    end,
    disk_log:truncate(?TARGET),
    ok.

close_target_file() ->
    disk_log:close(?TARGET).

get_log_processor(FileType) ->
    AllProcessors = case application:get_env(event_replayer, log_processors) of
        undefined -> [];
        {ok, All} -> All
    end,
    case proplists:get_value(FileType, AllProcessors) of 
        undefined -> throw("no registered processor for the log of type " 
                ++ atom_to_list(FileType) 
                ++ ". Specify the one in application env, see README");
        V -> V
    end.
    
% 'reader' function returns a list of item + continuation function, which should be run
% every time for the next portion of items.
open_tasks(Tasks) ->
    lists:map(fun ({FileType, File}) -> 
                Processor = get_log_processor(FileType),
                Processor:reader(File)
        end, Tasks).

merge([]) -> ok;
merge([{_,[R|_]} | _ ] = Handlers) -> 
    MinIdx = min_idx(
        1, 
        {1, replayer_utils:request_timestamp(R)}, 
        Handlers),
    {Request, NewHandlers} = get_and_read_next(MinIdx, Handlers),
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
        ({ContFun, [R|Rs]}, {N, _, Acc}) when N == Idx ->
            % found handler with the minimum el
            NewAcc = case Rs of   % check if we have more items in the chunk
                [] -> 
                    case ContFun() of   % no more elements read. Need next chunk
                        eof -> Acc;  % end of file, nothing to pass through
                        {_ContFun2, _NextRs} = El -> [El|Acc]
                    end;
                %have enough elements from the previos chunk to continue working
                _ -> [{ContFun,Rs}|Acc]
            end,
            {N+1, R, NewAcc};
        (El, {N, R, Acc}) -> {N+1, R, [El|Acc]}
    end, {1, undefined, []}, Handlers),
    {Request, NewHandlers}.

log_map_walk(LogFrom, LogTo, Cont, Fun) ->
    case disk_log:chunk(LogFrom, Cont, 1) of 
        {Cont2, [El_]} -> 
            El = Fun(El_),
            disk_log:log(LogTo, El), 
            log_map_walk(LogFrom, LogTo, Cont2, Fun);
        eof -> ok
    end.

log_map(LogFrom, LogTo, Fun) ->
    disk_log:open([{name, LogFrom}, {file, LogFrom}, {mode, read_only}]),
    disk_log:open([{name, LogTo}, {file, LogTo}]),
    log_map_walk(LogFrom, LogTo, start, Fun),
    disk_log:close(LogFrom),
    disk_log:close(LogTo),
    ok.
