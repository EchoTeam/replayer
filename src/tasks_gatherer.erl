%%% vim: set ts=4 sts=4 sw=4 et:
-module(tasks_gatherer).

-export([
    merge_logs/2,
    merge_logs/3,
    test/0,
    traverse_task_files/3
]).

-define(TARGET, merger_target).

-type task_type() :: disk_log | csv_log | http_log.
-type option() :: {filter, {Parameter :: string(), Value :: string()}} | {change_host, Host :: string()}.
-type task() :: {Type :: task_type(), FileName :: string()} | {Type :: task_type(), FileName :: string(), Options :: [option()]}.

% tasks_gatherer:merge_logs(
%               [
%                   {disk_log, "./test/submit_requests.log"},
%                   {disk_log, "./test/users_requests.log"},
%                   {disk_log, "./test/search_requests.log"},
%                   {disk_log, "./test/mux_requests.log"},
%                   {http_log, "/tmp/coser.1000.log",
%                                   [{filter, {"appkey", "test-1.js-kit.com"}}]}
%               ],
%               "./test/merged.log",
%               [{change_host, "foo.bar"}]
%           ).
-spec merge_logs(From :: [task()], To :: string(), Options :: [option()]) -> ok | {error, Why :: string()}.
merge_logs(Tasks, FileName) ->
	merge_logs(Tasks, FileName, []).

merge_logs(Tasks, FileName, Options) ->
    NormTasks = [case Task of
                    {Type, File} -> {Type, File, []};
                    V -> V
                end || Task <- Tasks],
    Files = [ F || {_, F, _} <- NormTasks ],
    io:format("The following logs are going to be merged into the ~p file:~n~p~n", [FileName, Files]),
    case confirm() of
        true -> 
            try merge_logs_ll(NormTasks, FileName, Options)
            catch C:R ->
                error_logger:error_msg("ERROR: failed due to error:~p~nStack:~p~n", [{C,R}, erlang:get_stacktrace()]),
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

merge_logs_ll(Tasks, FileName, Options) ->
    ok = open_target_file(FileName),
    traverse_task_files(Tasks, fun merge/2, Options),
    close_target_file(),
    ok.

merge(Request, Options) ->
    NewRequest = maybe_override_request(Request, Options),
    disk_log:log(?TARGET, NewRequest),
    Options.

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

get_log_parser(FileType) ->
    AllProcessors = replayer_utils:get_env(log_parsers, []),
    case proplists:get_value(FileType, AllProcessors) of 
        undefined -> throw("no registered processor for the log of type " 
                ++ atom_to_list(FileType) 
                ++ ". Specify the one in application env, see README");
        V -> V
    end.
    
% 'reader' function returns a list of item + continuation function, which should be run
% every time for the next portion of items.
open_tasks(Tasks) ->
    lists:map(fun ({FileType, File, Options}) ->
                Processor = get_log_parser(FileType),
                Processor:reader(File, Options)
        end, Tasks).

traverse_task_files(Tasks, Fun, Acc) ->
    OpenedTasks = open_tasks(Tasks),
    traverse_task_files_ll(OpenedTasks, Fun, Acc).

traverse_task_files_ll([], _Fun, Acc) -> Acc;
traverse_task_files_ll([eof | Rest], Fun, Acc) ->
    traverse_task_files_ll(Rest, Fun, Acc);
traverse_task_files_ll([{_,[R|_]} | _ ] = Handlers, Fun, Acc) -> 
    MinIdx = min_idx(
        1, 
        {1, replayer_utils:request_timestamp(R)}, 
        Handlers),
    {Request, NewHandlers} = get_and_read_next(MinIdx, Handlers),
    NewAcc = Fun(Request, Acc),
    traverse_task_files_ll(NewHandlers, Fun, NewAcc).

min_idx(_CurIdx, {MinIdx, _Min}, []) -> MinIdx;
min_idx(CurIdx, {_MinIdx, Min} = M, [{_, [R|_]} | Rest]) ->
    case replayer_utils:request_timestamp(R) of 
        Cur when Cur < Min -> min_idx(CurIdx+1, {CurIdx, Cur}, Rest);
        _ -> min_idx(CurIdx+1, M, Rest)
    end;
min_idx(CurIdx, {_MinIdx, _Min} = M, [eof | Rest]) ->
    min_idx(CurIdx+1, M, Rest).

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
        (eof, {N, R, Acc}) -> {N+1, R, Acc};
        (El, {N, R, Acc}) -> {N+1, R, [El|Acc]}
    end, {1, undefined, []}, Handlers),
    {Request, NewHandlers}.

maybe_override_request(Request, Options) ->
    Request1 = case proplists:get_value(change_host, Options) of
        undefined -> Request;
        NewHost -> change_request_host(Request, NewHost)
    end,
    case proplists:get_value(change_cname, Options) of
        undefined -> Request1;
        NewCName -> change_request_cname(Request1, NewCName)
    end.

change_request_host({get, TS, URL}, NewHost) ->
    {get, TS, change_url_host(URL, NewHost)};
change_request_host({post, TS, URL, Body}, NewHost) ->
    {post, TS, change_url_host(URL, NewHost), Body}.

change_request_cname({{rpc, call}, Ts, {{vrc, _CName},   {M, F, A}}}, NewCName) ->
                     {{rpc, call}, Ts, {{vrc, NewCName}, {M, F, A}}};
change_request_cname({{rpc, call}, Ts, {{vuc, CName},    {echo_vuc, call, [CName,    ViewId, M, F, A]}}}, NewCName) ->
                     {{rpc, call}, Ts, {{vuc, NewCName}, {echo_vuc, call, [NewCName, ViewId, M, F, A]}}};
change_request_cname(Request, _NewCName) -> Request.

change_url_host(URL, NewHost) ->
    case re:run(URL, "^(https?://)([^/]*)(/.*)$", [{capture, [1,2,3], list}]) of
        {match, [Proto, _Host, Rest]} -> Proto ++ NewHost ++ Rest;
        _ -> URL
    end.

test() ->
    SampleData = test_log_parser:test_sample_data(),
    Tasks = [
        {test_log, empty, [{filter, {"filter0", "true"}}]},
        {test_log, 1, [{filter, {"filter1", "true"}}]},
        {test_log, empty, [{filter, {"filter0", "true"}}]},
        {test_log, 2, [{filter, {"filter2", "true"}}]},
        {test_log, empty, [{filter, {"filter0", "true"}}]},
        {test_log, 3, [{filter, {"filter3", "true"}}]},
        {test_log, empty, [{filter, {"filter0", "true"}}]},
        {test_log, 4, [{filter, {"filter4", "true"}}]}
    ],
    T = fun(Req, Acc) -> [Req | Acc] end,
    TraversedData = traverse_task_files(Tasks, T, []),
    SampleData = lists:reverse(TraversedData).
