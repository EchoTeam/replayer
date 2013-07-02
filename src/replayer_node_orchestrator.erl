-module(replayer_node_orchestrator).

-export([
    start_link/0,
    append_task/1,
    create_meta/1,
    create_task_file/0,
    close_task_file/0,
    read_meta/0,
    replay/2,
    request/1
]).

-define(WORKER, replayer_worker).

-define(SLEEP_THRESHOLD, -1000). % in microsec, = 1ms
-define(EXIT_THRESHOLD, 1000000). % in microsec = 1sec

disk_log_file() ->
    "/tmp/" ++ atom_to_list(?MODULE) ++ "." ++ atom_to_list(node()) ++ ".disk_log".

meta_file() ->
    "/tmp/" ++ atom_to_list(?MODULE) ++ "." ++ atom_to_list(node()) ++ ".meta".

append_task(Req) ->
    disk_log:log(disk_log_file(), Req).

create_task_file() ->
    error_logger:info_msg("~p: creating ~p~n", [node(), disk_log_file()]),
    File = disk_log_file(),
    {ok, File} = disk_log:open([
            {name, File},
            {linkto, none},
            {file, File}
        ]),
    disk_log:truncate(File),
    {ok, File}.

close_task_file() ->
    error_logger:info_msg("~p: closing ~p~n", [node(), disk_log_file()]),
    ok = disk_log:close(disk_log_file()).

create_meta(Meta) ->
    MetaFile = meta_file(),
    file:write_file(MetaFile, io_lib:fwrite("~p.\n", [Meta])).

read_meta() ->
    MetaFile = meta_file(),
    {ok, [Meta]} = file:consult(MetaFile),
    Meta.

start_link() ->
    {ok, PoolSize} = application:get_env(worker_pool_size),
    poolboy:start([
            {name, {local, ?MODULE}},
            {size, PoolSize},
            {worker_module, ?WORKER},
            {max_overflow, 0}
        ]).

replay(RealStartTs, Speed) ->
    File = disk_log_file(),
    {ok, _} = disk_log:open([
            {name, File},
            {file, File},
            {mode, read_only}
        ]),

    try
        Meta = read_meta(),
        LogStartTs = proplists:get_value(start_time, Meta),
        TasksCount = proplists:get_value(tasks_count, Meta),
        replayer_stats:flush(),
        replayer_stats:set_tasks_count(TasksCount),
        replayer_utils:with_chunks(
            File,
            fun(Chunks, _Acc) ->
                    replay_requests(Chunks, RealStartTs, LogStartTs, Speed)
                end,
            disk_log:chunk(File, start),
            undefined),
        ok
    after
        disk_log:close(File)
    end.

replay_requests(Reqs, RealStartTs, LogStartTs, Speed) ->
    lists:map(fun(Req) ->
        Ts = replayer_utils:request_timestamp(Req),
        LogDiffTs = timer:now_diff(Ts, LogStartTs),
        RealDiffTs = timer:now_diff(now(), RealStartTs),
        case RealDiffTs - trunc(LogDiffTs/Speed) of
            Diff when Diff < ?SLEEP_THRESHOLD ->
                ToSleep = -Diff div 1000,
                timer:sleep(ToSleep),
                request(Req);
            Diff when Diff < ?EXIT_THRESHOLD ->
                request(Req);
            _ ->
                error(too_slow_replayer)
        end
    end, Reqs),
    ok.

request(Req) ->
    case poolboy:checkout(?MODULE, false) of
        Error when Error == full; Error == overflow ->%error(not_enough_workers)
            replayer_stats:bump(tasks_failed),
            error_logger:warning_msg("Not enough workers~n");
        Worker -> 
            proc_lib:spawn(fun() ->
                try 
                    replayer_stats:bump(tasks_processed),
                    case replayer_worker:request(Worker, Req) of
                        {ok, Status} ->
                            replayer_stats:bump({reply_statuses, Status});
                        {error, _} ->
                            replayer_stats:bump(reply_errors)
                    end
                after
                    ok = poolboy:checkin(?MODULE, Worker)
                end
            end)
    end.
