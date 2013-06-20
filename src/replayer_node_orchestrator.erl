-module(replayer_node_orchestrator).

-export([
    start_link/1,
    append_task/1,
    create_task_file/0,
    close_task_file/0,
    replay/2,
    request/1
]).

-define(WORKER, replayer_worker).

disk_log_file() ->
    "/tmp/" ++ atom_to_list(?MODULE) ++ "." ++ atom_to_list(node()) ++ ".disk_log".

append_task(Req) ->
    disk_log:log(disk_log_file(), Req).

create_task_file() ->
    io:format("~p: creating ~p~n", [node(), disk_log_file()]),
    File = disk_log_file(),
    {ok, _} = disk_log:open([
            {name, File},
            {linkto, none},
            {file, File},
            {repair, truncate}
        ]).

close_task_file() ->
    io:format("~p: closing ~p~n", [node(), disk_log_file()]),
    ok = disk_log:close(disk_log_file()).


start_link(PoolSize) ->
    poolboy:start([
                {name, {local, ?MODULE}},
                {size, PoolSize},
                {worker_module, ?WORKER}
            ]).

replay(RealStartTs, Speed) ->
    File = disk_log_file(),
    {ok, _} = disk_log:open([
            {name, File},
            {file, File},
            {mode, read_only}
        ]),

    FirstChunk = disk_log:chunk(File, start, 1),
    {Cont, LogStartTs} = case FirstChunk of
        {C, [{start_time, Ts}]} -> {C, Ts};
        _ -> throw("first log in a file should be {start_time, Ts}")
    end,
    replayer_controller:with_chunks(
        File,
        fun(Chunks, _Acc) -> replay_requests(Chunks, RealStartTs, LogStartTs, Speed) end,
        disk_log:chunk(File, Cont),
        undefined),
    ok.

-define(SLEEP_THRESHOLD, -1000). % in microsec
-define(EXIT_THRESHOLD, 1000000). % in microsec

replay_requests(Reqs, RealStartTs, LogStartTs, Speed) ->
    lists:map(fun(Req) ->
            Ts = tasks_gatherer:request_timestamp(Req),
            LogDiffTs = timer:now_diff(Ts, LogStartTs),
            RealDiffTs = timer:now_diff(now(), RealStartTs),
            case RealDiffTs - trunc(LogDiffTs/Speed) of
                Ts when Ts < ?SLEEP_THRESHOLD -> 
                    timer:sleep(-Ts*1000),
                    request(Req);
                Ts when Ts < ?EXIT_THRESHOLD ->
                    request(Req);
                _ ->
                    error(too_slow_replayer)
            end
        end, Reqs),
    ok.

request(Req) ->
    case poolboy:checkout(?MODULE) of
        full -> error(not_enough_workers);
        Worker -> 
            try
                replayer_worker:request(Worker, Req)
            after
                ok = poolboy:checkin(?MODULE, Worker)
            end
    end.
