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

    try
        FirstChunk = disk_log:chunk(File, start, 1),
        %io:format("FirstChunk:~p~n", [element(2,FirstChunk)]),
        {Cont, LogStartTs} = case FirstChunk of
            {C, [{start_time, Ts}]} -> {C, Ts};
            _ -> throw("first log in a file should be {start_time, Ts}")
        end,
        replayer_utils:with_chunks(
            File,
            fun(Chunks, _Acc) -> replay_requests(Chunks, RealStartTs, LogStartTs, Speed) end,
            disk_log:chunk(File, Cont),
            undefined),
        ok
    after
        disk_log:close(File)
    end.

-define(SLEEP_THRESHOLD, -1000). % in microsec
-define(EXIT_THRESHOLD, 1000000). % in microsec

replay_requests(Reqs, RealStartTs, LogStartTs, Speed) ->
    lists:map(fun(Req) ->
            %io:format("Req:~p~n", [Req]),
            Ts = replayer_utils:request_timestamp(Req),
            LogDiffTs = timer:now_diff(Ts, LogStartTs),
            RealDiffTs = timer:now_diff(now(), RealStartTs),
            %io:format("~n ~p - ~p -> LogDiffTs:~p~n", [Ts, LogStartTs, LogDiffTs]),
            %io:format(" ~p - ~p -> RealDiffTs:~p~n", [now(), RealStartTs, RealDiffTs]),
            case RealDiffTs - trunc(LogDiffTs/Speed) of
                Diff when Diff < ?SLEEP_THRESHOLD -> 
                    ToSleep = -Diff div 1000,
                    io:format("sleep ~p before request~n", [ToSleep]),
                    timer:sleep(ToSleep),
                    request(Req);
                Diff when Diff < ?EXIT_THRESHOLD ->
                    io:format("instant request ~n"),
                    request(Req);
                _ ->
                    io:format("res3~n"),
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
