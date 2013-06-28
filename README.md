Requests Replayer
=================

First, you log all the incoming http requests.
Then you can replay all the gathered events, at the same speed or with some acceleration/deceleration.

To replay the requests you need
-------------------------------
* the requests themselves.
* the cluster to repeat all the requests (with configurable option 'number of workers' on each node in the cluster)
* the event_replayer application :)


Quick start
-----------

1. Start event_replayer application on all the nodes 
    > application:start(event_replayer).

2. Given all the logs in different formats, merge them together, sorting by the request time
    > tasks_gatherer:merge_logs(
              [
                  {disk_log, "/test/submit_requests.log"},
                  {disk_log, "/test/users_update_requests.log"},
                  {disk_log, "/test/search_requests.log"},
                  {http_log, "/test/http_requests.log"}
              ],
              "/test/merged.log")

3. Prepare the cluster
 * Change the ring (if you haven't specified it in the environment before the application start)
    > replayer_controller:change_ring(['node1@host1', 'node2@host2', 'node3@host3']).
 * Specify the file with the requests
    > replayer_controller:change_tasks_file("/tmp/merged.log").
 * Split all the requests between cluster nodes, send the requests to the local node files
    > replayer_controller:prepare().

4. Replay all the requests with the acceleration koefficient
    > replayer_controller:replay(10.0). % with 10 times acceleration

5. You could optionally change the pool size of worker processes on worker nodes (default pool size = 5)
    > replayer_controller:change_workers_num(100).

6. As all the preparations have been made before, you could go on replaying with different speed as long as you need to
    > replayer_controller:replay(10.0).
    > replayer_controller:replay(20.0).
    > replayer_controller:replay(50.0).


Behind the scene
----------------

You could have your own log format, and to use it you should specify your own parser.
src/event_replayer.app.src specifies the environment:
    {env, [
        {worker_pool_size, 5},
        {log_parsers, [
            {disk_log, disk_log_parser},
            {csv_log,  csv_log_parser},
            {http_log, http_log_parser} ]}
      ]}


Your parser should have a **log_parser** behaiour, i.e. implement the **reader** function:
    -type request() ::
        {get,  Ts :: erlang:timestamp(), URL :: string()} |
        {post, Ts :: erlang:timestamp(), URL :: string(), Body :: binary()}.

    -type continuation_fun() :: fun(() -> eof | {continuation_fun(), [request()]}).
    -spec reader(File :: string()) -> {continuation_fun(), [request()]}.
