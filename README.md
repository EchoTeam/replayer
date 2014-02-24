Requests Replayer
=================

The replayer can re-issue the previously captured set of HTTP or erlang rpc 
requests with adjustable request rate relative to the captured rate.

To replay the requests you need
-------------------------------
* the requests themselves (use [replayer_logger](https://github.com/EchoTeam/replayer_logger) to store them)
* the cluster to repeat all the requests (with configurable option 'number of workers' on each node in the cluster),
* main cluster to handle all the requests
* the event_replayer application :)


Quick start
-----------

1. Start event_replayer application on all the nodes 
```
erlang> application:start(event_replayer).
or
make run
```

2. Given all the logs in different formats, merge them together into a unified format file, sorting by the request time
```
erlang> tasks_gatherer:merge_logs(
          [
              {http_log, "/test/http_requests.log",
                              [{filter, {"appkey", "test-1.js-kit.com"}}]},
              {disk_log, "/test/submit_requests.log"},
              {disk_log, "/test/users_update_requests.log"},
              {disk_log, "/test/search_requests.log"}
          ],
          "/test/merged.log",
          [{change_host, "foo.bar"}]
      ).
```

3. Prepare the cluster
 * Change the ring (if you haven't specified it in the environment before the application start)
```
erlang> replayer_controller:change_ring(['node1@host1', 'node2@host2', 'node3@host3']).
```
 * Specify the file with the requests
```
erlang> replayer_controller:change_tasks_file("/tmp/merged.log").
```
 * Split all the requests between cluster nodes, send the requests to the local node files
```
erlang> replayer_controller:prepare().
```

4. Replay all the requests with the pre-set speed.
```
erlang> replayer_controller:replay().
```

5. You could optionally change the pool size of worker processes on worker nodes (default pool size = 5) or the replay speed (default speed corresponds to capture rate).
```
erlang> replayer_controller:change_workers_num(100).
erlang> replayer_controller:set_replay_speed(10.0).  % 10x acceleration
```

6. As all the preparations have been made before, you could continue replaying with different request rates.
```
erlang> replayer_controller:replay(10.0).
erlang> replayer_controller:replay(20.0).
erlang> replayer_controller:replay(50.0).
```


Behind the scene
----------------

You could have your own log format, and to use it you should specify your own parser.
`src/event_replayer.app.src` specifies the environment:
```
{env, [
    {worker_pool_size, 5},
    {log_parsers, [
        {disk_log, disk_log_parser},
        {csv_log,  csv_log_parser},
        {http_log, http_log_parser} ]}
  ]}
```


Your parser should have a **log_parser** behaviour, i.e. implement the **reader** function:
```
-type continuation_fun() :: fun(() -> eof | {continuation_fun(), [request()]}).
-spec reader(File :: string(), Options :: [option()]) -> {continuation_fun(), [request()]}.
```


Replaying erlang rpc requests
----------------------

To replay the rpc calls you have to have not only MFA in the saved requests but also some information to found out the node where the MFA should be called.
That could be done in two ways: first, you could have the node name itself saved in the request. Second, you could store some node information (for example, cluster name). In order to get the node name from that information you should specify the handler function for the node_info term. You should also have some connector node in the main cluster where that function should be called.

In short, to replay erlang rpc requests you should:
* have access from the replayer cluster to the main cluster.
* specify the node name in each request 
* or specify some node information, have handler functions in the environment for that node information. And have connector node to call that function on.

Example of the environment:
```
{env, [
    {connector_node, user@host},
    {node_info_handlers, [
        {vrc, {echo_view_config, vrnodes_by_cname}},
        {vuc, {echo_view_config, vunodes_by_cname}}
    ]
}
```

Example of the saved requests:
```
[
{{rpc,call}, {1376,734754,299787}, {user@host, {erlang, node, []}}},
{{rpc,call}, {1376,734754,299787}, {{vrc, cluster1}, {erlang, node, []}}}
].
```
