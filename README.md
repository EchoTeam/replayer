Requests Replayer
=================

The idea is quite simple: you somehow log all the incoming events. And then replay all the gathered events, at the same speed or some acceleration/deceleration.

To replay the requests you need:
-------------------------------
* the requests themselves. They should be in a disk_log file format, with the entries with the following type:
** {Type :: get, Timestamp :: erlang:timestamp(), URL :: string()}
** {Type :: post, Timestamp :: erlang:timestamp(), URL :: string(), Body :: binary()}
* the cluster to repeat all the requests (with configurable option 'number of workers' on each node in the cluster)


Erlang commands:
----------------
* replayer_controller:start_link([{ring, ['gli2@dhcp-154', 'gli3@dhcp-154']}, {tasks_file, "./test/merged.log"}]).
* replayer_controller:prepare().
* replayer_controller:replay(10.0).
