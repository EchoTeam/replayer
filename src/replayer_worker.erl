-module(replayer_worker).

-export([
    append_task/1,
    create_task_file/0,
    close_task_file/0
]).

disk_log_file() ->
    "/tmp/" ++ atom_to_list(?MODULE) ++ "." ++ atom_to_list(node()) ++ ".disk_log".

append_task(Req) ->
    disk_log:log(disk_log_file(), Req).

create_task_file() ->
    io:format("~p: creating ~p~n", [node(), disk_log_file()]),
    File = disk_log_file(),
    disk_log:open([
            {name, File},
            {file, File},
            {repair, truncate}
        ]).

close_task_file() ->
    io:format("~p: closing ~p~n", [node(), disk_log_file()]),
    disk_log:close(?MODULE).
