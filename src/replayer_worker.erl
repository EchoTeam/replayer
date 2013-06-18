-module(replayer_worker).

-export([
    append_task/1,
    create_task_file/0,
    close_task_file/0
]).

disk_log_file() ->
    "/tmp/" ++ atom_to_list(?MODULE) ++ ".disk_log".

append_task(Req) ->
    disk_log:log(Req).

create_task_file() ->
    disk_log:open([
            {name, ?MODULE},
            {file, disk_log_file()},
            {repair, truncate}
        ]).

close_task_file() ->
    disk_log:close(?MODULE).
