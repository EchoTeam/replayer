-module(disk_log_processor).
-behaviour(log_parser).

-export([reader/1]).

-type continuation_fun() :: fun(() -> eof | {continuation_fun(), [replayer_utils:request()]}).
-spec reader(File :: string()) -> {continuation_fun(), [replayer_utils:request()]}.
reader(File) ->
    disk_log:open([{name, File}, {file, File}, {mode, read_only}]),
    Fun = reader_fun(File, start),
    Fun().

reader_fun(Name, Continuation) ->
    fun() ->
            case disk_log:chunk(Name, Continuation) of
                {Continuation2, Requests} -> 
                    {reader_fun(Name, Continuation2), Requests};
                eof -> 
                    disk_log:close(Name),
                    eof;
                Error ->
                    throw({"Error while reading log", Name, Error})
            end
    end.
