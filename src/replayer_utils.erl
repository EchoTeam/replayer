-module(replayer_utils).

-export([
    request_timestamp/1,
    with_chunks/4
]).

-export_type([
    request/0
]).

-type request() :: 
    {get,  Ts :: erlang:timestamp(), URL :: string()} |
    {post, Ts :: erlang:timestamp(), URL :: string(), Body :: binary()}.

-spec request_timestamp(request()) -> erlang:timestamp().
request_timestamp({get, Ts, _}) -> Ts;
request_timestamp({post, Ts, _, _}) -> Ts.

with_chunks(_LogName, _Fun, eof, _Acc) -> nop;
with_chunks(LogName, Fun, {Cont, Chunks}, Acc) ->
    with_chunks(LogName, Fun, {Cont, Chunks, undefined}, Acc);
with_chunks(LogName, Fun, {Cont, Chunks, _Badbytes}, Acc) ->
    NewAcc = Fun(Chunks, Acc),
    with_chunks(LogName, Fun, disk_log:chunk(LogName, Cont), NewAcc).

