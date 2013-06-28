-module(replayer_utils).

-export([
    request_timestamp/1,
    set_request_timestamp/2,
    request_url/1,
    set_request_url/2,
    request_body/1,
    set_request_body/2,
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

set_request_timestamp({get, _, U}, Ts) -> {get, Ts, U};
set_request_timestamp({post, _, U, B}, Ts) -> {post, Ts, U, B}.

request_url({get, _, Url}) -> Url;
request_url({post, _, Url, _}) -> Url.

set_request_url({get, Ts, _}, U) -> {get, Ts, U};
set_request_url({post, Ts, _, B}, U) -> {post, Ts, U, B}.

request_body({post, _, _, B}) -> B.

set_request_body({post, Ts, U, _}, B) -> {post, Ts, U, B}.


with_chunks(_LogName, _Fun, eof, _Acc) -> nop;
with_chunks(LogName, Fun, {Cont, Chunks}, Acc) ->
    with_chunks(LogName, Fun, {Cont, Chunks, undefined}, Acc);
with_chunks(LogName, Fun, {Cont, Chunks, _Badbytes}, Acc) ->
    NewAcc = Fun(Chunks, Acc),
    with_chunks(LogName, Fun, disk_log:chunk(LogName, Cont), NewAcc).

