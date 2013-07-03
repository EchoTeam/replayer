-module(replayer_utils).

-export([
    query_params_of_request/1,
    request_body/1,
    request_timestamp/1,
    request_url/1,
    set_request_body/2,
    set_request_timestamp/2,
    set_request_url/2,
    ts_of_string/1,
    unixtimestamp/0,
    unixtimestamp/1,
    with_chunks/4
]).

-export_type([
    request/0
]).

-include_lib("yaws/include/yaws.hrl").
-include_lib("yaws/include/yaws_api.hrl").

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


with_chunks(_LogName, _Fun, eof, Acc) -> Acc;
with_chunks(LogName, Fun, {Cont, Chunks}, Acc) ->
    with_chunks(LogName, Fun, {Cont, Chunks, undefined}, Acc);
with_chunks(LogName, Fun, {Cont, Chunks, _Badbytes}, Acc) ->
    NewAcc = Fun(Chunks, Acc),
    with_chunks(LogName, Fun, disk_log:chunk(LogName, Cont), NewAcc).

-spec query_params_of_request(request()) -> {ok, [{Key :: string(), Value :: string()}]} | {error, Reason :: term()}.
query_params_of_request({get, _, Url}) ->
    query_params_of_request_ll(Url, undefined);
query_params_of_request({post, _, Url, Body}) ->
    query_params_of_request_ll(Url, Body).

query_params_of_request_ll(Url, Body) ->
    case http_uri:parse(Url) of
        {ok, {_Scheme, _UserInfo, _Host, _Port, _Path, "?" ++ Query}} ->
            GP = httpd:parse_query(Query),
            Req = #http_request{method = 'POST'},
            PP = yaws_api:parse_post(#arg{clidata = Body, req = Req}),
            {ok, GP ++ PP};
        {ok, {_Scheme, _UserInfo, _Host, _Port, _Path, _Query}} ->
            Req = #http_request{method = 'POST'},
            PP = yaws_api:parse_post(#arg{clidata = Body, req = Req}),
            {ok, PP};
        {error, Reason} -> {error, Reason}
    end.

ts_of_string(S) ->
    {Double, []} = case string:to_float(S) of
        {error, no_float} -> string:to_integer(S);
        V -> V
    end,
    Int = trunc(Double),
    MicroS = trunc(1000000 * (Double - Int)),
    {Int div 1000000, Int rem 1000000, MicroS}.

unixtimestamp() ->
    unixtimestamp(os:timestamp()).

unixtimestamp({M, S, U}) -> M*1000000 + S + U/1000000.
