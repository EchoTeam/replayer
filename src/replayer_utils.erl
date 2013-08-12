-module(replayer_utils).

-export([
    query_params_of_request/1,
    random_element/1,
    request_timestamp/1,
    term_to_string/1,
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

% as node_info() we use smth like {ClusterType, ClusterName} -> {vrc, default}
-type node_info() :: node() | any(). 
-type request() :: 
% BC start
    {get,  Ts :: erlang:timestamp(), URL :: string()} |
    {post, Ts :: erlang:timestamp(), URL :: string(), Body :: binary()} |
% BC end
    {{http,get}, erlang:timestamp(), {Url :: string()}} |
    {{http,post}, erlang:timestamp(), {Url :: string(), Body :: binary()}} |
    {{rpc,call}, erlang:timestamp(), {NodeInfo :: node_info(), MFA :: mfa()}}.


-spec request_timestamp(request()) -> erlang:timestamp().
% BC start
request_timestamp({get, Ts, _}) -> Ts;
request_timestamp({post, Ts, _, _}) -> Ts;
% BC end
request_timestamp({_Type, Ts, _Opaque}) -> Ts.

with_chunks(_LogName, _Fun, eof, Acc) -> Acc;
with_chunks(LogName, Fun, {Cont, Chunks}, Acc) ->
    with_chunks(LogName, Fun, {Cont, Chunks, undefined}, Acc);
with_chunks(LogName, Fun, {Cont, Chunks, _Badbytes}, Acc) ->
    NewAcc = Fun(Chunks, Acc),
    with_chunks(LogName, Fun, disk_log:chunk(LogName, Cont), NewAcc).

-spec query_params_of_request(request()) -> {ok, [{Key :: string(), Value :: string()}]} | {error, Reason :: term()}.
% BC start
query_params_of_request({get, _, Url}) ->
    query_params_of_request_ll(Url, undefined);
query_params_of_request({post, _, Url, Body}) ->
    query_params_of_request_ll(Url, Body);
% BC end
query_params_of_request({{http,get}, _, {Url}}) ->
    query_params_of_request_ll(Url, undefined);
query_params_of_request({{http,post}, _, {Url, Body}}) ->
    query_params_of_request_ll(Url, Body).

query_params_of_request_ll(Url, Body) ->
    case yaws_api:parse_url(Url) of
        {_, _, _, _, _, Query} when Query /= [] ->
            GP = yaws_api:parse_query(#arg{querydata = Query}),
            Req = #http_request{method = 'POST'},
            PP = yaws_api:parse_post(#arg{clidata = Body, req = Req}),
            {ok, GP ++ PP};
        {_, _, _, _, _, _} ->
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

term_to_string(T) -> lists:flatten(io_lib:format("~p", [T])).
random_element(L) ->
    random:seed(now()),
    I = random:uniform(length(L)),
    lists:nth(I, L).
