-module(http_log_parser).
-behaviour(log_parser).

-export([reader/1]).

-export([parse/3]).

-type continuation_fun() :: fun(() -> eof | {continuation_fun(), [replayer_utils:request()]}).
-spec reader(File :: string()) -> {continuation_fun(), [replayer_utils:request()]}.
reader(File) -> 
    {ok, FH} = file:open(File, [read]),
    Fun = reader_fun({File, FH}),
    Fun().

-spec reader_fun({string(), file:io_device()}) -> eof | {continuation_fun(), [replayer_utils:request()]}. 
reader_fun({Name, FH}) ->
    fun() ->
            case parse(FH, waiting_ts, {get, undefined, undefined}) of
                {error, Error} -> throw({"Error while reading log", Name, Error});
                eof -> file:close(FH), eof;
                Request ->
                    %io:format("Request:~p~n", [Request]),
                    {reader_fun({Name, FH}), [Request]}
            end
    end.


-type state() :: 
        got_request        % got get or post request                                -> waiting_ts
      | waiting_ts     % previos parsing block has finished, waiting for the next   -> waiting_type_path
      | waiting_type_path % got ts, waiting for either GET or POST and path         -> get_waiting_host | post_waiting_host

        % GET
      | get_waiting_host % got path, waiting for the host                           -> get_waiting_connection_close
      | get_waiting_connection_close   % waiting for the end of the block           -> got_request

        % POST
      | post_waiting_host  % got path, waiting for the host                         -> post_waiting_connection_close
      | post_waiting_connection_close   % waiting for the end of the block          -> post_waiting_body
      | post_waiting_body  % waiting for the body of the post request               -> got_request
        .

-spec parse(FH :: file:io_device(), 
            State :: state(), 
            Acc :: replayer_utils:request()
        ) -> eof | {state(), Acc :: replayer_utils:request()} .
parse(FH, State, Acc) ->
    case io:get_line(FH, '') of
        {error, Error} -> 
            throw({error, Error});
        eof -> 
            eof;
        Str_ -> 
            Str = strip(Str_),
            case parse_ll(State, Acc, Str) of
                {got_request, NewAcc} -> NewAcc;
                {NewState, NewAcc} -> parse(FH, NewState, NewAcc)
            end
    end.

parse_ll(got_request, _, _) -> throw({error, "got_request shouldn't be used as a parsed state"});
parse_ll(State, Acc, Str) -> 
    {Re, CaptureFields} = state_re(State),
    case re:run(Str, Re, [{capture, CaptureFields, list}]) of
        nomatch -> {State, Acc};
        {match, Res} -> 
            NewAcc = apply_request_info(State, Acc, Res),
            NewState = next_state(State, Res),
            {NewState, NewAcc}
    end.

-spec state_re(state()) -> {string(), [non_neg_integer()]}.
state_re(waiting_ts)                    -> {"^[0-9]\+\.[0-9]\+$", [0]};
state_re(waiting_type_path)             -> {"^\(POST\|GET\) \([^ ]\+\) .*", [1,2]};
state_re(get_waiting_host)              -> {"^host: \([^ ]\+\)$", [1]};
state_re(get_waiting_connection_close)  -> {"^Connection: close", [0]};
state_re(post_waiting_host)             -> state_re(get_waiting_host);
state_re(post_waiting_connection_close) -> state_re(get_waiting_connection_close);
state_re(post_waiting_body)             -> {"^.\+$", [0]};
state_re(got_request)                   -> throw({error, "got_requests shouldn't be used as a parsed state"}).

-spec next_state(CurState :: state(), ParsedData :: [string()] ) -> state().
next_state(got_request, _)                      -> waiting_ts;
next_state(waiting_ts, _)                       -> waiting_type_path;
next_state(waiting_type_path, ["GET", _])       -> get_waiting_host;
next_state(waiting_type_path, ["POST", _])      -> post_waiting_host;
next_state(get_waiting_host, _)                 -> get_waiting_connection_close;
next_state(get_waiting_connection_close, _)     -> got_request;
next_state(post_waiting_host, _)                -> post_waiting_connection_close;
next_state(post_waiting_connection_close, _)    -> post_waiting_body;
next_state(post_waiting_body, _)                -> got_request.

-spec apply_request_info(
    state(), 
    PrevAcc :: replayer_utils:request(), 
    ParsedStr :: [string()]) 
        -> replayer_utils:request().
apply_request_info(got_request, _, _) -> 
    throw({error, "got_requests shouldn't be used as a parsed state"});
apply_request_info(waiting_ts, Req, [TsStr]) -> 
    {TsDouble, []} = string:to_float(TsStr),
    S = trunc(TsDouble),
    MicroS = trunc(1000000 * (TsDouble - S)),
    Ts = {S div 1000000, S rem 1000000, MicroS},
    replayer_utils:set_request_timestamp(Req, Ts);
apply_request_info(waiting_type_path, Req, [TypeStr, Path]) ->
    Ts = replayer_utils:request_timestamp(Req),
    case TypeStr of
        "GET" -> {get, Ts, Path};
        "POST" -> {post, Ts, Path, <<>>}
    end;
apply_request_info(get_waiting_host, Req, [Host]) ->
    U = replayer_utils:request_url(Req),
    case U of
        undefined -> throw({error, "got get_waiting_host with empty path", Req});
        _ -> nop
    end,
    replayer_utils:set_request_url(Req, "http://" ++ Host ++ U);
apply_request_info(get_waiting_connection_close, Req, _) ->
    Req;
apply_request_info(post_waiting_host, Req, Res) ->
    apply_request_info(get_waiting_host, Req, Res);
apply_request_info(post_waiting_connection_close, Req, Res) ->
    apply_request_info(get_waiting_connection_close, Req, Res);
apply_request_info(post_waiting_body, Req, [Body]) ->
    replayer_utils:set_request_body(Req, list_to_binary(Body)).

strip(Str) ->
    lists:foldl(fun(Char, Acc) ->
            string:strip(Acc, both, Char)
        end, Str, [$\r, $\n]).
