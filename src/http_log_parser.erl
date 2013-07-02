-module(http_log_parser).

-behaviour(log_parser).

-export([
    reader/2
]).

-type continuation_fun() :: fun(() -> eof | {continuation_fun(), [replayer_utils:request()]}).
-type option() :: {Key :: term(), Value :: term()}.

-record(state, {
    file_name :: string(),
    file_handle :: file:io_device(),
    options :: [option()]
}).

-spec reader(File :: string(), Options :: [option()]) ->
                            {continuation_fun(), [replayer_utils:request()]}.
reader(File, Options) ->
    {ok, FH} = file:open(File, [read]),
    Fun = reader_fun(#state{
                        file_name = File,
                        file_handle = FH,
                        options = Options
                    }),
    Fun().

reader_fun(#state{file_name = Name, file_handle = FH} = State) ->
    fun() ->
            case parse(FH) of
                {error, Error} ->
                    throw({"Error while reading log", Name, Error});
                eof -> file:close(FH), eof;
                Request ->
                    case maybe_filter_request(Request, State#state.options) of
                        true -> {reader_fun(State), [Request]};
                        false -> (reader_fun(State))()
                    end
            end
    end.


-spec parse(FH :: file:io_device()) -> eof | replayer_utils:request() .
parse(FH) ->
    case io:get_line(FH, '') of
        {error, Error} -> throw({error, Error});
        eof -> eof;
        Str_ ->
            case strip(Str_) of
                "" -> parse(FH);
                Str ->
                    case parse_ts(Str) of
                        {match, TS} -> parse_request(FH, TS);
                        nomatch -> parse(FH)
                    end
            end
    end.

parse_ts(Str) ->
    case re:run(Str, "^[0-9]\+\.[0-9]\+$", [{capture, [0], list}]) of
        {match, [TS]} -> {match, replayer_utils:ts_of_string(TS)};
        _ -> nomatch
    end.

parse_request(FH, TS) ->
    case parse_path(FH) of
        {match, {Type, Path}} when Type == "GET"; Type == "POST" ->
            case parse_headers(FH) of
                {match, Headers} ->
                    case Type of
                        "GET" -> assemble_request("GET",TS,Path,Headers,<<>>);
                        "POST" ->
                            case parse_body(FH, Headers) of
                                {match, Body} ->
                                    assemble_request("POST", TS, Path,
                                                                Headers, Body);
                                nomatch -> parse(FH);
                                eof -> eof
                            end
                    end;
                nomatch -> parse(FH);
                eof -> eof
            end;
        {match, {_Type, _Path}} -> parse(FH);
        nomatch -> parse(FH);
        eof -> eof
    end.

parse_path(FH) ->
    case io:get_line(FH, '') of
        {error, Error} -> throw({error, Error});
        eof -> eof;
        Str_ -> 
            Str = strip(Str_),
            case re:run(Str, "^\(POST\|GET\) \([^ ]\+\) .*",
                                                [{capture, [1, 2], list}]) of
                {match, [T, P]} -> {match, {T, P}};
                nomatch -> nomatch
            end
    end.

parse_headers(FH) ->
    parse_headers(FH, []).

parse_headers(FH, Acc) ->
    case io:get_line(FH, '') of
        {error, Error} -> throw({error, Error});
        eof -> eof;
        Str_ ->
            case strip(Str_) of
                "" -> {match, Acc};
                Str -> parse_headers(FH, [parse_header(Str) | Acc])
            end
    end.

parse_header(Str) ->
    {Header, Value} = lists:splitwith(fun(E) -> E /= $: end, Str),
    NValue = case Value of
        ":" ++ V -> V;
        _ -> Value
    end,
    Strip = fun(S) -> string:strip(strip(S)) end,
    {string:to_lower(Strip(Header)), Strip(NValue)}.

parse_body(FH, Headers) ->
    case proplists:get_value("content-length", Headers) of
        undefined -> nomatch;
        V ->
            case string:to_integer(V) of
                {I, []} ->
                    case file:read(FH, I) of
                        {error, Error} -> throw({error, Error});
                        eof -> eof;
                        {ok, B} -> {match, list_to_binary(B)}
                    end;
                _ -> nomatch
            end
    end.

assemble_request(Type, TS, Path, Headers, Body) ->
    Host = proplists:get_value("host", Headers),
    Url = "http://" ++ Host ++ Path,
    case Type of
        "GET" -> {get, TS, Url};
        "POST" -> {post, TS, Url, Body}
    end.

strip(Str) ->
    lists:foldl(fun(Char, Acc) ->
            string:strip(Acc, both, Char)
        end, Str, [$\r, $\n]).

maybe_filter_request(Request, Options) ->
    case proplists:get_value(filter, Options) of
        {K, V} ->
            case replayer_utils:query_params_of_request(Request) of
                {ok, Q} -> proplists:get_value(K, Q) == V;
                _ -> false
            end;
        _ -> true
    end.
