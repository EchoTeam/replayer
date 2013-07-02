-module(csv_log_parser).

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
    {ok, FH} = file:open(File, [read, binary]),
    Fun = reader_fun(#state{
                        file_name = File,
                        file_handle = FH,
                        options = Options
                    }),
    Fun().

-spec reader_fun(#state{}) -> {continuation_fun(), [replayer_utils:request()]}.
reader_fun(#state{file_name = FileName, file_handle = FH} = State) ->
    fun() ->
            case io:get_line(FH, '') of
                {error, Error} ->
                    throw({"Error while reading log", FileName, Error});
                eof -> file:close(FH), eof;
                "\n" -> (reader_fun(State))();
                "\r\n" -> (reader_fun(State))();
                Data -> 
                    Items = string:tokens(strip(binary_to_list(Data)), [$;]),
                    case item_process(Items, State#state.options) of
                        {ok, Request} -> {reader_fun(State), [Request]};
                        skip -> (reader_fun(State))()
                    end
            end
    end.

% see https://github.com/lkiesow/erlang-datetime/blob/master/datetime.erl for the help
-spec item_process([string()], [option()]) -> {ok, replayer_utils:request()} | skip.
item_process([DateTimeStr,Endpoint], Options) ->
    {ok, [MonStr, Day, Year, Hour, Min, Sec, MS], []} =
                            io_lib:fread("~3s ~d, ~d ~d:~d:~d.~d", DateTimeStr),
    Mon = month(MonStr),
    Secs  = calendar:datetime_to_gregorian_seconds({{Year,Mon,Day},{Hour,Min,Sec}}) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Ts = {Secs div 1000000, Secs rem 1000000, MS},
    Url = "http://api.aboutecho.com" ++ Endpoint,
    Filter = proplists:get_value(filter, Options),
    Request = {get, Ts, Url},
    case {replayer_utils:query_params_of_request(Request), Filter} of
        {{ok, Q}, {K, V}} ->
            case proplists:get_value(K, Q) == V of
                true -> Request;
                false -> skip
            end;
        {{ok, _Q}, _} -> Request;
        _ -> skip
    end.

month("Jan") -> 1;
month("Feb") -> 2;
month("Mar") -> 3;
month("Apr") -> 4;
month("May") -> 5;
month("Jun") -> 6;
month("Jul") -> 7;
month("Aug") -> 8;
month("Sep") -> 9;
month("Oct") -> 10;
month("Nov") -> 11;
month("Dec") -> 12.

strip(Str) ->
    lists:foldl(fun(Char, Acc) ->
            string:strip(Acc, both, Char)
        end, Str, [$\r, $\n]).
