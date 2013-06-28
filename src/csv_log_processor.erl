-module(csv_log_processor).
-behaviour(log_processor).

-export([reader/1]).

-type continuation_fun() :: fun(() -> eof | {continuation_fun(), [replayer_utils:request()]}).
-spec reader(File :: string()) -> {continuation_fun(), [replayer_utils:request()]}.
reader(File) -> 
    {ok, FH} = file:open(File, [read, binary]),
    Fun = reader_fun({File, FH}),
    Fun().

            
-spec reader_fun({string(), file:io_device()}) -> {continuation_fun(), [replayer_utils:request()]}. 
reader_fun({Name, FH}) ->
    fun() ->
            case io:get_line(FH, '') of
                {error, Error} -> throw({"Error while reading log", Name, Error});
                eof -> file:close(FH), eof;
                "\n" -> reader_fun({Name, FH});
                "\r\n" -> reader_fun({Name, FH});
                Data -> 
                    Items = string:tokens(strip(binary_to_list(Data)), [$;]),
                    Request = item_process(Items),
                    %io:format("Request:~p~n", [Request]),
                    {reader_fun({Name, FH}), [Request]}
            end
    end.

% see https://github.com/lkiesow/erlang-datetime/blob/master/datetime.erl for the help
-spec item_process([string()]) -> replayer_utils:request().
item_process([DateTimeStr,Endpoint]) ->
    {ok, [MonStr, Day, Year, Hour, Min, Sec, MS], []} =
                            io_lib:fread("~3s ~d, ~d ~d:~d:~d.~d", DateTimeStr),
    Mon = month(MonStr),
    Secs  = calendar:datetime_to_gregorian_seconds({{Year,Mon,Day},{Hour,Min,Sec}}) - calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    Ts = {Secs div 1000000, Secs rem 1000000, MS},
    Url = "http://api.aboutecho.com" ++ Endpoint,
    {get, Ts, Url}.

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
