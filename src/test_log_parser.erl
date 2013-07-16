-module(test_log_parser).

-behaviour(log_parser).

-export([
    reader/2,
    test_sample_data/0
]).

test_sample_data() ->
    lists:sort(
        lists:flatten(
            [test_data(N) || N <- lists:seq(1, 3)]
        )
    ).

test_data(1) ->
    [
        {get, {0, 0, 2}, "http://url2/"},
        {get, {0, 0, 5}, "http://url5/"},
        {get, {0, 0, 8}, "http://url8/"}
    ];
test_data(2) ->
    [
        {get, {0, 0, 3}, "http://url3/"},
        {get, {0, 0, 6}, "http://url6/"},
        {get, {0, 0, 9}, "http://url9/"}
    ];
test_data(3) ->
    [
        {get, {0, 0, 1}, "http://url1/"},
        {get, {0, 0, 4}, "http://url4/"},
        {get, {0, 0, 7}, "http://url7/"}
    ].

reader(DataNo, _Options) ->
    Data = test_data(DataNo),
    DataSize = length(Data),
    Fun = reader_fun({Data, 1, DataSize}),
    Fun().

reader_fun({Data, Pos, Size}) ->
    fun() ->
        case Pos > Size of
            true -> eof;
            false -> {reader_fun({Data, Pos + 1, Size}), [lists:nth(Pos, Data)]}
        end
    end.
