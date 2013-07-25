-module(test_log_parser).

-behaviour(log_parser).

-export([
    reader/2,
    test_sample_data/0
]).

test_sample_data() ->
    lists:keysort(2,
        lists:filter(fun(E) -> element(3, element(2, E)) rem 10 == 0 end,
            lists:flatten(
                [test_data(N) || N <- lists:seq(1, 3)]
            )
        )
    ).

test_data(empty) -> [];
test_data(1) ->
    [
            {get,  {0, 0, 11}, "http://url11/?filter1=false"},
            {post, {0, 0, 19}, "http://url19/?filter1=false", ""},
        {get,  {0, 0, 20}, "http://url20/?filter1=true&filter2=false"},
            {get,  {0, 0, 41}, "http://url41/?filter1=false"},
            {post, {0, 0, 49}, "http://url49/", ""},
        {post, {0, 0, 50}, "http://url50/?filter1=true&filter3=false", ""},
            {get,  {0, 0, 71}, "http://url71/?filter1=false"},
            {post, {0, 0, 79}, "http://url79/?filter1=false", ""},
        {get,  {0, 0, 80}, "http://url80/?filter1=true"},
            {get,  {0, 0, 82}, "http://url82/?filter1=false"},
            {post, {0, 0, 88}, "http://url88/", "filter1=false"}
    ];
test_data(2) ->
    [
            {get,  {0, 0, 21}, "http://url21/?filter2=false"},
            {post, {0, 0, 29}, "http://url29/?filter2=false", ""},
        {get,  {0, 0, 30}, "http://url30/?filter1=false&filter2=true"},
            {get,  {0, 0, 51}, "http://url51/?filter2=false"},
            {post, {0, 0, 59}, "http://url59/", ""},
        {get,  {0, 0, 60}, "http://url60/?filter2=true&filter3=false"},
            {get,  {0, 0, 81}, "http://url81/?filter2=false"},
            {post, {0, 0, 89}, "http://url89/?filter2=false", ""},
        {post, {0, 0, 90}, "http://url90/?filter2=true", ""},
            {get,  {0, 0, 92}, "http://url92/?filter2=false"},
            {post, {0, 0, 98}, "http://url92/", "filter2=false"}
    ];
test_data(3) ->
    [
            {get,  {0, 0, 1}, "http://url1/?filter3=false"},
            {post, {0, 0, 9}, "http://url9/?filter3=false", ""},
        {post, {0, 0, 10}, "http://url10/?filter1=false&filter3=true", ""},
            {get,  {0, 0, 31}, "http://url31/?filter3=false"},
            {post, {0, 0, 39}, "http://url39/", ""},
        {get,  {0, 0, 40}, "http://url40/?filter2=false&filter3=true"},
            {get,  {0, 0, 61}, "http://url61/?filter3=false"},
            {post, {0, 0, 69}, "http://url69/?filter3=false", ""},
        {get,  {0, 0, 70}, "http://url70/?filter3=true"},
            {get,  {0, 0, 72}, "http://url72/?filter3=false"},
            {post, {0, 0, 78}, "http://url78/", "filter3=false"}
    ];
test_data(4) ->
    [
        {get,  {0, 0, -1}, "http://url-1/?filter4=false"},
        {post, {0, 0, 100}, "http://url100/?filter4=false", ""}
    ].

reader(DataNo, Options) ->
    Data = test_data(DataNo),
    DataSize = length(Data),
    Fun = reader_fun({Data, 1, DataSize, Options}),
    Fun().

reader_fun({Data, Pos, Size, Options}) ->
    fun() ->
        case Pos > Size of
            true -> eof;
            false ->
                Request = lists:nth(Pos, Data),
                NewState = {Data, Pos + 1, Size, Options},
                case maybe_filter_request(Request, Options) of
                    true -> {reader_fun(NewState), [Request]};
                    false -> (reader_fun(NewState))()
                end
        end
    end.

maybe_filter_request(Request, Options) ->
    case proplists:get_value(filter, Options) of
        {K, V} ->
            case replayer_utils:query_params_of_request(Request) of
                {ok, Params} -> proplists:get_value(K, Params) == V;
                _ -> false
            end;
        _ -> false
    end.
