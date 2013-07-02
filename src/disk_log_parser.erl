-module(disk_log_parser).

-behaviour(log_parser).

-export([
    reader/2
]).

-type continuation_fun() :: fun(() -> eof | {continuation_fun(), [replayer_utils:request()]}).
-type option() :: {Key :: term(), Value :: term()}.

-record(state, {
    file_name :: string(),
    continuation :: disk_log:continuation(),
    options :: [option()]
}).

-spec reader(File :: string(), Options :: [option()]) ->
                            {continuation_fun(), [replayer_utils:request()]}.
reader(File, Options) ->
    disk_log:open([{name, File}, {file, File}, {mode, read_only}]),
    Fun = reader_fun(#state{
                        file_name = File,
                        continuation = start,
                        options = Options
                    }),
    Fun().

reader_fun(#state{file_name = Name, continuation = Continuation} = State) ->
    fun() ->
            case disk_log:chunk(Name, Continuation) of
                {NewContinuation, Requests} ->
                    NewState = State#state{continuation = NewContinuation},
                    case maybe_filter_requests(Requests, State#state.options) of
                        [] -> (reader_fun(NewState))();
                        FilteredRequests ->
                            {reader_fun(NewState), FilteredRequests}
                    end;
                eof -> 
                    disk_log:close(Name),
                    eof;
                Error ->
                    throw({"Error while reading log", Name, Error})
            end
    end.

maybe_filter_requests(Requests, Options) ->
    case proplists:get_value(filter, Options) of
        {K, V} -> filter_requests(Requests, K, V);
        _ -> Requests
    end.

filter_requests(Requests, FilterKey, FilterValue) ->
    lists:filter(fun(Request) ->
        case replayer_utils:query_params_of_request(Request) of
            {ok, Params} ->
                proplists:get_value(FilterKey, Params) == FilterValue;
            _ -> false
        end
    end, Requests).
