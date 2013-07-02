-module(replayer_worker).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/1,
    request/2
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-define(TIMEOUT, 60000).

start_link(_Args) ->
    gen_server:start_link(?MODULE, [], []).

request(Worker, Req) ->
    gen_server:call(Worker, {request, Req}, ?TIMEOUT),
    ok.
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, _State = ok}.

handle_call({request, Req}, _From, State) ->
    {Method, TS, Url, Body} = case Req of
        {get, TS_, U} -> {get, TS_, U, []};
        {post, TS_, U, B} -> {post, TS_, U, B}
    end,
    OUrl = override_params(TS, Url),
    OBody = case Body of
        [] -> [];
        _ -> list_to_binary(override_params(TS, binary_to_list(Body)))
    end,
    _Res = lhttpc:request(OUrl, Method, [], OBody, infinity),
    % TODO: check Res, at least code 20x
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

override_params(ReqTS, Str) ->
    case re:run(Str, "^(.+)([?|&])since=([0-9]+.[0-9]+)(.*)$",
                                            [{capture, [1,2,3,4], list}]) of
        {match, [Prefix, Delim, RTS, Suffix]} ->
            ParamTS = replayer_utils:ts_of_string(RTS),
            Diff = timer:now_diff(ReqTS, ParamTS),
            TS = replayer_utils:unixtimestamp() - Diff/1000000,
            [S] = io_lib:format("~.6f", [TS]),
            Prefix ++ Delim ++ "since=" ++ S ++ Suffix;
        _ -> Str
    end.
