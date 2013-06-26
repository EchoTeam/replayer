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

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

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
    %io:format("starting worker ~p~n", [self()]),
    {ok, _State = ok}.

handle_call({request, Req}, _From, State) ->
    io:format("~p (~p): requesting ~n", [self(), node()]),
    {Method, Url, Body} = case Req of 
        {get, _, U} -> {get, U, []};
        {post, _, U, B} -> {post, U, B}
    end,
    _Res = lhttpc:request(Url, Method, [], Body, infinity),
    % TODO: check Res, at least code 20x
    %io:format("Res:~p~n", [Res]),
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

