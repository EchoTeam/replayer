-module(replayer_web_messges).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    subscribe/0
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

-record(state, {
    subscribers = []
}).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

subscribe() ->
    gen_server:call(?MODULE, {subscribe, self()}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    start_timer(),
    {ok, #state{}}.

handle_call({subscribe, Pid}, _From, State) ->
    NewState = add_subscriber(State, Pid),
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    send_stats(State),
    start_timer(),
    {noreply, State};
handle_info({'DOWN', MRef, process, Pid, _Info},
                #state{subscribers = Subscribers} = State) ->
    NewSubscribers = case lists:keytake(MRef, 1, Subscribers) of
        {value, {MRef, Pid}, NSubscribers} -> NSubscribers;
        _ -> Subscribers
    end,
    {noreply, State#state{subscribers = NewSubscribers}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------

start_timer() ->
    erlang:send_after(1000, self(), timeout).

add_subscriber(#state{subscribers = Subscribers} = State, Pid) ->
    NewSubscribers = case lists:keysearch(Pid, 2, Subscribers) of
        {value, _} -> Subscribers;
        false ->
            MRef = erlang:monitor(process, Pid),
            [{MRef, Pid} | Subscribers]
    end,
    State#state{subscribers = NewSubscribers}.

send_stats(#state{subscribers = Subscribers}) ->
    {Ring, WorkersNum, File} = replayer_controller:get_stats(),
    Counters = case Ring of
        [] -> [];
        _ -> replayer_controller:execute_on_ring(replayer_stats, get_stats, [])
    end,
    Stats = {Ring, WorkersNum, File, Counters},
    lists:foreach(fun({_MRef, Pid}) ->
            Pid ! {stats, Stats}
        end, Subscribers).
