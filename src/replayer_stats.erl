-module(replayer_stats).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    bump/1,
    flush/0,
    get_stats/0,
    set_tasks_count/1,
    start_link/0
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
    tasks_count = 0,
    tasks_processed = 0,
    tasks_failed = 0,
    reply_errors = 0,
    reply_oks = 0,
    reply_error_msgs = dict:new(),
    reply_ok_msgs = dict:new()
}).


%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

bump(Counter) ->
    gen_server:cast(?MODULE, {bump, Counter}).

flush() ->
    gen_server:cast(?MODULE, flush).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

set_tasks_count(TasksCount) ->
    gen_server:cast(?MODULE, {set_tasks_count, TasksCount}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

get_top10(PropList) -> 
    lists:sublist(lists:keysort(2, PropList), 10).

handle_call(get_stats, _From, State) ->
    Stats = [
        {tasks_count,       State#state.tasks_count},
        {tasks_processed,   State#state.tasks_processed},
        {tasks_failed,      State#state.tasks_failed},
        {reply_errors,      State#state.reply_errors},
        {reply_oks,         State#state.reply_oks},
        {reply_error_msgs,  get_top10(dict:to_list(State#state.reply_error_msgs))},
        {reply_ok_msgs,     get_top10(dict:to_list(State#state.reply_ok_msgs))}
    ],
    {reply, Stats, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({bump, tasks_processed}, State) ->
    {noreply, State#state{tasks_processed = State#state.tasks_processed + 1}};
handle_cast({bump, tasks_failed}, State) ->
    {noreply, State#state{tasks_failed = State#state.tasks_failed + 1}};
handle_cast({bump, reply_errors}, State) ->
    {noreply, State#state{reply_errors = State#state.reply_errors + 1}};
handle_cast({bump, reply_oks}, State) ->
    {noreply, State#state{reply_oks = State#state.reply_oks + 1}};
handle_cast({bump, {reply_error_msg, ErrorMsg}}, State) ->
    ErrorMsgs = dict:update_counter(ErrorMsg, 1, State#state.reply_error_msgs),
    {noreply, State#state{reply_error_msgs = ErrorMsgs}};
handle_cast({bump, {reply_ok_msg, OkMsg}}, State) ->
    OkMsgs = dict:update_counter(OkMsg, 1, State#state.reply_ok_msgs),
    {noreply, State#state{reply_ok_msgs = OkMsgs}};
handle_cast(flush, _State) ->
    {noreply, #state{}};
handle_cast({set_tasks_count, TasksCount}, State) ->
    {noreply, State#state{tasks_count = TasksCount}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Functions
%% ------------------------------------------------------------------
