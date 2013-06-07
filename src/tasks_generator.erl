-module(tasks_generator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    change_tasks/1,
    change_ring/1
]).

-export_type([
    task/0
]).

% testing purposes
-export([
    create_tasks_ll/2
]).
%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        handler = ?MODULE :: module(),  % for eunit tests
        tasks = []   :: [task()],
        ring=[]      :: [node()]
}).

-type task_type() :: disk_log | csv_log.
-type task() :: {Type :: task_type(), FileName :: string()}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec change_tasks([task()]) -> {ok, changed} | {ok, no_change_needed} | {error, any()}.
change_tasks(Tasks) ->
    gen_server:call(?MODULE, {change_tasks, Tasks}).

change_ring(Ring) ->
    gen_server:call(?MODULE, {change_ring, Ring}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, #state{}}.

handle_call(clean_tasks, _From, State) ->
    NewState = State#state{tasks=[]},
    {reply, ok, NewState};
handle_call({change_tasks, Tasks}, _From, State) ->
    Handler = State#state.handler,
    CurTasks = State#state.tasks,
    NewTasks = lists:sort(Tasks),
    Res = case CurTasks == NewTasks of
        true -> {ok, no_change_needed};
        _ ->
            try Handler:create_tasks_ll(Tasks, State#state.ring) of
                _ -> {ok, changed}
            catch C:R ->
                {error, {C,R}}
            end
    end,
    case Res of
        {ok, _} -> {reply, Res, State#state{tasks = NewTasks}};
        _ -> {reply, Res, State}
    end;
handle_call({change_ring, NewRing_}, _From, State) ->
    Handler = State#state.handler,
    NewRing = lists:sort(NewRing_),
    Res = case NewRing == State#state.ring of
        true -> {ok, no_change_needed};
        _ ->
            try Handler:create_tasks_ll(State#state.tasks, NewRing) of
                _ -> {ok, changed}
            catch C:R ->
                {error, {C,R}}
            end
    end,
    case Res of
        {ok, _} -> {reply, Res, State#state{ring = NewRing}};
        _ -> {reply, Res, State}
    end;
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
-record(file_pos, {
        file = ""    :: string(),
        pos  = 0     :: non_neg_integer()
}).

create_tasks_ll(Tasks, Ring) -> 
    S = #file_pos{},
    throw("undefined function"),
    {Tasks, Ring, S}.
