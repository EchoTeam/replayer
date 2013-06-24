-module(replayer_controller).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/1,
    change_tasks_file/1,
    change_ring/1,
    change_workers_num/1,
    prepare/0,
    replay/1
]).

-export_type([
    task/0
]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
        handler = ?MODULE :: module(),  % for eunit tests
        tasks_file = undefined :: [string()],
        ring = []      :: [node()]
}).

-type task_type() :: disk_log | csv_log.
-type task() :: {Type :: task_type(), FileName :: string()}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

-spec change_tasks_file(File :: string()) -> {ok, changed} | {ok, no_change_needed} | {error, any()}.
change_tasks_file(File) ->
    gen_server:call(?MODULE, {change_tasks_file, File}).

change_ring(Ring) ->
    gen_server:call(?MODULE, {change_ring, Ring}).

change_workers_num(WorkersNum) ->
    gen_server:call(?MODULE, {change_workers_num, WorkersNum}, infinity).

prepare() ->
    gen_server:call(?MODULE, prepare, infinity).

-spec replay(Speed :: float()) -> ok | {error, any()}.
replay(Speed) ->
    gen_server:call(?MODULE, {replay, Speed}, infinity).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    State = #state{},
    Handler = proplists:get_value(handler, Args, State#state.handler),
    TasksFile = proplists:get_value(tasks_file, Args, State#state.tasks_file),
    Ring    = proplists:get_value(ring, Args, State#state.ring),
    {ok, State#state{
            handler = Handler,
            tasks_file = TasksFile,
            ring = Ring}}.

handle_call({change_tasks_file, File}, _From, State) ->
    Res = case State#state.tasks_file == File of
        true -> {ok, no_change_needed};
        _ -> {ok, changed}
    end,
    {reply, Res, State#state{tasks_file = File}};
handle_call({change_ring, NewRing_}, _From, State) ->
    NewRing = lists:sort(NewRing_),
    Res = case NewRing == State#state.ring of
        true -> {ok, no_change_needed};
        _ -> {ok, changed}
    end,
    {reply, Res, State#state{ring = NewRing}};
handle_call({change_workers_num, _}, _From, #state{ring=[]} = State) ->
    {reply, {error, "empty ring"}, State};
handle_call({change_workers_num, WorkersNum}, _From, State) ->
    Refs = [ {Node, jsk_async:run(fun() ->
            rpc:call(Node, event_replayer_sup, change_workers_num, [WorkersNum])  
        end)} || Node <- State#state.ring
    ],
    NodeRes = [{Node, jsk_async:join(Ref)} || {Node, Ref} <- Refs],
    Res = case lists:filter(fun({_, {ok, _}}) -> false; (_) -> true end, NodeRes) of
        [] -> ok;
        V -> {error, V}
    end,
    {reply, Res, State};
handle_call(prepare, _From, State) ->
    Ring = State#state.ring,
    Res = case disk_log:open([{name, ?MODULE}, {file, State#state.tasks_file}, {mode, read_only}]) of
        {error, _} = E -> E;
        _ ->
            copy_tasks_to_ring(Ring),
            ok
    end,
    {reply, Res, State};
handle_call({replay, _Speed}, _From, #state{ring=[]} = State) ->
    {reply, {error, "empty ring"}, State};
handle_call({replay, Speed}, _From, State) ->
    Ring = State#state.ring,
    StartTs = get_start_time(1),
    Refs = [ {Node, jsk_async:run(fun() ->
            rpc:call(Node, replayer_node_orchestrator, replay, [StartTs, Speed])  
        end)} || Node <- Ring
    ],
    NodeRes = [{Node, jsk_async:join(Ref)} || {Node, Ref} <- Refs],
    Res = case lists:filter(fun({_, {ok, _}}) -> false; (_) -> true end, NodeRes) of
        [] -> ok;
        V -> {error, V}
    end,
    {reply, Res, State};
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
%% Internal Functions
%% ------------------------------------------------------------------

% delay in seconds
get_start_time(Delay) ->
    {MegaS, S, MicroS} = now(),
    {NewMegaS, NewS} = case S + Delay of
        V when V >= 1000000 -> {MegaS + 1, V - 1000000};
        V -> {MegaS, V}
    end,
    {NewMegaS, NewS, MicroS}.

copy_tasks_to_ring(Ring) ->
    try 
        begin
            [ {ok, _} = rpc:call(Node, replayer_node_orchestrator, create_task_file, []) || Node <- Ring ],
            FirstChunk = disk_log:chunk(?MODULE, start, 1),
            FirstReqTs = case FirstChunk of
                {_, [Chunk]} ->     replayer_utils:request_timestamp(Chunk);
                {_, [Chunk], _} ->  replayer_utils:request_timestamp(Chunk);
                _ -> now() % doesn't matter as nothing to replay, just copy smth
            end,
            io:format("FirstReqTs:~p~n", [FirstReqTs]),

            [ rpc:call(Node, replayer_node_orchestrator, append_task, [{start_time, FirstReqTs}]) 
                || Node <- Ring ],

            replayer_utils:with_chunks(
                ?MODULE,
                fun(Chunks, Acc) -> copy_chunks(Chunks, Ring, Acc) end,
                FirstChunk,
                {1, length(Ring)})
        end
    catch C:R -> 
        io:format("Error occured while copying tasks to ring:~p~n", [{C,R}])
    after
        disk_log:close(?MODULE)
    end,
    [ rpc:call(Node, replayer_node_orchestrator, close_task_file, []) || Node <- Ring ],
    ok.

copy_chunks(Chunks, Ring, {NodeNo, RingSize}) ->
    NewNodeNo = lists:foldl(fun(Chunk, NodeNo_) ->
            {NewNodeNo_, Node} = get_next_node(NodeNo_, RingSize, Ring),
            case rpc:call(Node, replayer_node_orchestrator, append_task, [Chunk]) of
                {badrpc, _} = Error -> throw({Node, Error});
                {error, _} = Error -> throw({Node, Error});
                _V -> nop
            end,
            NewNodeNo_
        end, NodeNo, Chunks),
    {NewNodeNo, RingSize}.

get_next_node(NodeNo, RingSize, Ring) ->
    Node = lists:nth(NodeNo, Ring),
    {case NodeNo of
        V when V == RingSize -> 1;
        V -> V + 1
    end, Node}.
