-module(tasks_generator).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([
    start_link/0,
    change_tasks_file/1,
    change_ring/1,
    prepare/0
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

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec change_tasks_file(File :: string()) -> {ok, changed} | {ok, no_change_needed} | {error, any()}.
change_tasks_file(File) ->
    gen_server:call(?MODULE, {change_tasks_file, File}).

change_ring(Ring) ->
    gen_server:call(?MODULE, {change_ring, Ring}).

prepare() ->
    gen_server:call(?MODULE, prepare).

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
handle_call(prepare, _From, State) ->
    Ring = State#state.ring,
    Res = case disk_log:open([{name, ?MODULE}, {file, State#state.tasks_file}, {mode, read_only}]) of
        {error, _} = E -> E;
        _ ->
            [ rpc:call(Node, replayer_worker, create_task_file, []) || Node <- Ring ],
            with_chunks(
                fun(Chunks) -> copy_tasks_to_ring(Chunks, Ring) end,
                disk_log:chunk(?MODULE, start)),
            disk_log:close(?MODULE),
            [ rpc:call(Node, replayer_worker, close_task_file, []) || Node <- Ring ],
            ok
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

with_chunks(_Fun, eof) -> nop;
with_chunks(Fun, {Cont, Chunks}) ->
    with_chunks(Fun, {Cont, Chunks, undefined});
with_chunks(Fun, {Cont, Chunks, _Badbytes}) ->
    Fun(Chunks),
    with_chunks(Fun, disk_log:chunk(?MODULE, Cont)).

copy_tasks_to_ring(Chunks, Ring) ->
    [ rpc:call(random_node(Ring), replayer_worker, append_task, [Chunk]) || Chunk <- Chunks ],
    ok.

random_node(Ring) -> 
    random_element(Ring).

random_element(Alternatives) ->
    N = length(Alternatives),
    R = element(3, now()),
    lists:nth((R rem N) + 1, Alternatives).
    
