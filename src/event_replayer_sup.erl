
-module(event_replayer_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    change_workers_num/1
]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
        ?CHILD(tasks_generator, worker),
        {replayer_controller, {replayer_controller, start_link, [[]]},
            permanent, 5000, worker, [replayer_controller]},
        ?CHILD(replayer_node_orchestrator, worker)
            ]} }.

change_workers_num(WorkersNum) ->
    application:set_env(event_replayer, worker_pool_size, WorkersNum),
    supervisor:terminate_child(?MODULE, replayer_node_orchestrator),
    supervisor:restart_child(?MODULE, replayer_node_orchestrator),
    {ok, WorkersNum}.


