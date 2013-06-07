-module(tasks_generator_tests).

-include_lib("eunit/include/eunit.hrl").

-export([
    create_tasks_ll/2
]).

%FIXME: should I have a copy of it here?
-record(state, {
        handler = ?MODULE,
        tasks = [],
        ring=[]
}).
%stub function that will be called instead real one
create_tasks_ll(_Tasks, _Ring) -> 
    case get(create_tasks_ll) of
        undefined -> ok;
        F -> F()
    end.

create_stub_state() -> 
    erase(create_tasks_ll),
    {ok, State} = tasks_generator:init([{handler, ?MODULE}]),
    State.

change_tasks_empty_test() ->
    State = create_stub_state(),
    ?assertEqual(
        {reply, {ok, no_change_needed}, State},
        tasks_generator:handle_call({change_tasks, []}, undefined, State)
    ).
change_tasks_test() ->
    State = create_stub_state(),
    Tasks = [{csv_log, "csv_log.log"}, {disk_log, "disk_log.log"}],
    ?assertEqual(
        {reply, {ok, changed}, #state{tasks=Tasks}},
        tasks_generator:handle_call({change_tasks, Tasks}, undefined, State)
    ).
change_tasks_exception_test() ->
    State = create_stub_state(),
    put(create_tasks_ll, fun() -> throw("hello") end),
    Tasks = [{csv_log, "csv_log.log"}, {disk_log, "disk_log.log"}],
    ?assertEqual(
        {reply, {error, {throw, "hello"}}, State},
        tasks_generator:handle_call({change_tasks, Tasks}, undefined, State)
    ).


change_empty_ring_test() ->
    State = create_stub_state(),
    ?assertEqual(
        {reply, {ok, no_change_needed}, State},
        tasks_generator:handle_call({change_ring, []}, undefined, State)
    ).
change_ring_test() ->
    State = create_stub_state(),
    Ring = [node1, node2],
    ?assertEqual(
        {reply, {ok, changed}, State#state{ring=Ring}},
        tasks_generator:handle_call({change_ring, Ring}, undefined, State)
    ).
change_ring_exception_test() ->
    State = create_stub_state(),
    put(create_tasks_ll, fun() -> throw("hello") end),
    Ring = [node1, node2],
    ?assertEqual(
        {reply, {error, {throw, "hello"}}, State},
        tasks_generator:handle_call({change_ring, Ring}, undefined, State)
    ).
