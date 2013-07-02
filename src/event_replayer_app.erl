-module(event_replayer_app).

-behaviour(application).

-export([
    start/0
]).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

start() ->
    application:start(event_replayer).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = application:start(sasl),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(lhttpc),
    event_replayer_sup:start_link().

stop(_State) ->
    ok.
