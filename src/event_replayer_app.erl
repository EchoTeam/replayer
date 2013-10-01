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
    ok = application:start(mimetypes),
    ok = application:start(jiffy),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    start_web_server(),
    event_replayer_sup:start_link().

stop(_State) ->
    ok.

start_web_server() ->
    WWWRootDir = replayer_utils:get_env(www_dir, "./www"),
    Dispatch = cowboy_router:compile([
        {'_', [
                {"/", replayer_web_index, []},
                {"/websocket", replayer_web_socket, []},
                {"/static/[...]", cowboy_static, [
                        {directory, filename:join([WWWRootDir, "static"])},
                        {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
                    ]}
            ]}
    ]),
    Port = 8081, %replayer_utils:get_env(web_port, 8081),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}],
                    [
                        {middlewares, [
                                            cowboy_router,
                                            cowboy_cookie_session,
                                            cowboy_handler
                                        ]},
                        {env, [
                                {session_opts,
                                    {<<"sess">>,<<"echo!82_">>,3600,<<"/">>}},
                                {dispatch, Dispatch}
                            ]}
                    ]),
    ok.
