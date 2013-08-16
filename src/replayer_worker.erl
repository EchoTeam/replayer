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

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-define(TIMEOUT, 60000).

start_link(_Args) ->
    gen_server:start_link(?MODULE, [], []).

request(Worker, Req) ->
    gen_server:call(Worker, {request, Req}, ?TIMEOUT).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
    {ok, _State = ok}.

handle_call({request, Req}, _From, State) ->
    Res = handle_request(Req),
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
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec handle_request(replayer_utils:request()) -> {ok, any()} | {error, any()}.
% BC start
handle_request({get,Ts,Url}) -> handle_request({{http,get},Ts,{Url}});
handle_request({post,Ts,Url,Body}) -> handle_request({{http,post},Ts,{Url,Body}});
% BC end
handle_request({{http,_},_,_} = Req) -> 
    {Method, TS, Url, Body} = case Req of
        % BC start
        {get, TS_, U} -> {get, TS_, U, []};
        {post, TS_, U, B} -> {post, TS_, U, B};
        % BC end
        {{http,get}, TS_, {U}} -> {{http,get}, TS_, U, []};
        {{http,post}, TS_, {U, B}} -> {{http,post}, TS_, U, B}
    end,
    OUrl = override_params(TS, Url),
    OBody = case Body of
        [] -> [];
        _ -> list_to_binary(override_params(TS, binary_to_list(Body)))
    end,
    case lhttpc:request(OUrl, Method, [], OBody, ?TIMEOUT) of
        {ok, {{StatusCode, _ReasonPhrase}, _Hdrs, _ResponseBody}} ->
            {ok, StatusCode};
        {error, _} = Error -> Error
    end;
handle_request({{rpc,call},_Ts,{NodeInfo, {M,F,A}}}) ->
    case get_node_from_nodeinfo(NodeInfo) of
        {error, _} = Error -> Error;
        {ok, Node} ->
            case rpc:call(Node, M, F, A) of
                {badrpc, _} -> {error, badrpc_error_msg(Node)};
                {error, _} = Error -> Error;
                Res -> {ok, Res}
            end
    end.

-type nodeinfo() :: any(). % FIXME:
-spec get_node_from_nodeinfo(nodeinfo()) -> {ok, node()} | {error, any()}.
get_node_from_nodeinfo(NodeInfo) when is_atom(NodeInfo) -> {ok, NodeInfo};
get_node_from_nodeinfo({NodeType, NodeSpec} = NodeInfo) -> 
    Handlers = replayer_utils:get_env(node_info_handlers, []),
    case replayer_utils:get_env(node_info_executable_node) of
        undefined ->
            {error, "no node_info_executable_node specified"};
        RPCNode ->
            case proplists:get_value(NodeType, Handlers) of
                undefined -> 
                    NodeInfoStr = replayer_utils:term_to_string(NodeInfo),
                    {error, "no handler for nodeinfo: " ++ NodeInfoStr};
                {M,F} -> % echo_view_config:vrnodes_by_cname
                    case rpc:call(RPCNode, M, F, [NodeSpec]) of
                        {badrpc, _} -> 
                            {error, badrpc_error_msg(RPCNode)};
                        N when is_atom(N) ->
                            {ok, N};
                        [N|_] = Ns when is_atom(N) ->
                            {ok, replayer_utils:random_element(Ns)}
                    end
            end
    end.

badrpc_error_msg(Node) -> 
    NodeStr = replayer_utils:term_to_string(Node),
    "badrpc for information node " ++ NodeStr.


override_params(ReqTS, Str) ->
    case re:run(Str, "^(.+)([?|&])since=([0-9]+.[0-9]+)(.*)$",
                                            [{capture, [1,2,3,4], list}]) of
        {match, [Prefix, Delim, RTS, Suffix]} ->
            ParamTS = replayer_utils:ts_of_string(RTS),
            Diff = timer:now_diff(ReqTS, ParamTS),
            TS = replayer_utils:unixtimestamp() - Diff/1000000,
            [S] = io_lib:format("~.6f", [TS]),
            Prefix ++ Delim ++ "since=" ++ S ++ Suffix;
        _ -> Str
    end.
