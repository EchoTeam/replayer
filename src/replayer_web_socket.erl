-module(replayer_web_socket).

-behaviour(cowboy_websocket_handler).

-export([
    init/3,
    websocket_init/3,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3
]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, Opts) ->
    {session, Session, SessionOpts} = lists:keyfind(session, 1, Opts),
    {ok, Req, {Session, SessionOpts}}.

websocket_handle({text, Msg}, Req, {Session, _SessionOpts} = State) ->
    case dispatch_request(Session, jiffy:decode(Msg)) of
        {reply, Reply} -> {reply, {text, jiffy:encode(Reply)}, Req, State};
        _ -> {ok, Req, State}
    end;
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

a2b(A) -> list_to_binary(atom_to_list(A)).

websocket_info({stats, {Ring, WorkersNum, File, Counters}}, Req, State) ->
    Nodes = {[{
            a2b(Node),
            {[
                {<<"counters">>, {[{a2b(CN), CV} ||
                        {CN, CV} <- CS,
                        not lists:member(CN, [reply_error_msgs, reply_ok_msgs])
                    ]}},
                {<<"ok_messages">>, 
                    case lists:keysearch(reply_ok_msgs, 1, CS) of
                        false -> [];
                        {value, {_, L}} -> {L}
                    end },
                {<<"error_messages">>, 
                    case lists:keysearch(reply_error_msgs, 1, CS) of
                        false -> [];
                        {value, {_, L}} -> {L}
                    end }
            ]}
        } || {Node, CS} <- Counters, is_list(CS)]},
    Stats = {[
            {<<"ring">>, [a2b(Node) || Node <- Ring]},
            {<<"workers_num">>, WorkersNum},
            {<<"file">>, case File of
                            undefined -> undefined;
                            _ -> list_to_binary(File) end},
            {<<"nodes">>, Nodes}
        ]},
    Reply = {[{<<"stats">>, Stats}]},
    {reply, {text, jiffy:encode(Reply)}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

dispatch_request({role, admin}, {[{<<"request">>, <<"get_stats">>}]}) ->
    replayer_web_messges:subscribe(),
    noreply;
dispatch_request(_Session, {[{<<"request">>, <<"get_stats">>}]}) ->
    {reply, {[{<<"error">>, <<"Permission denied">>}]}};
dispatch_request(_Session, Request) ->
    error_logger:error_msg("unknown request: ~p~n", [Request]),
    noreply.
