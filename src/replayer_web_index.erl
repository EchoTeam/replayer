-module(replayer_web_index).

-export([
    init/3,
    handle/2,
    terminate/3
]).

init(_Transport, Req, Opts) ->
    RootDir = case application:get_env(event_replayer, www_dir) of
        undefined -> "./www";
        {ok, RootDir_} -> RootDir_
    end,
    {session, Session, CookieOpts} = lists:keyfind(session, 1, Opts),
    {ok, Req, {RootDir, {Session, CookieOpts}}}.

handle(Req, {RootDir, {_Session, CookieOpts}} = State) ->
    Auths = credentials(Req),
    Credentials = case application:get_env(event_replayer, credentials) of
        undefined -> undefined;
        {ok, Credentials_} -> Credentials_
    end,
    {ok, Req3} = case check_user(Auths, Credentials) of
        undefined ->
            unauthorized(Req);
        Role ->
            Html = get_html(RootDir),
            Req2 = cowboy_cookie_session:set_session({role,Role},CookieOpts,Req),
            cowboy_req:reply(200,
                [{<<"content-type">>, <<"text/html">>}],
                Html, Req2)
    end,
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

check_user(_Auth, undefined) -> admin;
check_user(Auth, Credentials) ->
    proplists:get_value(Auth, Credentials).

get_html(RootDir) ->
    Filename =filename:join([RootDir, "index.html"]),
    {ok, Binary} = file:read_file(Filename),
    Binary.

credentials(Req) ->
    {ok, Auth, _Req} = cowboy_req:parse_header(<<"authorization">>, Req),
    case Auth of
        {<<"basic">>, Credentials} -> Credentials;
        _ -> undefined
    end.

unauthorized(Req) ->
    Req2 = cowboy_req:set_resp_header(<<"Www-Authenticate">>, <<"Basic realm=\"Secure Area\"">>, Req),
    Req3 = cowboy_req:set_resp_body(unauthorized_body(), Req2),
    cowboy_req:reply(401, Req3).

unauthorized_body() ->
    <<"
    <!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"
     \"http://www.w3.org/TR/1999/REC-html401-19991224/loose.dt\">
    <HTML>
      <HEAD>
        <TITLE>Error</TITLE>
        <META HTTP-EQUIV=\"Content-Type\" CONTENT=\"text/html; charset=ISO-8859-1\">
      </HEAD>
      <BODY><H1>401 Unauthorized.</H1></BODY>
    </HTML>
    ">>.
