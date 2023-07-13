-module(refundcheck_config).

-export([
    init_conf/0,
    get_conf/1,
    set_conf/2,

    newToken/1,
    newLoginToken/0,
    isLoginToken/1,
    removeLoginToken/1,

    getDBHost/0,
    getDBUser/0,
    getDBPass/0,
    getRoutes/0,
    getTrails/0,
    getHTTPPort/0,
    getAuthEndpoint/0,
    getAuthRN/0,
    getAuthTokenEndpoint/0,
    getAuthTokenRN/0,
    getAuthClientId/0,
    getAuthClientSecret/0,
    getAuthRedirectURILoginCallback/0,
    getURIConsole/0,
    getURILogin/0,

    getSellerInitialCalls/0
]).

getDBHost() ->
    V = application:get_env(refundcheck, db_host, undefined),
    V.

getDBUser() ->
    V = application:get_env(refundcheck, db_user, undefined),
    V.

getDBPass() ->
    V = application:get_env(refundcheck, db_pass, undefined),
    V.

getRoutes() ->
    V = application:get_env(refundcheck, routes, undefined),
    V.

getTrails() ->
    V = application:get_env(refundcheck, trails, undefined),
    V.

getHTTPPort() ->
    S = application:get_env(refundcheck, http_port, "2020"),
    V = list_to_integer(S),
    V.

% -- -- -- -- -- -- -- -- -- -- --

getAuthEndpoint() ->
    V = application:get_env(refundcheck, auth_endpoint, undefined),
    V.

getAuthRN() ->
    V = application:get_env(refundcheck, auth_rn, undefined),
    V.

getAuthTokenEndpoint() ->
    V = application:get_env(refundcheck, auth_token_endpoint, undefined),
    V.

getAuthTokenRN() ->
    V = application:get_env(refundcheck, auth_token_rn, undefined),
    V.

getAuthClientId() ->
    V = application:get_env(refundcheck, auth_client_id, undefined),
    V.

getAuthClientSecret() ->
    V = application:get_env(refundcheck, auth_client_secret, undefined),
    V.

getAuthRedirectURILoginCallback() ->
    V = application:get_env(refundcheck, auth_redirect_uri_login_callback, undefined),
    V.

getURIConsole() ->
    V = application:get_env(refundcheck, uri_console, undefined),
    V.

getURILogin() ->
    V = application:get_env(refundcheck, uri_login, undefined),
    V.

getSellerInitialCalls() ->
    S = application:get_env(refundcheck, initial_calls, "20"),
    V = list_to_integer(S),
    V.
    
% -- -- -- -- -- -- -- -- -- -- --

newToken(Size) when is_integer(Size) ->
    base64:encode(crypto:strong_rand_bytes(Size)).

newLoginToken() ->
    LoginToken = newToken(32),
    set_conf({login_token, LoginToken}, []),
    LoginToken.

isLoginToken(LoginToken) ->
    case get_confs({login_token, LoginToken}) of
        [] -> false;
        _Exists -> true
    end.

removeLoginToken(LoginToken) ->
    delete_conf({login_token, LoginToken}).

% -- -- -- -- -- -- -- -- -- -- --


init_conf() ->
    case ets:info(refundcheck_conf_table) of
        undefined ->
            ets:new(refundcheck_conf_table, [bag, named_table, public]);
        _->
            ok
    end.

get_conf(ConfName) ->
    [{_ConfKey, ConfValue}] = ets:lookup(refundcheck_conf_table, ConfName),
    ConfValue.

get_confs(ConfName) ->
    ets:lookup(refundcheck_conf_table, ConfName).

set_conf(ConfName, ConfValue) ->
    ets:insert(refundcheck_conf_table, {ConfName, ConfValue}).

delete_conf(ConfName) ->
    ets:delete(refundcheck_conf_table, ConfName).

% -- -- -- -- -- -- -- -- -- -- --

