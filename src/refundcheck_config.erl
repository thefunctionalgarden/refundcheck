-module(refundcheck_config).

-export([
    init_conf/0,
    get_conf/1,
    set_conf/2,
    delete_conf/1,

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
    getAuthTokenEndpoint/1,
    getAuthTokenRN/1,
    getAuthClientId/1,
    getAuthClientSecret/1,
    getAuthRedirectURILoginCallback/0,
    getURIConsole/0,
    getURICheckout/0,
    getURILogin/0,

    getPaypalEndpoint/0,

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

getPaypalEndpoint() ->
    V = application:get_env(refundcheck, paypal_endpoint, undefined),
    V.

getAuthTokenEndpoint(<<"paypal">>) ->
    V = getPaypalEndpoint(),
    V;
getAuthTokenEndpoint(<<"google">>) ->
    V = application:get_env(refundcheck, auth_token_endpoint_google, undefined),
    V.

getAuthTokenRN(Provider) ->
    ConfKeyStr = <<"auth_token_rn_", Provider/bitstring>>,
    ConfKey = binary_to_atom(ConfKeyStr),
    V = application:get_env(refundcheck, ConfKey, undefined),
    V.

getAuthClientId(Provider) ->
    ConfKeyStr = <<"auth_client_id_", Provider/bitstring>>,
    ConfKey = binary_to_atom(ConfKeyStr),
    V = application:get_env(refundcheck, ConfKey, undefined),
    V.

getAuthClientSecret(Provider) ->
    ConfKeyStr = <<"auth_client_secret_", Provider/bitstring>>,
    ConfKey = binary_to_atom(ConfKeyStr),
    V = application:get_env(refundcheck, ConfKey, undefined),
    V.

getAuthRedirectURILoginCallback() ->
    V = application:get_env(refundcheck, auth_redirect_uri_login_callback, undefined),
    V.

getURIConsole() ->
    V = application:get_env(refundcheck, uri_console, undefined),
    V.

getURICheckout() ->
    V = application:get_env(refundcheck, uri_checkout, undefined),
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

