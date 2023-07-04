-module(refundcheck_config).

-export([
    getDBHost/0,
    getDBUser/0,
    getDBPass/0,
    getRoutes/0,
    getHTTPPort/0,
    getAuthEndpoint/0,
    getAuthRN/0,
    getAuthTokenEndpoint/0,
    getAuthTokenRN/0,
    getAuthClientId/0,
    getAuthClientSecret/0,
    getAuthRedirectURILoginCallback/0,
    getAuthRedirectURIHome/0
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

getAuthRedirectURIHome() ->
    V = application:get_env(refundcheck, auth_redirect_uri_home, undefined),
    V.



    
% -- -- -- -- -- -- -- -- -- -- --

