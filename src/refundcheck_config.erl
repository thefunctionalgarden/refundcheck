-module(refundcheck_config).

-export([
    getDBHost/0,
    getDBUser/0,
    getDBPass/0,
    getRoutes/0,
    getHTTPPort/0
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
