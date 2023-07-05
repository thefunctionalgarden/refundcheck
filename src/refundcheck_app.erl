%%%-------------------------------------------------------------------
%% @doc refundcheck public API
%% @end
%%%-------------------------------------------------------------------

-module(refundcheck_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->

    refundcheck_config:init_conf(),

    % start for epgsql
     {ok, _} = application:ensure_all_started(epgsql),

    % start cowboy
    Routes   = refundcheck_config:getRoutes(),
    Dispatch = cowboy_router:compile(Routes),
    HTTPPort = refundcheck_config:getHTTPPort(),
    {ok, _} = cowboy:start_clear(
        refundcheck_http_listener,
        [{port, HTTPPort}],
        #{
            env => #{dispatch => Dispatch},
            protocol_options => [
                {versions, ['HTTP/1.1', 'HTTP/2']}
            ],
            middlewares => [cowboy_router, refundcheck_auth_middleware, cowboy_handler]
        }
    ),

    % {ok, _} = cowboy:start_tls(refundcheck_https_listener,
    %     [
    %         {port, HTTPPort},
    %         {certfile, "/path/to/certfile"},
    %         {keyfile, "/path/to/keyfile"}
    %     ],
    %     #{env => #{dispatch => Dispatch}}
    % ),

    refundcheck_sup:start_link().

stop(_State) ->
    % ok = cowboy:stop_listener(refundcheck_https_listener),
    ok = cowboy:stop_listener(refundcheck_http_listener),
    ok.

%% internal functions