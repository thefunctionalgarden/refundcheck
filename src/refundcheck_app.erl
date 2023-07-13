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

    % Routes   = refundcheck_config:getRoutes(),
    % Dispatch = cowboy_router:compile(Routes),


    Handlers = [
        cowboy_swagger_handler,

        refundcheck_handler_customer_global,
        refundcheck_handler_customer,
        refundcheck_handler_purchase,
        refundcheck_handler_refund

        % refundcheck_handler_healthcheck,
        % refundcheck_handler_admin,
        % refundcheck_handler_auth,
        % refundcheck_handler_static
    ],

    ConfiguredTrails = refundcheck_config:getTrails(),
    HandlersTrails   = trails:trails(Handlers),
    Trails = lists:append(HandlersTrails, ConfiguredTrails),

    % io:format("~p:~p Trails:~p~n", [?MODULE, ?LINE, Trails]),
    
    % store them
    trails:store(Trails),
    % and then compile them
    Dispatch = trails:single_host_compile(Trails),


    HTTPPort = refundcheck_config:getHTTPPort(),
    {ok, _} = cowboy:start_clear(
        refundcheck_http_listener,
        [{port, HTTPPort}],
        #{
            env => #{dispatch => Dispatch},
            protocol_options => [
                {versions, ['HTTP/1.1', 'HTTP/2']}
            ],
            middlewares => [cowboy_router, refundcheck_middleware_auth, cowboy_handler]
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


    NewSpec = #{
        openapi => "3.0.0",
        info => #{title => "Sellers Guard"},
        servers => [
            #{url => "https://api.sellersguard.com"}
        ]
    },
    cowboy_swagger:set_global_spec(NewSpec),
    

    refundcheck_sup:start_link().

stop(_State) ->
    % ok = cowboy:stop_listener(refundcheck_https_listener),
    ok = cowboy:stop_listener(refundcheck_http_listener),
    ok.

%% internal functions