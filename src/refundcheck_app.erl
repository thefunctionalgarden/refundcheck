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
        % swagger => "2.0",
        info => #{title => "SellersGuard"},
        servers => [
            #{url => "https://api.sellersguard.com"}
        ],
        components => #{
            securitySchemes => #{
                user_key => #{
                    type => "apiKey",
                    name => "user_key",
                    in => "header"
                }
            }
        }
    },
    cowboy_swagger:set_global_spec(NewSpec),
    
    Definition_CustomerInfo_Name = <<"customer_info_response">>,
    Definition_CustomerInfo_Properties = refundcheck_handler_customer_global:get_schema_customer_risk_info(),
    ok = cowboy_swagger:add_definition(Definition_CustomerInfo_Name, Definition_CustomerInfo_Properties),

    Definition_CustomerPurchaseReport_Name = <<"customer_purchase_report">>,
    Definition_CustomerPurchaseReport_Properties = refundcheck_handler_purchase:get_schema_customer_purchase_report(),
    ok = cowboy_swagger:add_definition(Definition_CustomerPurchaseReport_Name, Definition_CustomerPurchaseReport_Properties),

    Definition_CustomerPurchaseReportResp_Name = <<"customer_purchase_report_response">>,
    Definition_CustomerPurchaseReportResp_Properties = refundcheck_handler_purchase:get_schema_customer_purchase_report_response(),
    ok = cowboy_swagger:add_definition(Definition_CustomerPurchaseReportResp_Name, Definition_CustomerPurchaseReportResp_Properties),

    Definition_CustomerRefundReport_Name = <<"customer_refund_report">>,
    Definition_CustomerRefundReport_Properties = refundcheck_handler_refund:get_schema_customer_refund_report(),
    ok = cowboy_swagger:add_definition(Definition_CustomerRefundReport_Name, Definition_CustomerRefundReport_Properties),

    Definition_CustomerRefundReportResp_Name = <<"customer_refund_report_response">>,
    Definition_CustomerRefundReportResp_Properties = refundcheck_handler_refund:get_schema_customer_refund_report_response(),
    ok = cowboy_swagger:add_definition(Definition_CustomerRefundReportResp_Name, Definition_CustomerRefundReportResp_Properties),

    refundcheck_sup:start_link().

stop(_State) ->
    % ok = cowboy:stop_listener(refundcheck_https_listener),
    ok = cowboy:stop_listener(refundcheck_http_listener),
    ok.

%% internal functions


