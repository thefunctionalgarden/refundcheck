%%%-------------------------------------------------------------------
%% @doc refundcheck public API
%% @end
%%%-------------------------------------------------------------------

-module(refundcheck_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    % start for epgsql
     {ok, _} = application:ensure_all_started(epgsql),

    Routes = [
        {'_',[
            {"/refund_check/:api_version/customer/global/:customer_mail",  refundcheck_customer_global_handler, []},
            {"/refund_check/:api_version/customer/:customer_mail",         refundcheck_customer_handler, []},
            {"/refund_check/:api_version/purchase", refundcheck_purchase_handler, []},
            {"/refund_check/:api_version/refund",   refundcheck_refund_handler,   []}
            ]
        }
    ],
    Dispatch = cowboy_router:compile(Routes),
    HTTPPort = refundcheck_config:getHTTPPort(),
    {ok, _} = cowboy:start_clear(
        refundcheck_http_listener,
        [{port, HTTPPort}],
        #{env => #{dispatch => Dispatch}}
    ),
    % {ok, _} = cowboy:start_tls(
    %     refundcheck_https_listener,
    %     [{port, 2023}],
    %     #{env => #{dispatch => Dispatch}}
    % ),

    refundcheck_sup:start_link().

stop(_State) ->
    ok.

%% internal functions