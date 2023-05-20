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
        {'_', [
            {"/refund_check/:api_version/customer", refundcheck_customer_handler, []},
            {"/refund_check/:api_version/purchase", refundcheck_purchase_handler, []},
            {"/refund_check/:api_version/refund",   refundcheck_refund_handler,   []}
            ]
        }
    ],
    Dispatch = cowboy_router:compile(Routes),
    {ok, _} = cowboy:start_clear(refundcheck_http_listener,
        [{port, 2020}],
        #{env => #{dispatch => Dispatch}}
        ),    

    refundcheck_sup:start_link().

stop(_State) ->
    ok.

%% internal functions