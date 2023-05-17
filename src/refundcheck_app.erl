%%%-------------------------------------------------------------------
%% @doc refundcheck public API
%% @end
%%%-------------------------------------------------------------------

-module(refundcheck_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    % % start for pgapp
    % application:ensure_all_started(pgapp),
    % pgapp:connect(db_pool_0, [
    %     {size, 5}, 
    %     {database, "cam"}, 
    %     {username, "cam"}, 
    %     {password, "Casteril0!"},
    %     {host,     "localhost"}
    % ]),

    % start for epgsql
     {ok, _} = application:ensure_all_started(epgsql),
    % Host = "localhost",
    % Username = "cam",
    % Password = "Casteril0!",
    % {ok, _Conn} = epgsql:connect(Host, Username, Password, []),

    % % start for pooler + epgsql
    % PoolConfig = #{
    %     name => sql_db,
    %     max_count => 5,
    %     init_count => 2,
    %     start_mfa => {riakc_pb_socket, start_link, ["localhost", 8081]}
    % },
    % pooler:new_pool(PoolConfig).

    Dispatch = cowboy_router:compile([
        {'_', [{"/refund_check/v1/customer", refundcheck_customer_handler, []}]},
        {'_', [{"/refund_check/v1/purchase", refundcheck_purchase_handler, []}]},
        {'_', [{"/refund_check/v1/refund",   refundcheck_refund_handler, []}]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener,
        [{port, 2020}],
        #{env => #{dispatch => Dispatch}}
        ),    

    refundcheck_sup:start_link().

stop(_State) ->
    ok.

%% internal functions