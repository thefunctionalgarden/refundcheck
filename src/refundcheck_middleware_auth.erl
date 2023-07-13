-module(refundcheck_middleware_auth).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    {Peer, _} = cowboy_req:peer(Req),
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    % error_logger:info_msg("~p: [~p]: ~p ~p", [calendar:universal_time(), Peer, Method, Path]),
    logger:info("~p - ~p ~p", [Peer, Method, Path]),



    Resp = case Path of
        <<"/">> ->  % open for landing page
            {ok, Req, Env};

        <<"/css/sellersguard.css">> ->  % open for css
            {ok, Req, Env};

        <<"/favicon.ico">> ->  % open for favicon.ico
            {ok, Req, Env};

        <<"/login">> ->  % open for login
            {ok, Req, Env};

        <<"/login_callback">> ->  % open for login
            {ok, Req, Env};

        <<"/healthcheck">> ->  % open for healthcheck
            {ok, Req, Env};

        <<"/api-docs", _MoreAPIDocsPath/bitstring>> ->  % open for api-docs
            {ok, Req, Env};

        _Other ->
            % % headers do not go to redirected url
            UserAPIKey = refundcheck_handler_helper:get_api_key(Req),

            % validate User API Key
            case refundcheck:isValidUserAPIKey(UserAPIKey) of
                true ->
                    io:format("~p:~p ok - Path:~p ~n", [?MODULE, ?LINE, Path]),
                    {ok, Req, Env};
                false ->
                    io:format("~p:~p err - Invalid User Key - Path:~p ~n", [?MODULE, ?LINE, Path]),
                    {stop, Req}
            end
        end,

    Resp.



