-module(refundcheck_auth_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    {Peer, _} = cowboy_req:peer(Req),
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    % error_logger:info_msg("~p: [~p]: ~p ~p", [calendar:universal_time(), Peer, Method, Path]),
    logger:info("~p - ~p ~p", [Peer, Method, Path]),

    Resp = case Path of
        <<"/login">> ->  % open for login
            {ok, Req, Env};

        <<"/login_callback">> ->  % open for login
            {ok, Req, Env};

        _Other ->
            % get User API Key
            UserAPIKey = case cowboy_req:header(<<"Authorization">>, Req, <<"">>) of
                <<"Bearer ", API_Key/bitstring>> ->
                    API_Key;
                <<"bearer ", API_Key/bitstring>> ->
                    API_Key
            end,

            % validate User API Key
            case refundcheck:isValidUserAPIKey(UserAPIKey) of
                true ->
                    {ok, Req, Env};
                false ->
                    {stop, Req}
            end
        end,

    Resp.