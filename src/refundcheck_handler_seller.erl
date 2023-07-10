-module(refundcheck_handler_seller).

-behavior(cowboy_handler).

-include_lib("kernel/include/logger.hrl").

-export([
    init/2,
    allowed_methods/2,
    % content_types_accepted/2
    content_types_provided/2
]).
-export([
    % from_json/2
    to_json/2
    , processData/1
]).


init(Req0, State) ->
    {cowboy_rest, Req0, State}.


allowed_methods(Req0, State) ->
    Req = cowboy_req:set_resp_headers(#{
        <<"Access-Control-Allow-Origin">> => <<"*">>,
        <<"Access-Control-Allow-Methods">> => <<"GET,POST">>,
        <<"Access-Control-Allow-Headers">> => <<"Content-Type">>
    }, Req0),
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

% content_types_accepted(Req, State) ->
%     %%  define AcceptCallback callback for PUT, POST, PATCH
%     Handler = [
%         {<<"application/json">>, from_json}
%     ],
%     {Handler, Req, State}.

content_types_provided(Req, State) ->
    %%  define ProvideResource callback (ProvideCallback) for GET, HEAD
    Handler = [
        {<<"application/json">>, to_json}
    ],
    {Handler, Req, State}.



% %% AcceptCallback   (for PUT, POST, PATCH)
% %%      Result  :: true
% %%               | {created, URI :: iodata()}
% %%               | {see_other, URI :: iodata()}
% %%               | false
% from_json(Req0, State) ->
%     {ok, OrgBodyEnc, Req1} = cowboy_req:read_body(Req0),
%     OrgBody = jsx:decode(OrgBodyEnc, [return_maps]),
%     RespBody = processData(OrgBody),
%     RespBodyEnc = jsx:encode(RespBody),
%     ReqN = cowboy_req:set_resp_body(RespBodyEnc, Req1),
%     {stop, ReqN, State}.  %%  {Result, Req, State}


%% ProvideCallback   (for GET, HEAD)
%%      Result :: cowboy_req:resp_body()
to_json(Req0, State) ->
    ?LOG_INFO("seller handler"),

    % get User API Key
    UserAPIKey = getUserAPIKey(Req0),
    io:format("~p:~p UserAPIKey:~p~n", [?MODULE, ?LINE, UserAPIKey]),

    RespBody = processData(UserAPIKey),
    io:format("~p:~p RespBody:~p~n", [?MODULE, ?LINE, RespBody]),

    RespBodyEnc = jsx:encode(RespBody),
    HTTPRespStatus = case RespBody of
        #{result := <<"ok">>}    -> 200;
        #{result := <<"error">>} -> 400;
        _Other -> 500
    end,
    ReqN = cowboy_req:reply(
        HTTPRespStatus, 
        #{ <<"content-type">> => <<"application/json">> },
        RespBodyEnc,
        Req0
    ),
    {stop, ReqN, State}.  %%  {Result, Req, State}


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

% get User API Key
getUserAPIKey(Req) ->
    #{
        user_key := UserAPIKey
    } = cowboy_req:match_qs([user_key], Req),
    UserAPIKey.

processData(UserAPIKey) ->
    RespBody = case refundcheck:getSeller(UserAPIKey) of
        [] ->
            #{result => <<"error">>};
        Seller -> 
            maps:merge(Seller, #{result => <<"ok">>})
    end,
    RespBody.

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

