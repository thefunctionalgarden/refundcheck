-module(refundcheck_handler_purchase).

-behaviour(trails_handler).
-behavior(cowboy_handler).

-include_lib("kernel/include/logger.hrl").

-export([trails/0]).
-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2
    % content_types_provided/2
]).
-export([
    from_json/2
    % to_json/2
]).


trails() ->
    Metadata = #{
        get => #{description => "POST method", 'content-type' => "application/json"}
    },
    [trails:trail("/refund_check/:api_version/purchase", ?MODULE, [], Metadata)].


init(Req0, State) ->
    {cowboy_rest, Req0, State}.


allowed_methods(Req0, State) ->
    io:format("HHHHHHHHHHHH~n"),
    ?LOG_INFO("HHHHHHHHHHHH"),
    Req = cowboy_req:set_resp_headers(#{
        <<"Access-Control-Allow-Origin">> => <<"*">>,
        <<"Access-Control-Allow-Methods">> => <<"POST">>,
        <<"Access-Control-Allow-Headers">> => <<"Content-Type">>
    }, Req0),
    {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
    %%  define AcceptCallback callback for PUT, POST, PATCH
    Handler = [
        {<<"application/json">>, from_json}
    ],
    {Handler, Req, State}.

% content_types_provided(Req, State) ->
%     %%  define ProvideResource (or ProvideCallback) callback for GET, HEAD
%     Handler = [
%         {<<"application/json">>, to_json}
%     ],
%     {Handler, Req, State}.



%% AcceptCallback   (for PUT, POST, PATCH)
%%      Result  :: true
%%               | {created, URI :: iodata()}
%%               | {see_other, URI :: iodata()}
%%               | false
from_json(Req0, State) ->
    ?LOG_INFO("starting purchase reg"),

    ApiVersion = cowboy_req:binding(api_version, Req0, <<"v1">>),
    io:format("ApiVersion:~p~n", [ApiVersion]),

    {ok, OrgBodyEnc, Req1} = cowboy_req:read_body(Req0),
    OrgBody = jsx:decode(OrgBodyEnc, [return_maps, {labels, atom}]),

    UserAPIKey = refundcheck_handler_helper:get_api_key(Req1),
    Seller = refundcheck:getSeller(UserAPIKey),

    RespBody = processData(OrgBody, Seller),
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
        Req1
    ),
    {stop, ReqN, State}.  %%  {Result, Req, State}


% %% ProvideCallback   (for GET, HEAD)
% %%      Result :: cowboy_req:resp_body()
% to_json(Req0, State) ->
%     {Result, Req0, State}.  %%  {Result, Req0, State}


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

processData(OrgBody, Seller) ->
    RespBody = refundcheck:registerPurchase(OrgBody, Seller),
    RespBody.

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


