-module(refundcheck_handler_admin).

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
]).


init(Req0, State) ->
    {cowboy_rest, Req0, State}.


allowed_methods(Req0, State) ->
    Req = cowboy_req:set_resp_headers(#{
        <<"Access-Control-Allow-Origin">> => <<"*">>,
        <<"Access-Control-Allow-Methods">> => <<"GET">>,
        <<"Access-Control-Allow-Headers">> => <<"Content-Type">>
    }, Req0),
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

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
    ?LOG_INFO("--ADMIN-------"),
    io:format("~p:~p - ~n", [?MODULE, ?LINE]),

    _ApiVersion = cowboy_req:binding(api_version, Req0, <<"v1">>),
    ResourcePath = cowboy_req:binding(resource_path, Req0, <<"">>),
    ResourceName = cowboy_req:binding(resource_name, Req0, <<"">>),

    RespBody = case ResourcePath of
        <<"lapedrera">> ->
            case ResourceName of
                <<"seller">> ->
                    getSellersMails();
                OtherRN ->
                    io:format("~p:~p - RN:~p~n", [?MODULE, ?LINE, OtherRN]),
                    #{result => <<"error">>}
            end;
        OtherResPath ->
            io:format("~p:~p - RP:~p~n", [?MODULE, ?LINE, OtherResPath]),
            #{result => <<"error">>}
    end,

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

getSellersMails() ->
    RespBody = refundcheck:getSellersMails(),
    RespBody.

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

