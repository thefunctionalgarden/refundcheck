-module(refundcheck_refund_handler).

-behavior(cowboy_handler).

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


init(Req0, State) ->
    {cowboy_rest, Req0, State}.


allowed_methods(Req0, State) ->
    Req = cowboy_req:set_resp_headers(#{
        <<"Access-Control-Allow-Origin">> => <<"*">>,
        <<"Access-Control-Allow-Methods">> => <<"POST">>,
        <<"Access-Control-Allow-Headers">> => <<"Content-Type">>
    }, Req0),
    {[<<"POST">>, <<"OPTIONS">>], Req, State}.

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
    {ok, OrgBodyEnc, Req1} = cowboy_req:read_body(Req0),
    OrgBody = jsx:decode(OrgBodyEnc, [return_maps]),
    RespBody = processData(OrgBody),
    RespBodyEnc = jsx:encode(RespBody),
    ReqN = cowboy_req:set_resp_body(RespBodyEnc, Req1),
    {stop, ReqN, State}.  %%  {Result, Req, State}


% %% ProvideCallback   (for GET, HEAD)
% %%      Result :: cowboy_req:resp_body()
% to_json(Req0, State) ->
%     {Result, Req0, State}.  %%  {Result, Req0, State}


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

processData(OrgBody) ->
    RespBody = refundcheck:registerRefund(OrgBody),
    RespBody.

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

