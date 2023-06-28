-module(refundcheck_static_handler).

-behavior(cowboy_handler).

% -include_lib("kernel/include/logger.hrl").
-include_lib("kernel/include/file.hrl").

-export([
    init/2,
    allowed_methods/2,
    % content_types_accepted/2
    content_types_provided/2
]).
-export([
    % from_json/2
    to_send/2
]).


init(Req0, State) ->
    {cowboy_rest, Req0, State}.


% valid_content_headers(Req, State) ->
%     {true, Req, State}.

allowed_methods(Req0, State) ->
    Req = cowboy_req:set_resp_headers(#{
        <<"Access-Control-Allow-Origin">> => <<"*">>,
        <<"Access-Control-Allow-Methods">> => <<"GET, HEAD">>,
        <<"Access-Control-Allow-Headers">> => <<"Content-Type">>
    }, Req0),
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.

% content_types_accepted(Req, State) ->
%     %%  define AcceptCallback callback for PUT, POST, PATCH
%     Handler = [
%         {<<"application/json">>, from_json}
%     ],
%     {Handler, Req, State}.

content_types_provided(Req, State) ->
    %%  define ProvideResource callback (ProvideCallback) for GET, HEAD
    Handler = [
        {<<"text/html">>, to_send}
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
to_send(Req0, State) ->
    HostTokens = cowboy_req:host_info(Req0),
    io:format("request from host: ~p~n", [HostTokens]),

    Path = cowboy_req:path(Req0),
    io:format("requested path: ~p~n", [Path]),

    Filename = case Path of
        <<"/">>             -> <<"public/index.html">>;
        <<"/index.html">>   -> <<"public/index.html">>;
        <<"/privacy.html">> -> <<"public/privacy.html">>;
        _OtherPath -> <<"public/index.html">>
    end,
    
    {ok, #file_info{size = Size}} = file:read_file_info(Filename),
    Req1 = cowboy_req:set_resp_body({sendfile, 0, Size, Filename}, Req0),
    ReqN = cowboy_req:reply(
        200,
        #{ <<"content-type">> => <<"text/html">> },
        Req1
    ),

    {stop, ReqN, State}.  %%  {Result, Req, State}

    


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


