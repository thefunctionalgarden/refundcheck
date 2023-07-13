-module(refundcheck_handler_customer_global).

-behaviour(trails_handler).
-behaviour(cowboy_handler).

-include_lib("kernel/include/logger.hrl").

-export([trails/0]).

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


trails() ->
    DefinitionName = <<"CustomerInfo">>,
    DefinitionProperties =
        #{ <<"name">> =>
            #{ type => <<"string">>
            , description => <<"Customer risk information">>
            }
        , <<"description">> =>
            #{ type => <<"string">>
            , description => <<"Newspaper description">>
            }
        },
    % Add the definition
    ok = cowboy_swagger:add_definition(DefinitionName, DefinitionProperties),

    Param_APIVersion = #{
        name => <<"api_version">>,
        in => path,
        description => <<"API version.  Currently \"v1\" is the expected value.">>,
        required => true,
        example => <<"v1">>
    },
    Param_CustomerMail = #{
        name => <<"customer_mail">>,
        in => path,
        description => <<"The email of the customer that about whom risk information is required.">>,
        required => true,
        example => <<"customer@example.com">>
    },
    % SecurityReq = #{
    %     api_key => <<"user_key">>
    % },
    RespOK = #{
        description => "Customer risk information"
    },
    Metadata = #{
        get => #{
            tags => ["Sellers Guard"],
            summary => "Risk information about a customer",
            description => "Returns risk information about a customer.  This should be called before confirming a purchase.  This allows to know whether the customer has purchased and then refunded to different sellers.",
            parameters => [Param_APIVersion, Param_CustomerMail],
            % security => SecurityReq,
            'content-type' => "application/json",
            responses => #{
                200 => RespOK
            }
        }
    },
    [trails:trail("/refund_check/:api_version/customer/global/:customer_mail", ?MODULE, [], Metadata)].



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
    ?LOG_INFO("starting processing"),

    CustomerMail = cowboy_req:binding(customer_mail, Req0, <<"">>),
    ApiVersion   = cowboy_req:binding(api_version,   Req0, <<"v1">>),
    io:format("ApiVersion:~p~n", [ApiVersion]),
    io:format("CustomerMail:~p~n", [CustomerMail]),

    RespBody = processData(CustomerMail),
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

processData(Data) ->
    RespBody = refundcheck:getCustomerHistoryGlobal(Data),
    RespBody.

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

