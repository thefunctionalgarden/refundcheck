-module(refundcheck_handler_checkout).

-behavior(cowboy_handler).
-behaviour(trails_handler).

% -include_lib("kernel/include/logger.hrl").

-export([
    init/2,
    allowed_methods/2,
    content_types_accepted/2,
    content_types_provided/2
]).
-export([
    from_json/2
    % to_send/2
]).
-export([trails/0]).


-define(PROVIDER, <<"paypal">>).
-define(CREATE_ORDER_PATH,  <<"/refund_check/create_paypal_order">>).
-define(CAPTURE_ORDER_PATH, <<"/refund_check/capture_paypal_order">>).


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


trails() ->
    [
        trails:trail(?CREATE_ORDER_PATH,  ?MODULE, [], #{}),
        trails:trail(?CAPTURE_ORDER_PATH, ?MODULE, [], #{})
    ].


init(Req0, State) ->
    {cowboy_rest, Req0, State}.


% valid_content_headers(Req, State) ->
%     {true, Req, State}.

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

content_types_provided(Req, State) ->
    %%  define ProvideResource callback (ProvideCallback) for GET, HEAD
    Handler = [
        {<<"application/json">>, to_send}
    ],
    {Handler, Req, State}.



%% AcceptCallback   (for PUT, POST, PATCH)
%%      Result  :: true
%%               | {created, URI :: iodata()}
%%               | {see_other, URI :: iodata()}
%%               | false
from_json(Req0, State) ->
    io:format("~p:~p ~n", [?MODULE, ?LINE]),
    application:ensure_all_started(restc),
    
    Path = cowboy_req:path(Req0),
    {ok, OrgBodyEnc, Req1} = cowboy_req:read_body(Req0),
    CheckoutBody = jsx:decode(OrgBodyEnc, [return_maps, {labels, atom}]),
    io:format("~p:~p CheckoutBody:~p ~n", [?MODULE, ?LINE, CheckoutBody]),

    UserAPIKey = refundcheck_handler_helper:get_api_key(Req0),
    Seller = refundcheck:getSeller(UserAPIKey),

    {_RespCode, HTTPRespCode, _H, RespBody} = case Path of
        ?CREATE_ORDER_PATH ->
            SKU = maps:get(sku, CheckoutBody),
            #{amount := OrderAmount} = refundcheck:getPlan(SKU),

            CreateOrderResp = create_order(OrderAmount),
            {_, _, _, CreateOrderData} = CreateOrderResp,
            refundcheck:payment_start(Seller, SKU, OrderAmount, CreateOrderData),
            CreateOrderResp;

        ?CAPTURE_ORDER_PATH ->
            OrderId = maps:get(orderID, CheckoutBody),
            CapturePaymentResp = capture_payment(OrderId),
            {_, _, _, PaymentData} = CapturePaymentResp,
            refundcheck:payment_complete(Seller, PaymentData),
            CapturePaymentResp
    end,

    % io:format("~p:~p RespBody:~p ~n", [?MODULE, ?LINE, RespBody]),
    
    RespBodyEnc = jsx:encode(RespBody),
    % ReqN = cowboy_req:set_resp_body(RespBodyEnc, Req1),
    ReqN = cowboy_req:reply(
        HTTPRespCode, 
        #{ <<"content-type">> => <<"application/json">> },
        RespBodyEnc,
        Req1
    ),
    {stop, ReqN, State}.  %%  {Result, Req, State}
    

% navigateToConsole(SellerKey, Req) ->
%     LandingURI = refundcheck_handler_helper:build_landing_url(SellerKey),
%     ReqN = cowboy_req:reply(
%         303,   %308, %304, %300, %302, %301, %307, %303,
%         #{
%             <<"location">> => LandingURI  % to the console
%             % <<"authorization">> => <<"Bearer ", SellerKey/bitstring>>
%             % <<"sellersguard-key">> => SellerKey
%         },
%         Req
%     ),
%     ReqN.


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


get_access_token() ->
    OAuth2TokenEndpoint = refundcheck_config:getAuthTokenEndpoint(?PROVIDER),
    OAuth2TokenRN       = refundcheck_config:getAuthTokenRN(?PROVIDER),
    ClientId = refundcheck_config:getAuthClientId(?PROVIDER),
    ClientSecret = refundcheck_config:getAuthClientSecret(?PROVIDER),

    IdColonSecret = base64:encode(<<ClientId/bitstring, ":", ClientSecret/bitstring>>),
    Headers = [{<<"authorization">>, <<"Basic ", IdColonSecret/bitstring>>}],
    Body = #{grant_type => client_credentials},

    TokenURL = restc:construct_url(OAuth2TokenEndpoint, OAuth2TokenRN, []),
    io:format("~p:~p TokenURL:~p ~n", [?MODULE, ?LINE, TokenURL]),

    OAuthData = case restc:request(post, percent, TokenURL, [], Headers, Body) of
        {ok, 200, _H, RespBody} ->
            io:format("~p:~p RespBody:~p ~n", [?MODULE, ?LINE, RespBody]),
            % {
            %     "access_token": "1/fFAGRNJru1FTz70BzhT3Zg",
            %     "expires_in": 3920,
            %     "token_type": "Bearer",
            %     "scope": "https://www.googleapis.com/auth/drive.metadata.readonly",
            %     "refresh_token": "1//xEoDL4iW3cxlI7yDbSRFYNG01kVKM2C-259HOF2aQbI"
            % }
            AccessToken  = maps:get(<<"access_token">>,  RespBody, undefined),
            ExpiresIn    = maps:get(<<"expires_in">>,    RespBody, undefined),
            RefreshToken = maps:get(<<"refresh_token">>, RespBody, undefined),
            #{
                access_token  => AccessToken,
                expires_in    => ExpiresIn,
                refresh_token => RefreshToken
            };

        Other ->
            io:format("~p:~p Other:~p ~n", [?MODULE, ?LINE, Other]),
            #{
                access_token  => <<"">>,
                expires_in    => undefined,
                refresh_token => <<"">>
            }
    end,
    % OAuthData.
    maps:get(access_token, OAuthData).


create_order(Amount) when is_integer(Amount) ->
    AccessToken = get_access_token(),
    PaypalEndpoint     = refundcheck_config:getPaypalEndpoint(),
    OrdersResourceName = <<"v2/checkout/orders">>,
    PaypalOrdersURL = restc:construct_url(PaypalEndpoint, OrdersResourceName, []),
    Headers = [{<<"Authorization">>, <<"Bearer ", AccessToken/bitstring>>}],
    Body = #{
      intent => <<"CAPTURE">>,
      purchase_units => [
        #{
          amount => #{
            currency_code => <<"USD">>,
            value => format_amount(Amount)
          }
        }
      ]
    },
    RequestResp = restc:request(post, json, PaypalOrdersURL, [201], Headers, Body),
    RequestResp.


format_amount(Amount) when is_integer(Amount) ->
    AmountBin = integer_to_binary(Amount),
    <<AmountBin/bitstring, ".00">>.



capture_payment(OrderId) when is_bitstring(OrderId) ->
    AccessToken = get_access_token(),
    PaypalEndpoint     = refundcheck_config:getPaypalEndpoint(),
    OrdersResourceName = <<"v2/checkout/orders/", OrderId/bitstring, "/capture">>,
    PaypalOrdersURL = restc:construct_url(PaypalEndpoint, OrdersResourceName, []),
    Headers = [{<<"Authorization">>, <<"Bearer ", AccessToken/bitstring>>}],

    RequestResp = restc:request(post, json, PaypalOrdersURL, [200], Headers),
    RequestResp.




%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --