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
-export([get_schema_customer_risk_info/0]).



trails() ->
    [trails:trail("/refund_check/:api_version/customer/global/:customer_mail", ?MODULE, [], get_metadata())].



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

get_schema_customer_risk_info() ->
    Schema_CustomerRiskInfo = #{
        <<"risk">> => #{
            type => <<"number">>,
            description => <<"the refund risk index.  0 = lowest risk, 100 = highest risk">>
        },
        <<"description">> => #{
            type => <<"string">>,
            description => <<"A text describing the customer risk">>
        },
        <<"purchases_num">> => #{
            type => <<"number">>,
            description => <<"The number of purchases done by the customer">>
        },
        <<"refunds_num">> => #{
            type => <<"number">>,
            description => <<"The number of refunds requested by the customer">>
        },
        <<"refunds_p">> => #{
            type => <<"number">>,
            description => <<"The percentage of purchases for which the customer requested refunds">>
        },
        <<"result">> => #{
            type => <<"string">>,
            description => <<"'ok' if the query was succesful, 'error' if the query failed">>
        }
    },
    Schema_CustomerRiskInfo.


get_metadata_customer_resp_ok() ->
    RespOKExample0 = #{
        <<"result">> => <<"ok">>,
        <<"description">> => <<"no data for this customer">>,
        <<"purchases_num">> => 0,
        <<"refunds_num">> => 0,
        <<"refunds_p">> => 0,
        <<"risk">> => 0
    },
    RespOKExample1 = #{
        <<"result">> => <<"ok">>,
        <<"description">> => <<"customer has some refunds, looking fair">>,
        <<"purchases_num">> => 4,
        <<"refunds_num">> => 1,
        <<"refunds_p">> => 25,
        <<"risk">> => 10
    },
    RespOKExample2 = #{
        <<"result">> => <<"ok">>,
        <<"description">> => <<"customer looks suspicious">>,
        <<"purchases_num">> => 4,
        <<"refunds_num">> => 2,
        <<"refunds_p">> => 50,
        <<"risk">> => 30
    },
    RespOKExample3 = #{
        <<"result">> => <<"ok">>,
        <<"description">> => <<"customer looks abusive">>,
        <<"purchases_num">> => 4,
        <<"refunds_num">> => 3,
        <<"refunds_p">> => 75,
        <<"risk">> => 90
    },
    RespOK = #{
        description => "Customer risk information. Use the **risk** value as reference: 0 = lowest risk, 100 = highest risk",
        content => #{
            'application/json' => #{
                schema => cowboy_swagger:schema(<<"customer_info_response">>),
                examples => #{
                    <<"ok_0">> => #{
                        summary => "No data for this customer yet",
                        value => jsx:encode(RespOKExample0)
                    },
                    <<"ok_1">> => #{
                        summary => "Customer has very few refunds",
                        value => jsx:encode(RespOKExample1)
                    },
                    <<"ok_2">> => #{
                        summary => "Customer looks suspicious",
                        value => jsx:encode(RespOKExample2)
                    },
                    <<"ok_3">> => #{
                        summary => "Customer looks abusive",
                        value => jsx:encode(RespOKExample3)
                    }
                }
            }
        }
    },
    RespOK.

get_metadata() ->
    Param_APIVersion = refundcheck_handler_helper:get_metadata_param_api_ver(),
    Param_CustomerMail = refundcheck_handler_helper:get_metadata_param_customer_mail(),
    RespOK = get_metadata_customer_resp_ok(),
    Security = #{user_key => ["apiKey"]},
    Metadata = #{
        get => #{
            tags => ["Sellers Guard"],
            summary => "Risk information about a Customer",
            description => "Returns risk information about a customer.  \
                            This function must be called **before** confirming a purchase.  \
                            The returned information allows you to make an informed decision on whether to proceed with the sale or cancel it based on the customer's risk score.  \
                            This feature provides insights into whether the customer has engaged in previous purchases and subsequently requested refunds from different sellers.",
            parameters => [Param_APIVersion, Param_CustomerMail],
            security => [Security],
            'content-type' => "application/json",
            responses => #{
                200 => RespOK
            }
        }
    },
    Metadata.

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



