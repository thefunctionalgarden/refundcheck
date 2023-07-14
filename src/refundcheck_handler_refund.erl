-module(refundcheck_handler_refund).

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
-export([get_schema_customer_refund_report_response/0]).
-export([get_schema_customer_refund_report/0]).


trails() ->
    [trails:trail("/refund_check/:api_version/refund", ?MODULE, [], get_metada())].


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
    ?LOG_INFO("starting refund reg"),

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
    RespBody = refundcheck:registerRefund(OrgBody, Seller),
    RespBody.

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


get_schema_customer_refund_report_response() ->
    Schema_CustomerRefundReportResp = #{
        <<"description">> => #{
            type => <<"string">>,
            description => <<"A text describing the result of the operation">>
        },
        <<"result">> => #{
            type => <<"string">>,
            description => <<"'ok' if the report was succesful, 'error' if the report failed">>
        }
    },
    Schema_CustomerRefundReportResp.


get_schema_customer_refund_report() ->
    Schema_CustomerRefundReport = #{
        <<"customer_mail">> => #{
            type => <<"string">>,
            description => <<"Customer email">>
        },
        <<"purchase_trx_id">> => #{
            type => <<"string">>,
            description => <<"Id of the purchase transaction that is refunded">>
        },
        <<"refund_type">> => #{
            type => <<"number">>,
            description => <<"An Id describing the kind of product or service refundd by the customer in this transaction:\n
1 : Total refund \n
2 : Partial refund">>
        },
        <<"extra_info">> => #{
            type => <<"string">>,
            description => <<"Some additional information about the refund.  Can be \"\".">>
        }
    },
    Schema_CustomerRefundReport.


get_metadata_refund_resp_ok() ->
    RespOKExample0 = #{
        <<"result">> => <<"ok">>,
        <<"description">> => <<"refund has been registered">>
    },
    RespOK = #{
        description => "Refund information correctly registered.",
        content => #{
            'application/json' => #{
                schema => cowboy_swagger:schema(<<"customer_refund_report_response">>),
                examples => #{
                    <<"ok_0">> => #{
                        summary => "Refund correctly registered",
                        value => jsx:encode(RespOKExample0)
                    }
                }
            }
        }
    },
    RespOK.

get_metadata_refund_request_body() ->
    ReqExample0 = #{
        <<"customer_mail">> => <<"new.customer.01@somemail.com">>,
        <<"purchase_trx_id">> => <<"TRX-0123456789">>,
        <<"refund_type">> => 2,
        <<"extra_info">> => <<"a description">>
    },
    #{
        description => "Customer refund data to be registered.  This will be used to update customer's risk score.",
        content => #{
            'application/json' => #{
                schema => cowboy_swagger:schema(<<"customer_refund_report">>),
                examples => #{
                    <<"ok_0">> => #{
                        summary => "A standard refund",
                        value => jsx:encode(ReqExample0)
                    }
                }
            }
        },
        required => true
    }.




%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


get_metada() ->
    Param_APIVersion = refundcheck_handler_helper:get_metadata_param_api_ver(),
    RequestBody = get_metadata_refund_request_body(),
    RespOK = get_metadata_refund_resp_ok(),
    Security = #{user_key => ["apiKey"]},
    Metadata = #{
        post => #{
            tags => ["Sellers Guard"],
            summary => "Report a Refund",
            description => "Report information about a refund.  \
                            This function must be called after a refund has been requested.  \
                            This reporting plays an important role in the calculation process for assessing the customer's risk of refund.",
            parameters => [Param_APIVersion],
            requestBody => RequestBody,
            security => [Security],
            'content-type' => "application/json",
            responses => #{
                200 => RespOK
            }
        }
    },
    Metadata.


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

