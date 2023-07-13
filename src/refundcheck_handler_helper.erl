-module(refundcheck_handler_helper).

-export([
    get_api_key/1
]).
-export([get_metadata_param_api_ver/0]).
-export([get_metadata_param_customer_mail/0]).
-export([get_metadata_resp_ok/0]).

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



get_api_key(Req) ->
    case cowboy_req:header(<<"user_key">>, Req, <<"">>) of
        <<"">> -> 
            #{
                user_key := UserAPIKey
            } = cowboy_req:match_qs([{user_key, [], <<"">>}], Req),
            UserAPIKey;
        HeaderUserAPIKey -> HeaderUserAPIKey
    end.



get_metadata_param_api_ver() ->
    Param_APIVersion = #{
        name => <<"api_version">>,
        in => path,
        description => <<"API version.  Currently \"v1\" is the expected value.">>,
        required => true,
        example => <<"v1">>
    },
    Param_APIVersion.

get_metadata_param_customer_mail() ->
    Param_CustomerMail = #{
        name => <<"customer_mail">>,
        in => path,
        description => <<"The email of the customer that about whom risk information is required.">>,
        required => true,
        example => <<"customer@example.com">>
    },
    Param_CustomerMail.

    % SecurityReq = #{
    %     api_key => <<"user_key">>
    % },
    
get_metadata_resp_ok() ->
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
        description => "Customer risk information. Use the 'risk' value as reference.",
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

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


