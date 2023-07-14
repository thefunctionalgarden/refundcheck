-module(refundcheck_handler_helper).

-export([
    get_api_key/1
]).
-export([get_metadata_param_api_ver/0]).
-export([get_metadata_param_customer_mail/0]).


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

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --



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

    
    

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


