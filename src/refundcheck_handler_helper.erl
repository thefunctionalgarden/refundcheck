-module(refundcheck_handler_helper).

-export([
    get_api_key/1
]).

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


