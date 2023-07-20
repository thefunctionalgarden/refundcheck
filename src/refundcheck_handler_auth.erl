-module(refundcheck_handler_auth).

-behavior(cowboy_handler).

% -include_lib("kernel/include/logger.hrl").

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

-define(PROVIDER, <<"google">>).


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


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
    io:format("~p:~p ~n", [?MODULE, ?LINE]),
    application:ensure_all_started(restc),
    Path = cowboy_req:path(Req0),
    case Path of
        <<"/login">> ->
            LoginStateToken = refundcheck_config:newLoginToken(),
            OAuthURL = build_oauth_url(LoginStateToken),

            ReqN = cowboy_req:reply(
                303,
                #{ <<"location">> => OAuthURL },
                Req0
            );

        <<"/login_callback">> ->
            #{
                code  := Code,
                state := LoginTokenCallback
            } = cowboy_req:match_qs([code, state], Req0), % if there's no code, there's an error q

            case refundcheck_config:isLoginToken(LoginTokenCallback) of
                false ->
                    % invalid login token, back to the login
                    io:format("~p:~p Invalid LoginTokenCallback:~p ~n", [?MODULE, ?LINE, LoginTokenCallback]),
                    LoginURI = refundcheck_config:getURILogin(),
                    ReqN = cowboy_req:reply(
                        303,
                        #{ <<"location">> => LoginURI }, % to the console
                        Req0
                    );

                true ->
                    refundcheck_config:removeLoginToken(LoginTokenCallback),
                    OAuthData = get_access_token(Code),
                    PeopleData = getPeopleData(maps:get(access_token, OAuthData)),
                    io:format("~p:~p PeopleData:~p ~n", [?MODULE, ?LINE, PeopleData]),

                    % verify if user is registered, or create
                    SellerData = refundcheck:login(PeopleData),
                    SellerKey  = maps:get(seller_key, SellerData),

                    ConsoleURI = build_console_url(SellerKey),
                    ReqN = cowboy_req:reply(
                        303,   %308, %304, %300, %302, %301, %307, %303,
                        #{
                            <<"location">> => ConsoleURI % to the console
                            % <<"authorization">> => <<"Bearer ", SellerKey/bitstring>>
                            % <<"sellersguard-key">> => SellerKey
                        },
                        Req0
                    )
            end
    end,

    {stop, ReqN, State}.  %%  {Result, Req, State}

    


%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


% service_account() ->
%     io:format("~p:~p ~n", [?MODULE, ?LINE]),
%     application:ensure_all_started(oauth2c),
%     application:ensure_all_started(ssl),
%     io:format("~p:~p ~n", [?MODULE, ?LINE]),

%     F = <<"/home/cam/sec/oauth2/service-account-key-sellers-guard-dd25e33776e0.json">>,
%     Scope = <<"https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile">>,

%     Client0 = oauth2c:from_service_account_file(F, Scope),
%     {ok, _Headers, Client1} = oauth2c:retrieve_access_token(Client0, []),

%     io:format("~p:~p Client:~p ~n", [?MODULE, ?LINE, Client1]),

%     ok.


build_console_url(SellerKey) ->
    ConsoleRN = refundcheck_config:getURIConsole(),
    Q1 = {"user_key", SellerKey},
    ConsoleURL = restc:construct_url(<<"/">>, ConsoleRN, [Q1]),
    ConsoleURL.



build_oauth_url(StateToken) ->
    io:format("~p:~p ~n", [?MODULE, ?LINE]),

    % https://developers.google.com/identity/protocols/oauth2/web-server
    OAuth2Endpoint = refundcheck_config:getAuthEndpoint(),
    OAuth2RN = refundcheck_config:getAuthRN(),
    Q1 = {"client_id", refundcheck_config:getAuthClientId(?PROVIDER)},
    Q2 = {"redirect_uri", refundcheck_config:getAuthRedirectURILoginCallback()},
    Q3 = {"response_type", <<"code">>},
    Q4 = {"scope", <<"https://www.googleapis.com/auth/userinfo.email https://www.googleapis.com/auth/userinfo.profile">>},
    Q5 = {"access_type", <<"online">>},
    Q6 = {"state", StateToken}, % save this state and verify it comes untouched after the redirection in step2
    Q7 = {"include_granted_scopes", <<"true">>},
    % Q8 = {"login_hint",},
    % Q9 = {"prompt",},
 
    OAuthURL = restc:construct_url(OAuth2Endpoint, OAuth2RN, [Q1, Q2, Q3, Q4, Q5, Q6, Q7]),
 
    io:format("~p:~p OAuthURL:~p ~n", [?MODULE, ?LINE, OAuthURL]),

    OAuthURL.


get_access_token(undefined) ->
    #{
        access_token  => undefined,
        expires_in    => undefined,
        refresh_token => undefined
    };
get_access_token(Code) ->
    OAuth2TokenEndpoint = refundcheck_config:getAuthTokenEndpoint(?PROVIDER),
    OAuth2TokenRN       = refundcheck_config:getAuthTokenRN(?PROVIDER),
    Q1 = {"client_id", refundcheck_config:getAuthClientId(?PROVIDER)},
    Q2 = {"client_secret", refundcheck_config:getAuthClientSecret(?PROVIDER)},
    Q3 = {"code", Code},
    Q4 = {"grant_type", <<"authorization_code">>},
    Q5 = {"redirect_uri", refundcheck_config:getAuthRedirectURILoginCallback()},

    TokenURL = restc:construct_url(OAuth2TokenEndpoint, OAuth2TokenRN, [Q1, Q2, Q3, Q4, Q5]),
    io:format("~p:~p TokenURL:~p ~n", [?MODULE, ?LINE, TokenURL]),
    OAuthData = case restc:request(post, percent, TokenURL, []) of
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
                access_token  => undefined,
                expires_in    => undefined,
                refresh_token => undefined
            }
    end,
    OAuthData.

getPeopleData(AccessToken) ->
    PeopleEndpoint     = <<"https://people.googleapis.com/v1/">>,
    PeopleResourceName = <<"people/me">>,
    Q1 = {"personFields", <<"names,emailAddresses">>},
    PeopleURL = restc:construct_url(PeopleEndpoint, PeopleResourceName, [Q1]),
    Headers = [{<<"Authorization">>, <<"Bearer ", AccessToken/bitstring>>}],

    PeopleData = case restc:request(get, json, PeopleURL, [200], Headers) of
        {ok, 200, _H, RespBody} ->
            #{
                name  => getPeopleName(RespBody),
                email => getPeopleEmail(RespBody)
            };
        _Other ->
            #{
                name  => undefined,
                email => undefined
            }
    end,
    PeopleData.

getPeopleName(#{<<"names">> := Names}) ->
    getPrimaryName(Names).

getPrimaryName([]) ->
    <<"">>;
getPrimaryName([#{<<"displayName">> := DisplayName, <<"metadata">> := #{<<"primary">> := true}} | _MoreNames]) ->
    DisplayName;
getPrimaryName([_NotPrimary | MoreNames]) ->
    getPrimaryName(MoreNames).


getPeopleEmail(#{<<"emailAddresses">> := Emails}) ->
    getPrimaryEmail(Emails).

getPrimaryEmail([]) ->
    <<"">>;
getPrimaryEmail([#{<<"value">> := PrimaryEmail, <<"metadata">> := #{<<"primary">> := true}} | _MoreEmails]) ->
    PrimaryEmail;
getPrimaryEmail([_NotPrimary | MoreEmails]) ->
    getPrimaryEmail(MoreEmails).

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

% {
%   "resourceName": "people/114828984764373600194",
%   "etag": "%EgcBAgkuNz0+GgQBAgUHIgxOSFA2MkZPTmt2az0=",
%   "names": [
%     {
%       "metadata": {
%         "primary": true,
%         "source": {
%           "type": "PROFILE",
%           "id": "114828984764373600194"
%         },
%         "sourcePrimary": true
%       },
%       "displayName": "Camilo",
%       "givenName": "Camilo",
%       "displayNameLastFirst": "Camilo",
%       "unstructuredName": "Camilo"
%     },
%     {
%       "metadata": {
%         "source": {
%           "type": "CONTACT",
%           "id": "f"
%         }
%       },
%       "displayName": "Yo",
%       "givenName": "Yo",
%       "displayNameLastFirst": "Yo",
%       "unstructuredName": "Yo"
%     }
%   ],
%   "emailAddresses": [
%     {
%       "metadata": {
%         "primary": true,
%         "verified": true,
%         "source": {
%           "type": "ACCOUNT",
%           "id": "114828984764373600194"
%         },
%         "sourcePrimary": true
%       },
%       "value": "camilo.cerchiari@gmail.com"
%     },
%     {
%       "metadata": {
%         "verified": true,
%         "source": {
%           "type": "ACCOUNT",
%           "id": "114828984764373600194"
%         }
%       },
%       "value": "cam@montevideo.com.uy"
%     },
%     {
%       "metadata": {
%         "source": {
%           "type": "CONTACT",
%           "id": "f"
%         },
%         "sourcePrimary": true
%       },
%       "value": "camilo.cerchiari@gmail.com",
%       "type": "other",
%       "formattedType": "Other"
%     }
%   ]
% }

%% -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --


   
   