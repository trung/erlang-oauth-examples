-module(oauth_google_client).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

-define(REQUEST_TOKEN_URL, "https://www.google.com/accounts/OAuthGetRequestToken").

-define(AUTHORIZE_TOKEN_URL, "https://www.google.com/accounts/OAuthAuthorizeToken").

-define(ACCESS_TOKEN_URL, "https://www.google.com/accounts/OAuthGetAccessToken").


titles(Feed) ->
  [Node#xmlText.value || Node <- xmerl_xpath:string("//feed/entry/title/text()", Feed)].

get_request_token(Consumer) ->
  get_request_token(Consumer, []).

get_request_token(Consumer, Scope) ->
  oauth_client:get(?REQUEST_TOKEN_URL, [{"scope", Scope}], Consumer, "", "", fun(Response) ->
    RParams = oauth_http:response_params(Response),
    URL = oauth:uri(?AUTHORIZE_TOKEN_URL, [{"oauth_token", oauth:token(RParams)}]),
    put(oauth_google_consumer, Consumer),
    put(oauth_google_request_token_response_params, RParams),
    io:format("~nPlease authorize the token at the following address:~n~s~n~n", [URL]),
    ok
  end).

get_access_token() ->
  oauth_client:get(?ACCESS_TOKEN_URL, [], consumer(), request_token_response_params(), fun(Response) ->
    Params = oauth_http:response_params(Response),
    put(oauth_google_access_token_response_params, Params),
    ok
  end).

consumer() ->
  get(oauth_google_consumer).

request_token_response_params() ->
  get(oauth_google_request_token_response_params).

access_token_response_params() ->
  get(oauth_google_access_token_response_params).

get_feed(URL) ->
  get_feed(URL, []).

get_feed(URL, Params) ->
  oauth_client:get(URL, Params, consumer(), access_token_response_params(), fun(Response) ->
    Body = oauth_http:response_body(Response),
    {XML, _} = xmerl_scan:string(Body),
    {ok, XML}
  end).
