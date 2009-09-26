-module(oauth_google).

-compile(export_all).

-include_lib("xmerl/include/xmerl.hrl").

get_request_token(Client, Scope) ->
  URL = "https://www.google.com/accounts/OAuthGetRequestToken",
  oauth_client:get_request_token(Client, URL, [{"scope", Scope}]).

authorize_url(Token) ->
  oauth:uri("https://www.google.com/accounts/OAuthAuthorizeToken", [{"oauth_token", Token}]).

get_access_token(Client) ->
  URL = "https://www.google.com/accounts/OAuthGetAccessToken",
  oauth_client:get_access_token(Client, URL).

feed_titles(XML) ->
  [Node#xmlText.value || Node <- xmerl_xpath:string("//feed/entry/title/text()", XML)].
