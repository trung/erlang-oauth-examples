%%
%% This is an example client for the Google Calendar API.
%%
%% In order to use this client you will need your own consumer
%% key/secret, which you can obtain by registering a domain with Google:
%%
%%   http://code.google.com/apis/accounts/docs/OAuth.html
%%
%%   https://www.google.com/accounts/ManageDomain
%%
%% Example usage:
%%
%%   $ make
%%   ...
%%   $ erl -pa ebin -pa path/to/erlang-oauth/ebin -s crypto -s inets -s ssl
%%   ...
%%   1> Consumer = {"...KEY...", "...SECRET...", hmac_sha1}.
%%   ...
%%   2> {ok, Client} = oauth_gcal:start(Consumer).
%%   ...
%%   3> {ok, Token} = oauth_gcal:get_request_token(Client).
%%   ...
%%   4> AuthorizeURL = oauth_gcal:authorize_url(Token).
%%   ...
%%   5> ok = oauth_gcal:get_access_token(Client).
%%   ...
%%   6> {ok, Headers, XML} = oauth_gcal:get_feed(Client).
%%   ...
%%   7> oauth_gcal:feed_titles(XML).
%%   ...
%%
%% Note that before fetching the access token (step 5) you need to have
%% authorized the request token.
%%
-module(oauth_gcal).

-compile(export_all).

start(Consumer) ->
  oauth_client:start(Consumer).

get_request_token(Client) ->
  oauth_google:get_request_token(Client, "http://www.google.com/calendar/feeds/").

authorize_url(Token) ->
  oauth_google:authorize_url(Token).

get_access_token(Client) ->
  oauth_google:get_access_token(Client).

get_feed(Client) ->
  URL = "https://www.google.com/calendar/feeds/default/allcalendars/full",
  % Requesting this URL will return a redirect response. If we blindly follow
  % the redirect the request will fail because the OAuth signature will be
  % invalid for this request. We therefore extract the "gsessionid" parameter,
  % and generate a fresh request including this parameter.
  {{_, 302, _}, Headers, _} = oauth_client:get(Client, URL),
  Location = proplists:get_value("location", Headers),
  {_, _, _, _, _, [$?|Params]} = http_uri:parse(Location),
  Gsessionid = proplists:lookup("gsessionid", oauth_uri:params_from_string(Params)),
  oauth_client:get(Client, URL, [Gsessionid]).

feed_titles(XML) ->
  oauth_google:feed_titles(XML).
