%%
%% This is an example client for the Google Contacts Data API.
%%
%% Example usage:
%%
%%   $ make
%%   ...
%%   $ erl -pa ebin -pa path/to/erlang-oauth/ebin -s crypto -s inets -s ssl
%%   ...
%%   1> {ok, Client} = oauth_gcontacts:start().
%%   ...
%%   2> {ok, Token} = oauth_gcontacts:get_request_token(Client).
%%   ...
%%   3> AuthorizeURL = oauth_gcontacts:authorize_url(Token).
%%   ...
%%   4> ok = oauth_gcontacts:get_access_token(Client).
%%   ...
%%   5> {ok, Headers, XML} = oauth_gcontacts:get_feed(Client).
%%   ...
%%   6> oauth_gcontacts:feed_titles(XML).
%%   ...
%%
%% Note that before fetching the access token (step 4) you need to have
%% authorized the request token.
%%
-module(oauth_gcontacts).

-compile(export_all).

start() ->
  % cf. http://groups.google.com/group/oauth/msg/0cf50121f946a889
  oauth_client:start({"weitu.googlepages.com", "data/oauth_gcontacts_pkey.pem", rsa_sha1}).

get_request_token(Client) ->
  oauth_google:get_request_token(Client, "http://www.google.com/m8/feeds").

authorize_url(Token) ->
  oauth_google:authorize_url(Token).

get_access_token(Client) ->
  oauth_google:get_access_token(Client).

get_feed(Client) ->
  oauth_client:get(Client, "http://www.google.com/m8/feeds/contacts/default/base").

feed_titles(XML) ->
  oauth_google:feed_titles(XML).
