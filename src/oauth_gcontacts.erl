%%
%% This is an example client for the Google Contacts Data API.
%%
%% Usage is similar to oauth_gcal. The 'crypto', 'inets', and 'ssl'
%% applications need to be running. Call get_request_token/0 first,
%% and then authorize the request token as prompted. You can then
%% call get_access_token/0 followed by get_feed/0.
%%
%% cf. http://groups.google.com/group/oauth/msg/0cf50121f946a889
%%

-module(oauth_gcontacts).

-compile(export_all).

-define(URL, "http://www.google.com/m8/feeds/contacts/default/base").


consumer() ->
  {"weitu.googlepages.com", "data/oauth_gcontacts_pkey.pem", rsa_sha1}.

get_request_token() ->
  oauth_google_client:get_request_token(consumer(), "http://www.google.com/m8/feeds").

get_access_token() ->
  oauth_google_client:get_access_token().

get_feed() ->
  oauth_google_client:get_feed(?URL).
