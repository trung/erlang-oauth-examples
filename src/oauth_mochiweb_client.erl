%%
%% This is an example client for oauth_mochiweb_server.
%%
%% Usage is the same as oauth_termie.
%%

-module(oauth_mochiweb_client).

-compile(export_all).


echo() ->
  echo([{"bar", "baz"}, {"method", "foo"}]).

echo(Params) ->
  echo(Params, consumer(hmac_sha1)).

consumer(SigMethod) ->
  {"key", "secret", SigMethod}.

echo(Params, Consumer) ->
  put(oauth_termie_request_token_url, "http://0.0.0.0:8000/oauth/request_token"),
  put(oauth_termie_access_token_url, "http://0.0.0.0:8000/oauth/access_token"),
  put(oauth_termie_echo_url, "http://0.0.0.0:8000/echo"),
  oauth_termie:echo(Params, Consumer).
