%%
%% This is an example client for oauth_mochiweb_server.
%%
%% Usage is the same as oauth_termie.
%%
-module(oauth_mochiweb_client).

-compile(export_all).

start() ->
  start(hmac_sha1).

start(SigMethod) when is_atom(SigMethod) ->
  start(consumer(SigMethod));
start(Consumer) ->
  oauth_client:start(Consumer).

consumer(rsa_sha1) ->
  {"key", "data/rsa_pkey.pem", rsa_sha1};
consumer(SigMethod) ->
  {"key", "secret", SigMethod}.

get_request_token(Client) ->
  URL = "http://0.0.0.0:8000/oauth/request_token",
  oauth_client:get_request_token(Client, URL, [], querystring).

get_access_token(Client) ->
  URL = "http://0.0.0.0:8000/oauth/access_token",
  oauth_client:get_access_token(Client, URL, [], querystring).

echo(Client) ->
  echo(Client, [{"bar", "baz"}, {"method", "foo"}]).

echo(Client, Params) ->
  case oauth_client:get(Client, "http://0.0.0.0:8000/echo", Params, querystring) of
    {ok, _Headers, Body} ->
      {ok, lists:keysort(1, oauth_uri:params_from_string(Body))};
    Error ->
      Error
  end.
