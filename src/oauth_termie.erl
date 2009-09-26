%%
%% This is an example client for the test server at http://term.ie/oauth/example/.
%%
%% Example usage:
%%
%%   $ make
%%   ...
%%   $ erl -pa ebin -pa path/to/erlang-oauth/ebin -s crypto -s inets
%%   ...
%%   1> {ok, Client} = oauth_termie:start().
%%   ...
%%   2> {ok, _Token} = oauth_termie:get_request_token(Client).
%%   ...
%%   3> ok = oauth_termie:get_access_token(Client).
%%   ...
%%   4> oauth_termie:echo(Client).
%%   ...
%%
-module(oauth_termie).

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
  URL = "http://term.ie/oauth/example/request_token.php",
  oauth_client:get_request_token(Client, URL, [], querystring).

get_access_token(Client) ->
  URL = "http://term.ie/oauth/example/access_token.php",
  oauth_client:get_access_token(Client, URL, [], querystring).

echo(Client) ->
  echo(Client, [{"bar", "baz"}, {"method", "foo"}]).

echo(Client, Params) ->
  URL = "http://term.ie/oauth/example/echo_api.php",
  case oauth_client:get(Client, URL, Params, querystring) of
    {ok, _Headers, Body} ->
      {ok, lists:keysort(1, oauth_uri:params_from_string(Body))};
    Error ->
      Error
  end.
