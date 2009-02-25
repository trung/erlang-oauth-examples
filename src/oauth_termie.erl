%%
%% This is an example client for the test server at http://term.ie/oauth/example/.
%%
%% Use echo/0 to call the server with the default params and the default
%% signature method (HMAC-SHA1). Use echo/1 to specify *either* different
%% params, *or* a different signature method. Use echo/2 if you want to
%% specify *both* the params and the signature method.
%%

-module(oauth_termie).

-compile(export_all).

-export([echo/0, echo/1, echo/2]).


echo() ->
  echo(hmac_sha1).

echo(Params) when is_list(Params) ->
  echo(Params, hmac_sha1);
echo(SigMethod) when is_atom(SigMethod) ->
  echo([{"bar", "baz"}, {"method", "foo"}], SigMethod).

echo(Params, SigMethod) when is_atom(SigMethod) ->
  echo(Params, {"key", consumer_secret(SigMethod), SigMethod});
echo(Params, Consumer) ->
  oauth_client:get(url(oauth_termie_request_token_url), [], Consumer, "", "", fun(Response) ->
    echo(Params, Consumer, oauth_http:response_params(Response))
  end).

echo(Params, Consumer, RParams) ->
  oauth_client:get(url(oauth_termie_access_token_url), [], Consumer, RParams, fun(Response) ->
    echo(Params, Consumer, RParams, oauth_http:response_params(Response))
  end).

echo(Params, Consumer, _, AParams) ->
  oauth_client:get(url(oauth_termie_echo_url), Params, Consumer, AParams, fun(Response) ->
    {ok, lists:keysort(1, oauth_http:response_params(Response))}
  end).

consumer_secret(plaintext) ->
  "secret";
consumer_secret(hmac_sha1) ->
  "secret";
consumer_secret(rsa_sha1) ->
  "data/rsa_pkey.pem".

default(oauth_termie_request_token_url) ->
  "http://term.ie/oauth/example/request_token.php";
default(oauth_termie_access_token_url) ->
  "http://term.ie/oauth/example/access_token.php";
default(oauth_termie_echo_url) ->
  "http://term.ie/oauth/example/echo_api.php".

url(Name) ->
  case get(Name) of
    undefined -> default(Name);
    Other -> Other
  end.
