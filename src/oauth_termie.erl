%%
%% This is an example client for the test server at http://term.ie/oauth/example/.
%%
%% The echo/0 function calls the server with the default params, using the
%% default (HMAC-SHA1) signature method. To call the server with different
%% params, use echo/1. To call the server with a different signature method,
%% use echo/2, together with consumer/1.
%%

-module(oauth_termie).

-compile(export_all).

-export([echo/0, echo/1, consumer/1, echo/2]).


echo() ->
  echo([{"bar", "baz"}, {"method", "foo"}]).

echo(Params) ->
  echo(Params, consumer(hmac_sha1)).

consumer(SigMethod) ->
  {"key", "secret", SigMethod}.

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
