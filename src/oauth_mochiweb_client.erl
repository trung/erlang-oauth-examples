%%
%% This is an example client for oauth_mochiweb_server.
%%
%% Usage is the same as oauth_termie.
%%

-module(oauth_mochiweb_client).

-compile(export_all).


echo() ->
  oauth_termie(echo, []).

echo(Params) ->
  oauth_termie(echo, [Params]).

echo(Params, Consumer) ->
  oauth_termie(echo, [Params, Consumer]).

oauth_termie(F, Args) ->
  case get(oauth_termie_request_token_url) of
    undefined ->
      put(oauth_termie_request_token_url, "http://0.0.0.0:8000/oauth/request_token"),
      put(oauth_termie_access_token_url, "http://0.0.0.0:8000/oauth/access_token"),
      put(oauth_termie_echo_url, "http://0.0.0.0:8000/echo"),
      apply(oauth_termie, F, Args);
    _ ->
      apply(oauth_termie, F, Args)
  end.
