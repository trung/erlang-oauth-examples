-module(oauth_client).

-compile(export_all).


get(URL, Params, Consumer, ResponseParams, Fun) ->
  Token = oauth:token(ResponseParams),
  TokenSecret = oauth:token_secret(ResponseParams),
  get(URL, Params, Consumer, Token, TokenSecret, Fun).

get(URL, Params, Consumer, Token, TokenSecret, Fun) ->
  case tee(oauth:get(URL, Params, Consumer, Token, TokenSecret)) of
    {ok, Response} ->
      case oauth_http:response_code(Response) of
        Code when Code >= 400 ->
          {error, {http, Code}};
        _ ->
          Fun(Response)
      end;
    Response ->
      io:format("Error requesting ~p~n~n~p~n~n", [URL, Response]),
      {error, bad_response}
  end.

tee(Term) ->
  io:format("~p~n", [Term]), Term.
