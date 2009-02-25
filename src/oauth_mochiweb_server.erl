%%
%% This is an example of how to use erlang-oauth in a MochiWeb server.
%%
%% The HTTP resources that it exposes are a clone of those provided by
%% http://term.ie/oauth/example/. Client code is in oauth_mochiweb_client.
%%
%% It is easiest to start the server via the supervisor (oauth_mochiweb:start/0).
%%

-module(oauth_mochiweb_server).

-export([start/1, stop/0]).

-import(proplists, [get_value/2]).


start(Options) ->
  mochiweb_http:start([{name, ?MODULE}, {loop, fun serve/1}|Options]).

stop() ->
  mochiweb_http:stop(?MODULE).

serve(Request) ->
  case Request:get(path) of
    "/" ->
      serve_root(Request);
    "/oauth/request_token" ->
      serve_oauth_request_token(Request);
    "/oauth/access_token" ->
      serve_oauth_access_token(Request);
    "/echo" ->
      serve_echo(Request);
    _ ->
      Request:not_found()
  end.

serve_root(Request) ->
  case lists:member(Request:get(method), ['GET', 'HEAD']) of
    true ->
      Request:respond({303, [{"Location", "/echo"}], <<>>});
    false ->
      method_not_allowed(Request)
  end.

serve_oauth_request_token(Request) ->
  case Request:get(method) of
    'GET' ->
      serve_oauth(Request, fun(URL, Params, Consumer, Signature) ->
        case oauth:verify(Signature, "GET", URL, Params, Consumer, "") of
          true ->
            ok(Request, <<"oauth_token=requestkey&oauth_token_secret=requestsecret">>);
          false ->
            bad(Request, "invalid signature value.")
        end
      end);
    _ ->
      method_not_allowed(Request)
  end.

serve_oauth_access_token(Request) ->
  case Request:get(method) of
    'GET' ->
      serve_oauth(Request, fun(URL, Params, Consumer, Signature) ->
        case oauth:token(Params) of
          "requestkey" ->
            case oauth:verify(Signature, "GET", URL, Params, Consumer, "requestsecret") of
              true ->
                ok(Request, <<"oauth_token=accesskey&oauth_token_secret=accesssecret">>);
              false ->
                bad(Request, "invalid signature value.")
            end;
          _ ->
            bad(Request, "invalid oauth token.")
        end
      end);
    _ ->
      method_not_allowed(Request)
  end.

serve_echo(Request) ->
  case Request:get(method) of
    'GET' ->
      serve_oauth(Request, fun(URL, Params, Consumer, Signature) ->
        case oauth:token(Params) of
          "accesskey" ->
            case oauth:verify(Signature, "GET", URL, Params, Consumer, "accesssecret") of
              true ->
                EchoParams = lists:filter(fun({K, _}) -> not lists:prefix("oauth_", K) end, Params),
                ok(Request, oauth_uri:params_to_string(EchoParams));
              false ->
                bad(Request, "invalid signature value.")
            end;
          _ ->
            bad(Request, "invalid oauth token")
        end
      end);
    _ ->
      method_not_allowed(Request)
  end.

serve_oauth(Request, Fun) ->
  Params = Request:parse_qs(),
  case get_value("oauth_version", Params) of
    "1.0" ->
      ConsumerKey = get_value("oauth_consumer_key", Params),
      SigMethod = get_value("oauth_signature_method", Params),
      case consumer_lookup(ConsumerKey, SigMethod) of
        none ->
          bad(Request, "invalid consumer (key or signature method).");
        Consumer ->
          Signature = proplists:get_value("oauth_signature", Params),
          URL = string:concat("http://0.0.0.0:8000", Request:get(path)),
          Fun(URL, proplists:delete("oauth_signature", Params), Consumer, Signature)
      end;
    _ ->
      bad(Request, "invalid oauth version.")
  end.

consumer_lookup("key", "PLAINTEXT") ->
  {"key", "secret", plaintext};
consumer_lookup("key", "HMAC-SHA1") ->
  {"key", "secret", hmac_sha1};
consumer_lookup("key", "RSA-SHA1") ->
  {"key", "data/rsa_cert.pem", rsa_sha1};
consumer_lookup(_, _) ->
  none.

ok(Request, Body) ->
  Request:respond({200, [], Body}).

bad(Request, Reason) ->
  Request:respond({400, [], list_to_binary("Bad Request: " ++ Reason)}).

method_not_allowed(Request) ->
  Request:respond({405, [], <<>>}).
