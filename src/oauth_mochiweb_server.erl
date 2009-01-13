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
        case oauth_signature:value("GET", URL, Params, Consumer, "") of
          Signature ->
            ok(Request, <<"oauth_token=requestkey&oauth_token_secret=requestsecret">>);
          _ ->
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
        case get_value("oauth_token", Params) of
          "requestkey" ->
            case oauth_signature:value("GET", URL, Params, Consumer, "requestsecret") of
              Signature ->
                ok(Request, <<"oauth_token=accesskey&oauth_token_secret=accesssecret">>);
              _ ->
                bad(Request, "invalid signature value.")
            end;
          _ ->
            bad(Request, "invalid oauth token")
        end
      end);
    _ ->
      method_not_allowed(Request)
  end.

serve_echo(Request) ->
  case Request:get(method) of
    'GET' ->
      serve_oauth(Request, fun(URL, Params, Consumer, Signature) ->
        case get_value("oauth_token", Params) of
          "accesskey" ->
            case oauth_signature:value("GET", URL, Params, Consumer, "accesssecret") of
              Signature ->
                EchoParams = lists:filter(fun({K, _}) -> not lists:prefix("oauth_", K) end, Params),
                ok(Request, oauth_uri:params_to_string(EchoParams));
              _ ->
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
      Key = get_value("oauth_consumer_key", Params),
      case fetch_consumer_secret(Key) of
        {just, ConsumerSecret} ->
          case signature_method(Params) of
            {just, SignatureMethod} ->
              Consumer = {Key, ConsumerSecret, SignatureMethod},
              Signature = proplists:get_value("oauth_signature", Params),
              URL = string:concat("http://0.0.0.0:8000", Request:get(path)),
              Fun(URL, proplists:delete("oauth_signature", Params), Consumer, Signature);
            nothing ->
              bad(Request, "invalid signature method.")
          end;
        nothing ->
          bad(Request, "invalid consumer key.")
      end;
    _ ->
      bad(Request, "invalid oauth version.")
  end.

signature_method(Params) ->
  case proplists:get_value("oauth_signature_method", Params) of
    "PLAINTEXT" -> {just, plaintext};
    "HMAC-SHA1" -> {just, hmac_sha1};
    _ -> nothing
  end.

ok(Request, Body) ->
  Request:respond({200, [], Body}).

bad(Request, Reason) ->
  Request:respond({400, [], list_to_binary("Bad Request: " ++ Reason)}).

method_not_allowed(Request) ->
  Request:respond({405, [], <<>>}).

fetch_consumer_secret("key") ->
  {just, "secret"};
fetch_consumer_secret(_) ->
  nothing.
