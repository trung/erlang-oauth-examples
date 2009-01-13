%%
%% This is an example client for the Google Calendar API.
%%
%% In order to use this client you will need your own consumer
%% key/secret, which you can obtain by registering a domain with Google:
%%
%%   http://code.google.com/apis/accounts/docs/OAuth.html
%%
%%   https://www.google.com/accounts/ManageDomain
%%
%%
%% Make sure the 'crypto', 'inets', and 'ssl' applications are running.
%% Then call oauth_gcal:get_request_token/1 with your consumer tuple.
%% This should prompt you to authorize the request token. The consumer
%% tuple and the response parameters are stored in the process dictionary
%% for ease of use.
%%
%% Once you have granted access, call oauth_gcal:get_access_token/0,
%% and then oauth_gcal:get_feed/0. This should return {ok, XML}, where
%% XML is an xmerl xmlElement record. The oauth_google_client:titles/1
%% function can be used to extract all the entry titles from the data.
%%

-module(oauth_gcal).

-compile(export_all).

-define(URL, "http://www.google.com/calendar/feeds/default/allcalendars/full").


get_request_token(Consumer) ->
  oauth_google_client:get_request_token(Consumer, "http://www.google.com/calendar/feeds/").

get_access_token() ->
  oauth_google_client:get_access_token().

get_feed() ->
  AParams = oauth_google_client:access_token_response_params(),
  oauth_client:get(?URL, [], oauth_google_client:consumer(), AParams, fun({_, Headers, _}) ->
    % The first request to ?FEED_URL will return a redirect response.
    % If we blindly follow the redirect the request will fail. This is
    % because the new location contains an additional parameter ("gsessionid"),
    % and so the OAuth signature will be invalid for this new request.
    % Instead, the parameter is extracted and used to generate a fresh request.
    Location = proplists:get_value("location", Headers),
    {_, _, _, _, _, [$?|ParamsString]} = http_uri:parse(Location),
    Gsessionid = proplists:lookup("gsessionid", oauth_uri:params_from_string(ParamsString)),
    oauth_google_client:get_feed(?URL, [Gsessionid])
  end).
