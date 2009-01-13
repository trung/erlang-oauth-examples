%%
%% This is a supervisor for oauth_mochiweb_server.
%%

-module(oauth_mochiweb).

-behaviour(supervisor).

-export([start/0, start_link/0, init/1]).


start() ->
  {ok, Pid} = start_link(), unlink(Pid).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 3, 10}, [
    {web,
      {oauth_mochiweb_server, start, [[{ip, "0.0.0.0"}, {port, 8000}]]},
      permanent,
      5000,
      worker,
      dynamic
    }
  ]}}.
