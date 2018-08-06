%%%-------------------------------------------------------------------
%% @doc monitor top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(monitor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { supervision_flags(), child_spec()}}.

%%====================================================================
%% Internal functions
%%====================================================================
supervision_flags() -> #{
  strategy  => one_for_one,
  intensity => 0,
  period    => 1
}.

child_spec() -> [
  #{
    id       => socket_sup,
    restart  => permanent,
    shutdown => brutal_kill,
    start    => { socket_sup, start_link, [] },
    type     => supervisor
  }
].
