-module(enspector_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SUPERVISOR, ?MODULE).

%% API.

-spec start_link() -> {ok, Pid::pid()}.
start_link() ->
	supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% supervisor.

init([]) ->
	{ok, {{one_for_one, 10, 10}, []}}.
