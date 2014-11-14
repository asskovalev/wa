-module(wa_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  Workers = [
    {wa_fetcher, {wa_fetcher, start_link, [aucs_db]}, permanent, 5000, worker, [wa_fetcher]}
  ],
  Supervisors = [
  ],
  {ok, { {one_for_one, 5, 10}, Workers ++ Supervisors} }.

