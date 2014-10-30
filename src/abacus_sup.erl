%%%=============================================================================
%%% @author Michal Slaski t: @michalslaski
%%% @doc Supervisor behaviour callback module.
%%%
%%% Erlang workshop https://github.com/michalslaski/erlang-workshop
%%% @end
%%%=============================================================================
-module(abacus_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%%=============================================================================
%%% External functions
%%%=============================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================
init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(abacus_srv, worker)]} }.
