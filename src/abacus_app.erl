%%%=============================================================================
%%% @author Michal Slaski t: @michalslaski
%%% @doc Application behaviour callback module.
%%%
%%% Erlang workshop https://github.com/michalslaski/erlang-workshop
%%% @end
%%%=============================================================================
-module(abacus_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================
start(_StartType, _StartArgs) ->
    abacus_sup:start_link().

stop(_State) ->
    ok.
