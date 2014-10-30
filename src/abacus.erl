%%%=============================================================================
%%% @author Michal Slaski t: @michalslaski
%%% @doc Abacus library provides functions for basic arithmetic operations.
%%%
%%% Erlang workshop https://github.com/michalslaski/erlang-workshop
%%% @end
%%%=============================================================================
-module(abacus).

%% API
-export([addition/2,
	 subtraction/2,
         multiplication/2,
	 division/2]).

addition(X, Y) ->
    X + Y.

subtraction(X, Y) ->
    X - Y.

multiplication(X, Y) ->
    X * Y.

division(X, Y) ->
    %% integer division rather than floating point division
    X div Y.
