%%%=============================================================================
%%% @author Michal Slaski t: @michalslaski
%%% @doc Unit tests of the abacus library
%%%
%%% Erlang workshop https://github.com/michalslaski/erlang-workshop
%%% @end
%%%=============================================================================
-module(abacus_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

add_test() ->
    ?assertEqual(2 + 3, abacus:addition(2, 3)).

sub_test() ->
    ?assertEqual(5 - 2, abacus:subtraction(5, 2)).

mul_test() ->
    ?assertEqual(4 * 6, abacus:multiplication(4, 6)).

div_test() ->
    ?assertEqual(5, abacus:division(15, 3)).
