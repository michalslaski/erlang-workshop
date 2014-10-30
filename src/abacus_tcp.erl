%%%=============================================================================
%%% @author Michal Slaski t: @michalslaski
%%% @doc Abacus TCP server listens on a given port for commands.
%%% Executes commands by applying arithmetic operations on the current value.
%%%
%%% Erlang workshop https://github.com/michalslaski/erlang-workshop
%%% @end
%%%=============================================================================
-module(abacus_tcp).

%% API
-export([start/1]).

%%%=============================================================================
%%% External functions
%%%=============================================================================
start(Port) ->
    %% open TCP listening socket on a given port
    {ok, ListenSocket} = gen_tcp:listen(Port, []),

    %% wait for a client to connect
    {ok, Socket} = gen_tcp:accept(ListenSocket),

    %% enter receive loop where commands are processed
    loop(Socket, 0),

    %% stop listening
    ok = gen_tcp:close(ListenSocket).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
loop(Socket, CurrentValue) ->
    receive
	{tcp, Socket, [$+ | String]} ->
	    Integer = parse_integer(String),
	    loop(Socket, abacus:addition(CurrentValue, Integer));

	{tcp, Socket, [$- | String]} ->
	    Integer = parse_integer(String),
	    loop(Socket, abacus:subtraction(CurrentValue, Integer));

	{tcp, Socket, [$* | String]} ->
	    Integer = parse_integer(String),
	    loop(Socket, abacus:multiplication(CurrentValue, Integer));

	{tcp, Socket, [$/ | String]} ->
	    Integer = parse_integer(String),
	    loop(Socket, abacus:division(CurrentValue, Integer));

	{tcp, Socket, [$= | _]} ->
	    gen_tcp:send(Socket, io_lib:format("~p~n", [CurrentValue])),
	    loop(Socket, CurrentValue);

	{tcp_closed, _} ->
	    ok;

	_Other -> % ignoring commands, which can't be understood
	    io:format("ignoring ~p~n", [_Other]),
	    loop(Socket, CurrentValue)
    end.

parse_integer(String) ->
    case string:to_integer(String) of
	{error, no_integer} ->
	    exit({error, no_integer});
	{Integer, _} ->
	    Integer
    end.
