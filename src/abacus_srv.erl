%%%=============================================================================
%%% @author Michal Slaski t: @michalslaski
%%% @doc Abacus TCP server implemented with gen_server behaviour.
%%% It listens on a given port for commands. Executes commands
%%% by applying arithmetic operations on the current value.
%%%
%%% Erlang workshop https://github.com/michalslaski/erlang-workshop
%%% @end
%%%=============================================================================
-module(abacus_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% Application callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {value = 0,
                socket}).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================
init(Port) when is_integer(Port) ->
    %% open TCP listening socket on a given port
    {ok, ListenSocket} = gen_tcp:listen(Port, [{reuseaddr, true}]),

    %% let the inet driver accept new clients
    {ok,_Ref} = prim_inet:async_accept(ListenSocket, -1),

    {ok, #state{socket = ListenSocket}}.

handle_call(Request, _From, _State) ->
    exit({unknown_call, Request}).

handle_cast(Request, _State) ->
    exit({unknown_cast, Request}).

handle_info({inet_async, ListSock, _Ref, {ok, CliSocket}}, State) ->
    %%    ok = gen_tcp:controlling_process(CliSocket, self()),
    %% Taken from prim_inet
    true = inet_db:register_socket(CliSocket, inet_tcp),
    ok = prim_inet:setopts(CliSocket, [{active,true},
                                       {nodelay,false},
                                       {keepalive,false},
                                       {delay_send,false},
                                       {priority,0},
                                       {tos,0}]),
    {ok, _NewRef} = prim_inet:async_accept(ListSock, -1),
    {noreply, State};
handle_info({tcp, _Socket, [$+ | String]}, State) ->
    Integer = parse_integer(String),
    {noreply, State#state{value = abacus:addition(State#state.value, Integer)}};
handle_info({tcp, _Socket, [$- | String]}, State) ->
    Integer = parse_integer(String),
    {noreply, State#state{value = abacus:addition(State#state.value, Integer)}};
handle_info({tcp, _Socket, [$* | String]}, State) ->
    Integer = parse_integer(String),
    {noreply, State#state{
                value = abacus:multiplication(State#state.value, Integer)}};
handle_info({tcp, _Socket, [$/ | String]}, State) ->
    Integer = parse_integer(String),
    {noreply, State#state{value = abacus:division(State#state.value, Integer)}};
handle_info({tcp, Socket, [$= | _]}, State) ->
    gen_tcp:send(Socket, io_lib:format("~p~n", [State#state.value])),
    {noreply, State};
handle_info({tcp_closed, _}, State) ->
    {noreply, State};
handle_info(Request, _State) ->
    exit({unknown_info, Request}).

terminate(_Reason, State) ->
    ok = gen_tcp:close(State#state.socket).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% External functions
%%%=============================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, 1234, []).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
parse_integer(String) ->
    case string:to_integer(String) of
        {error, no_integer} ->
            exit({error, no_integer});
        {Integer, _} ->
            Integer
    end.
