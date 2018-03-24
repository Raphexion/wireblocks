-module(program0).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([start_link/0,
	 wires/0]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

wires() ->
    gen_server:call(?MODULE, wires).

init([]) ->
    {ok, [], 0}.

handle_call(_What, _From, State) ->
    {reply, {ok, State}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(timeout, _State) ->
    io:fwrite("timeout~n", []),

    {ok, X} = wire:start(2),
    {ok, Y} = wire:start(3),

    {ok, Z1} = wire:start(0),
    {ok, _Adder1} = adder:start(X, Y, Z1),

    {ok, Z2} = wire:start(0),
    {ok, _Mul1} = mul:start(X, Y, Z2),

    {ok, Z3} = wire:start(0),
    {ok, _Mul2} = mul:start(Z1, Z2, Z3),

    {ok, Z4} = wire:start(0),
    {ok, _Inc1} = inc:start(X, Z4, no_local_fstate),

    State = #{
      x => X,
      y => Y,
      z1 => Z1,
      z2 => Z2,
      z3 => Z3,
      z4 => Z4},

    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
