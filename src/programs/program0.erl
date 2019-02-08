-module(program0).

-behaviour(gen_subprogram).

-export([start_link/1,
	 subprogram_info/1,
	 subprogram_config/1]).

start_link(_DependencyMap) ->
    gen_subprogram:start_link(?MODULE, #{}).

subprogram_info(Program) ->
    gen_subprogram:subprogram_info(Program).

subprogram_config(_DependencyMap) ->
    {ok, X} = wire_sup:start_wire(2),
    {ok, Y} = wire_sup:start_wire(3),

    {ok, Z1} = wire_sup:start_wire(0),
    {ok, _Adder1} = adder:start(X, Y, Z1),

    {ok, Z2} = wire_sup:start_wire(0),
    {ok, _Mul1} = mul:start(X, Y, Z2),

    {ok, Z3} = wire_sup:start_wire(0),
    {ok, _Mul2} = mul:start(Z1, Z2, Z3),

    {ok, Z4} = wire_sup:start_wire(0),
    {ok, _Inc1} = inc:start(X, Z4, no_local_fstate),

    #{x => X,
      y => Y,
      z1 => Z1,
      z2 => Z2,
      z3 => Z3,
      z4 => Z4}.
