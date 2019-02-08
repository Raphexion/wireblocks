-module(program2).

-behaviour(gen_subprogram).

-export([start_link/1,
	 subprogram_info/1,
	 subprogram_config/1]).

start_link(_DependencyMap) ->
    gen_subprogram:start_link(?MODULE, #{}).

subprogram_info(Program) ->
    gen_subprogram:subprogram_info(Program).

subprogram_config(_DependencyMap) ->
    Target = (rand:uniform() - 0.5) * 100,
    Guess = 0.0,

    {ok, X1} = wire_sup:start_wire(Target),
    {ok, X2} = wire_sup:start_wire(Guess),

    {ok, K1} = wire_sup:start_wire(1.0),
    {ok, K2} = wire_sup:start_wire(10.0),

    {ok, M1} = wire_sup:start_wire(10.0),
    {ok, M2} = wire_sup:start_wire(1.0),

    {ok, Y1} = wire_sup:start_wire(0),
    {ok, Y2} = wire_sup:start_wire(0),

    {ok, _Linear1} = linear:start_link(#{x => X1,
					 k => K1,
					 m => M1,
					 y => Y1}),

    {ok, _Linear2} = linear:start_link(#{x => X2,
					 k => K2,
					 m => M2,
					 y => Y2}),

    {ok, Error} = wire_sup:start_wire(0),
    {ok, _Sub} = sub:start(Y1, Y2, Error),

    #{guess => X2, error => Error}.
