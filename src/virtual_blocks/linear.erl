-module(linear).

-behaviour(gen_subprogram).

-export([start_link/1,
	 subprogram_info/1,
	 subprogram_config/1]).

start_link(DependencyMap) ->
    gen_subprogram:start_link(?MODULE, DependencyMap).

subprogram_info(Linear) ->
    gen_subprogram:subprogram_info(Linear).

subprogram_config(DependencyMap) ->
    X = maps:get(x, DependencyMap),
    K = maps:get(k, DependencyMap),
    M = maps:get(m, DependencyMap),
    Y = maps:get(y, DependencyMap),

    {ok, I} = wire:start(0),

    {ok, Mul} = mul:start(X, K, I),
    {ok, Add} = adder:start(I, M, Y),

    #{i => I,
      mul => Mul,
      add => Add}.
