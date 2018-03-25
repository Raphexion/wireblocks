-module(wire_poc).

-export([start/0]).

start() ->
    {ok, X} = wire:start(2),
    {ok, Y} = wire:start(3),

    {ok, Z1} = wire:start(0),
    {ok, _Adder1} = adder:start(X, Y, Z1),

    {ok, Z2} = wire:start(0),
    {ok, _Mul1} = mul:start(X, Y, Z2),

    {ok, Z3} = wire:start(0),
    {ok, _Mul2} = mul:start(Z1, Z2, Z3),

    {ok, Z4} = wire:start(0),
    {ok, _Inc1} = inc:start(X, Z4, no_local_fstate).
