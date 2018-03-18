-module(wire_poc).

-export([start/0]).

start() ->
    {ok, Supervisor} = wireblocks_sup:start_link(),

    {ok, X} = wire:start(Supervisor, 2),
    {ok, Y} = wire:start(Supervisor, 3),

    {ok, Z1} = wire:start(Supervisor, 0),
    {ok, _Adder1} = adder:start(X, Y, Z1),

    {ok, Z2} = wire:start(Supervisor, 0),
    {ok, _Mul1} = mul:start(X, Y, Z2),

    {ok, Z3} = wire:start(Supervisor, 0),
    {ok, _Mul2} = mul:start(Z1, Z2, Z3),

    {ok, Z4} = wire:start(Supervisor, 0),
    {ok, _Inc1} = inc:start(X, Z4, no_local_fstate).
