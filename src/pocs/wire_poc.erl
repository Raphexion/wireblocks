-module(wire_poc).

-export([start/0]).

start() ->
    {ok, Supervisor} = wireblocks_sup:start_link(),

    {ok, X} = wire:start(Supervisor, 2),
    {ok, Y} = wire:start(Supervisor, 3),

    {ok, Z1} = wire:start(Supervisor, 0),
    {ok, _Adder1} = adder:start(adder1, X, Y, Z1),

    {ok, Z2} = wire:start(Supervisor, 0),
    {ok, _Mul1} = mul:start(mul1, X, Y, Z2),

    {ok, Z3} = wire:start(Supervisor, 0),
    {ok, _Mul2} = mul:start(mul2, Z1, Z2, Z3),

    {ok, Z4} = wire:start(Supervisor, 0),
    {ok, _Inc1} = inc:start(inc1, X, Z4, no_local_fstate).
