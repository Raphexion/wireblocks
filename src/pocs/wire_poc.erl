-module(wire_poc).

-export([start/0]).

start() ->
    {ok, Supervisor} = wireblocks_sup:start_link(),
    {ok, _X} = wire:start(Supervisor, [x, 2]),
    {ok, _Y} = wire:start(Supervisor, [y, 3]),

    {ok, _Z1} = wire:start(Supervisor, [z1, 0]),
    {ok, _Adder1} = adder:start(adder1, x, y, z1),

    {ok, _Z2} = wire:start(Supervisor, [z2, 0]),
    {ok, _Mul1} = mul:start(mul1, x, y, z2),

    {ok, _Z3} = wire:start(Supervisor, [z3, 0]),
    {ok, _Mul2} = mul:start(mul2, z1, z2, z3),

    {ok, _Z4} = wire:start(Supervisor, [z4, 0]),
    {ok, _Inc1} = inc:start(inc1, x, z4, no_local_fstate),

    ok.
