-module(reactor_poc).

-export([start/0]).

start() ->
    {ok, Supervisor} = wireblocks_sup:start_link(),
    {ok, _X} = wire:start(Supervisor, [x, 0]),

    {ok, _Debug} = debug:start(debug0, x, no_local),

    {ok, _Reactor} = reactor:start_link(),
    reactor:exec_ms(x, 5, 5000),
    reactor:exec_ms(x, 4, 4000),
    reactor:exec_ms(x, 3, 3000),
    reactor:exec_ms(x, 9, 9000),
    reactor:exec_ms(x, 1, 1000),
    reactor:exec_ms(x, 8, 8000),
    reactor:exec_ms(x, 7, 7000),
    reactor:exec_ms(x, 6, 6000),
    reactor:exec_ms(x, 2, 2000).
