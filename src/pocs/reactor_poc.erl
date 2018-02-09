-module(reactor_poc).

-export([start/0]).

start() ->
    {ok, Supervisor} = wireblocks_sup:start_link(),
    {ok, _X} = wire:start(Supervisor, [x, 2]),

    {ok, _Debug} = debug:start(debug0, x, no_local),

    {ok, _Reactor} = reactor:start_link(),
    reactor:exec(x, 42, 100).
