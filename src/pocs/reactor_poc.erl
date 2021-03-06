-module(reactor_poc).

-export([start/0]).

start() ->
    {ok, X} = wire_sup:start_wire(0),

    {ok, _Debug} = debug:start(X, "X: "),

    reactor:exec_ms(X, 5, 5000),
    reactor:exec_ms(X, 4, 4000),
    reactor:exec_ms(X, 3, 3000),
    reactor:exec_ms(X, 9, 9000),
    reactor:exec_ms(X, 1, 1000),
    reactor:exec_ms(X, 8, 8000),
    reactor:exec_ms(X, 7, 7000),
    reactor:exec_ms(X, 6, 6000),
    reactor:exec_ms(X, 2, 2000).
