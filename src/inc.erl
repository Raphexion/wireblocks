-module(inc).

-behaviour(gen_unaryblock).

-export([start/4]).

-export([f/2]).

start(Name, X, Y, Local) ->
    gen_unaryblock:start_link(?MODULE, Name, X, Y, Local).

f(X, Local) ->
    {ok, X + 1, Local}.
