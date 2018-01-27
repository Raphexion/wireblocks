-module(mul).

-behaviour(gen_binaryblock).

-export([start/4]).

-export([f/2]).

start(Name, X, Y, Z) ->
    gen_binaryblock:start_link(?MODULE, Name, X, Y, Z).

f(X, Y) ->
    X * Y.
