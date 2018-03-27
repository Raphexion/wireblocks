-module(sub).

-behaviour(gen_binaryblock).

-export([start/3]).

-export([f/2]).

start(X, Y, Z) ->
    gen_binaryblock:start_link(?MODULE, X, Y, Z).

f(X, Y) ->
    X - Y.
