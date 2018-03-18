-module(delay).

-behaviour(gen_unaryblock).

-export([start/3]).

-export([f/2]).

start(X, Y, Local) ->
    gen_unaryblock:start_link(?MODULE, X, Y, Local).

f(_X, _Local) ->
    {error, not_implemented}.
