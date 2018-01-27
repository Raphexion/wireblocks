-module(delay).

-behaviour(gen_unaryblock).

-export([start/4]).

-export([f/2]).

start(Name, X, Y, Local) ->
    gen_unaryblock:start_link(?MODULE, Name, X, Y, Local).

f(_X, _Local) ->
    {error, not_implemented}.
