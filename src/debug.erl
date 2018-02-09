-module(debug).

-behaviour(gen_sinkblock).

-export([start/3]).

-export([f/2]).

start(Name, Wire, Local) ->
    gen_sinkblock:start_link(?MODULE, Name, Wire, Local).

f(X, Local) ->
    io:fwrite("~p~n", [X]),
    {ok, Local}.
