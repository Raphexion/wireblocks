-module(debug).

-behaviour(gen_sinkblock).

-export([start/2]).

-export([f/2]).

start(Wire, Local) ->
    gen_sinkblock:start_link(?MODULE, Wire, Local).

f(X, Local) ->
    io:fwrite("~p~p~n", [Local, X]),
    {ok, Local}.
