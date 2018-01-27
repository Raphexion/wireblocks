-module(adder).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start/4]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,     %% only two good terminater movies
	 code_change/3]).

%%%
%%
%%%

start(Name, X, Y, Z) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Name, X, Y, Z}, []).

%%%
%%
%%%

init({Name, X, Y, Z}) ->
    {ok, ValX} = wire:probe(X),
    {ok, ValY} = wire:probe(Y),

    update(Z, ValX, ValY),
    wire:add_observer(X, self(), x),
    wire:add_observer(Y, self(), y),

    {ok, {Name, Z, ValX, ValY}}.

handle_call(_What, _From, State) ->
    {reply, ok, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info({x, X}, {Name, Z, _, ValY}) ->
    update(Z, X, ValY),
    {noreply, {Name, Z, X, ValY}};

handle_info({y, Y}, {Name, Z, ValX, _}) ->
    update(Z, ValX, Y),
    {noreply, {Name, Z, ValX, Y}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%
%%%

f(X, Y) ->
    X + Y.

update(Z, X, Y) ->
    wire:set(Z, f(X, Y)).
