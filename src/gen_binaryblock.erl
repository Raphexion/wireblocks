-module(gen_binaryblock).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([behaviour_info/1]).

-export([start_link/5,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%

behaviour_info(callbacks) ->
    [{start, 4}, {f, 2}];
behaviour_info(_) ->
    undefined.

%%%
%% gen_binaryblock API
%%%

start_link(Mod, Name, X, Y, Z) ->
    gen_server:start_link(?MODULE, {Mod, Name, X, Y, Z}, []).

%%%
%% gen_server Behaviour
%%%

init({Mod, Name, X, Y, Z}) ->
    {ok, ValX} = wire:probe(X),
    {ok, ValY} = wire:probe(Y),

    update(Mod, Z, ValX, ValY),
    wire:add_observer(X, self(), x),
    wire:add_observer(Y, self(), y),

    {ok, {Mod, Name, Z, ValX, ValY}}.

handle_call(_What, _From, State) ->
    {reply, ok, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info({x, X}, {Mod, Name, Z, _, ValY}) ->
    update(Mod, Z, X, ValY),
    {noreply, {Mod, Name, Z, X, ValY}};

handle_info({y, Y}, {Mod, Name, Z, ValX, _}) ->
    update(Mod, Z, ValX, Y),
    {noreply, {Mod, Name, Z, ValX, Y}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%% Private
%%%

update(Mod, Z, X, Y) ->
    wire:set(Z, Mod:f(X, Y)).
