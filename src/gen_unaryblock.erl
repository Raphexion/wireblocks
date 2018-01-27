-module(gen_unaryblock).

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

start_link(Mod, Name, X, Z, FState) ->
    gen_server:start_link(?MODULE, {Mod, Name, X, Z, FState}, []).

%%%
%% gen_server Behaviour
%%%

init({Mod, Name, X, Z, FState0}) ->
    {ok, ValX} = wire:probe(X),

    FState = update(Mod, Z, ValX, FState0),
    wire:add_observer(X, self(), x),

    {ok, {Mod, Name, Z, ValX, FState}}.

handle_call(_What, _From, State) ->
    {reply, ok, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info({x, X}, {Mod, Name, Z, _, FState0}) ->
    FState = update(Mod, Z, X, FState0),
    {noreply, {Mod, Name, Z, X, FState}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%% Private
%%%

update(Mod, Z, X, FState0) ->
    {ok, Val, FState} = Mod:f(X, FState0),
    wire:set(Z, Val),
    FState.
