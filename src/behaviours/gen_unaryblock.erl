%%%
%% @author Niklas Johansson <raphexion@gmail.com>
%% @doc Behaviour to reduce boilerplate code of functin blocks
%%%

-module(gen_unaryblock).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/4,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%

-callback start(any(), any(), any()) -> any().
-callback f(any(), any()) -> any().

%%%
%% gen_binaryblock API
%%%

start_link(Mod, X, Z, FState) ->
    gen_server:start_link(?MODULE, {Mod, X, Z, FState}, []).

%%%
%% gen_server Behaviour
%%%

%% @hidden
init({Mod, X, Z, FState0}) ->
    {ok, ValX} = wire:probe(X),

    FState = update(Mod, Z, ValX, FState0),
    wire:add_observer(X, self(), x),

    {ok, {Mod, Z, ValX, FState}}.

%% @hidden
handle_call(_What, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info({x, X}, {Mod, Z, _, FState0}) ->
    FState = update(Mod, Z, X, FState0),
    {noreply, {Mod, Z, X, FState}}.

%% @hidden
terminate(_Reason, _State) ->
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%% Private
%%%

%% @hidden
update(Mod, Z, X, FState0) ->
    {ok, Val, FState} = Mod:f(X, FState0),
    wire:set(Z, Val),
    FState.
