%%%
%% @author Niklas Johansson <raphexion@gmail.com>
%% @doc Behaviour to reduce boilerplate code of functin blocks
%%%

-module(gen_sinkblock).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/3,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%

-callback start(any(), any()) -> any().
-callback f(any(), any()) -> any().

%%%
%% gen_binaryblock API
%%%

start_link(Mod, Wire, FState) ->
    gen_server:start_link(?MODULE, {Mod, Wire, FState}, []).

%%%
%% gen_server Behaviour
%%%

%% @hidden
init({Mod, Wire, FState0}) ->
    wire:add_observer(Wire, self(), x),
    {ok, {Mod, FState0}}.

%% @hidden
handle_call(_What, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info({x, X}, {Mod, FState0}) ->
    FState = update(Mod, X, FState0),
    {noreply, {Mod, FState}}.

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
update(Mod, X, FState0) ->
    {ok, FState} = Mod:f(X, FState0),
    FState.
