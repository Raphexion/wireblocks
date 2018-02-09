%%%
%% @author Niklas Johansson <raphexion@gmail.com>
%% @doc Behaviour to reduce boilerplate code of functin blocks
%%%

-module(gen_sinkblock).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([behaviour_info/1]).

-export([start_link/4,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%

behaviour_info(callbacks) ->
    [{start, 3}, {f, 2}];

behaviour_info(_) ->
    undefined.

%%%
%% gen_binaryblock API
%%%

start_link(Mod, Name, Wire, FState) ->
    gen_server:start_link(?MODULE, {Mod, Name, Wire, FState}, []).

%%%
%% gen_server Behaviour
%%%

%% @hidden
init({Mod, Name, Wire, FState0}) ->
    wire:add_observer(Wire, self(), x),
    {ok, {Mod, Name, FState0}}.

%% @hidden
handle_call(_What, _From, State) ->
    {reply, ok, State}.

%% @hidden
handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info({x, X}, {Mod, Name, FState0}) ->
    FState = update(Mod, X, FState0),
    {noreply, {Mod, Name, FState}}.

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
