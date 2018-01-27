-module(wire).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/1,
	 start/2,
	 set/2,
	 add_observer/3,
	 probe/1,
	 kill/1]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%%
%% API
%%%

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%

start(Supervisor, Name) ->
    supervisor:start_child(Supervisor, [Name]).

set(Wire, Value) ->
    gen_server:cast(Wire, {set, Value}).

add_observer(Wire, Observer, Tag) ->
    gen_server:cast(Wire, {add_observer, Observer, Tag}).

probe(Wire) ->
    gen_server:call(Wire, probe).

kill(Wire) ->
    gen_server:cast(Wire, kill).

%%%
%% Behaviour
%%%

init([Name, Value]) ->
    io:fwrite("New Wire ~p with value ~p~n", [Name, Value]),
    register(Name, self()),
    {ok, {Value, []}}.

handle_call(probe, _From, State = {Value, _}) ->
    {reply, {ok, Value}, State};

handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

handle_cast({add_observer, Observer, Tag}, {Value, Observers}) ->
    Observer ! {Tag, Value},
    {noreply, {Value, [{Tag, Observer}] ++ Observers}};

handle_cast({set, Value}, {_, Observers}) ->
    update(Value, Observers),
    {noreply, {Value, Observers}};

handle_cast(kill, State) ->
    {stop, normal, State};

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(What, State) ->
    io:fwrite("~p", [What]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:fwrite("Dying :(~n", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%%
%%%

update(_, []) ->
    ok;

update(X, [{Tag, Observer} | Rest]) ->
    Observer ! {Tag, X},
    update(X, Rest).
