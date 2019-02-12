%%%
%% @author Niklas Johansson <raphexion@gmail.com>
%% @doc Main component of the WireBlocks library
%%%

-module(wire).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/1,
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

%% @doc Set the value of a Wire
set(Wire, Value) ->
    gen_server:cast(Wire, {set, Value}).

%% @doc Register a Observer that will get
%%      notified when a Wire changes value
add_observer(Wire, Observer, Tag) ->
    gen_server:cast(Wire, {add_observer, Observer, Tag}).

%% @doc Debug-purposes only.
%%      Read the current value of a Wire
%% @hidden
probe(Wire) ->
    gen_server:call(Wire, probe).

%% @doc Kill a Wire
%% @hidden
kill(Wire) ->
    gen_server:cast(Wire, kill).

%% @doc Called by the Supervisor
%%      Should normally not be used by developer.
%% @hidden
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%%%
%% Behaviour implementations for GenServer
%%%

-record(wire_state, {value, observers = []}).

%% @hidden
init(InitialValue) ->
    {ok, #wire_state{value=InitialValue}}.

%% @hidden
handle_call(probe, _From, State=#wire_state{value=Value}) ->
    {reply, {ok, Value}, State};

%% @hidden
handle_call(What, _From, State) ->
    {reply, {error, What}, State}.

%% @hidden
handle_cast({add_observer, Observer, Tag}, State=#wire_state{observers=OrgObservers}) ->
    Observers=[{Tag, Observer}|OrgObservers],
    {noreply, State#wire_state{observers=Observers}};

%% @hidden
handle_cast({set, Value}, State=#wire_state{observers=Observers}) ->
    update(Value, Observers),
    {noreply, State#wire_state{value=value}};

%% @hidden
handle_cast(kill, State) ->
    {stop, normal, State};

%% @hidden
handle_cast(_What, State) ->
    {noreply, State}.

%% @hidden
handle_info(What, State) ->
    io:fwrite("~p", [What]),
    {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
    io:fwrite("Dying :(~n", []),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%% Private
%%%

%% @hidden
update(_, []) ->
    ok;

%% @hidden
update(X, [{Tag, Observer} | Rest]) ->
    Observer ! {Tag, X},
    update(X, Rest).
