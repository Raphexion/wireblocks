-module(reactor).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%%

-export([start_link/0,
	 exec/3]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

exec(Wire, Value, Delay) ->
    gen_server:cast(?MODULE, {exec, Wire, Value, Delay}).

%%

init([]) ->
    T0 = gb_trees:empty(),
    {ok, {T0, 0}}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast({exec, Wire, Value, Delay}, {Tree0, Time0}) ->
    Tree1 = gb_trees:enter(Time0 + Delay,
			   fun() -> wire:set(Wire, Value) end,
			   Tree0),
    {Time1, _} =  gb_trees:smallest(Tree1),
    {noreply, {Tree1, Time1}, Time1 - Time0}.

handle_info(timeout, {Tree0, Time0}) ->
    case gb_trees:size(Tree0) of
	0 ->
	    {noreply, {Tree0, Time0 + 1}, 1};
	1 ->
	    {_, F, Tree1} = gb_trees:take_smallest(Tree0),
	    F(),
	    {noreply, {Tree1, Time0 + 1}, 1};
	_ ->
	    {_, F, Tree1} = gb_trees:take_smallest(Tree0),
	    F(),
	    {Time1, _} =  gb_trees:smallest(Tree1),
	    {noreply, {Tree1, Time1}, Time1 - Time0}
    end.

terminate(_Reason, _State) ->
    io:fwrite("Dying :(~n", []),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
