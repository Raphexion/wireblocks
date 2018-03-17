-module(reactor).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(DEFAULT_EMTPY_TICK, 100000).

%%

-export([start_link/0,
	 exec_ms/3,
	 exec_us/3]).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

exec_ms(Wire, Value, DelayMs) ->
    exec_us(Wire, Value, DelayMs * 1000).

exec_us(Wire, Value, DelayUs) ->
    ExecTime = DelayUs +  erlang:monotonic_time(microsecond),
    gen_server:cast(?MODULE, {exec, Wire, Value, ExecTime}).

%%

init([]) ->
    Tree = gb_trees:empty(),
    {ok, {Tree}}.

handle_call(What, _From, State) ->
    {reply, {ok, What}, State}.

handle_cast({exec, Wire, Value, ExecTime}, {Tree0}) ->
    CurrentTime = erlang:monotonic_time(microsecond),
    {Tree1, Time} = handle_exec(Wire, Value, ExecTime, CurrentTime, Tree0),
    Timeout = round(Time / 1000),
    {noreply, {Tree1}, Timeout}.

handle_info(timeout, {Tree}) ->
    {Tree1, Time} = handle_timeout(Tree),
    Timeout = round(Time / 1000),
    {noreply, {Tree1}, Timeout}.

terminate(_Reason, _State) ->
    io:fwrite("Dying :(~n", []),
    ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%% private
%%%

handle_exec(Wire, Value, ExecTime, CurrentTime, Tree) when ExecTime < CurrentTime ->
    wire:set(Wire, Value),
    calc_next_timeout(Tree);

handle_exec(Wire, Value, ExecTime, _CurrentTime, Tree0) ->
    Tree1 = gb_trees:enter(ExecTime,
			   fun() -> wire:set(Wire, Value) end,
			   Tree0),
    calc_next_timeout(Tree1).

%%
%%

handle_timeout(Tree) ->
    handle_timeout(Tree, gb_trees:size(Tree)).

handle_timeout(Tree, 0) ->
    calc_next_timeout(Tree, 0);

handle_timeout(Tree0, _N) ->
    {_, F, Tree1} = gb_trees:take_smallest(Tree0),
    F(),
    calc_next_timeout(Tree1).

%%
%%

calc_next_timeout(Tree) ->
    calc_next_timeout(Tree, gb_trees:size(Tree)).

calc_next_timeout(Tree, 0) ->
    {Tree, ?DEFAULT_EMTPY_TICK};

calc_next_timeout(Tree, _N) ->
    {Time, _} =  gb_trees:smallest(Tree),
    Now = erlang:monotonic_time(microsecond),
    {Tree, Time - Now}.
