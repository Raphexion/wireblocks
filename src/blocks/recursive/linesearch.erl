-module(linesearch).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/3,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%%%
%% gen_binaryblock API
%%%

start_link(X, Error, Alpha) ->
    gen_server:start_link(?MODULE, {X, Error, Alpha}, []).

%%%
%% gen_server Behaviour
%%%

init({X, Error, Alpha}) ->
    E = Error,

    {ok, ValX} = wire:probe(X),
    {ok, ValE} = wire:probe(E),

    % wire:add_observer(X, self(), x),
    wire:add_observer(E, self(), e),

    {ok, {X, Alpha, ValX, ValE}, 1000}.

handle_call(_What, _From, State) ->
    {reply, ok, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(timeout, {X, Alpha, ValX, ValE}) ->
    ValX1 = update(X, Alpha, ValX, ValE),
    {noreply, {X, Alpha, ValX1, ValE}};

handle_info({x, ValX}, {X, Alpha, _, ValE}) ->
    ValX1 = update(X, Alpha, ValX, ValE),
    {noreply, {X, Alpha, ValX1, ValE}};

handle_info({e, ValE}, {X, Alpha, ValX, _}) ->
    ValX1 = update(X, Alpha, ValX, ValE),
    {noreply, {X, Alpha, ValX1, ValE}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%
%% Private
%%%

update(X, Alpha, ValX0, ValE) when ValE > Alpha ->
    ValX1 = ValX0 + Alpha * ValE,
    reactor:exec_ms(X, ValX1, 1),
    ValX1;

update(X, Alpha, ValX0, ValE) when ValE < -Alpha ->
    ValX1 = ValX0 - Alpha * (- ValE),
    reactor:exec_ms(X, ValX1, 1),
    ValX1;

update(_, _, ValX, ValE) ->
    io:fwrite("Line Search found ~p (~p)~n", [ValX, ValE]),
    ValX.
