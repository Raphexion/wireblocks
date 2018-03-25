-module(gen_subprogram).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-export([start_link/1,
	 subprogram_info/0]).

-callback start_link() -> {ok, any()}.
-callback subprogram_info() -> any().

%%%
%% API
%%%

start_link(Mod) ->
    gen_server:start_link(?MODULE, Mod, []).

subprogram_info() ->
    gen_server:call(?MODULE, subprogram_info).

%%%
%% Behaviour
%%%

init(Mod) ->
    {ok, #{mod => Mod}, 0}.

handle_call(subprogram_info, _From, State) ->
    {ok, State, State};

handle_call(_What, _From, State) ->
    {reply, {ok, State}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    Mod = maps:get(mod, State),
    Info = Mod:subprogram_info(),
    maps:put(mod, Mod, Info),
    {noreply, Info}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
