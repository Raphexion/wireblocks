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
	 start_link/2,
	 subprogram_info/1]).

-callback start_link(map()) -> {ok, pid()}.
-callback subprogram_info(pid()) -> map().
-callback subprogram_config(map()) -> map().

%%%
%% API
%%%

start_link(Mod) ->
    start_link(Mod, #{}).

start_link(Mod, DependencyMap) ->
    gen_server:start_link(?MODULE, {Mod, DependencyMap}, []).

subprogram_info(SubProgram) ->
    gen_server:call(SubProgram, subprogram_info).

%%%
%% Behaviour
%%%

init({Mod, DependencyMap}) ->
    {ok, #{mod => Mod, dep => DependencyMap}, 0}.

handle_call(subprogram_info, _From, State) ->
    {reply, State, State};

handle_call(_What, _From, State) ->
    {reply, {ok, State}, State}.

handle_cast(_What, State) ->
    {noreply, State}.

handle_info(timeout, State) ->
    Mod = maps:get(mod, State),
    Dep = maps:get(dep, State),

    Info0 = Mod:subprogram_config(Dep),

    Info1 = maps:put(mod, Mod, Info0),
    Info2 = maps:put(dep, Dep, Info1),

    {noreply, Info2}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
