%%%-------------------------------------------------------------------
%% @doc wireblocks top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(simulator).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy => one_for_all,
		 intensity => 1,
		 period => 5},
    ChildSpecs = [
		  #{id => wire_sup,
                    start => {wire_sup, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [wire_sup]},
		  #{id => reactor,
                    start => {reactor, start_link, []},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [reactor]},
		  #{id => program0,
                    start => {program0, start_link, [#{}]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [program0]},
		  #{id => program1,
                    start => {program1, start_link, [#{}]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [program1]},
		  #{id => program2,
                    start => {program2, start_link, [#{}]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [program2]}
	 ],
    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal functions
%%====================================================================
