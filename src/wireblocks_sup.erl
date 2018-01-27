%%%-------------------------------------------------------------------
%% @doc wireblocks top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wireblocks_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 count_children/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

count_children() ->
    supervisor:count_children(?MODULE).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    WireSpec = {wire, {wire, start_link, []},
		temporary, 2000, worker, [wire]},
    {ok, {{simple_one_for_one, 1, 1}, [WireSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
