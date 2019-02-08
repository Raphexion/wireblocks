%%%-------------------------------------------------------------------
%% @doc wireblocks top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wire_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_wire/1,
	 count_children/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Create a new un-named Wire.
start_wire(InitialValue) ->
    supervisor:start_child(?MODULE, [[InitialValue]]).

count_children() ->
    supervisor:count_children(?MODULE).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    WireSpec = {wire, {wire, start_link, []},
		temporary, 2000, worker, [wire]},
    {ok, {{simple_one_for_one, 1, 1}, [WireSpec]}}.

%%====================================================================
%% Internal functions
%%====================================================================
