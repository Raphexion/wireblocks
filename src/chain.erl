-module(chain).

-export([start/1,
	 test/2]).

link(0, Last, _Supervisor, Acc) ->
    {ok, Last, Acc};

link(N, Parent, Supervisor, Acc) ->
    WireName = list_to_atom(lists:concat(["link", N])),
    {ok, Child} = wire:start(Supervisor, [WireName, 0]),

    BlockName = list_to_atom(lists:concat(["block", N])),
    {ok, _} = alias:start(BlockName, Parent, Child, no_local_state),

    link(N-1, Child, Supervisor, [Child | Acc]).

test(Root, Last) ->
    wire:add_observer(Last, self(), done),

    {ok, Current} = wire:probe(Root),
    Next = Current + 1,

    wire:set(Root, Next),
    receive
	{done, Next} ->
	    ok;
	X  ->
	    {error, X}
    after 5000 ->
	    timeout
    end.


start(N) ->
    {ok, Supervisor} = wireblocks_sup:start_link(),
    {ok, Root} = wire:start(Supervisor, [root, 0]),

    {ok, Last, All} = link(N, Root, Supervisor, []),

    {ok, Root, Last, All},

    timer:tc(chain, test, [Root, Last]).
