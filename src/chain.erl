-module(chain).

-export([start/1,
	 test/2]).

link(0, Last, Acc) ->
    {ok, Last, Acc};

link(N, Parent, Acc) ->
    {ok, Child} = wire_sup:start_wire(0),
    {ok, _} = alias:start(Parent, Child, no_local_state),

    link(N-1, Child, [Child | Acc]).

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
    catch  wire_sup:start_link(),
    {ok, Root} = wire_sup:start_wire(0),

    {ok, Last, All} = link(N, Root, []),

    {ok, Root, Last, All},

    timer:tc(chain, test, [Root, Last]).
