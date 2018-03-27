-module(linesearch_poc).

-export([start/0]).

start() ->
    simulator:start_link(),

    Children = supervisor:which_children(simulator),

    {ok, Pid} = find_program(Children),

    Info = gen_subprogram:subprogram_info(Pid),

    Guess = maps:get(guess, Info),
    Error = maps:get(error, Info),

    {ok, DebugG} = debug:start(Guess, "Try:   "),
    {ok, DebugE} = debug:start(Error, "Error: "),

    {ok, LineSearch} = linesearch:start_link(Guess, Error, 0.01),

    LineSearch.

find_program([]) ->
    error;

find_program([{program2, Pid, worker, _} | _T]) ->
    {ok, Pid};

find_program([_H | T]) ->
    find_program(T).
