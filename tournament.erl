%%% @author Jose Castro <>
%%% @copyright (C) 2013, Jose Castro
%%% @doc
%%%
%%% @end
%%% Created :  2 Sep 2013 by Jose Castro <>

-module(tournament).

-compile(export_all).

start(Processes) ->
    io:format("start server...~n"),
    server:start(self()),
    Competitors = lists:map(fun(X) -> {X, 0} end, Processes),
    Results = all_against_all(Competitors),
    Ranking = lists:sort(fun({_,X},{_,Y}) -> X > Y end, Results),
    io:format("el ranking es ~w", [lists:map(fun({Module,Score}) -> {Module,Score} end, Ranking)]),
    server:stop().
    %[A,B,C,D,E,F,G,H|Rest] = Ranking,
    %SudenDeath = suden_death([A,H,D,E,C,F,B,G]),
    %report_results(SudenDeath ++ Rest),
    %server:stop().

all_against_all([X]) ->	[X];
all_against_all([X|Rest]) ->
    [NewX | XResults] = one_against_all(X, Rest, []),
    [NewX | all_against_all(XResults)].

one_against_all(X, [Y|Rest], Evaluated) ->
    {X1,Y1} = compete(X,Y),
    {Y2,X2} = compete(Y1,X1),
    one_against_all(X2, Rest, [Y2|Evaluated]);
one_against_all(X, [], Evaluated) -> [X|Evaluated].

compete({ModuleX,PointsX}, {ModuleY,PointsY}) ->
    PX = ModuleX:start(),          delay(),
    PY = ModuleY:start(),          delay(),
    ModuleX:connect(black),        delay(),
    ModuleY:connect(white),        delay(),
    ProcX = get_proc(ModuleX, PX), delay(),
    ProcY = get_proc(ModuleY, PY), delay(),
    process_flag(trap_exit, true),
    link(ProcX),
    link(ProcY),
    server:new_game(),
    {NewPointsX, NewPointsY} = wait_results(ProcX, ProcY, PointsX, PointsY),
    {{ModuleX, NewPointsX}, {ModuleY, NewPointsY}}.

wait_results(ProcX, ProcY, PointsX, PointsY) ->
    io:format("Esperando resultados...~n"),
    receive
	{game_value, X} ->
	    unregister(ProcX),
	    unregister(ProcY),
	    exit(ProcX, game_ended),
	    exit(ProcY, game_ended),
	    io:format("actualizando el valor del juego~n"),
	    {PointsX - X, PointsY + X};
	{'EXIT', ProcX, _} ->
	    io:format("las negras abortaron ...~n"),
	    exit(ProcY, procX_died),
	    io:format("actualizando el valor del juego~n"),
	    {PointsX - 10, PointsY};
	{'EXIT', ProcY, _} ->
	    io:format("las blancas abortaron ...~n"),
	    exit(ProcX, procY_died),
	    io:format("actualizando el valor del juego~n"),
	    {PointsX, PointsY - 10}
    end.

get_proc(Module, true) -> whereis(Module);
get_proc(_, Pid) -> Pid. 

test() ->
    server:start(),delay(),
    agent:start(),delay(),
    client:start(),delay(),
    agent:connect(black),delay(),
    client:connect(white),delay(),
    server:new_game().

delay() ->
    receive
    after 500 ->
	    continue
    end.
