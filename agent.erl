%% HOLA

%%% File    : agent.erl
%%% Author  : JCastro <>
%%% Description : 
%%% Created : 14 Oct 2010 by JCastro <>

%%% Modifications pending:
%%% Use internally a bitfield representation for the configuration
%%% use the erlang table to store games
%%% use iterative deepening to get results

-module(agent).

-export([start/1, start/0, stop/1, stop/0, connect/1, connect/2, disconnect/1, disconnect/0]).
-compile(export_all).
-include("othello.hrl").

connect(Color)        -> call(agent, connect, Color).
disconnect()          -> call(agent, disconnect).
stop()                -> call(agent, stop).

connect(Agent, Color) -> call(Agent, connect, Color).
disconnect(Agent)     -> call(Agent, disconnect).
stop(Agent)           -> call(Agent, stop).

call(Agent, Id, Data) -> Agent ! {Id, Data}.
call(Agent, Id)       -> Agent ! Id.

color(white) ->  1;
color(black) -> -1.

start() -> start(agent).
start(Agent) ->
    register(
      Agent,
      spawn(fun() ->
		    GS = gs:start(),
		    Win  = gs:create(window, GS, [{width, 200}, {height,280}, {title, atom_to_list(Agent)}, {map, true}]),
		    Exit = gs:create(button, Win, [{x,0},{y,210},{label,{text, "Exit"}}]),
		  
		    % Should change radiobutton declaration
		    gs:radiobutton(negras,  Win, [{label, {text, "negras" }}, {value, negras }, {x,  0}, {y,245}, {group, Agent}]),
		    gs:radiobutton(blancas, Win, [{label, {text, "blancas"}}, {value, blancas}, {x,100}, {y,245}, {group, Agent}]),

		    TxtDepth = gs:entry(Win, [{y,10},{x,5},{width,50},{keypress,true}, {setfocus,true},{text,"5"}]),
		    LbDepth  = gs:listbox(Win,[{x,5},{y,35},{width,80}, {height,150}, {click,true},{doubleclick,true}]), 
		    gs:config(LbDepth,[{items,[1,2,3,4,5]}]), 
		    loop(Agent, LbDepth, TxtDepth, Exit, 4)
	    end)).

loop(Agent, LbDepth, TxtDepth, ExitButton, Depth) ->
    receive
	% user requests
	{connect, Color} ->
	    server:connect(Color),
	    loop(Agent, LbDepth, TxtDepth, ExitButton, Depth);
	disconnect ->
	    server:disconnect(),
	    loop(Agent, LbDepth, TxtDepth, ExitButton, Depth);

	% server notification
	{your_turn, #game{current=Player, board=Board, border=Border}} ->
	    io:format("MY TURN!!!~n"),
	    Move = make_move(color(Player), Board, Depth, Border),
	    % io:format("agent ~w: my turn ~w, state ~w~n", [self(), Move, Board]),
	    server:make_move(Player, Move),
	    loop(Agent, LbDepth, TxtDepth, ExitButton, Depth);
	{your_turn, X} ->
	    io:format("this is what I received ~w~n", [X]),
	    loop(Agent, LbDepth, TxtDepth, ExitButton, Depth);

	% gs callback events
	{gs, negras, click, _Data, [_Text, _Grp, negras| _Rest]} ->
	    io:format("agent ~w: update player to negras\n", [self()]),
	    server:disconnect(),
	    server:connect(black),
	    loop(Agent, LbDepth, TxtDepth, ExitButton, Depth);
	{gs, blancas, click, _Data, [_Text, _Grp, blancas| _Rest]} ->
	    io:format("agent ~w: update player to blancas\n", [self()]),
	    server:disconnect(),
	    server:connect(white),
	    loop(Agent, LbDepth, TxtDepth, ExitButton, Depth);
	{gs, LbDepth, click, _Data, [Idx, Txt|_]} ->
	    io:format("agent ~w: change depth to ~w\n", [self(), Idx+1]),
	    gs:config(TxtDepth, [{text, Txt}]),
	    loop(Agent, LbDepth, TxtDepth, ExitButton, Idx+1);

	{gs, _, destroy, _, _} ->
	    self() ! stop;
	{gs, ExitButton, click, _, _} -> 
	    self() ! stop;
	stop ->
	    server:disconnect(),
	    unregister(Agent),
	    io:format("agent ~w: othello client stopped~n", [self()]),
	    ok
    end.
    
make_move(Player, Board, _Depth, _Border) -> {1,1}.

%Obtiene los posibles movimientos de un jugador
%Se le envia una lista de 12 -> 89
posibles_movimientos([H|T]) ->
	Valido = othello:check_move(H, othello:board(), othello:directions(), -1),
	if Valido ->
		[H |  posibles_movimientos(T)];
	true ->
		posibles_movimientos(T)
	end;

posibles_movimientos([]) -> [].