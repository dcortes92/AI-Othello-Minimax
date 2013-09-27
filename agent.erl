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
-include("minimax.hrl").

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
    
%make_move(Player, Board, _Depth, _Border) -> 57. %Esta tupla se cambia por un número

make_move(Player, Board, Depth, Border) ->
	%Se actualiza el estado con el tablero actual, la profundidad y los posibles movimientos.
	State = #minimax{board=Board, depth=Depth, turn=Player},
	io:format("Entrando a alfabeta~n"),
	Move = alpha_beta_search(State),
	io:format("Move es ~w~n", [Move]),
	Move.


%Algoritmo Alfa-Beta implementado con el pseudocógido del libro AIMA pág. 170
alpha_beta_search(State) ->
	max_value(State, 100000, -100000).

max_value(State, Alpha, Beta) ->
	%Se comprueba si ya se llegó al objetivo
	if State#minimax.depth == State#minimax.search ->
		io:format("Entra primer if~n"),
		State#minimax.move;
	true ->
		io:format("Entrando primer else~n"),
	%En otro caso se obtienen los posibles movimientos del estado actual "$/6A"
		Moves = get_moves(State#minimax.turn),
		%State#minimax{childs = Moves},
		%MovesTemp = State#minimax.childs,
		
		State#minimax{cost=-234567},
		MovesTemp = State#minimax.cost,
		State#minimax.move, %Se inicia en beta porque el algoritmo inicia V en -100000
		%foreach_max_value(Moves, Alpha, Beta, State)
	end.

foreach_max_value([H | T], Alpha, Beta, State) ->
	Temp = State#minimax.cost,
	State#minimax{turn = 1},
	Cost = State#minimax.cost,
	State#minimax{cost=Cost+1},
	V = max(Temp, min_value(H, Alpha, Beta)),
	if V > Beta ->
		%Se poda el árbol
		State#minimax{move= V},
		State#minimax.move;
	true ->
		Alpha = max(Alpha, V),
		State#minimax{alfa = Alpha},
		State#minimax{cost = V},
		foreach_max_value(T, Alpha, Beta, State)
	end;

foreach_max_value([H], Alpha, Beta, State) ->
	Temp = State#minimax.cost,
	Cost = State#minimax.cost,
	State#minimax{cost=Cost+1},
	V = max(Temp, min_value(H, Alpha, Beta)),
	if V > Beta ->
		%Se poda el árbol
		State#minimax{move= V},
		State#minimax.move;
	true ->
		Alpha = max(Alpha, V),
		State#minimax{alfa = Alpha},
		State#minimax{cost = V}
	end;

foreach_max_value([], _Alpha, _Beta, State) ->
	State#minimax.cost.


min_value(State, Alpha, Beta) ->
	%Se comprueba si ya se llegó al objetivo
	if State#minimax.depth == State#minimax.search ->
		State#minimax.move;
	true ->
	%En otro caso se obtienen los posibles movimientos del estado actual
		State#minimax{childs = get_moves(State#minimax.turn)},
		Moves = State#minimax.childs,
		io:format("Los childs de ~w son ~w~n", [State#minimax.turn], [Moves]),
		State#minimax{cost=100000}, %Se inicia en alfa porque el algoritmo inicia V en 100000
		foreach_min_value(Moves, Alpha, Beta, State)
	end.

foreach_min_value([H | T], Alpha, Beta, State) ->
	Temp = State#minimax.cost,
	State#minimax{turn = -1},
	Cost = State#minimax.cost,
	State#minimax{cost=Cost+1},
	V = min(Temp, max_value(H, Alpha, Beta)),
	if V =< Alpha ->
		%Se poda el árbol
		State#minimax{move=V},
		State#minimax.move;
	true ->
		Beta = min(Beta, V),
		State#minimax{beta = Beta},
		State#minimax{cost = V},
		foreach_min_value(T, Alpha, Beta, State)
	end;

foreach_min_value([H], Alpha, Beta, State) ->
	Temp = State#minimax.cost,
	State#minimax{turn = -1},
	Cost = State#minimax.cost,
	State#minimax{cost=Cost+1},
	V = min(Temp, min_value(H, Alpha, Beta)),
	if V =< Alpha ->
		%Se poda el árbol
		State#minimax{move=V},
		State#minimax.move;
	true ->
		Beta = min(Beta, V),
		State#minimax{beta = Beta},
		State#minimax{cost = V}
	end;

foreach_min_value([], _Alpha, _Beta, State) ->
	State#minimax.cost.




%Obtiene los posibles movimientos de un jugador
%Se le envía una lista con todos los posibles movimientos de un jugador
%la lista tiene números del 12-19,22-29, y retorna una tupla con las
%posibles casillas en donde se puede mover.
get_moves(Player) ->
	io:format("Obteniendo moves de ~w~n", [Player]),
	get_moves(valid_moves(), Player).

get_moves([H|T], Player) ->
	Valido = othello:check_move(H, othello:board(), othello:directions(), Player),
	if Valido ->
		[H |  get_moves(T, Player)];
	true ->
		get_moves(T, Player)
	end;

get_moves([], _Player) -> [].


%Genera la lista de movimientos que utiliza get_moves
valid_moves() ->
	lists:seq(12,19) ++
	lists:seq(22,29) ++
	lists:seq(32,39) ++
	lists:seq(42,49) ++
	lists:seq(52,59) ++
	lists:seq(62,69) ++
	lists:seq(72,79) ++
	lists:seq(82,89).



%%%%%%%% Función de Evaluación %%%%%%%%%%%%%%%%%%%
%%%%%%%% Falta: tomar en cuenta la estabilidad de una posicion
%Valor de cada posición en el tablero, los valores
%fueron tomados y adaptados del score asignado al juego
%reversi de Microsoft Windows http://www.samsoft.org.uk/reversi/strategy.htm#position


%	  _, C, A, B, B, A, C, _, 
%     C, X, 0, 0, 0, 0, X, C, 
%     A, 0, 0, 0, 0, 0, 0, A, 
%     B, 0, 0, 0, 0, 0, 0, B, 
%     B, 0, 0, 0, 0, 0, 0, B, 
%     A, 0, 0, 0, 0, 0, 0, A, 
%     C, X, 0, 0, 0, 0, X, C, 
%     _, C, A, B, B, A, C, _, 



%Esquinas
score(12) -> 99;
score(19) -> 99;
score(82) -> 99;
score(89) -> 99;

%Cuadros X
score(23) -> -24;
score(28) -> -24;
score(73) -> -24;
score(78) -> -24;

%Cuadros C
score(13) -> -8;
score(18) -> -8;
score(22) -> -8;
score(29) -> -8;
score(72) -> -8;
score(79) -> -8;
score(83) -> -8;
score(88) -> -8;

%Cuadros A
score(14) -> 8;
score(17) -> 8;
score(32) -> 8;
score(39) -> 8;
score(62) -> 8;
score(69) -> 8;
score(84) -> 8;
score(87) -> 8;

%Cuadros B
score(15) -> 6;
score(16) -> 6;
score(42) -> 6;
score(49) -> 6;
score(52) -> 6;
score(59) -> 6;
score(85) -> 6;
score(86) -> 6;

%Otros
score(24) -> -4;
score(27) -> -4;
score(33) -> -4;
score(38) -> -4;
score(63) -> -4;
score(68) -> -4;
score(74) -> -4;
score(77) -> -4;

score(25) -> -3;
score(26) -> -3;
score(43) -> -3;
score(48) -> -3;
score(53) -> -3;
score(58) -> -3;
score(75) -> -3;
score(76) -> -3;

score(34) -> 7;
score(37) -> 7;
score(64) -> 7;
score(67) -> 7;

score(44) -> 4;
score(47) -> 4;
score(54) -> 4;
score(57) -> 4;

score(_) -> 0.