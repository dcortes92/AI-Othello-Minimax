%%%-------------------------------------------------------------------
%%% File    : server.erl
%%% Author  : Jose Castro <jose.r.castro@gmail.com>
%%% Description : server for othello game application
%%%
%%% Created :  8 Jul 2009 by Jose Castro <jose.r.castro@gmail.com>
%%%-------------------------------------------------------------------
-module(server).

%% API
-vsn(1).
-author('jose.r.castro@gmail.com').

% Server management
-export([start/0, start/1, stop/0]).

% User API
-export([init/0, connect/1, disconnect/0, get_status/0, make_move/2, proxy/0, call_proxy/3, new_game/0]).

-include("othello.hrl").

init()                   -> #game{board=othello:board(), border=othello:border()}.
connect(Color)           -> call(connect, Color).
disconnect()             -> call(disconnect).
get_status ()            -> call(get_status).
make_move  (Color, Pos)  -> call(move,  {Color, Pos}).
new_game()               -> oserver ! {new_game, self()}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
stop() -> oserver ! stop.
start() -> start(none).
start(Tournament) ->
    register(
      oserver,
      spawn(fun() ->
		    GS        = gs:start(),
		    Win       = gs:create(window, GS, [{width, 200}, {height,280}, {title, "Othello Server"}, {map, true}]),
		                gs:create(canvas, canvas,  Win, [{x,0},{y,0},{width,200},{height,200}, {buttonpress, true}]),
		                gs:create(button, newgame, Win, [{x,0}, {y,235}, {label, {text, "New Game"}}]), 
		                gs:create(button, exit,    Win, [{x,100},{y,235},{label,{text, "Exit"}}]),
		    OldBoard  = othello:empty(),
		    GameState = init(),

		    gs:radiobutton(negras,  Win, [{label, {text, "negras" }}, {value, negras }, {x,  0}, {y,205}]),
		    gs:radiobutton(blancas, Win, [{label, {text, "blancas"}}, {value, blancas}, {x,100}, {y,205}]),

		    #game{board = Board} = GameState,

		    display:draw_lines(canvas),
		    display:change(canvas, OldBoard, Board),

		    event_loop(Tournament, 0, false, GameState)
	    end)).

event_loop(Tournament, Iteration, Playing, #game{current=Player,border=Bder}=State) ->
    if (Bder =:= []) and Playing ->
	    io:format("about to report winner (1)~n"),
	    reportWinner(Tournament, State),
	    event_loop(Tournament, 0, false, State);
       true ->
	    receive
		{connect, Client, Color} ->
		    io:format("connecting ~w to ~w~n", [Client, Color]),
		    case Color of
			white -> 
			    Client ! {ok, State},
			    event_loop(Tournament, Iteration, Playing, State#game{white=Client});
			black ->
			    Client ! {ok, State},
			    event_loop(Tournament, Iteration, Playing, State#game{black=Client});
			_ ->
			    Client ! {error, State},
			    event_loop(Tournament, Iteration, Playing, State)
		    end;
		
		{disconnect, Client} ->
		    #game{black=Black, white=White} = State,
		    case Client of
			Black ->
			    Client ! {ok, State},
			    event_loop(Tournament, Iteration, Playing, State#game{black=none});
			White ->
			    Client ! {ok, State},
			    event_loop(Tournament, Iteration, Playing, State#game{white=none});
			_ ->
			    Client ! {error, State},
			    event_loop(Tournament, Iteration, Playing, State)
		    end;

		{new_game, _} ->
		    io:format("server: New Game\n"),
		    #game{white=W, black=B, board=Board} = State,
		    NewBoard = othello:board(),
		    NewState = #game{white=W, black=B, board=NewBoard, current=black, border=othello:border()},
		    display:change(canvas, Board, NewBoard),
		    timer(0, State#game.seconds),
		    notify_players(black, NewState),
		    event_loop(Tournament, 0, true, NewState); 

		{get_status, Client} ->
		    Client ! {ok, State},
		    event_loop(Tournament, Iteration, Playing, State);

		{change_player, Iter} ->
		    if
			not Playing ->
			    event_loop(Tournament, Iteration, Playing, State);
			Iter =:= Iteration ->
			    Pass = State#game.pass,
			    if 
				Pass > 2 -> 
				    io:format("about to report winner (2)~n"),
				    reportWinner(Tournament, State),
				    event_loop(Tournament, 0, false, State);
				true ->
				    NewState = State#game{current=other(Player), pass=Pass+1},
				    timer(Iteration+1, State#game.seconds),
				    event_loop(Tournament, Iteration+1, Playing, NewState)
			    end;
			true ->
			    event_loop(Tournament, Iteration, Playing, State)
		    end;

		{move, Client, {Player, Pos}} ->
		    if
			not Playing ->
			    event_loop(Tournament, Iteration, Playing, State);
			true ->
			    io:format("server: make a move for ~w at ~w~n", [Player, Pos]),
			    {Reply, NewState} = move(Player, Pos, State),
			    % io:format("reply = ~w~n", [Reply]),
			    #game{board=Board} = State,
			    Client ! Reply,
			    case Reply of
				{ok, #game{board=NewBoard}} ->
				    display:change(canvas, Board, NewBoard),
				    timer(Iteration+1, State#game.seconds),
				    event_loop(Tournament, Iteration+1, Playing, NewState#game{pass=0});
				_ ->
				    event_loop(Tournament, Iteration, Playing, State)
			    end
		    end;
		stop ->
		    unregister(oserver),
		    finish;


		% gs callback events
		{gs, _id, destroy, _Data, _Args} ->
		    unregister(oserver),
		    finish;
		
		{gs, canvas, buttonpress, [], [1,A,B|_]} ->
		    %io:format("button press at ~w~n", [display:pos(A,B)]),
		    {Reply, NewState} = move(Player, display:pos(A,B), State),
		    %io:format("server: reply = ~w~n", [Reply]),
		    #game{board=Board} = State,
		    case Reply of
			{ok, #game{board=NewBoard}} ->
			    display:change(canvas, Board, NewBoard),
			    timer(Iteration+1, State#game.seconds),
			    event_loop(Tournament, Iteration+1, true, NewState);
			_ -> 
			    event_loop(Tournament, Iteration, Playing, State)
		    end;
		
		{gs, exit, click, _, _} ->
		    unregister(oserver),
		    finish;
		
		{gs, newgame, click, _, _} ->
		    new_game(),
		    event_loop(Tournament, Iteration, Playing, State)

	    end
    end.

% Utility functions for calling the othello server
call(Id, Data) ->
    oserver ! {Id, self(), Data},
    receive
	{ok, Result} ->
	    Result;
	Error -> Error
    end.


call(Id) ->
    oserver ! {Id, self()},
    receive
	{ok, Result} ->
	    Result;
	Error -> Error
    end.


%%--------------------------------------------------------------------


other(white) -> black;
other(black) -> white.

color(white) ->  1;
color(black) -> -1.

%% MAKE_MOVE
move(Player, _Pos, State=#game{current=Other}) when Player /= Other ->
    % io:format("server: not your turn player = ~w, other = ~w, state = ~w...\n", [Player, Other, State]),
    {{not_your_turn, Player}, State};

move(Player, Pos, State=#game{current=Player, board=Board, border=Border}) ->
    % io:format("~w, ~w~n", [Pos, Border]),
    case lists:member(Pos, Border) of
	true  -> case othello:check_move(Pos, Board, othello:directions(), color(Player)) of
                     true  -> NewBorder = othello:new_frontier(Border, Pos, Board),
                              NewBoard  = othello:make_move(Pos, Board, othello:directions(), color(Player)),
                              NewState  = State#game{current=other(Player), board=NewBoard, border=NewBorder},
			      notify_players(other(Player), NewState),
			      {{ok, NewState}, NewState};
		     false ->
			 io:format("oops~n", []),
			 {{invalid_move, Pos}, State}
		 end;
	false -> {{invalid_move, Pos}, State}
    end.

notify_players(Color, #game{white=White, black=Black}=State) ->
    if 
	Color =:= white ->
            io:format("~n-----------SERVER: white turn-----------~n"),
	    notify(white, White, {your_turn, State}),
	    notify(black, Black, {ok, State});
	Color =:= black ->
            io:format("~n-----------SERVER: black turn-----------~n"),
	    notify(black, Black, {your_turn, State}),
	    notify(white, White, {ok, State})
    end.

notify(Color, none, Message) -> io:format("Interactive ~w~n",[Color]), ok;
notify(_, Proc,  Message) -> Proc ! Message.

proxy_loop() ->
    receive
	stop ->
	    io:format("proxy ~w: good bye~n", [self()]),
	    no_problem;
	{Fun, Args} ->
	    RET = erlang:apply(server, Fun, Args),
	    io:format("proxy ~w: ~w~n", [self(), RET]),
	    proxy_loop()
    end.

proxy() ->
    spawn(fun() -> proxy_loop() end).
call_proxy(Proxy, Fun, Args) ->
    Proxy ! {Fun, Args}.

reportWinner(Tournament, #game{board=Board}) ->
    Value = lists:sum(tuple_to_list(Board)),
    io:format("Valor del juego es ~w~n", [Value]),
    io:format("Tournament = ~w~n", [Tournament]),
    if
	Tournament =/= none ->
	    Tournament ! {game_value, Value};
	true ->
	    nothing
    end,
    Value.

timer(Iteration, Seconds) ->
    Miliseconds = Seconds * 1000,
    Proc = self(),
    spawn(fun() ->
		  receive
		  after Miliseconds ->
			  Proc ! {change_player, Iteration}
		  end
	  end).
