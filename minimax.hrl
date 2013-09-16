
-record(minimax,
	{
	  board  =  none,   % Bit representation of board
	  depth  =   0,     % depth of board
	  search =   0,     % depth of search so far (starting at this level)
	  turn   =  -1,     % -1 black, 1 white
	  childs =  [],     % key (boards) of childs
	  move   =  {0,0},  % best move so far
	  cost   =  0,      % cost guaranteed by taking move
	  alfa   = -100000, % best cost guaranteed by MAX higher up the MINIMAX search tree
	  beta   =  100000  % best cost guaranteed by MIN higher up the MINIMAX search tree
	}).
