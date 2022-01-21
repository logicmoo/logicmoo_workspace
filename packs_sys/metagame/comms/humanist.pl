%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% humanist.pl

move(Role,Move,SIn,SOut) :- 
	my_name(Me),
	format("~nPlayer ~p, as ~p, wants your help to select his move.~n",[Me,Role]),
	human_choose(Role,Move,SIn,SOut).
