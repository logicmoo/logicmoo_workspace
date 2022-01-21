%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% instantist.pl

move(Role,Move,SIn,SOut) :- 
	my_name(Me),
	format("~nPlayer ~p, as ~p, will select an INSTANT move.~n",[Me,Role]),
	instant_choose(Role,Move,SIn,SOut).
