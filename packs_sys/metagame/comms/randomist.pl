%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% randomist.pl

move(Role,Move,SIn,SOut) :- 
	my_name(Me),
	format("~nPlayer ~p, as ~p, will select a RANDOM move.~n",[Me,Role]),
%	set_verbose,
%	spy(random_choose),
%	debug,
	random_choose(Role,Move,SIn,SOut),
	verbosely_format("~nPlayer ~p, as ~p, has chosen RANDOM move: ~p.~n",[Me,Role,Move]).
