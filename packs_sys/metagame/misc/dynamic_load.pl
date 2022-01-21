%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================
%%% dynamic_load.pl
%%%   Loads a theory file.

dl([]) :- !.
dl([F|Files]) :- !, dl(F), dl(Files).
dl(Filename) :- dynamic_load(Filename).

dynamic_load(Filename) :-
	open(Filename,read,Stream),
	format("Loading theory file: ~w~n",[Filename]),
	dynamic_load_stream(Stream),
	format("  Finished loading Theory file: ~w~n",[Filename]),
	close(Stream).

dynamic_load_stream(Stream) :-
	read(Stream,Term),
	Term \== end_of_file 
            -> ( process_term(Term),
		dynamic_load_stream(Stream) )
            ; true.

%process_term(Term) :- assert(Term).
process_term(Term) :- theory_assert(Term).
	
