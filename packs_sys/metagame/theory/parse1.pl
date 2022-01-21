%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% parse1.pl

% Experimenting with different versions of generating and
% saving games.

generate_make_test_game :-
	generate_game(G),
	make_test_game(G).

% GENERATE_AND_LOAD(+File)
% Generates a new game into file 'File.game', 
% then loads it. 
generate_and_load(File) :- 
	random_game_to_file(File),
	load_game(File).


% LOAD_GAME(+GameName)
% Finds a file GameName.game in a library directory.
% Then loads this file as the current (test) game.
load_game(Name) :-
	find_game_file(Name,File),
	file_make_test_game(File).

% Uses predicate in sysdev.pl
find_game_file(Name,File) :- 
	find_suffixed_library_file(Name,game,File).


% file_make_test_game('~/prolog/play/chess.game').
file_make_test_game(File) :-
	read_game_from_file_to_list(File,Game),
	parse_make_test_game(Game).
	
% STRING_MAKE_TEST_GAME(+String)
string_make_test_game(String) :-
	read_game_from_string_to_list(String,Game),
	parse_make_test_game(Game).


parse_make_test_game(Game) :-
	set_parsing_mode,
	format("~nParsing game.~n",[]),
	game(G,Game,[]),
	make_test_game(G).
	
:- dynamic player_current_game/1, opponent_current_game/1.

% index_sym_dirs/0 defined in compile_syms.pl
% To compare speed without it, comment out that line.

make_test_game(G) :- 
	game_name(G,Name),
	format("~nMaking ~w the current test game~n",[Name]),
	abolish(player_current_game/1),
	abolish(opponent_current_game/1),
	 assert(player_current_game(G)),
	invert(G,opponent,G1),
	 assert(opponent_current_game(G1)),
	maybe_compile_syms.

maybe_compile_syms :- 
	( parameter(compile_symmetries,on) ->
	index_sym_dirs
	; true
	).


