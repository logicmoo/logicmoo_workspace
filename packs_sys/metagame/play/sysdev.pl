%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sysdev.pl --- misc utilities used for debugging or developing the system
%%
%% external routines: practically everything (this is a utilty file); BUT,
%%   none of these routines should be called by the system proper -- it's
%%   all "researcher support"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

my_absolute_file_name(N,N).


% is_profiling/0 defined differently for prolog versions, 
% in file <prolog>-version
my_use_module(M) :- 
	is_profiling
        -> profiling_load(M)
        ;  use_module(M).

my_ensure_loaded(M) :- 
	is_profiling
        -> profiling_load(M)
        ;  ensure_loaded(M).

profiling_load(M) :- 
%	current_predicate(
	compile(M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- my_ensure_loaded('../misc/aux').


make_library_directory(Path) :-
	my_absolute_file_name(Path,D),
	assert(library_directory(D)).


% METAGAME_DIRECTORY(Metagame)
% This is usually asserted into the system during a build by 
% the makefile. The directory name should be the logical name, 
% insensitive to automounting, otherwise trouble arises
% for automounted systems when later invocations can't find the
% absolute file name saved eariler. 
% An example of a good name is:
%     assert(metagame_directory('~bdp/Metagame')).

metagame_subpath(Path,Dir) :-
	metagame_directory(Metagame),
	concat(Metagame,/,Sub1),
	concat(Sub1,Path,SubDir),
	my_absolute_file_name(SubDir,Dir).

make_metagame_subpath(Path) :-
	metagame_subpath(Path,Dir),
	make_library_directory(Dir).

find_index_preds_file :- 
	metagame_subpath('state/index_preds.pl',F),
        abolish(index_preds_file/1),
	assert(index_preds_file(F)).


find_games_library :- 
	metagame_subpath(games,D),
	abolish(games_library_directory/1),
	assert(games_library_directory(D)).


find_theory_directory :- 
	metagame_subpath(theory,D),
	abolish(theory_directory/1),
	assert(theory_directory(D)).

find_dynamic_preds_file :- 
	metagame_subpath('theory/dynamic_preds.pl',F),
        abolish(dynamic_preds_file/1),
	assert(dynamic_preds_file(F)).
 

bind_environment_paths :-
 	make_metagame_subpath(generator),
	make_metagame_subpath(misc),
	make_metagame_subpath(play),
	make_metagame_subpath(games),
	make_metagame_subpath(evals),
	make_metagame_subpath(state),
	make_metagame_subpath(theory),
	make_metagame_subpath(comms),
	make_metagame_subpath(coding),
	make_metagame_subpath(learning),
	find_dynamic_preds_file,
	find_index_preds_file,
	find_games_library,
	find_theory_directory,
	assert(library_directory(.)).


% Metagame System Filenames

sys_filename_suffix(prolog,'.pl').
sys_filename_suffix(state_compile,'_stat').
sys_filename_suffix(game,'.game').
sys_filename_suffix(eval,'.eval').
sys_filename_suffix(record,'.rec').


sys_suffixed_filename(File,Sys,Name) :-
	sys_filename_suffix(Sys,Suf),
	suffixed_filename(File,Suf,Name).
	
suffixed_filename(File,Suf,Name) :-
	bi_concat(File,Suf,Name).


find_suffixed_library_file(Name,Suffix,File) :- 
	nofileerrors,
	sys_suffixed_filename(Name,Suffix,File1),
	exists_absolute_file_name(library(File1),File),
	!,
	fileerrors.
find_suffixed_library_file(Name,Suffix,_File) :- 
	fileerrors,
	format("~nError: Couldn't find file ~w.~p~n",[Name,Suffix]),
	fail.

%----------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% loading parts of the system
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Files defined by player system
% and other systems, are
% now in player_files.pl


load_main_system_files :- 
	compile(library(player_files)).


% LOAD_METAGAME
% This should be called from the directory in which the 
% player files reside.  
load_metagame :- 
	bind_environment_paths,
	load_main_system_files,
	load_system(library),
	load_system(generator),
	load_system(play),
	load_system(analysis),
	compile_and_load_player,
	add_system_portrayals.


load_system(System) :-
	system_files(System,Files),
	whenever(member(F,Files),compile(library(F))).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Saving system 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% SAVE_METAGAME 
% Defined differently for different prolog versions,
% see the respective files.

enter_metagame :- 
	metagame_version(V),
	format('~nMETAGAME Game-Playing Workbench ~p~n',[V]),
	format('Copyright (c) 1992 Barney D. Pell~n~n',[]),
	unix(argv(Argv)),
	process_commands(Argv),
	metagame.

process_commands(Argv) :-
	( append(_,[seed,N|_],Argv) -> randomize(N) ; true ),
	whenever( 
	      (append(_,[P,V|_],Argv),parameter(P,_)), 
	       set_parameter(P,V) ),
	whenever(append(_,[file,D|_],Argv),
	         compile(D)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% modifying parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_parameter(P,V) :-
	( retract(parameter(P,_)) -> assert(parameter(P,V)) 
        ; otherwise -> format('Unknown parameter <~p>!~n',[P])
        ).

setp(P,V) :- set_parameter(P,V).

% Adds a new parameter P, with initial value V.
add_parameter(P,V) :-
	retractall(parameter(P,_)),
	assert(parameter(P,V)). 

change_parameter(P,Old,V) :-
	( retract(parameter(P,Old)) -> assert(parameter(P,V)) 
        ; otherwise -> format('Unknown parameter <~p>!~n',[P])
        ).


show :- show_parameters.

show_parameters :- 
	listing(parameter),
	getrand(R),
	format('~nrandom seed = ~p~n',[R]).


save_parameters :- 
	findall(P-V,parameter(P,V),Params),
	retractall(saved_parameters(_)),
	assert(saved_parameters(Params)).

restore_parameters :- 
	( retract(saved_parameters(Params)) -> 
	  restore_parameters(Params)
	; true
	).

restore_parameters([]).
restore_parameters([P-V|Rest]) :- 
	set_parameter(P,V),
	restore_parameters(Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% modifying globals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

set_global(P,V) :-
	( retract(global(P,_)) -> assert(global(P,V)) 
        ; otherwise -> format('Unknown global <~p>!~n',[P])
        ).

% Adds a new global P, with initial value V.
add_global(P,V) :-
	retractall(global(P,_)),
	assert(global(P,V)). 

change_global(P,Old,V) :-
	( retract(global(P,Old)) -> assert(global(P,V)) 
        ; otherwise -> format('Unknown global <~p>!~n',[P])
        ).


setg(P,V) :- set_global(P,V). 

showg :- show_globals.

show_globals :- 
	listing(global),
	getrand(R),
	format('~nrandom seed = ~p~n',[R]).


save_globals :- 
	findall(P-V,global(P,V),Params),
	retractall(saved_globals(_)),
	assert(saved_globals(Params)).

restore_globals :- 
	( retract(saved_globals(Params)) -> 
	  restore_globals(Params)
	; true
	).

restore_globals([]).
restore_globals([P-V|Rest]) :- 
	set_global(P,V),
	restore_globals(Rest).



%================================================================================
% Verbosity
%================================================================================
% Some routines call the predicate VERBOSELY(Call) instead of call directly,
% which means only call when tracing mode set to verbose.

verbosely(Call) :- 
	( verbose -> call(Call) ; true ).

% Might cause trouble later when want to use streams also.
verbosely_format(String,Args) :- verbosely(format(String,Args)).

verbose :- parameter(verbosity,X), X > 0.

set_verbose :- set_parameter(verbosity,1).
set_quiet :- set_parameter(verbosity,0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% portrayals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

portray_conj((X,Y)) :- print(X), write(','), print(Y).
portray_disj((X;Y)) :- print(X), write(' ; '), print(Y).
portray_if((X:-Y)) :- write('('), writeq(X), write((:-)), print(Y), write(')').
portray_var('$VAR'(N)) :- put(0'|), write('$VAR'(N)).

% portray square, player, piece, moving, game
% defined in grammar.pl


% Careful, if this is just an array but not a state, could really cause problems.
% Thus, we should check this.  So, we check that a player is in control.
% This could be cleaned up if we wrap a STATE around our states.

portray_state(state(S)) :- 
	format("<State: ~n",[]), print_state(state(S)), 
	format(">~n",[]).

add_portray(Func) :- 
	functor(Goal,Func,1),
	arg(1,Goal,Term),
	assert((portray(Term) :- Goal)).

system_portrayals([portray_conj,portray_disj,portray_if,portray_var,
	portray_piece, portray_player, portray_square, portray_moving, 
	portray_game, portray_state]).

add_system_portrayals :- system_portrayals(Ps), add_portrayals(Ps).

add_portrayals(Ps) :- whenever(member(P,Ps),add_portray(P)).



