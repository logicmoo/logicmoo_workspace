%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% statify_theory.pl
%%% To transform a domain theory with true, add, and del statements
%%% into one with a threaded StateIn, and StateOut pair.

:- my_ensure_loaded(library(aux)).
:- my_ensure_loaded(library(stat)).

%----------------------------------------
% The following are defined in sysdev.pl

% THEORY_FILES(Files)

% DYNAMIC_PREDS_FILE(File)

%----------------------------------------


% COMPILE_AND_LOAD_PLAYER
% This command does the state compilation on all the player theory files,
% and then loads them.  
% Not nec. to call:  load_compiled_play_theory/0 here, 
% as we compile-load each file immediately after writing it.  
%
compile_and_load_player :-
	state_compile_player.
	

% STATE_COMPILE_PLAYER
% Compile the dynamic pred declarations into a theory file.
% Then index them for efficient state representation.
% Then load all theory files as data, and perform the
% state compilation on the theory.
% Finally,  clear the loaded theory files, as after this 
% we'll only use the compiled prolog versions.
% (Systems which want the theory around after that should load
% it again.)
%
state_compile_player :-
	compile_dynamic_preds,
	index_dynamic_preds,
	player_theory_load,
	player_theory_compile,
	theory_clear.

player_theory_load :- 
	format("Loading Theory: MetaGame~n",[]),
	theory_clear,
	player_theory_files(Files),
	dl(Files).

player_theory_compile :- 
	format("Compiling Theory: MetaGame~n",[]),
	stativity_analysis,
	compile_player_files.

compile_player_files :- 
	theory_files(Files),
	whenever(member(File,Files),
	  thcomp(File)).

player_compiled_files(CompFiles) :- 
	theory_files(Files),
	bagof(CompFile,
	 File^(member(File,Files),
	   theory_statname(File,CompFile)),
	 CompFiles).  

player_theory_files(CompFiles) :- 
	theory_files(Files),
	bagof(CompFile,
	 File^(member(File,Files),
	   theory_filename(File,CompFile)),
	 CompFiles).  



% LOAD_COMPILED_PLAY_THEORY
% Loads all the compiled player theory files.  
% If they have been state_compiled already, and none have changed,
% this is the only call nec. to load them.
% (Thus, someone just using the player need only know this command
% once he has compiled the first time.
%
load_compiled_play_theory :-
	player_compiled_files(Fs),
	whenever(member(F,Fs),
	    compile(F)).


theory_filename(FileRoot,CompFileName) :-
	sys_suffixed_filename(FileRoot,prolog,File),
	theory_directory(TheoryDir),
	concat_list([TheoryDir,'/',File],CompFileName).

theory_statname(FileRoot,CompFileName) :-
	sys_suffixed_filename(FileRoot,state_compile,File),
	theory_directory(TheoryDir),
	concat_list([TheoryDir,'/',File],CompFileName).


thcomp(FileRoot) :- 
	theory_filename(FileRoot,FileIn),
	theory_statname(FileRoot,FileOut),
	state_compile_file(FileIn,FileOut),
	compile(FileOut).

state_compile_file(FileIn,FileOut) :-
	format("Compiling theory file: ~w~n",[FileIn]),
	see(FileIn),
	tell(FileOut),
	compile_clauses, !,  % don't know where nondet is.
	seen,
	told,
	format("Wrote compiled file: ~w~n",[FileOut]).

compile_clauses :-
	read(ClauseIn),
	ClauseIn \== end_of_file 
            -> ( compile_clause(ClauseIn,ClauseOut),
	         portray_clause(ClauseOut),
		compile_clauses)
            ; true.


compile_clause(CIn,COut) :-
	clause_parts(CIn,HIn,BIn),
	thread_clause(HIn,BIn,COut).

thread_clause(HeadIn,BodyIn,ClauseOut) :-
	add_state(HeadIn,SIn,SOut,HeadOut),
	thread(BodyIn,SIn,SOut,BodyOut),
	clause_parts(ClauseOut,HeadOut,BodyOut).


/*
A goal G should be threaded with SIn/SOut if:
a. It is an Add/DEL literal (thread SIn,SOut).
b. It is a True literal (thread SIn).
c. There is an interpreted pred:  G :- Body,
   and the body should be threaded.

A body B1,B2, ... should be threaded if one of its Bi should be.
*/


add_state(GoalIn,Stativity,GoalOut) :- add_state(GoalIn,_SIn,_SOut,Stativity,GoalOut).

add_state(GoalIn,SIn,SOut,GoalOut) :-
	stat(GoalIn,Stat),
	add_state(GoalIn,SIn,SOut,Stat,GoalOut).


%ADD_STATE(Goalin,SIn,SOut,Stativity,GoalOut)
% add_state(foo(a,b),A,B,0,GoalOut) --> GoalOut = foo(a,b).
% add_state(foo(a,b),A,B,1,GoalOut) --> GoalOut = foo(a,b,A).
% add_state(foo(a,b),A,B,2,GoalOut) --> GoalOut = foo(a,b,A,B).
add_state(GoalIn,SIn,SIn,0,GoalIn).
add_state(GoalIn,SIn,SIn,1,GoalOut) :- thread_in_state(GoalIn,SIn,GoalOut).
add_state(GoalIn,SIn,SOut,2,GoalOut) :-
	thread_in_state(GoalIn,SIn,G1),
	thread_in_state(G1,SOut,GoalOut).

thread_in_state(GoalIn,SIn,GoalOut) :-
	increase_term_arity(GoalIn,SIn,GoalOut).



%================================================================================
% Compiling Dynamic Predicates.
%================================================================================

% Declarations of dynamic predicates.
% These are as they appear in the game theory definitions.
%
% Ex.:  move_count(L) ==>  move_count(L,S) :- true_in(move_count(L),S).
%
dynamic_predicates([
	control(_Player),
%	empty(_Square),
	on(_Piece,_Square),
	moved_onto(_Piece,_Sq),
	move_count(_L),
	stage(_Stage),
%	effect(_Effect),
	effects(_Effect,_Captures),
%	captured(_Piece,_Sq),
%	opponent_displaces,
	in_hand(_Piece,_Player),
	opponent_promotes(_OldPiece,_Sq)
	]).


compile_dynamic_preds :- dynamic_preds_file(F), compile_dynamics(F).


compile_dynamics(Dest) :-
	format("~nCompiling Dynamic Predicates to theory file: ~w~n",[Dest]),
	tell(Dest),
	dynamic_predicates(Preds),
	compile_preds(Preds),
	told.

compile_preds([]).
compile_preds([H|T]) :-
	make_state(H),
	compile_preds(T).

make_state(Pred) :-
	statify(Pred,Clause),
	portray_clause(Clause).


statify(PredIn,Clause) :-
	Clause = (PredIn :- 
			true(PredIn)).

