%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% history.pl
%%%
%%% Interface routines:
%%% -------------------
%%% From top menu:
%%% start <PosName>:  Start game in pos checkpointed as <PosName>
%%% save <F>: Save record of current game as <F>.rec
%%% load <F>: Load record of game <F>.rec 
%%%
%%% From move menu: 
%%% next <N>:  Advance <N> positions in current game history.
%%% prev <N>:  Regress <N> positions in current game history.
%%% next: next 1.
%%% prev: prev 1.
%%% clear history: Delete *all* history entries and saved positions.

help_history :- 
	help_history_overview,
	help_history_top,
	help_history_com.

help_history_overview :- 
	format("
Game History Management
-----------------------
These routines allow you to review the history of the current
game. To use them, you must set the white player to be human,
otherwise the program will begin playing from the chosen position
and there will be no chance to review the past game.  

In addition, see the menu: EXAMINING AND MODIFYING STATE OF GAME,
which allows you to save and restore important positions even when in
the middle of a game.  Note however that these positions will not
necessarily be hooked into the history mechanisms discussed here. 
",[]).

help_history_top :- 
	format("
From top menu:
save <F>: Save record of current game as <F>.rec
load <F>: Load record of game <F>.rec as current history.  
start <PosName>:  Start game in pos checkpointed as <PosName>
",[]).

help_history_com :- 
	format("
From move menu: 
next <N>:  Advance <N> positions in current game history.
prev <N>:  Regress <N> positions in current game history.
next: next 1.
prev: prev 1.
clear history: Delete *all* history entries and saved positions.

Note the playing a new move erases whatever moves may have come after 
in the present game record. 
",[]).

next_com(_M,_SIn,SOut) :- 
	forward_hist(1,SOut).

next_com(_M,_SIn,SOut,N) :- 
	forward_hist(N,SOut).

prev_com(_M,_SIn,SOut) :- 
	reverse_hist(1,SOut).

prev_com(_M,_SIn,SOut,N) :- 
	reverse_hist(N,SOut).

clear_com(_,_,_,history) :- clear_history.

% Saving and loading games

save_top(Game) :- print_game_record_to_file(Game).

save_com(_,_,_,Game) :- print_game_record_to_file(Game).

load_top(Game) :- read_game_record_from_file(Game).

load_com(_,_,_,Game) :- read_game_record_from_file(Game).


%================================================================================

:- dynamic follows/3, current_pos_name/1.


forward_hist(N,SOut) :-
	current_pos_name(NameIn),
	forward_name(N,NameIn,NameOut),
	checkpoint(NameOut,SOut),
	set_current_pos_name(NameOut),
	print_state(SOut),
	last_move(LastMove),
	print_notation(LastMove).

reverse_hist(N,SOut) :-
	current_pos_name(NameIn),
	forward_name(N,NameOut,NameIn),
	checkpoint(NameOut,SOut),
	set_current_pos_name(NameOut),
	print_state(SOut),
	last_move(LastMove),
	print_notation(LastMove).


forward_name(0,NameIn,NameIn).
forward_name(N,NameIn,NameOut) :-
	N>0,
	N1 is N-1,
	follows_history(NameIn,Name1),
	forward_name(N1,Name1,NameOut).

follows_history(N1,N2) :- follows(N1,_,N2).	
	
follows_history(N1,Move,N2) :- follows(N1,Move,N2).	


initialize_history(SIn) :- 
	checkpoint_state(init,SIn),
	set_current_pos_name(init).


% CURRENT_POSITION(-Pos)
% Pos is the current position in the history. 
current_position(Pos) :- 
	current_pos_name(Name),
	checkpoint(Name,Pos).

% LAST_MOVE(-Move)
% Returns the internal representation (if any) of the move made prior
% to this one in the current position.
last_move(Move) :- 
	current_pos_name(Name),
	follows_history(_,Move,Name).


set_current_pos_name(Name) :- 
	retractall(current_pos_name(_)),
	assert(current_pos_name(Name)).

change_current_pos_name(Name,Next) :- 
	retract(current_pos_name(Name)),
	assert(current_pos_name(Next)).


set_follows_history(Name,Move,Next) :- 
	clear_history_after(Name),
	assert(follows(Name,Move,Next)).

add_state_to_history(Move,State) :- 
	checkpoint_state_gensym(State,Next),
	update_history(Move,Next).

update_history(Move,Next) :-
	change_current_pos_name(Name,Next),
	set_follows_history(Name,Move,Next).
	
clear_history :- 
	retractall(checkpoint(_,_)),
	retractall(follows(_,_,_)).

clear_history_visible :- 
	( clear_history_after(_Name), fail
	; true).

clear_history_after(Name) :- 
	( retract(follows(Name,_,Next))
	-> retract(checkpoint(Next,_)),
	   clear_history_after(Next)
	; true
	).
	
% Unifies State with each state occuring in the current game
% order. 
history_state(State) :- 
	checkpoint(init,State).
history_state(State) :- 
	follows_history(_Name1,Name2),
	checkpoint(Name2,State).

restore_state(N,State) :- 
	( current_predicate(checkpoint,_),
	  checkpoint(N,State)
	-> checkpoint(N,State),
	   format("~nState named: <~w> now current state~n",[N])
	;  format("~nError: No state: <~w> has been is checkpointed~n",[N])
	).
	
	
checkpoint_state(State) :-
	checkpoint_state_gensym(State,_Name).

checkpoint_state_gensym(State,Name) :-
	gensym(checkpoint,Name),
	checkpoint_state(Name,State).
	
checkpoint_state(Name,State) :-
	retractall(checkpoint(Name,_)),
	assert(checkpoint(Name,State)),
	format("~nState checkpointed under index: <~w>~n",[Name]).


%================================================================================
% Saving and loading game records
%================================================================================
% Game Record file format:  
% A saved game record consists of a sequence of grammatical move descriptions,
% POSSIBLY ABBREVIATED (so long as they are unambiguous).  This format is
% just what would have been read from a keyboard to enter the moves from the 
% human interface. 
% The file may also contain arbitrary comments, prefaced as usual by '%'. 
% The ability to abbreviate game records makes this format extremely flexible. 
%
% One caution: The program replays the game in order to save or load the file,
% thus if move generation for a given game is slow this takes noticeable time.    


% If MoveString is instantiated, will backtrack over moves
% in the game which are completions of MoveString. 
move_string(Move,MoveString) :-
	follows_history(Name1,Name2),
	checkpoint(Name1,State1),
	checkpoint(Name2,State2),
	( completed_move(MoveString,Move,State1,State2) -> true).

print_next_move(MoveString) :-
	move_string(_Move,MoveString),
	print_tokens(MoveString), nl.


read_next_move(MoveString) :-
	read_keyboard_tokens(MoveString),
	current_position(State1),
	( completed_move(MoveString,Move,State1,State2) -> true),	
	add_state_to_history(Move,State2).

print_game_record :- 
	( print_next_move(_),
	  fail
	; true
	). 

get_initialize_history(In) :- 
	get_initial_state(In),
	initialize_history(In).


read_game_record :- 
	get_initialize_history(_In),
	read_record_moves.

read_record_moves :- 
	read_next_move(_), !,
	read_record_moves.
read_record_moves.



% PRINT_GAME_RECORD_TO_FILE(+File)
% Outputs a game to file File.rec.
print_game_record_to_file(File) :-
	sys_suffixed_filename(File,record,GameFile),
	format("~nWriting game record to file: ~w~n",[GameFile]),
	with_output_file(GameFile,append,
	   print_game_record).


% PRINT_GAME_RECORD_TO_FILE(+File)
% Outputs a game to file File.rec.
read_game_record_from_file(File) :-
	sys_suffixed_filename(File,record,GameFile),
	format("~nReading game record from file: ~w~n",[GameFile]),
	see(GameFile),
%	write_old_seed,
	read_game_record,
	seen.


