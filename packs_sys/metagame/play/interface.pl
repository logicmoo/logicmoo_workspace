%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% interface.pl

:- my_ensure_loaded(library(shells)).
:- my_ensure_loaded(library(menus)).



% ASK_MOVE(-Move,+SIn,-SOut)
% Top level menu.  Process moves and commands from user.
% Loops until the user has chosen a legal move.
ask_move(Move,SIn,SOut) :-
	format("~nEnter a move or command (help  gives  more information)~n",[]),
	read_keyboard_tokens(String),
	process_move_or_command(String,Move,SIn,S1), !,
	really_get_move(Move,SIn,S1,SOut).
ask_move(Move,SIn,SOut) :-
	control(Player,SIn),
	format("~n~p failed to select a legal move. Please try again!~n",[Player]),
	format("~n ('display.' to redisplay board)~n",[]),
	ask_move(Move,SIn,SOut).


% REALLY_GET_MOVE(?Move,+SIn,+S1,-SOut).
% Ensure that a move has been chosen, else back to the menu.
% If Move and S1 are bound, we accept S1 as the result.
% If only S1 is bound, we accept this as a new current state,
% and use it to get the final move.
% If neither, then some other command didn't do any work, 
% and we start again from our original state to get the move.
%
really_get_move(Move,_SIn,S1,S1) :- nonvar(Move), nonvar(S1), !.
really_get_move(Move,_SIn,S1,SOut) :- nonvar(S1), !,
	ask_move(Move,S1,SOut).	
really_get_move(Move,SIn,_S1,SOut) :- ask_move(Move,SIn,SOut).


% PROCESS_MOVE_OR_COMMAND(MoveString,Move,SIn,SOut)
% If the string is a legal command for this menu, call it.
% If it is a complete move notation, offer to select that move.
% If it is an incomplete move notation, complete it and offer it
%   (only when COMPLETETIONS parameter is ON.
%
process_move_or_command(MoveString,Move,SIn,SOut) :-
	process_command(MoveString,'com',[Move,SIn,SOut]).
process_move_or_command(MoveString,Move,SIn,SOut) :-
	nl,
	set_parsing_mode,
	move_notation(Move,MoveString), !,
	verbosely_format("~nTrying Move: ~p~n",[Move]),
	select_move(Move,SIn,SOut).
process_move_or_command(MoveString,Move,SIn,SOut) :-
	format("Attempting to find a completion for move: ",[]),
	print_tokens(MoveString), nl,
	parameter(completions,on),
	choose_completed_move(MoveString,Move,SIn,SOut).



% SELECT_MOVE(Move,SIn,SOut)
% Backtrack over possible legal moves until one is selected.
select_move(Move,SIn,SOut) :-
%	format("~nBefore:~n",[]),
%	print_state(SIn),
	legal(Move,SIn,SOut),
	format("~nAfter:~n",[]),
	print_state(SOut),
	print_notation(Move),
	ask_accept_choice(Answer),
	( ( Answer = abort -> !, fail )
          ; Answer = yes ).


% PRINTING_CHOOSE(Method,Move,SIn,SOut)
% Method is the functor of a procedure which  generates moves on backtracking.
% Here we call this procedure, timing it, and offer the user the generated moves,
% in order.
%
% Ex:      random_move(Move,SIn,SOut)   generates random moves
% Call:    printing_choose(random_move,Move,SIn,SOut)
% 
printing_choose(Method,Move,SIn,SOut) :-
	Chooser =.. [Method,Move,SIn,SOut],
	selecting_choice(
             (timing(Chooser),
	      format("~nAfter:~n",[]),
	      print_state(SOut),
	      print_notation(Move)
	     )).

% RANDOM_PRINTING_CHOOSE(Method,Move,SIn,SOut)
% Like printing_choose, but offers user only a randomly chosen
% generated move.
random_printing_choose(Method,Move,SIn,SOut) :-
	Chooser =.. [Method,Move,SIn,SOut],
	selecting_choice(
             (timing(random_success(Chooser)),
	      format("~nAfter:~n",[]),
	      print_state(SOut),
	      print_notation(Move)
	     )).


%===========================================================================
% Move Completion
%===========================================================================

% COMPATIBLE(Op,Tokens)
% (Described in help menu)
% Note that in the human-friendly abbreviated move notation,
% only the (letter,number) = (column,row) notation is used for 
% denoting squares.  
% When the parameter SAFETY is ON, this will only complete to 
% moves which do not immediately lose for the moving player.
% (So if you can not make any moves but you aren't stalemated, you might
% have to turn safety off to make your last losing move ...)

compatible(Op,Tokens) :-
	with_alpha_squares(
	  ( move_notation(Op,FullToks),
	    mesh(Completion,Tokens,FullToks)
	  )),
	verbosely_format("~nTokens: ~w\c
	     ~nCompletion: ~w\c
	     ~nFull: ~w\c
	     ~n",[Tokens,Completion,FullToks]). 


completed_move(Tokens,Move,SIn,SOut) :-
	legal(Move,SIn,SOut),
	compatible(Move,Tokens),
	acceptable(Move,SIn,SOut).

acceptable(Move,SIn,SOut) :- 
	( parameter(safety,on) 
	-> check_safe_move(Move,SIn,SOut)
	; true).


choose_completed_move(Tokens,Move,SIn,SOut) :-
	Chooser = completed_move(Tokens,Move,SIn,SOut),
	selecting_choice(
             (timing(Chooser),
	      format("~nAfter:~n",[]),
	      print_choice(Move,SIn,SOut)
	     )).


% Added a flush so we always have updated record when
% writing to files. 
% Can set printing of states and moves with
% '(un)trace play state' and '(un)trace play move'.
%
print_choice(Move,_SIn,SOut) :- 
	current_output(O),
	flush_output(O),
	tracing_play(state,
	    print_state(SOut)),
	tracing_play(move,
	    print_notation(Move)),
	linebreak.

linebreak :- format("
===============================================================
",[]).

print_notation(Op) :-
	set_printing_mode,
	with_alpha_squares(move_notation(Op,Notate)), !,
	format("Notated Move played: ~n",[]),
	print_tokens(Notate),
	nl,nl,
	verbosely_format("~n~nFull Move played: ~p~n",[Op]).
print_notation(Op) :-
	format("Move played: ~p~n",[Op]).


print_move(Move) :-
	set_printing_mode,
	with_alpha_squares(move_notation(Move,Notate)), !,
	print_tokens(Notate), nl.
print_move(Move) :-
	format("Move: ~p~n",[Move]).


%======================================================================
% Offering and Accepting Choices
%======================================================================

% SELECTING_CHOICE(+Goal)
% Backtrack over possible choices until one is selected,
% or user wishes to abort. 
selecting_choice(Goal) :-
	call(Goal),
	ask_accept_choice(Answer),
	( ( Answer = abort -> !, fail )
          ; Answer = yes, ! ).



ask_accept_choice(yes) :- parameter(confirm_choices,off), !.
ask_accept_choice(Answer) :-
	menu_command("~nAccept this choice? (yes, next, abort, help)~n",
	   accept,[Answer]), !.
ask_accept_choice(Answer) :-
	format("~n~p failed to answer appropriately. Please try again!~n",['You']),
	ask_accept_choice(Answer).

%----------------------------------------
% Accept menu commands
%----------------------------------------

yes_accept(yes) :- format("~nChoice accepted.~n",[]).
y_accept(yes) :- yes_accept(yes).

next_accept(next) :- format("~nTrying next choice.~n",[]).
no_accept(next) :- next_accept(next).
n_accept(next) :- next_accept(next).


abort_accept(abort) :-
	format("~nAttempt aborted!~n",[]).

help_accept(Answer) :-
	help_accept,
	ask_accept_choice(Answer).


%===============================================================================
% Top level (_com) Menu	
%===============================================================================

%----------------------------------------
% Help from move menu
%----------------------------------------

% See file help.pl


%----------------------------------------
% Options from move menu
%----------------------------------------

select_com(Move,SIn,SOut) :- select_move(Move,SIn,SOut).

display_com(_Move,SIn,_) :- 
	format("~nCurrent State:  ~n",[]),
	print_state(SIn).

restart_com(_,_,_) :- 
	format("~nRestarting ...~n",[]),
	metagame.

quit_com(_,_,_) :- 
	print_quit.


prolog_com(_,_,_) :- print_abort.

abort_com(_,_,_) :- 
	print_abort.

verbose_com(_,_,_) :-
	set_verbose.

quiet_com(_,_,_) :-
	set_quiet.


break_com(_,_,_) :-
	format("~nBreak command not implemented.",[]).

set_com(_,_,_,P,V) :-
	set_parameter(P,V).

set_com(_,_,_) :-
	show_parameters.

% Setting globals.

setg_com(_,_,_,P,V) :- add_global(P,V).

showg_com(_,_,_) :- showg.


randomize_com(_,_,_,N) :- randomize(N).

pieces_com(_,_,_) :- 
	show_piece_names.

show_piece_names :-
	player_current_game(Game),
	game_piece_names(Game,Names),
	format("
The current game has the following pieces: 
~p
Note that pieces are displayed on the board by the their first letter. 
",[Names]).

game_piece_names(Game,Names) :- 
	setof(Name,game_piece_name(Game,Name),Names).

define_com(_,_,_,PieceName) :-
	show_piece_definition(PieceName).

show_piece_definition(PieceName) :-
	player_current_game(G),
	game_piece_def(G,PieceName,Def), !,
	set_printing_mode,
	piece_def(Def,String,[]),
	set_parsing_mode,
	format("~nPiece <~p> is defined as follows: ~n~n",[PieceName]),
	print_tokens(String),
	nl, nl.
show_piece_definition(PieceName) :-
	format("~nSorry, Piece <~p> is not defined in this game.~n~n",[PieceName]).


rules_com(_,_,_) :- 
	show_rules.

board_com(_,_,_) :- 
	show_board.

goals_com(_,_,_) :-
	show_game_goals.



cd_com(_,_,_,Dir) :- 
	cd_print(Dir).


pwd_com(_,_,_) :- pwd_print.

ls_com(_,_,_) :- ls.

%-----------------------------------------------------------
% Accessing state from the interface
%-----------------------------------------------------------

% ACCESS
access_com(_Move,SIn,SOut) :-
	format("
Enter a goal of the form:
	SIn^SOut^goal(...,SIn,...,SOut).
Calling this goal will bind the current state to SOut,
and return you to the move selection menu.
To accept the current state as your move, use the command: accept.

",[]),
	read(SIn^SOut^Goal),
	call(Goal).

% CALL
% Could be merged with ACCESS.
call_com(_,SIn,SOut) :- 
	get_state_goal(Goal),
	call_state_goal(Goal,SIn,SOut).

get_state_goal(Goal) :-
	format("
Entering a goal of the form:
	goal(a,b,...)
Will result in the following 'statified' goal being called:
	goal(a,b,...,SIn,SOut)
If successful, this goal will bind the current state to SOut,
and return you to the move selection menu.
To accept the current state as your move, use the command: accept.

",[]),
	read(Goal).

call_state_goal(Goal,SIn,SOut) :-
	Goal=..[H|Args],
	append(Args,[SIn,SOut],NewArgs),
	SGoal=..[H|NewArgs],
	( current_predicate(_,SGoal) ->
	  call(SGoal)
	; format("*** Unknown_goal: ~w~n",[SGoal]),
	  fail
	).



% RESTORE <name>
% Set current state to be a previously named checkpoint state.
restore_com(_,_SIn,SOut,Name) :-
	restore_state(Name,SOut).
	
	
% ACCEPT
accept_com(accept,SIn,SIn) :- 
	format("Current state accepted as chosen move.~n",[]).

% CHECKPOINT
checkpoint_com(_,SIn,_) :- 
	checkpoint_state(SIn).

% CHECKPOINT <NAME>
checkpoint_com(_,SIn,_,Name) :- 
	checkpoint_state(Name,SIn).


%-----------------------------------------------------------------
% Queries
%-----------------------------------------------------------------

%       query goal (Player),
% Check if a goal has been achieved (ie the game is over here)
%	query mobility (Player)
% Counts mobility for both (or just Player)
%        query material (Player)
% Counts material for both (or just Player)

query_com(_,SIn,_,goal) :-
	query_goal(SIn).

query_com(_,SIn,_,material) :-
	query_material(SIn).
query_com(_,SIn,_,mobility) :-
	query_mobility(SIn).

query_com(_,SIn,_,goal,Player) :-
	query_goal(SIn,Player).
query_com(_,SIn,_,material,Player) :-
	query_material(SIn,Player).
query_com(_,SIn,_,mobility,Player) :-
	query_mobility(SIn,Player).


query_goal(SIn) :-
	format("Checking goals ...~n",[]),
	( timing((game_outcome(O,SIn))) ->
	  format("Outcome is ~p~n",[O])
        ; format("Game is not over yet.~n",[])
	).


query_goal(SIn,Player) :-
	format("Checking goals for <~p> ...~n",[Player]),
	( timing((goal_achieved(Player,SIn))) ->
	  format("<~p> has achieved a goal!~n",[Player])
	; format("<~p> has not achieved a goal.~n",[Player])
	).

% Dummy.
goal_achieved(P,S) :- goal_achieved(P,_,S,_).

query_goal_slow_com(_,SIn,_) :-
	query_goal_slow(SIn).
query_goal_slow(SIn) :-
	format("Checking goals ...~n",[]),
	( timing((game_outcome(player,SIn))) ->
	  format("<~p> has achieved a goal!~n",[player])
        ; format("Player has not won yet.~n",[])
	).


query_material(SIn) :-
	query_material(SIn,player),
	query_material(SIn,opponent).

query_material(S,Player) :- 
	format("Counting material for <~p> ...~n",[Player]),
	timing((material(Player,S,Mat))),
	format("<~p> has material count of ~p~n",[Player,Mat]).


query_mobility(SIn) :-
	query_mobility(SIn,player),
	query_mobility(SIn,opponent).

query_mobility(S,Player) :- 
	format("Counting mobility for <~p> ...~n",[Player]),
	timing((mobility(Player,S,Mat))),
	format("<~p> has mobility count of ~p~n",[Player,Mat]).


%-----------------------------------------------------------------
% Tracing
%-----------------------------------------------------------------

trace_com(_,_,_,Module) :-
	set_tracing(Module,on).
trace_com(_,_,_,Module,Component) :-
	set_tracing(Module,Component,on).

untrace_com(_,_,_,Module) :-
	set_tracing(Module,off).
untrace_com(_,_,_,Module,Component) :-
	set_tracing(Module,Component,off).

list_tracing_com(_,_,_) :- 
	list_tracing.

list_tracing :- 
	traced_modules(M), 
	format("The following modules are being traced: ~n~p~n",[M]).

%----------------------------------------
% Special Move selection methods (Advisors)
%----------------------------------------
% The individual move methods are defined in file advisors.pl

pass_com(pass,SIn,SOut) :-
	pass_move(SIn,SOut).


random_com(Move,SIn,SOut) :- 
	printing_choose(random_move,Move,SIn,SOut).


random1_com(Move,SIn,SOut) :- 
	random_printing_choose(legal,Move,SIn,SOut).


instant_com(Move,SIn,SOut) :-
	printing_choose(instant_move,Move,SIn,SOut).


victor_com(Move,SIn,SOut) :- 
	printing_choose(victor_move,Move,SIn,SOut).


endgame_com(Move,SIn,SOut) :- 
	timing(endgame_move(Move,SIn,SOut)),
	select_move(Move,SIn,SOut).


cautious_com(Move,SIn,SOut) :- 
	printing_choose(cautious_move,Move,SIn,SOut).

random_aggressive_com(Move,SIn,SOut) :- 
	printing_choose(random_aggressive_move,Move,SIn,SOut).


mate_com(Move,SIn,SOut) :- 
	timing(mate_move(Move,SIn,SOut)),
	select_move(Move,SIn,SOut).

threaten_com(Move,SIn,SOut) :- 
	timing(threaten_move(Move,SIn,SOut)),
	select_move(Move,SIn,SOut).

enough_rope_com(Move,SIn,SOut) :- 
	timing(enough_rope_move(Move,SIn,SOut)),
	select_move(Move,SIn,SOut).


help_clock :- format("
============================================================================
Game Clock Routines
-------------------
clock adjust <color> <msec> : increments time player has used this game
clock reset: resets time used by each player to 0.
clock print: prints time used and remaining for each player.
clock unlimit: disables the clock by giving both players unlimited time.

clock: short form for clock print. 

Example:
     clock adjust white 2000.     : adds 2 secs to time white used.
     clock adjust black '-2500'.  : subtracts 2.5 secs from time black used.
============================================================================
",[]).

clock_com(_,_,_,adjust,Color,Time) :- 
	player_color(Player,Color),
	adjust_player_clock(Player,Time).

clock_com(_,_,_) :- print_clock.
clock_com(_,_,_,print) :- print_clock.

clock_com(_,_,_,reset) :- reset_clock.

clock_com(_,_,_,unlimit) :- 
	clock_unlimit.


%================================================================================
% TRACING execution of analysis routines
%================================================================================

% This main tracing module is called:  play.
% The following tracing modules are used in this file:
%	state:  print state as moves are chosen by players.
%       move:   print move as moves are chosen by players. 
%       clock:  print the clock as moves are played. 
% Each module can be set on/off, using set_play_verbosity (see below), or 
% using trace_play_<module>. 
%
% All can be turned off with silent_play.

:- my_ensure_loaded(library(tracing)).

tracing_play(Type,Call) :- 
	( tracing(play(Type)) -> call(Call) ; true ).

% Might cause trouble later when want to use streams also.
tracing_play_format(Type,String,Args) :- 
	( tracing(play(Type))
	-> format(String,Args)
	; true 
	).

tracing_play_timing(Type,Call) :- 
	trace_timing(play(Type),Call).

set_play_verbosity(Level,Status) :- set_tracing(play(Level),Status).

silent_play :- all_play(off).
loud_play :- all_play(on).

all_play(Status) :- 
	set_play_verbosity(state,Status), 
	set_play_verbosity(move,Status), 
	set_play_verbosity(clock,Status). 

trace_play_state :- set_play_verbosity(state,on). 
trace_play_move :- set_play_verbosity(move,on). 
trace_play_clock :- set_play_verbosity(clock,on). 

:- loud_play.
%:- silent_play.


