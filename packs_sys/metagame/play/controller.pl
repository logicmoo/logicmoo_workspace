%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% controller.pl


%================================================================================

%========================================
% GET_CURRENT_GAME
%========================================
% External hook, which should (at least)
% ensure that the internal representation of the current game
% to be played is saved in the predicates:
%    player_current_game/1
%    opponent_current_game/1
% This can be achieved by calling the predicate:
%    file_make_test_game/1
% with a file in which a game is stored.  

%========================================
% GET_PLAYERS(-White,-Black)
%========================================
% An external hook, which returns names attached
% to the two players.

%========================================
% GET_RANDOM_ASSIGNMENT(-Assignments)
%========================================
% External hook to determine random assignments
% when necessary.

%================================================================================
% CHOOSE(+Name,+Role,+SIn,-SOut)
%================================================================================
% Contains a clause for each named decision method (or player),
% when playing in a particular role (player or opponent),
% to update the present state by selecting a move. 

%================================================================================
% SHOULD_CONTINUE(+State)
%================================================================================
% External hook to determine whether to continue 
% controlling the game.  
% Should return true if the player wants to continue.

%================================================================================
% TERMINATE_GAME(+FinalState)
%================================================================================
% Hook to call to determine what to do when the game has ended.

%================================================================================


% GET_IF_RANDOM_ASSIGNMENT
% If the game requires a random setup,
% then call the HOOK:  get_random_assignment(A)
% to get an assignment, and record it.
get_if_random_assignment :-
	current_random_setup_game, !,
        get_random_assignment(Assignment),
	set_random_assignment(Assignment).
get_if_random_assignment.

% Used by file parse.pl
set_random_assignment(Assignment) :-
	retractall(random_assignment(_)),
	assert(random_assignment(Assignment)).


%==================
% START_CONTROLLER
%==================
% Decide what game will be played, and who will play each color.
% Then start the controller with those players played their colors.
% This is top level for playing a particular contest
% of a particular game.

start :- start_controller.

start_top(PosName) :- 
	checkpoint(PosName,SIn),
	set_current_pos_name(PosName),
	start_controller(SIn).



start_controller :- 
	get_current_game,
	get_initial_state(SIn),
	initialize_history(SIn),
	start_controller(SIn).  

get_initial_state(SIn) :- 
	get_if_random_assignment,
	start_game(SIn).
	

%==================================
% START_CONTROLLER(SIn)
%==================================
% Resets the clock at the start of each game. 
% Starts the game (initialize board, etc),
% then has the players play it in the main control loop.
% Finds out which game it is playing.
% Gets a random assignment if necessary for that game.
start_controller(SIn) :-
	format("~nInitial Position:~n",[]),
	print_state(SIn),
%        initialize_checkpoints(SIn),
	linebreak,
	reset_clock, % new
	controller(SIn,_), 
	!.


start_game(Init) :- 
	new_state(State),
	start_game(State,Init).


% CONTROLLER(SIn,SOut)
% Either the game ends, or we have the current player
% make a move, and then continue.
% 
% The clock is only printed if tracing(play(clock)).
% To (un)set this, use: '(un)trace play clock'.
controller(SIn,SIn) :-
	game_ends_in_outcome(SIn,Outcome),
	!,
	record_game_outcome(Outcome),
	checkpoint_state(final,SIn).
controller(SIn,SOut) :-
	tracing_play(clock,
	   print_clock),
	play_in_control(SIn,S1),
	should_continue(S1), !,
	cleanup_state(S1,S2),       % new hook
	controller(S2,SOut).
controller(SIn,SIn) :-
	format("The game has been halted prematurely!!",[]).



% PLAY_IN_CONTROL(SIn,SOut)
% Initialize parameters for the player to move,
% and call the appropriate choice method. 
%
play_in_control(SIn,SOut) :-
	control(Role,SIn),
	initialize_player_move(Role),  %
	role_chooser(Role,Chooser),
	choose_or_resign(Chooser,Role,SIn,SOut).

% CHOOSE_OR_RESIGN(Chooser,Role,SIn,-SOut)
%
% Possibly add resignation as a kind of move later.
% For now, failing to choose a legal move is resignation.
% Now uses real-time instead of runtime. This means human
% players get timed correctly also!
choose_or_resign(Chooser,Role,SIn,SOut) :- 
%	realtime_success(choose(Chooser,Role,SIn,SOut),Time), !,
	realtime_success(choose(Chooser,Role,Move,SIn,SOut),Time), !,
	adjust_player_clock(Role,Time),
	add_state_to_history(Move,SOut),
	restore_parameters.
choose_or_resign(Chooser,Role,SIn,_) :- 
	opposite_role(Role,OppRole),
	role_chooser(OppRole,OppChooser),
	format("\c
 ~n~p, as ~p, failed to select a legal move.\c
 ~nThus, ~p, as ~p, is declared the Winner!~n",
 [Chooser,Role,OppChooser,OppRole]),
	checkpoint_state(final,SIn),
	restore_parameters,
	fail.
	

%================================================================================
% SHOULD_CONTINUE
%================================================================================
% A hook to controller.  
% Should return true if the player wants to continue the game.
% Querying is disabled  when parameter CONTINUOUS = yes. 

continuous :- parameter(continuous,yes).

set_continuous :- set_parameter(continuous,yes).
set_stepping :- set_parameter(continuous,no).

should_continue(_SIn) :- continuous, !.
should_continue(_SIn) :- ask_continue(y).

ask_continue(Answer) :- 
	ask_ynp('Continue',Answer1), !,
	Answer1 = Answer.

ask_ynp(Query,Answer) :- 
	format("~a? (y or n)~n",[Query]),
	read(Answer1),
	ynp(Answer1,Answer,ask_ynp(Query,Answer)).

%=============================================================================
% PRE-MOVE INITIALIZATION
% -----------------------
%
% role_chooser(Role,Chooser):  
% Moves for player role Role (player, opponent)
% are chosen by choice method Chooser.  This is set by the 
% "player <color> chooser" command from the interface.
% 
% role_file(Role,File):
% File contains info about pre-move initiliazations for Role.
% File is either <none>, a filename to be loaded, or a list of
% parameters.  This list can be modified using the command template:
% "set <color> <parameter> <value>". 
%

% INITIALIZE_PLAYER_MOVE(Role)
% Either load the initialization file for Player,
% or override to the player-specific parameters.
% If there is no info, do nothing. 
% Before changing parameters, the old version is saved,
% and will be restored at the end of this move. 
% This ensures that no player can modify the parameters
% for the opponent.  
initialize_player_move(Role) :- 
	save_parameters,
	role_file(Role,File),
	( member(File,[none,[]])
	-> true
	; load_player_eval(File)
	).

% Assert a list of parameters only if that's what is defined for
% player_file.  
load_player_eval([P|Ps]) :- !,
	restore_parameters([P|Ps]).
load_player_eval(Name) :- 
	load_player_eval_file(Name).
	

set_com(_,_,_,C,P,V) :-
	set_color_parameter(C,P,V).

set_top(C,P,V) :-
	set_color_parameter(C,P,V).

unset_top(C) :-
	color_player(C,Role),
	clear_player_parameters(Role).

set_color_parameter(Color,P,V) :- 
	color_player(Color,Role),
	set_player_parameter(Role,P,V).

set_player_parameter(Role,Param,Val) :-
	role_file(Role,List1),
	set_assoc(List1,Param,Val,New),
	set_role_file(Role,New).
	
clear_player_parameters(Role) :-
	set_role_file(Role,none).



load_player_eval_file(Name) :- 
	find_eval_file(Name,File),
	save_parameters,
	compile(File).
	
%-------------------------------------------------------------------------
role_chooser(Role,Chooser) :-
	player_method_parameter(Role,Param),
	parameter(Param,Chooser).

player_method_parameter(player,player_method).
player_method_parameter(opponent,opponent_method).

role_file(Role,File) :-
	player_file_parameter(Role,Param),
	parameter(Param,File).

set_role_file(Role,File) :-
	player_file_parameter(Role,Param),
	set_parameter(Param,File).


player_file_parameter(player,player_file).
player_file_parameter(opponent,opponent_file).

% Human mode is operative when one of the players is a human 
% chooser.  This hook determines when questions and confirmations
% should be made from the local console.  Otherwise the controller
% assumes it has the correct information to begin with. 
human_mode :- 
	role_chooser(_Role,human) -> true. 

%============================================================================
% Game Clock Routines
% -------------------
% adjust_player_clock: increments time player has used this game
% reset_clock: resets time used by each player to 0.
% print_clock: prints time used and remaining for each player.
% game_time_left: returns time left for a player in this game.
% time_out_outcome: returns game outcome if timeout for at least one  player.
%============================================================================

% ADJUST_PLAYER_CLOCK(+Player,+Time)
% Adds Time units to players total elapsed time this game.  
% This makes available the predicate:
%
% time_used(?Player,?T) : Player has used T units of time this game.
%
adjust_player_clock(Player,Time) :- 
	retract(time_used(Player,TOld)),
	TNew is TOld + Time,
	assert(time_used(Player,TNew)).

% RESET_CLOCK
% Sets both players' elapsed times to 0 (to be used at the start of each game). 
reset_clock :- 
	reset_player_clock(player),
	reset_player_clock(opponent).

reset_player_clock(Player) :- 
	retractall(time_used(Player,_)),
	assert(time_used(Player,0)).


% PRINT_CLOCK
% Prints time used and left for each player.
print_clock :- 
	format("~*c~n",[40,0'-]),
	format("Clock times (in seconds):~n",[]),
	print_player_clock(player),
	print_player_clock(opponent),
	format("~*c~n",[40,0'-]).

print_player_clock(Player) :- 
	time_used(Player,Used),
	game_time_left(Player,Left),
	format("<~p>: 	~3d used, 	~3d left~n",[Player,Used,Left]).
	
% TIME_OUT_OUTCOME(?Outcome)
% Outcome is DRAW if both players out of time, the player
% who still has time left if only one is out, and fails 
% if both still have time. 
time_out_outcome(Outcome) :- 
	player_time_out(player,PTime),
	player_time_out(opponent,OTime),
	time_out_outcome(PTime,OTime,Outcome).

time_out_outcome(yes,yes,draw) :- !, format("Both players are out of time!~n",[]).
time_out_outcome(no,yes,player) :- !, format("<~p> is out of time~n",[opponent]).
time_out_outcome(yes,no,opponent) :- !, format("<~p> is out of time~n",[player]).


% PLAYER_TIME_OUT(Player,Out)
% 
% A player is out of time if he has no time remaining.
player_time_out(Player,Out) :- 
	game_time_left(Player,Time),
	( Time =< 0
	-> Out = yes
	;  Out = no
	).



% GAME_TIME_LEFT(?Player,-Time)
% Time is the amount of time Player has left in the current game.
game_time_left(Player,Time) :- 
	game_time_limit(Limit),
	time_used(Player,Used),
	Time is max(Limit - Used,0).

game_time_limit(Limit) :- 
	parameter(game_time_limit,Limit).
	
% Nullify time limits for both players.  
clock_unlimit :- 
	set_parameter(game_time_limit,99999999),
	set_parameter(move_time_limit,99999999),
	set_parameter(move_horizon,1),
	reset_clock.


%=============================================================================

game_ends_in_outcome(SIn,Outcome) :- 
	game_over(SIn), !,
	game_outcome(Outcome,SIn).
game_ends_in_outcome(_SIn,Outcome) :- 
	time_out_outcome(Outcome).

% game_over and game_outcome gets compiled to have 2 args, as it 
% thinks the check for no legal moves actually might affect state.

game_over(SIn) :- game_over(SIn,_).

game_outcome(O,S) :- game_outcome(O,S,_).



:- dynamic recorded_game_outcome/1.
%========================================
% RECORDED_GAME_OUTCOME(?Outcome)
%========================================
% The last game ended in Outcome (player, opponent, or draw)
%


% RECORD_GAME_OUTCOME(O)
% Prints the outcome of the game.
% Makes available the predicate:
%        recorded_game_outcome/1.
record_game_outcome(O) :- 
	retractall(recorded_game_outcome(_O)),
	assert(recorded_game_outcome(O)),
	write_outcome(O).


write_outcome(draw) :- format("~n~nThe game is over. Ends in a draw!~n",[]), !.
write_outcome(Player) :- format("~n~nThe game is over.  ~p wins!~n",[Player]).

% cleanup_state(Old,New)
% This provides a hook for different state representations
% to purge temporary data from their state, if necessary. 
cleanup_state(S,S).

