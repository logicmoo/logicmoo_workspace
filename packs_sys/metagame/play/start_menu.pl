%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% start_menu.pl

% Top level menu before playing a game. 
% Process commands from user.
% When user has chosen to play a game finally, then starts controller.
% After game has ended, controller releases control back here, and
% we continue!

metagame :- 
	recover_metagame_state,
	menu_command("~nEnter a command ('help.'  gives  more information)~n",
	top,[]), !,
	metagame.
metagame :- 
	format("~nI did not understand your command. Please try again!~n",[]),
	metagame.


% Clean up any errors left from possibly aborted programs. 
recover_metagame_state :- 
	restore_parameters,
	recover_grammar. 

game_top(File) :- load_game(File).

player_top(white,Player) :- 
	format("~p moves will now be chosen by: ~p~n",[white,Player]),
	set_parameter(player_method,Player).
player_top(black,Player) :- 
	set_parameter(opponent_method,Player),
	format("~p moves will now be chosen by: ~p~n",[black,Player]).

players_top(Player,Opp) :- 
	player_top(white,Player),
	player_top(black,Opp).	

players_top :- 
	get_player_color(player,White),
	get_player_color(opponent,Black),
	format("Playing ~p as ~p vs. ~p as ~p.~n",
	       [White,player,Black,opponent]).

	


show_players_top :- get_players(_,_).


games_library_top :-
	games_library.


games_library :-
	games_library_directory(D),
	games_library(D).


games_library(D) :-
	current_directory(Current),
	absolute_file_name(D,AbsD),
	cd(AbsD),
	Games = '*.game',
 format("~nThe following games are available in directory <~p>:~n~n",[AbsD]),
	shell([ls, Games]),
	cd(Current).

games_in_directory(D) :- 
	games_library(D).


cd_top(Dir) :-
	cd_print(Dir).

cd_print(Dir) :-
	format("~nChanging current directory to: ~p~n~n",[Dir]),
	cd(Dir).

pwd_top :- pwd_print.

ls_top :- ls.

pwd_print :-
	current_directory(D),
	format("~nCurrent directory is: ~p~n~n",[D]).


define_top(PieceName) :-
	show_piece_definition(PieceName).

goals_top :-
	show_game_goals.


show_game_goals :-
	player_current_game(G),
	game_name(G,GameName), 
	game_goal(G,Def), 
	set_printing_mode,
	with_alpha_squares(goal_defs(Def,String,[])),
	set_parsing_mode,
	format("~nGame <~p> has the following goals: ~n~n",[GameName]),
	print_tokens(String),
	nl, nl.

rules_top :- show_rules.

show_rules :-
	player_current_game(G),
	game_name(G,GameName), 
	format("~nGame <~p> is defined as follows: ~n~n",[GameName]),
	with_alpha_squares(print_game_struct(G)),
	nl, nl.


pieces_top :- show_piece_names.

board_top :- show_board.

show_board :-
	player_current_game(G),
	game_name(G,GameName), 
	game_board(G,Def), 
	set_printing_mode,
	with_alpha_squares(board(Def,String,[])),
	set_parsing_mode,
	format("~nGame <~p> has the board information: ~n~n",[GameName]),
	print_tokens(String),
	nl, nl.



set_top(P,V) :-
	set_parameter(P,V).

set_top :-
	show_parameters.

% Setting Globals
% (Don't need to document for now) 
setg_top(P,V) :- add_global(P,V).

showg_top :- showg.


randomize_top(N) :- randomize(N).

quit_top :- 
	print_quit.

print_quit :-
	format("~nBye!~n",[]),
	halt.



abort_top :-
	print_abort.

prolog_top :- print_abort.

print_abort :- 
	format("~nTo return to METAGAME, type: 'metagame.'~n",[]),
	abort.

play_top :- start.

start_top :- start.


generate_top(File) :- 
%	random_game_to_file(File).        
	generate_and_load(File).
	
generate_top :- 
%	random_game_to_file(random).
	generate_and_load(random).


%============================================================================
% Game Clock Routines
% -------------------
% Documented in interface.pl
%============================================================================

clock_top :- print_clock.

clock_top(unlimit) :- 
	clock_unlimit.

clock_top(print) :- print_clock.

clock_top(reset) :- reset_clock.

clock_top(adjust,Color,Time) :- 
	player_color(Player,Color),
	adjust_player_clock(Player,Time).

%-----------------------------------------------------------------
% Tracing
%-----------------------------------------------------------------

trace_top(Module) :-
	set_tracing(Module,on).
trace_top(Module,Component) :-
	set_tracing(Module,Component,on).

untrace_top(Module) :-
	set_tracing(Module,off).
untrace_top(Module,Component) :-
	set_tracing(Module,Component,off).

list_tracing_top :- 
	traced_modules(M), 
	format("The following modules are being traced: ~n~p~n",[M]).
