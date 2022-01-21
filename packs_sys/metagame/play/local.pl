%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% local.pl
%%% Control hooks for using the console to play two humans or programs against
%%% each other locally (i.e. without using internet communication).
%%%
%%% This file contains the parts of the interface which are specific to this
%%% mode of play only.  Thus, for a local match, we find the players and the game
%%% by querying the user, whereas in a remote match, each player receives this 
%%% info from the referee, and the referee sends it. 


%========================================
% GET_PLAYERS(White,Black)
%========================================

% If already have a game to play, use it.
% Else query user.

get_players(White,Black) :-
	get_player_color(player,White),
	get_player_color(opponent,Black),
	query_accept_players(White,Black), !.
get_players(_,_) :-
	format("
Please use 'player' to select the players.
'help player' gives more info, and a list of available players.
",[]),
fail.


get_player_color(Color,Player) :-
	player_method_parameter(Color,Param),
	parameter(Param,Player).


query_accept_players(White,Black) :-
	human_mode, !,
	format("
~p as ~p vs. ~p as ~p?
(y to accept, n to select new players): ",
	[White,player,Black,opponent]),
	read(Answer),
	Answer = y.
query_accept_players(White,Black) :- 
	format("Playing ~p as ~p vs. ~p as ~p?",
	       [White,player,Black,opponent]).


%========================================
% GET_CURRENT_GAME
%========================================
% Provides external hook, storing the internal representation 
% of the current game (after choosing it) 
% in the predicates:
%    player_current_game/1
%    opponent_current_game/1

% If already have a game to play, use it.
% Else query user.
get_current_game :-
	player_current_game(Game), 
	query_use_current_game(Game), !.
get_current_game:-
	format("
Please use 'game' to select a game.
'games_library' will show you a list of available games.
'generate <file> will generate a new game as <file>.game.
",[]),
fail.

% File saved as [path/]GameName.game
query_use_current_game(Game) :-
	human_mode, !,
	game_name(Game,Name),
	format("~nPlay game ~w? (y to play, n to choose another): ",[Name]),
	read(Answer),
	Answer = y.
query_use_current_game(Game) :- 
	game_name(Game,Name),
	format("~nPlaying game ~w",[Name]).


%========================================
% GET_RANDOM_ASSIGNMENT(Assignments)
%========================================
% By default, generates a random assignment.
% If assignment_method(ask), then queries user.

get_random_assignment(Assignment) :-
	game_assignments(_Game,As),
	assignment_decision(As,random,PieceNames,Squares),
	produce_assignment(PieceNames,Squares,Assignment).


produce_assignment(PieceNames,Squares,Assignment) :-
	parameter(assignment_method,ask), !,
	ask_random_assignment(PieceNames,Squares,Assignment).
produce_assignment(PieceNames,Squares,Assignment) :-
	generate_random_assignment(PieceNames,Squares,Assignment).
	

generate_random_assignment(PieceNames,Squares,Assignment) :-
	format("~nGenerating Random Assignment~n",[]),
	assign_pieces_to_squares(PieceNames,Squares,Assignment).  % defined in generator

% The ['.'|Rest] is because the assignment_defs doesn't end in a period,
% while the end of string requires it.  
% 
ask_random_assignment(PieceNames,Squares,Assignment) :-
	format("~nPlease assign ~w to ~w\c
	 ~nExample syntax: piece1 at {(1,1)} piece2 at {(2,2) (3,3)}.\c
	 ~n(C-d on new line to end)\c
	 ~n|: ",[PieceNames,Squares]),
	read_tokens(AssignmentString),
	nl,
	set_parsing_mode,
	assignments(Assignment,AssignmentString,['.'|_Rest]).

ask_random_assignment(PieceNames,Squares,Assignment) :-
	format("~nPlease assign ~w to ~w\c
	 ~n('.' for help)\c
	 ~n|: ",[PieceNames,Squares]),
	read_keyboard_tokens(AssignmentString),
	help_or_process_assign(AssignmentString,PieceNames,Squares,Assignment).

help_or_process_assign(['.'],PieceNames,Squares,Assignment) :-
	format("~nExample syntax:\c
	 ~npiece1 at {(1,1)} piece2 at {(2,2) (3,3)}.\c
	 ~n",[]),
	 ask_random_assignment(PieceNames,Squares,Assignment).

help_or_process_assign(AssignmentString,_,_,Assignment) :-
	nl,
	set_parsing_mode,
	assignments(Assignment,AssignmentString,['.'|_Rest]).

      
/*

%================================================================================
% TERMINATE_GAME(SIn)
%================================================================================
% Saves final state for debugging purposes.
% May not want to print checkpoint info every time.
terminate_game(SIn) :-
	ask_ynp('Would you like to play another game',Answer), !,
	play_again_if(Answer).

play_again_if(y) :- !, metagame.
play_again_if(_).
*/


%================================================================================
% Move Selection methods
%================================================================================

% CHOOSE(Chooser,Role,Move,SIn,SOut)
% CHOOSER is an atom.
% Calls the procedure CHOOSER_choose(Role,Move,SIn,SOut).
%
% Currently define choice methods are:
%   HUMAN
%   INSTANT
%   RANDOM
%   THREATEN
%   CAUTIOUS
%   QUICK_CAUTIOUS
%   VARIABLE
%   SMARTER
%   RANDOM_AGGRESSIVE

choose(Name,Role,Move,SIn,SOut) :-
	concat(Name,'_choose',NameChoose),
	Goal =.. [NameChoose,Role,Move,SIn,SOut],
	call(Goal).


variable_choose(Role,Move,SIn,SOut) :-
	player_method_parameter(Role,Param),
	parameter(Param,Chooser),
	choose(Chooser,Role,Move,SIn,SOut).

