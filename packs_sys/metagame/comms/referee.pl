%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% referee.pl
%%%
	
:- prolog_flag(redefine_warnings,_,off).

%================================================================================
% REFEREE
%================================================================================
%
% REFEREE prolog process will be called with command like:
%	start_sicstus_shell(any,
%	  [server_name, ServerAddr,
%	   server_port, Port,
%	   player1_name, Player1,
%	   player2_name, Player2,
%	   file,'~/prolog/play/referee']).

% Redefine metagame here, so we don't give menu and instead just start referee.
metagame :- start_ref.

start_ref :- 
	find_server(Server,Port),
	find_players(Player1,Player2),
	referee(Server,Port,Player1,Player2).

find_server(S,P) :-
	parameter(server_name,S),
	parameter(server_port,P).


find_players(Player1,Player2) :-
	parameter(player1_name,Player1),
	parameter(player2_name,Player2).


	
referee(ServerAddr,Port,Player1,Player2) :-
	use_module(library('linda/client')),
	port_number(Port,PortNum),
	linda_client_wait(ServerAddr:PortNum),
	greet_players(Player1,Player2),
	format("~nI'm your referee!~n",[]),
	assert(players(Player1,Player2)),
	start_controller.

greet_players(P1,P2) :- 
	format("~nHello, player1: ~w, player2: ~w~n",[P1,P2]).


%================================================================================
	

% Play a number of contests between these
% players, for a given game definition.

referee_game_contests(_P1,_P2,_G,0) :- !,
	record_statistics.
referee_game_contests(P1,P2,G,Contests) :-
	Contests > 0, 
	referee_contest(P1,P2,G),
	C1 is Contests - 1,
	referee_game_contests(P1,P2,G,C1).
	

% For now, shut off everything.  Eventually, can play
% role in more complicated tournament.

close_match(P1,P2) :-
	close_players(P1,P2),
	close_server,
	record_statistics,
	close_client.
	
close_server :-
	linda:linda_call(halt).


%===========================================================================
% CALL_FOR_PLAYERS(Player^Call)
%===========================================================================
% Call is a goal, possibly requiring a variable Player.
% The goal will be called, instantiated in turn for both players.

call_for_players(PlayerCall) :-
	players(Player1,Player2),
	call_for_players(Player1,Player2,PlayerCall).

call_for_players(Player1,Player2,PlayerCall) :-
	call_for_player(Player1,PlayerCall),
	call_for_player(Player2,PlayerCall).

call_for_player(Name,Player^Call) :-
	verify((Player=Name,call(Call))).

% OUT_TO_PLAYERS(Pattern)
% Send the same message to both players, personally addressed.
out_to_players(Pattern) :-
	call_for_players(P^out_personal(P,Pattern)).


% Sends a personal message, of the form:
% message(Player,Pattern)
out_personal(Player,Pattern) :-
	out1(message(Player,Pattern)).

out1(Pattern) :-
	format("~nSending pattern: ~w~n",[Pattern]),
	out(Pattern).


%================================================================================
% Hooks to standard controller script
%================================================================================

%========================================
% GET_PLAYERS(White,Black)
%========================================
get_players(White,Black) :-
	next_players(White,Black),
	set_players(White,Black).


% NEXT_PLAYERS(White,Black)
% Could be determined from an external source, like a tournament
% director, based on the current results of the tournament.
% For now, just alternate colors each game. 
% players(White,Black) is initially saved upon entry above. 
next_players(White,Black) :-
	retract(players(Black,White)),
	assert(players(White,Black)).
	

% Sends to each player the information about the roles
% both players are playing.
set_players(White,Black) :-
	out_to_players(players(White,Black)).


%========================================
% GET_CURRENT_GAME
%========================================

% Provides external hook.
% Selects next game, and loads it as the current game. 
% Then send the name of the game to the players.
% We assume the players have already been sent the full
% definitions of the games with these names. 
% Probably this could be done over email. 

get_current_game :-
	select_next_game(GameName),
	load_game(GameName),
	send_game_name_to_players(GameName).

send_game_name_to_players(GameName) :-
	out_to_players(game_name(GameName)).

% SELECT_NEXT_GAME(GameName).
% Could be determined from an external source, like a tournament
% director, based on the current results of the tournament.
% Could also be determined from a file.
% For the time being, to demonstrate the functionality,
% we'll have them play chess, checkers, shogi, and turncoat_chess,
% and then tell them the match is over. 

select_next_game(GameName) :- next_alternate_game(GameName), !.
select_next_game(chess).

next_alternate_game(GameName) :-
	player_current_game_name(OldGame), !,
	game_follows(OldGame,GameName).
	
player_current_game_name(Name) :-
	player_current_game(G),
	game_name(G,Name).

game_follows(chess,checkers).
game_follows(checkers,shogi).
game_follows(shogi,turncoat_chess).


%========================================
% GET_RANDOM_ASSIGNMENT(-Assignments)
%========================================
% External hook to determine random assignments
% when necessary.
% Here, the ref. generates a random assignment,
% and sends the string representation to each of the players.
get_random_assignment(Assignment) :-
	game_assignments(_Game,As),
	assignment_decision(As,random,PieceNames,Squares),
	generate_random_assignment(PieceNames,Squares,Assignment),
	send_assignment(Assignment).


% assign_pieces_to_squares is defined in generator
generate_random_assignment(PieceNames,Squares,Assignment) :-
	format("~nGenerating Random Assignment~n",[]),
	assign_pieces_to_squares(PieceNames,Squares,Assignment). 


% Ex:
% ?- send_assignment([piece1=[square(2,2)],piece2=[square(1,1)]]).
%piece1 at { ( 2 , 2 ) } 
%piece2 at { ( 1 , 1 ) } . 
% Uses command call_for_players, to send copy to each player.
% Send by name?
% assignments_string/2 defined in game grammar file grammar.pl
send_assignment(Assignment) :-
	assignments_string(Assignment,AssignmentString),
	format("~nSending random assignment: ~w~n",[Assignment]),
	out_to_players(init_state(AssignmentString)).
	


%================================================================================
% TERMINATE_GAME(+FinalState)
%================================================================================
% Hook called when the game has ended.

terminate_game(SIn) :-
	format("~nThe game has finished~n",[]),
	process_results(SIn),
	restart_or_end.

% How results are processed to be defined later.
process_results(_SIn).

restart_or_end :-
	next_alternate_game(_GameName)
          -> restart
          ;  goodbye_players.
	
restart :-
	out_to_players(reset(new)), 
	start_controller.

goodbye_players :-
	out_to_players(reset(end)), 
	format("~nTournament is finished. Bye!~n",[]).



%================================================================================
% SHOULD_CONTINUE(SIn)
%================================================================================
% Another hook to controller.  
% Should return true if the player wants to continue the game.


should_continue(_SIn) :- continuous, !, not_abort.
should_continue(_SIn) :- ask_continue(y).

% So someone else (like ref. or human) 
% can emergency abort by connecting to the server also.
not_abort :-
	rd_noblock(abort), !, 
	format("~nUser chose to abort!~n"),
	fail.
not_abort.

%================================================================================
% Move Selection methods
%================================================================================

% CHOOSE(Chooser,Role,SIn,SOut)
% Contains a clause for each decision method (or player)
% Currently:
%   HUMAN
%   COMPUTER
%   RANDOM

choose(Player,Role,SIn,SOut) :- observe_choice(Player,Role,SIn,SOut).

% If move is legal, then say it is, so the other player can process it.
% (This is one attempt to avoid problems when both trying to rd/in
% at the same time, so only 1 gets it).
% Note we receive a move in string representation, and parse it 
% and find a legal interpretation in the current position.
% If the move isn't legal, we crash at the moment.
observe_choice(Player,Role,SIn,SOut) :- 
	format("~nPlayer ~w, as ~w, must select his move.~n",[Player,Role]),
	observe_move(Player,MoveString), 
	nl, format("Observed: ~s",[MoveString]), nl,
	find_correct_interpretation(Player,Role,MoveString,SIn,SOut),
	other_player(Player,Other),
	out_personal(Other,legal),
	format("~nPlayer ~w, as ~w, has selected his move.~n",[Player,Role]).
	
find_correct_interpretation(Player,Role,MoveString,SIn,SOut) :-
	move_notation_string(Move,MoveString),
	check_legality(Player,Role,Move,SIn,SOut), !.
find_correct_interpretation(Player,Role,MoveString,SIn,SOut) :-
	format("~nCouldn't interpret the move, ~s, of player ~w, as ~w~n",[MoveString,Player,Role]),
	assert(checkpoints(Player,Role,MoveString,SIn,SOut)),
	break.
 
check_legality(Player,Role,Move,SIn,SOut) :- 
	format("~nVerifying legality of move by Player ~w, as ~w:~w~n",[Player,Role,Move]),
	legal(Move,SIn,SOut), 
	format("~nMove ~w  passed legality check!~n",[Move]),
	print_state(SOut),
	print_notation(Move).
	   


observe_choice(Player,Role,_SIn,_SOut) :- 
	format("~nPlayer ~w, as ~w, selected an illegal move!!!~n",[Player,Role]), !, fail.

observe_move(Player,Move) :-
	rd_wait(moved(Player,Move)).

role_player(player,White,_,White).
role_player(opponent,_,Black,Black).

other_player(Player,Other) :-
	players(Player,Other).
other_player(Player,Other) :-
	players(Other,Player).


