%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% player.pl
%%%


:- prolog_flag(redefine_warnings,_,off).


%================================================================================
% PLAYER
%================================================================================
%
% PLAYER prolog process will be called with command like:
%	start_sicstus_shell(any,
%	 [ server_name, ServerAddr,
%	   server_port, Port,
%	   local_name, Player,
%	   file, MoveFile,
%	   file,'~/prolog/play/player'
%	 ]).
% The MoveFile should define the search method which will be used when
% this player is to move.  


% Redefine metagame here, so we don't give menu and instead just start player.
metagame :- start_player.

start_player :-
	find_my_name(Name),
	assert(my_name(Name)), 
	find_server(Addr,Port),
	play(Addr,Port,Name).

find_my_name(N) :- parameter(player1_name,N).

find_server(S,P) :-
	parameter(server_name,S),
	parameter(server_port,P).

play(ServerAddr,Port,Player) :-
	use_module(library('linda/client')),
	port_number(Port,PortNum),
	format("Server address is ~q:~q~n",[ServerAddr,PortNum]),
	linda_client_wait(ServerAddr:PortNum),
	greet_ref(Player),
	start_controller.

greet_ref(Player) :- format("~nHello, I'm player: ~w",[Player]).





% These are the interface routines for a player to receive and send
% information to a controller, when playing against an opponent.


% Waits for a personal message, of the form:
% message(<Me>,Pattern)
in_wait_personal(Pattern) :-
	my_name(Me),
	in_wait(message(Me,Pattern)).




% GET_MOVE(Player,Move)
% If communicating directly (not via ref) don't need
% above transmit, just one puts in, other takes out, and
% vice versa.
% Someone must say the move is legal, before I'll get it.
% If not ref, the other player himself can do so.
% This gets the string notation, which must later be parsed
% into an internal representation denoting the legal move.
%     moved(player,move) is the only message not personally addressed.
get_move(Player,Move) :-
	in_wait_personal(legal),
	in_wait(moved(Player,Move)).

% COMMUNICATE_MOVE(Player,Move)
% Sends the move, in string notation.  
% Doesn't need to be personal, as it mentions the 
% player and so won't be intercepted by anyone not involved.
% And the other player won't see it till the ref. has said it is 
% legal.
communicate_move(Player,Move) :-
	out(moved(Player,Move)).

% RECEIVE_PLAYERS(White,Black)
receive_players(White,Black) :-
	in_wait_personal(players(White,Black)).

process_game(GameName) :-
	atom(GameName), !,
	file_make_test_game(GameName).
process_game(GameString) :-
	string_make_test_game(GameString).
	

%================================================================================
% Hooks to standard controller script
%================================================================================

get_players(White,Black) :-
	receive_players(White,Black),
	my_name(Me),
	find_opponent(White,Black,Me,Opponent),
	retractall(my_opponent(_)),
	assert(my_opponent(Opponent)).

find_opponent(White,Black,White,Black).
find_opponent(White,Black,Black,White).


%========================================
% GET_CURRENT_GAME
%========================================
% Receive the name of the next game to be played.
% Then load it as the current game.

get_current_game :-
	receive_game_name(GameName),
	load_game(GameName).

receive_game_name(G) :-
	in_wait_personal(game_name(G)).


%========================================
% GET_RANDOM_ASSIGNMENT(-Assignments)
%========================================
% External hook to determine random assignments
% when necessary.
% Gets the random assignment string from the referee,
% then parses it.
get_random_assignment(Assignment) :-
	format("~nRequesting Random Assignment~n",[]),
	get_init_state(AssignmentString),
	assignments_string(Assignment,AssignmentString),
	format("~nReceived random assignment: ~w~n",[Assignment]).
	

% GET_INIT_STATE(-Assignment)
% Gets the string representing the initial state assignment
% (for random-setup games).  
get_init_state(Assignment) :-
	in_wait_personal(init_state(Assignment)).


%================================================================================
% TERMINATE_GAME(+FinalState)
%================================================================================
% Hook called when the game has ended.


terminate_game(SIn) :-
	format("~nI'm finished playing the game.~n",[]),
	analyze_game(SIn),
	restart_or_end.

% To be defined by player's own programs.
analyze_game(_).
	

restart_or_end :-
	in_wait_personal(reset(R)), !,
	restart_if(R).

restart_if(new) :- format("~nStarting again!~n",[]), start_controller.
restart_if(end) :- format("~nTournament is finished. Bye!~n",[]), close_player.

close_player.  

%================================================================================
% SHOULD_CONTINUE(SIn)
%================================================================================
% Another hook to controller.  
% Should return true if the player wants to continue the game.
%
% Some routines here defined in local.pl

should_continue(_) :- continuous, !, not_abort.
should_continue(_) :- ask_continue(y).


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
% Contains a clause for whether local player is moving or not.
% Currently:
% If ME, calls MOVE as loaded in designated file.
% If NOT ME, gets move from remote opponent.
% Note my_name was saved above.

choose(Me,Role,SIn,SOut) :- my_name(Me), !, my_choice(Role,SIn,SOut).
choose(_Other,Role,SIn,SOut) :- other_choice(Role,SIn,SOut).




%================================================================================
% MOVE(Role,Move,SIn,SOut)
%================================================================================
% A hook into a file which is loaded by this player to determine how to 
% move.  The result is the move that the player will choose.
% By splitting this up, the PLAYER script can be used by all different
% programs to play remotely, just as the CONTROLLER script is used
% by all participants.
% Some choices of files:  humanist, randomist, instantist.
%================================================================================


% MY_CHOICE(Role,SIn,SOut) 
% What to do when it is my turn.
% Uses move/4 defined in accompanying file to select a 
% move, returning the internal representation.
% Then translate this into the appropriate grammatical string
% representation, and communicate this to the other player.
my_choice(Role,SIn,SOut) :- 
	my_name(Me),
	move(Role,Move,SIn,SOut),
	move_notation_string(Move,MoveString),
	communicate_move(Me,MoveString).


% OTHER_CHOICE(Role,SIn,SOut) 
% What to do when it's the other player's turn.
% Get his move, parse it, and find the interpretation which is 
% legal (assumed unambiguous w.r.t. current position).
other_choice(Role,SIn,SOut) :- 
	my_opponent(Player),
	get_move(Player,MoveString),
	nl, format("Received: ~s",[MoveString]), nl,
%	nl, format(MoveString,[]), nl,
	find_correct_interpretation(Player,Role,MoveString,SIn,SOut).


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


