%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% advisors.pl
%%% Provides move filters and corresponding choice methods (players).

%================================================================================
% Choice Methods
%================================================================================
% Here are a few built-in choice methods which can be called for different players.
% See the file local.pl for how they are used with the local interface,
% or the file randomist.pl for an example how they can serve as the basis for
% remote players.
%
% The move generators (or filters) on which they are based are defined here also. 

%================================
% HUMAN_CHOOSE(Player,Move,SIn,SOut)
%================================
% Calls the nice user a nice interface for selecting  moves.
%
human_choose(Player,Move,SIn,SOut) :-
	control(Player,SIn),
	ask_move(Move,SIn,SOut).

	
%================================
% THREATEN_CHOOSE(Player,Move,SIn,SOut)
%================================
% Choose randomly at start, as checking for stalemate
% at start of game takes so long.  
% After start, play threats if have them, else random.
%
threaten_choose(Role,Move,SIn,SOut) :- 
	( move_count(N,SIn) -> N =< 1), !,
	random_choose(Role,Move,SIn,SOut).
threaten_choose(_,Move,SIn,SOut) :- 
	timing(threaten_move(Move,SIn,SOut)), !,
	print_choice(Move,SIn,SOut).
threaten_choose(Role,Move,SIn,SOut) :- 
	random_choose(Role,Move,SIn,SOut).


%================================
% INSTANT_CHOOSE(Player,Move,SIn,SOut)
%================================
instant_choose(Player,Move,SIn,SOut) :-
	control(Player,SIn),
	format("~nThe Instant Moves:~n",[]),
	instant_move(Move,SIn,SOut), 
	print_choice(Move,SIn,SOut).
	
% INSTANT_MOVE(Move,SIn,SOut)
instant_move(Move,SIn,SOut) :-
	legal(Move,SIn,SOut), !.


%========================================
% RANDOM_CHOOSE(Player,Move,SIn,SOut)
%========================================
random_choose(Player,Move,SIn,SOut) :-
	control(Player,SIn),
	timing(random_move(Move,SIn,SOut)), !,
	print_choice(Move,SIn,SOut).

% RANDOM_MOVE(M,SIn,SOut)
random_move(M,SIn,SOut) :-
	random_success(legal(M,SIn,SOut)).



%========================================
% CAUTIOUS_CHOOSE(Player,Move,SIn,SOut)
%========================================
cautious_choose(_Player,Move,SIn,SOut) :-
	timing(cautious_move(Move,SIn,SOut)), !,
	print_choice(Move,SIn,SOut).


% A CAUTIOUS_MOVE is one which does not lose immediately,
% and which does not allow a victor_move response.

cautious_move(Move,SIn,SOut) :- 
	safe_move(Move,SIn,SOut),
	\+ victor_move(_M2,SOut,_).


% A SAFE_MOVE is one which does not lose immediately.

safe_move(Move,SIn,SOut) :- 
	control(Player,SIn),
	opposite_role(Player,Opponent),
	legal(Move,SIn,SOut),
	\+ game_outcome(Opponent,SOut).

% Assumes legal already, just makes sure the player who
% moved doesn't lose as a result.  
check_safe_move(_Move,SIn,SOut) :- 
	control(Player,SIn),
	opposite_role(Player,Opponent),
	\+ game_outcome(Opponent,SOut).




%========================================
% RANDOM_AGGRESSIVE_CHOOSE(Player,Move,SIn,SOut)
%========================================
% Wins immediately if possible, else plays a random move 
% which doesn't lose immediately or allow opponent to 
% win on the next move.  If all moves can lose,
% plays any random move.


random_aggressive_choose(_Player,Move,SIn,SOut) :-
	timing(random_aggressive_move(Move,SIn,SOut)), !,
	print_choice(Move,SIn,SOut).


random_aggressive_move(Move,SIn,SOut) :- victor_move(Move,SIn,SOut), !.
random_aggressive_move(Move,SIn,SOut) :- 
	random_cautious_move(Move,SIn,SOut).


%========================================
% RANDOM_CAUTIOUS_CHOOSE(Player,Move,SIn,SOut)
%========================================
% Plays a random move which doesn't lose immediately or allow opponent to 
% win on the next move.
% If none available, plays any random move (really, should play
% one which doesn't cause a lose in preference to one which does ...).

random_cautious_choose(_Player,Move,SIn,SOut) :-
	timing(random_cautious_move(Move,SIn,SOut)), !,
	print_choice(Move,SIn,SOut).

random_cautious_move(Move,SIn,SOut) :- 
	legal_moves(Moves,SIn),
	random_non_losing(Moves,Move,SIn,SOut), !.
random_cautious_move(Move,SIn,SOut) :- 
	print_resign_notice,
	random_move(Move,SIn,SOut).

print_resign_notice :- 
	format("
A cautious player would resign now ... 
but perhaps my opponent won't see it!
",[]).

print_forced_notice :- 
	format("Forced choice: only 1 legal move~n",[]).

print_rushed_notice :- 
	format("Rushed choice: no time to think!~n",[]).

print_forced_or_lost_notice :- 
	format("Forced choice: only 1 legal move (or all others lose!)~n",[]).

random_non_losing([Move],Move,SIn,SOut) :- !,
	print_forced_or_lost_notice,
	legal(Move,SIn,SOut).
random_non_losing(Moves,Move,SIn,SOut) :-
	random_select(Move1,Moves,RestMoves),
	nl_or_next(Move1,RestMoves,Move,SIn,SOut).

nl_or_next(Move,_,Move,SIn,SOut) :-
	cautious_move(Move,SIn,SOut), !.
nl_or_next(_,Moves,Move,SIn,SOut) :-
	random_non_losing(Moves,Move,SIn,SOut), !.

legal_moves(Moves,SIn) :- 
	setof(Move,S1^legal(Move,SIn,S1),Moves).

%============================================================

% A PASS_MOVE isn't really a legal move, it just transfers the
% player on move.

pass_move(SIn,SOut) :-
	control(P1,SIn),
	transfer_control(_,SIn,SOut),
	control(P2,SOut),
	format("Passing, control has now transferred from ~p to ~p~n",[P1,P2]).



% A VICTOR_MOVE is one which wins the game immediately. 
victor_move(Move,SIn,SOut) :-
	control(Player,SIn),
	legal(Move,SIn,SOut),
	game_outcome(Player,SOut).

% An ENDGAME_MOVE is one which ends the game immediately.
endgame_move(Move,SIn,SOut) :- 
	legal(Move,SIn,SOut),
	game_over(SOut).


% A MATE_MOVE is one which does not allow a cautious_move response.
% That is, either it ends the game immediately, or for all moves
% of the opponent, we have a victory move.
mate_move(Move,SIn,SOut) :- 
	legal(Move,SIn,SOut),
	\+ cautious_move(_M2,SOut,_).


% A THREATEN_MOVE is one which threatens victory, if the opponent doesn't respond
% to stop it. (This is like a CHECK in chess).

threaten_move(Move,SIn,SOut) :- 
	legal(Move,SIn,SOut),
	( transfer_control(_,SOut,S1) ->
	victor_move(_M2,S1,_)).


% An ENOUGH_ROPE_MOVE is one which allows the opponent to play a move
% which allows us to win next. 

enough_rope_move(Move,SIn,SOut) :- 
	legal(Move,SIn,SOut),
	legal(_M2,SOut,S1),
	victor_move(_M3,S1,_).

