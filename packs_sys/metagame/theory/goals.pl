%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% goals.pl

% Representation of goal achievement and complete legal moves
% for symmetric chess-like games.
% These routines form the skeleton of the game definition,
% providing three components:
%
% 1. legal(Move,Player,StateIn,StateOut)
%    True when Move is legal for Player in StateIn, and Produces StateOut.
%
% 2. game_over(State,_)
%    True when the game in StateIn has ended.  (Second var is dummy).
%
% 3. game_outcome(FinalState,Outcome)
%    Outcome is the final outcome of Game, which ends in FinalState.
%    Outcome is either PLAYER, OPPONENT, or DRAW.
%
% Any 2-player game must provide exactly these three procedures,
% and an interface which relies on these three is thus fully
% general to cover 2-player games.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				Goals
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The GAME IS OVER when ANY of the following is true:
%  1. some player has achieved a goal
%  2. the maximum number of moves have been played.
%  3. the position has repeated a certain number of times.
%     (REPETITION IS NOT IMPLEMENTED YET, maybe never!).
game_over :- goal_achieved(_Player,Game).
game_over :- exceeded_move_limit(Game).
game_over :- too_many_repetitions(Game).


%========================================
% K-Move Rule
%========================================

exceeded_move_limit(Game) :-
	game_move_limit(Game,L),
	move_count(L).

% GAME_MOVE_LIMIT(+Game,Number).
% Could be generated differently with each game,
% or set arbitrarily.  
% Here we just set it for all games. 
game_move_limit(_Game,200).

% MOVE_LIMIT_OUTCOME(+Game,-Outcome).
% If the game ended by exceeding the move limit,
% each game could decide what the outcome is (who wins).
% Here we say it is a draw for all games.
move_limit_outcome(_Game,draw).


%========================================
% Repetition Rule
%========================================
% THIS IS NOT IMPLEMENTED!  Hooks are placed here in case we
% do implement it sometime.

% GAME_REPETITIONS(+Game,Number).
% Could be generated differently with each game, or set arbitrarily.  
% Here we just say a game ends if a position repeats 3 times.
game_repetitions(_Game,3).

% REPETITION_OUTCOME(+Game,-Outcome).
% If the game ended by too many repetitions,
% each game could decide what the outcome is (who wins).
% Here we say it is a draw for all games.
repetition_outcome(_Game,draw).

% TOO_MANY_REPETITIONS
% There are too many repetitions when the present position
% repeats a previous one a number of times.
% 
too_many_repetitions(_Game) :-
	fail.


% GAME_OUTCOME(?Outcome)
% There is a game outcome only when the game is over.
% Determines the outcome based on which players have achieved one of their goals,
% or on the game-specific outcome in case of exceeding the move limit or 
% excess-repetition.
game_outcome(Outcome) :-
	too_many_repetitions(_Game), !,
	repetition_outcome(_Game,Outcome).
game_outcome(Outcome) :-
	exceeded_move_limit(_Game), !,
	move_limit_outcome(_Game,Outcome).
game_outcome(Outcome) :-
	player_outcome(player,WinP),
	player_outcome(opponent,WinO),
	outcome(WinP,WinO,Outcome).


% PLAYER_OUTCOME(+Player,?Yes/No)
player_outcome(Player,Outcome) :- 
	goal_achieved(Player,Game) 
        -> Outcome = yes
        ;  Outcome = no.


% OUTCOME(Player1_Outcome,Player2_Outcome,Outcome).
% Outcome is the player who achieved the goal, 
% or draw if both.
outcome(yes,yes,draw).
outcome(yes,no,player).
outcome(no,yes,opponent).


% GOAL_ACHIEVED(Player,Game)
% Player is one of the players, and has achieved one of his goals in Game.
goal_achieved(Player,Game) :- 
	game_player_has_goal(Game,Player,Goal),
	goal_true(Goal),
	verbosely_format("Goal achieved: ~p achieved goal ~p~n",[Player,Goal]).


% An ARRIVAL goal has been achieved when a piece matching Description
% has arrived on one of the Squares.
goal_true(arrive(Descr,Squares))  :-
	member(Sq,Squares),
	on(Piece,Sq),
	matches(Descr,Piece).

% An ERADICATE goal has been achieved when there are no pieces
% *on the board* matching a description, when players are no longer assigning pieces.
% (Else these goals would be satisfied immediately and trivially).
% still_assigning defined in legal.pl
goal_true(eradicate(Descr)) :-
	\+ still_assigning, 
	\+ exists(Descr).

% A player is STALEMATED when it is his turn to move, 
% but he can't make any legal moves.
goal_true(stalemate(Player)) :- 
	control(Player),
	\+ legal_move(_M,Player).

% A Description exists when a piece on a square matches it.
% There is an efficiency tradeoff here. 
% 1. We could examine each piece on the board, to see if it matches the 
% description.
% 2. We could consider any piece which could match the description,
% and see if it is on the board.  
%
% The second case is much better if we have indexing to see if 
% specific pieces are on the board.

exists(Descr) :-
	on(Piece,Sq),
	matches(Descr,Piece).
	

