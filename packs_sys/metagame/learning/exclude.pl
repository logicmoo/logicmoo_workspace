%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% exclude.pl
%%% Filtering piece movement transitions based on goals.
%%% The only external module here is SAFE_TRANSITION_TYPE/8.

% GOAL_SQUARE(Piece,Sq,Player,Goal)
% Player has goal Goal to get Piece to Sq (in the current game).
goal_square(Piece,Sq,Player,Goal) :- 
	game_player_has_goal(_,Player,Goal),
	arrive_goal(Goal,Descr,Squares),
	matches(Descr,Piece),
	member1(Sq,Squares).



safe_transitions_type(Piece,PieceIndex,Sq,SqIndex,Type,SqTIndices,State) :- 
	square_index(Sq,SqIndex),
	( setof(SqTIndex, SqT^
	       safe_transition_type(Piece,PieceIndex,Sq,SqIndex,SqT,SqTIndex,Type,State),
	       SqTIndices
	       )
	-> true
	; SqTIndices=[]
	).

safe_transition(Piece,Sq,SqT,State) :- 
	safe_transition(Piece,_PieceIndex,Sq,_SquareIndex,SqT,_SqTIndex,State).
	  
safe_transition(Piece,PieceIndex,Sq,SquareIndex,SqT,SqTIndex,State) :- 
	safe_transition_type(Piece,PieceIndex,Sq,SquareIndex,SqT,SqTIndex,moving,State).


% SAFE_TRANSITION_TYPE(Piece,PieceIndex,Sq,SquareIndex,SqT,SqTIndex,Type,State)
%
% Check here before calling piece-movement that Sq is not a goal square for Piece
% (for either player), as if it is these transitions are irrelevant as the game will
% end first.  
% Then check that target square SqT is not necessarily a *loss* (goal square for the
% enemy alone) as if it is this transition will likely never be made.  
% Not sure about this:
% I might capture your piece by landing on a losing square, thus ending the 
% game in a draw (but we're probably not doing captures here!).
%
safe_transition_type(Piece,PieceIndex,Sq,SquareIndex,SqT,SqTIndex,Type,State) :- 
%	new_empty_state(State), % for testing
	square_index(Sq,SquareIndex),
	piece_index(Piece,PieceIndex),
	\+ excluded_from(Piece,Sq),
	piece_move_for_type(Type,Piece,Sq,SqT,State),
	\+ excluded_to(Piece,SqT),
	square_index(SqT,SqTIndex),
	tracing_anal_format(detailed,"~p: ~p -> ~p~n",[Piece,Sq,SqT]).



% A piece will never move from a goal square for either player, 
% as the game will already have ended. 
excluded_from(Piece,Sq) :- 
	goal_square(Piece,Sq,_Player,_Goal). 
	
% A piece will never move to a goal square for the opponent,
% unless immediately on that square it is  able to force promotion into 
% a piece which is not *only* an opponent goal on that square.  
%
% Similarly, a piece will never move onto a square where it is forced to promote
% into a goal for the opponent. 
excluded_to(Piece,SqT) :- 
	owns(Piece,Player),
	goal_square(Piece,SqT,Opp,_), 
	opposite_role(Player,Opp),
	\+ goal_square(Piece,SqT,Player,_),
	\+ safe_promotion(Piece,Player,SqT),
	tracing_anal_format(filter,
	    "Filter: <~p> can't safely move to ~p~n",[Piece,SqT]).
excluded_to(Piece,SqT) :- 
	owns(Piece,Player),
	player_promotion_square(Player,SqT),
	\+ safe_prom1(Piece,Player,SqT),
	tracing_anal_format(filter,
	   "Filter: <~p> can't safely promote on ~p~n",[Piece,SqT]).

	
% A safe promotion occurs when we are in promotion zone and either:
% 1. We choose the promotion, and there is some non-losing choice.
% 2. Opponent chooses, and there is no losing choice.  

safe_promotion(Piece,Player,SqT) :- 
	owns(Piece,Player),
	player_promotion_square(Player,SqT),
	safe_prom1(Piece,Player,SqT).

% Case 1: we can choose safely. 
% Case 2: opponent must choose safely. 
safe_prom1(Piece,Player,SqT) :- 
	player_safe_prom(Piece,Player,SqT).
safe_prom1(Piece,Player,SqT) :- 
	opponent_safe_prom(Piece,Player,SqT).


player_safe_prom(Piece,Player,SqT) :- 
	promotes_into(Piece,PieceT,Player,Player),
	opposite_role(Player,Opp),
	\+ goal_square(PieceT,SqT,Opp,_).


/*  Moved this to prom.pl.  
% Player can promote Piece to PieceT on SqT.
% Backtracks over all SqT. 
player_safe_new_prom(Piece,Player,PieceT,SqT) :- 
	promotes_into(Piece,PieceT,Player,Player),
	Piece \== PieceT,
	player_promotion_square(Player,SqT),
	opposite_role(Player,Opp),
	\+ goal_square(PieceT,SqT,Opp,_).
*/

opponent_safe_prom(Piece,Player,SqT) :- 
	opponent_promotes(Piece),
	\+ opponent_wins_prom(Piece,Player,SqT).

opponent_wins_prom(Piece,Player,SqT) :- 
	promotes_into(Piece,PieceT,Player,Opp),
	opposite_role(Player,Opp),
	goal_square(PieceT,SqT,Opp,_).


	

%arrive_value(Piece,Sq,SqT,Goal,Value,Pos) :- 
%	game_player_has_goal(_,Player,Goal),
%	arrive_goal(Goal,Player,_Type,_Sqs),
%	arrive_goal(Goal,Descr,Squares),
