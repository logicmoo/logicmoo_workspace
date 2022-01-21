%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% possess.pl
%
% A possessed piece gets some amount of value based on
% the value of each square it could possibly be placed
% on.
% Here we divide the potential value by the size of the board,
% to roughly average the contributions from each square.  
% One consequence of this is that having a piece in hand is never
% better than having it on the board on its best square.  
% This is incorrect, but a start anyway.  
possess_value(Piece,_Player,advice(possess,Sq,Value),Position,Tables) :-
	total_square_count(Total),
	empty(Sq,Position),
	local_evaluation(Piece,Sq,LVal,Position,Tables),
	Value is LVal/Total.


initprom_value(advice(initprom,best(OldPiece,NewPiece),Value),Position,Tables) :- 
	opponent_promotes(OldPiece,Sq,Position),
	control(Player,Position),
	best_choice(OldPiece,Player,Sq,NewPiece,Value,Position,Tables).

best_choice(OldPiece,Player,Sq,NewPiece,Value,Position,Tables) :- 
	findall(NewVal-NewPiece,
	   initprom_value(OldPiece,Sq,Player,NewPiece,NewVal,Position,Tables),
	   Pairs),
	best_player_choice(Player,Pairs,NewPiece,Value). 

best_player_choice(Player,Pairs,NewPiece,Value) :-  
	keysort_for_player(Player,Pairs,Ordered),
	Ordered = [Value-NewPiece|_Rest]. 


% Pairs is some list [v1-item1,...].  Ordered is a sorted 
% version, such that first new v1 is best choice for Player
% (min if black, max if white). 
keysort_for_player(Player,Pairs,Ordered) :-
	keysort(Pairs,Sorted),
	reverse_for_player(Player,Sorted,Ordered).

% The sorted list gives the minimum node first. 
% We assume the black player is trying to minimize the evaluation,
% so he just chooses this first (lowest) choice.  
% The white player is maximizing, so he will choose the last (highest) choice. 
reverse_for_player(opponent,Sorted,Sorted).
reverse_for_player(player,Sorted,Ordered) :- 
	reverse(Sorted,Ordered). 


% Init_promote_option is defined as part of the theory, legal.pl
%
initprom_value(OldPiece,Sq,Player,NewPiece,NewVal,Position,Tables) :- 
	init_promote_option(OldPiece,Player,NewPiece,Position),
	local_evaluation(NewPiece,Sq,NewVal,Position,Tables).


%================================================================================
% Routines used to estimate effect of possessing a piece when
% thinking about threats. 
%================================================================================

% Predict a piece is N times as valuable possessed as it would be
% on the board. 
% possess_offset(2).
% Actually there is more to it than we use here.  If we possess-capture
% opponent's piece, it is
possess_offset(N) :- parameter(possess_offset,N). 


favor_possess(_Player,Val1,Value,_Position) :-
	possess_offset(Offset),
	Value is Val1*Offset. 


% ESTIMATE_POSSESS_VALUE(+Possessor,+Piece,+LVal,-EVal,+Position,+Tables).
% EValue is the estimated value accrued to Possessor if he possesses
% Piece, given that in Position, Piece was worth LVal. 
% We would like to use possess_value above, but that is too expensive
% to use every time we need this estimate. 
%
% Thus:  if Possessor already owns piece, then say it is just worth the
% original value it had on the board.
% If not, there will be a change of possession, so say it is worth the 
% opposite of its current value. 
% We could use static value, value of special arrival squares, etc. 
%
estimate_possess_value(Possessor,Piece,LVal,EVal,_Position,_Tables) :-
	( owns(Piece,Possessor) 
	-> EVal = LVal
	;  EVal is (-LVal)
	). 
	
