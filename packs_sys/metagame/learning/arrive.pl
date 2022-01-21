%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% arrive.pl

% ARRIVAL Strategy
% This strategy for a player favors positions in which that player
% owns pieces which are closer to the destination for his arrival goals.
% 
% We check that the piece is owned by the player
% who has the goal, as this strategy will not in general help us 
% get our opponent's pieces to square they don't want to be on.  
%
% The value returned by this strategy is a probability, + or -, 
% that the player will achieve a given goal (on backtracking,
% gives value for each of his goals).  

arrive_value(Piece,Sq,SqT,Goal,Value,Pos,Tables) :- 
	owns(Piece,Player),
	game_player_has_goal(_,Player,Goal),
	arrive_goal(Goal,Player,_Type,_Sqs),
	arrive_goal(Goal,Descr,Squares),
	arrive_distance(Piece,Sq,SqT,Descr,Squares,SqDist,Tables),
	clear_path_cost(Piece,Player,Sq,SqT,SqDist,PathCost,Pos),
	arrive_likelihood(PathCost,Prob),
	reasonable_likelihood(Prob),
	arrive_goal_value(Piece,IValue),
	expected_value(Prob,IValue,AbsVal),
	negate_for_player(Player,AbsVal,Value).

arrive_distance(Piece,Sq,SqT,arrive(Descr,Squares),Dist,Tables) :- 
	arrive_distance(Piece,Sq,SqT,Descr,Squares,Dist,Tables).

% We approximate the distance by using the static piece distance,
% instead of checking the current board dynamically.  

arrive_distance(Piece,Sq,SqT,Descr,Squares,Dist,Tables) :- 
	matches(Descr,Piece),
	member(SqT,Squares),
	approx_path_distance(Sq,Piece,SqT,Dist,Tables).


% How much do we value achieving arrival goals?  Can't make it 
% infinite, as this will then dominate all other considerations.
% It may be a function of different pieces.  
% For now we'll set it to 1, and use a parameter to control it. 
arrive_goal_value(_Piece,1).

% The likelihood of getting a piece to a square decreases
% exponentially with the number of moves required to 
% achieve it.  This uses the same decay rate as is used 
% in eventual mobility determination, and for promotions.  
arrive_likelihood(Distance,Prob) :- 
	distance_value(Distance,Prob).

% DISTANCE_VALUE(Distance,Value)
% Distance is a positive integer, usually an abstract number of 
% moves. The likelihood of realizing a goal decreases
% by some decreasing function of the number of moves required to 
% achieve it.  
distance_value(Distance,Value) :- 
	parameter(discount,D),
	discount_fn(D,Distance,Value).

discount_fn(inverse,Distance,Value) :-
	Value is 1/(1+Distance).
discount_fn(exponent,Distance,Value) :-
	Value is 1 / (1 << Distance).


% If there is some distance, use it.
% Otherwise assume it is very far away (make this a parameter also?)  
% This at least gives some points for having the right type of 
% piece on the board. 
approx_path_distance(Sq,Piece,SqT,Dist,Tables) :- 
	square_piece_distance(Sq,Piece,SqT,Dist,Tables), !.
approx_path_distance(_Sq,_Piece,_SqT,10,_Tables).


% The cost (in moves) of clearing the path to a square
% and then moving the piece to it.
% Could take into account whether piece could capture occupier
% and thus give a discount as nobody else need do so. 
% Now adding 1 move penalty if player is not in control in
% the current position, as this would be a handicap in a race 
% to get somewhere.  
clear_path_cost(_Piece,Player,_Sq,SqT,SqDist,PathCost,Pos) :- 
	clear_square_cost(SqT,Player,SqClearCost,Pos),
	control_cost(Player,ControlCost,Pos),
	PathCost is ControlCost + SqDist + SqClearCost.

clear_square_cost(SqT,Player,SqClearCost,Pos) :- 
	on(Occupier,SqT,Pos),
	clear_occupier(Occupier,Player,SqClearCost).
	
% Rather arbitrary value for cost of clearing opponent
% from the square!  
clear_occupier(empty,_,0).
clear_occupier(piece(_Type,Player),Player,1) :- !.
clear_occupier(piece(_Type,_Player),_Opponent,5).


reasonable_likelihood(Prob) :- Prob > 0.

expected_value(Prob,IVal,Value) :- Value is Prob*IVal.

	

test(Sq,SqT,Value) :- 
	arrive_value(piece(slug,player),
	Sq,SqT,piece_desc(any_player,any_piece),[SqT],Value).



