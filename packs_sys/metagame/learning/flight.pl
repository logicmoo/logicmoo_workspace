%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================


% SAFE_FLIGHT_SQUARE(Piece,Player,SqF,SqT,Pos,Tables)
% A safe flight square is one where the piece can move (perhaps by capture),
% the new square doesn't win for the enemy, and in the resulting position 
% no piece is attacking it. 
% We could restrict attacking pieces to those which wouldn't be attacked in the 
% next position, to pieces which could safely move onto their dest square when
% capturing us, etc., but this should do as a first cut!  

safe_flight_square(Piece,Player,SqF,SqT,Pos,Tables) :-
	opposite_role(Player,Opp),
	flight_square(Piece,Player,SqF,SqT,Pos,PosOut,Tables),
	\+ threatens(_PieceA,Opp,_SqA,_SqTA,Piece,SqT,_Effect,PosOut).


% The exclusion check now in global.pl when we build tables in the first place. 
flight_square(Piece,Player,SqF,SqT,Pos,PosOut,Tables) :-
	blank_state_if(Pos),
%	find_advice_tables_if(Tables),
%	add_dynamic_tables_if(Pos,Tables),
	on(Piece,SqF,Pos),
	piece_index(Piece,_),
	general_move(Piece,Player,SqF,SqT,Pos,PosOut,Tables).


general_move(Piece,Player,SqF,SqT,Pos,PosOut,Tables) :-
	done_move_threat(Piece,Player,SqF,SqT,Pos,PosOut,Tables).
general_move(Piece,Player,SqF,SqT,Pos,PosOut,Tables) :-
	done_capture_threat(Piece,Player,SqF,SqT,_PieceV,_SqV,_Effect,Pos,PosOut,Tables).


testflight :- 
	checkpoint(cap,Pos), 
	flight_square(_Piece,player,SqF,SqT,Pos,_ZOut,_Tables), 
	format("~p->~p~n",[SqF,SqT]).




%================================================================================
% Flight_square without changing position (not used). 
%================================================================================


flight_square(Piece,Player,SqF,SqT,Pos,Tables) :-
	blank_state_if(Pos),
%	find_advice_tables_if(Tables),
%	add_dynamic_tables_if(Pos,Tables),
	on(Piece,SqF,Pos),
	piece_index(Piece,_),
	general_move(Piece,Player,SqF,SqT,Pos,Tables).
	

% use routines in global.pl
general_move(Piece,Player,SqF,SqT,_Pos,Tables) :-
	move_threat(Piece,Player,SqF,SqT,Tables).
general_move(PieceA,Player,SqA,SqT,_Pos,Tables) :-
	capture_threat(PieceA,Player,SqA,SqT,PieceV,SqV,Effect,Tables).


%================================================================================
% Using flight info for dominate goals
%================================================================================

/*
 A piece is tightly-dominated to the extent that its flight squares are covered by 
 enemy pieces.  
 Domination is slightly worse for the player who has to move, because of 
  Zugswang. [?]

Also important for domination is the number of pieces left to capture before the 
goal is achieved.  Thus when a player has just one king, this is vital to protect,
whereas when he has lots of checkers, dominating any 1 isn't that crucial until 
the numbers are smaller.  

To model this, we should first count the number matching this goal, and then 
weight the resulting domination value for each target piece by some value which
decreases as the number of such targets increases. In fact, maybe don't even
think about domination until the number of targets is smaller.  
*/


eradicate_safety(Player,Piece,Sq,Goal,Value,Pos,Tables) :- 
	opposite_role(Player,Opponent),
	game_player_has_goal(_,Opponent,Goal),
	eradicate_goal(Goal,Player,_Type),
	eradicate_goal(Goal,Descr),
	owns(Piece,Player),
	weighted_dominate(Descr,Player,Piece,Sq,Value,Pos,Tables).

weighted_dominate(Descr,Player,Piece,Sq,Value,Pos,Tables) :- 
%	find_advice_tables_if(Tables),
%	add_dynamic_tables_if(Pos,Tables),
	on(Piece,Sq,Pos),
	piece_index(Piece,_),
	blank_state_if(Pos),

	dominate_targets(Descr,Targets,Pos),
	enough_target_urgency(Targets,Urgency),
	member(Piece@Sq,Targets),
	dominate_val1(Player,Piece,Sq,Val1,Pos,Tables), 
	Value is Val1*Urgency.  

dominate_targets(Descr,Targets,Pos) :- 
	findall(Piece@Sq,
	  matching_square(Piece,Sq,Descr,Pos),
	  Targets).


% A dominated piece gets points for its owner for each safe
% flight square it has, with each square weighted by the 
% eventual mobility the piece would have from that square.
% Thus, a king near the corner, with 8 flight squares, may still be
% worse than a king in the center with only 3 flight squares.  
% Also, this says that if we want to take away just 1 flight square
% from the piece, take the more central one to force it backward! 
% If this is annoying we can change it to just count the number of 
% moves it has. 
dominate_val1(Player,Piece,SqF,Value,Pos,Tables) :- 
	blank_state_if(Pos),
	safe_flight_square(Piece,Player,SqF,SqT,Pos,Tables),
	eventual_piece_mobility(Piece,SqT,Val,Pos,Tables),	
	negate_for_player(Player,Val,Value).
	


matching_square(Piece,Sq,Descr,Pos) :- 
	on(Piece,_,Sq,Pos),
	matches(Descr,Piece).
	

% VITAL_NUMBER parameter controls max number left st we consider
% remaining eradicate targets to be vital to our safety.  
enough_target_urgency(Targets,Weight) :- 
	parameter(vital_number,N),
	length(Targets,Length), 
	Length =< N,	
	Weight is 1/Length.


vital_piece(Piece,Sq,Player,Goal,Weight,Pos,_Tables) :-
	opposite_role(Player,Opponent),
	eradicate_goal_targets(Opponent,Goal,Targets,Pos),
	enough_target_urgency(Targets,Weight),
	member(Piece@Sq,Targets),
	owns(Piece,Player).
	
	
eradicate_goal_targets(Player,Goal,Targets,Pos) :-
	opposite_role(Player,Opponent),
	game_player_has_goal(_,Player,Goal),
	eradicate_goal(Goal,Opponent,_Type),
	eradicate_goal(Goal,Descr),
	dominate_targets(Descr,Targets,Pos).
	
	
% May want to take the particular effect into accout, as not as 
% serious if we possess the piece afterward.  
% Just checks if there is some threat to this piece, doesn't count
% duplicates. 
% If black needs to kill N more white pieces to achieve his goal, the
% value to white for having each of these pieces threatened is -1/N.
% So when down to last vital piece, he scores -1 for having it threatened. 
% Thus by setting the parameter which uses this function to have high value,
% it only kicks into effect when we get threatened, and can override all other
% considerations. 
% Also, when there are multiple goals threatened, we take more seriously the ones
% which are closer to achievement.  
% 
threatened_vital_piece(Piece,Sq,Player,Goal,Value,Pos,Tables) :-
%	find_advice_tables_if(Tables),
%	add_dynamic_tables_if(Pos,Tables),
	opposite_role(Player,Opponent),
	vital_piece(Piece,Sq,Player,Goal,Weight,Pos,_Tables),
	( capture_threat(_PieceA,Opponent,_SqA,_SqT,Piece,Sq,Effect,Tables) 
	-> true
	),
	negate_for_player(Opponent,Weight,Value).
	
% If opponent to move and threatening our last piece, this is 
% absolutely terrible, so make value much more intense (override all else). 
%  Otherwise use normal value.  
% Note this only applies after we've assigned all the pieces, as 
% eradicate goals are not checked until then. 
threatened_vital_piece_value(Piece,Sq,Player,Goal,Value,Pos,Tables) :- 
	\+ still_assigning(Pos),
	threatened_vital_piece(Piece,Sq,Player,Goal,Val1,Pos,Tables),
	( ( control(Player,Pos) ; abs(Val1) < 1 ) 
	-> Value = Val1
	; Value is 10000*Val1
	).
	
	
	
