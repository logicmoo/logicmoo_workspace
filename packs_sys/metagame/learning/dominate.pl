%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

attacks_i(PieceAI,SqAI,SqTI,PieceVI,SqVI,S) :- 
	attacks(PieceA,SqA,SqT,PieceV,SqV,S),
	piece_index(PieceA,PieceAI),
	piece_index(PieceV,PieceVI),
	square_index(SqA,SqAI),
	square_index(SqV,SqVI).



attacks(PieceA,SqA,SqT,PieceV,SqV,S) :- 
	( ( nonvar(SqA) ; nonvar(SqT) )
	-> true
	;  board_square(SqA)
	),
	attacks1(PieceA,SqA,SqT,PieceV,SqV,S).


% PieceA is on SqA in the 
% position, and he can take PieceV on SqV by moving to SqT.  
% (Need to set the right player in control to do this correctly).

attacks0(PieceA,SqA,SqT,PieceV,SqV,S) :- 
	blank_state(S),
	player_role(Player),
	control(Player,S),
	owns(PieceA,Player),
	on(PieceA,SqA,S),
	captures(PieceA,Player,SqA,SqT,_Effect,Captured,S),
	captured_piece(PieceV,SqV,Captured).

/*
attacks1(PieceA,SqA,SqT,PieceV,SqV,S) :- 
	blank_state_if(S),
	player_role(Player),
	control(Player,S),
	owns(PieceA,Player),
	captures(PieceA,Player,SqA,SqT,_Effect,Captured,S),
	captured_piece(PieceV,SqV,Captured).
*/

% If PieceA were on SqA and Player in control in the 
% position, he could take PieceV on SqV by moving to SqT.  
% 
attacks1(PieceA,SqA,SqT,PieceV,SqV,S) :- 
	blank_state_if(S),
	player_role(Player),
	put_control(Player,S,S1),
	owns(PieceA,Player),
	captures(PieceA,Player,SqA,SqT,_Effect,Captured,S1),
	captured_piece(PieceV,SqV,Captured).

% This is very inefficient if we know the position already,
% as it first genetes hypothetical attacks, and then sees if the
% piece really is on the board!
attackshow(PieceA,SqA,SqT,PieceV,SqV,S) :- 
	attacks(PieceA,SqA,SqT,PieceV,SqV,S),
	on(PieceA,SqA,S).


blank_state_if(S) :- 
	( var(S) -> blank_state(S) ; true).


blank_state(S) :- 
	blank_state(_Player,_Stage,0,S).

blank_state(Player,Stage,Move,S) :- 
	new_state(S0), 
	initialize_state_properties(Stage,Player,Move,S0,S).


% PA on SqA is Dist moves from a square SqI on which it 
% would attack PV on SqV, in State. 
%
attack_distance(PA,SqA,SqI,PV,SqV,Dist,State) :-
%	attackshow(PA,SqI,_SqT,PV,SqV,State),
	attacks(PA,SqI,_SqT,PV,SqV,State),
	square_piece_distance(SqA,PA,SqI,Dist).

closest_attack(PA,SqA,SqI,PV,SqV,Dist,State,Tables) :- 
	setof(Dist1-SqI-State,
	   attack_distance(PA,SqA,SqI,PV,SqV,Dist1,State),
	   Places),
	   Places = [Dist-SqI-State|_Rest].
	   


testa(Dist,State) :- 
	attack_distance(piece(king,player),square(5,3),SqI,
	                piece(man,opponent),square(2,2),Dist,State).


/*
% A piece is dominated to the extent that there are enemy pieces close to
% its square and squares it can move to.  
% The simplest way to do this analysis may be to build a graph which says for 
% each square, the pieces (now on the board?) which would attack it if they got to
% some square.  
% Well, first find the target squares.  Consider each one separately.
% Dominating a target square:
% For each piece type, find the squares it would be on to attack the target.
% Then consider the pieces on the board separately.  Look up their entry (set of 
% squares) in this table, find the distance to each of these, and take the minimum.
% Score some points which decrease as the distance gets farther.

This can be done most simply to start by having just 1 piece on the board.

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

/*

dominate_value(PieceA,SqA,PieceV,SqV,Goal,Value,Pos,Tables) :- 
	game_player_has_goal(_,Player,Goal),
	eradicate_goal(Goal,Opponent,_Type),
	eradicate_goal(Goal,Descr),
	opposite_role(Player,Opponent),
	dominate_val1(PieceA,Player,SqA,PieceV,SqV,Descr,Value,Pos,Tables).

dominate_val1(PieceA,Player,SqA,PieceV,SqV,Descr,Value,Pos,Tables) :- 
	blank_state_if(Pos),
	on(PieceV,_,SqV,Pos),
	matches(Descr,PieceV),
	closest_attack(PieceA,SqA,_SqI,PieceV,SqV,Dist,_State,Tables),
	distance_value(Dist,Val),
	negate_for_player(Player,Val,Value).
*/

dominate_value(PieceA,SqA,PieceV,SqV,Goal,Value,Pos,Tables) :- 
	blank_state_if(Pos),
	game_player_has_goal(_,Player,Goal),
	opposite_role(Player,Opponent),
	eradicate_goal(Goal,Opponent,_Type),
	eradicate_goal(Goal,Descr),
	weighted_dominate(Descr,PieceA,Player,SqA,PieceV,SqV,Value,Pos,Tables).

weighted_dominate(Descr,PieceA,Player,SqA,PieceV,SqV,Value,Pos,Tables) :- 
	dominate_targets(Descr,Targets,Pos),
	enough_target_urgency(Targets,Urgency),
	member(PieceV@SqV,Targets),
	dominate_val1(PieceA,Player,SqA,PieceV,SqV,Val1,Pos,Tables), 
	Value is Val1*Urgency.  

dominate_targets(Descr,Targets,Pos) :- 
	findall(Piece@Sq,
	  matching_square(Piece,Sq,Descr,Pos),
	  Targets).


dominate_val1(PieceA,Player,SqA,PieceV,SqV,Value,Pos,Tables) :- 
	blank_state_if(Pos),
	closest_attack(PieceA,SqA,_SqI,PieceV,SqV,Dist,_State,Tables),
	distance_value(Dist,Val),
	negate_for_player(Player,Val,Value).
	


matching_square(Piece,Sq,Descr,Pos) :- 
	on(Piece,_,Sq,Pos),
	matches(Descr,Piece).
	

enough_target_urgency(Targets,Weight) :- 
	length(Targets,Length), 
	Length < 4,	% Make a param
	Weight is 1/Length.



test2(PV,SV,Val,S) :- checkpoint(test,S),
	dominate_value(piece(king,player),square(4,2),PV,SV,Goal,Val,S).

test3 :-  setof((A,B,C,D,E),S^attacks(A,B,C,D,E,S),Sets),
	ppl(Sets).

/*
 black king ,( a , 1 ) ,( c , 3 ) ,white king ,( b , 2 )
   black king ,( a , 1 ) ,( c , 3 ) ,white man ,( b , 2 )
   black king ,( a , 2 ) ,( c , 4 ) ,white king ,( b , 3 )
   black king ,( a , 2 ) ,( c , 4 ) ,white man ,( b , 3 )
*/

test4 :- 
	setof((D,A,E,B),(C,S)^attacks(A,B,C,D,E,S),Sets),
	ppl(Sets).


test5 :- 
	setof((D,A,E,B),(C,S)^attacks_i(A,B,C,D,E,S),Sets),
	ppl(Sets).

/*
   1,3,45,38
   1,3,45,52
   1,3,45,54
   1,3,46,37
   1,3,46,39
*/


attackset(Attacks,State) :- 
	setof((PieceV^PieceA^SqV^SqA),
		(SqT,State)^attacks_i(PieceA,SqA,SqT,PieceV,SqV,State),
		 Attacks).


% Attackset gives us:  [Targ^Attack^TargSq^AttackSq|Rest]
dom_table(DomTable,State) :- 
	attackset(Attacks,State),
	do_graph(Attacks,GroupedTargs),
	group_attacks(GroupedTargs,DomTable).

% Grouped attacks gives us:  [Targ-[Attack^TargSq^AttackSq|RestAttacks]|RestTargs]

group_attacks([],[]).
group_attacks([T-As|Rest],[T-GroupedAs|GRest]) :- 
	do_graph(As,Grouped1),
	group_targsq(Grouped1,GroupedAs),
	group_attacks(Rest,GRest).

group_targsq([],[]).
group_targsq([A-Sqs|Rest],[A-GroupedSqs|GRest]) :- 
	do_graph(Sqs,GroupedSqs),
	group_targsq(Rest,GRest).


