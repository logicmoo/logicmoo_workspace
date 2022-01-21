%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% step.pl


%--------------------------------------------------------------------------------
% Piece has independent static advice for each player. 

%--------------------------------------------------------------------------------
% ind_advice(Player,Piece,Advice,Tables)
%--------------------------------------------------------------------------------
% 
% Nondeterm routine, tells us various sources of static advice accrued to Player
% by having a piece on the board owned by him, independent of the board
% and of possible interactions between Piece and other pieces.  
%
% o  Useful to have  pieces which can capture many victims (including pieces of
%    his own).  The more pieces, the better. (victims)
% o  Useful to have piece which cannot be captured by many enemy pieces. (immunity)
% o  Useful to have piece which  can be captured by many of our own pieces (giveaway)
% o  Useful to have a piece with high max static mobility, or (max_static_mob)
%    high max eventual mobility. (max_eventual_mob)
% o  Useful to have a piece with high average static mobility, or (avg_static_mob)
%    high average eventual mobility. (avg_eventual_mob)
% o  Bad to have a piece when *we* have a goal to eradicate it. (Also converse) (eradicate)
% o  Bad to have a piece when goal is stalemate ourself. (Also converse) (stalemate)
% o  Good to have  piece if we have an arrival goal for it, (arrive)
%    or if it can generate such a one by promotion. 
%    -- In this case, the value of this as a target decreases with the number of intermediate proms
%    necessary to create a piece matching the arrive goal.  (Also converse)
%    A separate rule does the opposite, when we generate a goal for the opponent, and
%    encourages us to stay away. 
% *. Also good to player-possess-capture a piece (owned by either player) for which we have 
%    an arrive goal!  Perhaps use generator fn to measure how good to capture. 
% Note: There is a recurrence here which says useful to capture enemy piece if it can capture
%     our piece which is useful to capture it.  
ind_advice(_Player,Piece,advice(victims,Piece,Value),_Tables) :-
	victim_counts(Piece,Value).
ind_advice(Player,Piece,advice(immunity,Piece,Value),_Tables) :-
	immunity_value(Player,Piece,Value).
ind_advice(Player,Piece,advice(giveaway,Piece,Value),_Tables) :-
	giveaway_value(Player,Piece,Value).
ind_advice(_Player,Piece,advice(max_static_mob,Piece,Value),Tables) :-
	max_static_mob(Piece,Value,Tables).
ind_advice(_Player,Piece,advice(max_eventual_mob,Piece,Value),Tables) :-
	max_eventual_mob(Piece,Value,Tables).
ind_advice(_Player,Piece,advice(avg_static_mob,Piece,Value),Tables) :-
	average_static_mob(Piece,Value,Tables).
ind_advice(_Player,Piece,advice(avg_eventual_mob,Piece,Value),Tables) :-
	average_eventual_mob(Piece,Value,Tables).
ind_advice(Player,Piece,advice(eradicate,Player-Piece,Value),_Tables) :-
	player_eradicate_target(Player,Piece),
	Value = -1.
ind_advice(Player,Piece,advice(eradicate,Opp-Piece,Value),_Tables) :-
	opposite_role(Player,Opp),
	player_eradicate_target(Opp,Piece),
	Value = 1.
ind_advice(Player,Piece,advice(stalemate,Player,Value),_Tables) :-
	player_stalemate_target(Player,Piece),
	Value = -1.
ind_advice(Player,Piece,advice(stalemate,Opp,Value),_Tables) :-
	opposite_role(Player,Opp),
	player_stalemate_target(Opp,Piece),
	Value = 1.
ind_advice(Player,Piece,advice(arrive,Player-Piece,Value),Tables) :-
	player_arrive_generator(Player,Piece,Dist,Tables),
	Value is 1/(1+Dist).
ind_advice(Player,Piece,advice(arrive,Opp-Piece-Dist,Value),Tables) :-
	opposite_role(Player,Opp),
	player_arrive_generator(Opp,Piece,Dist,Tables),
	Value is -1/(1+Dist).

	
ind_advice(Player,Piece,Value) :-
	advice_tables(Tables), 
	ind_advice(Player,Piece,Value,Tables).


independent_advice(Player,Piece,Value,Tables) :-
	owns(Piece,Player),
	piece_index(Piece,_),
	ind_advice(Player,Piece,Val1,Tables),
	negate_advice_for_player(Player,Val1,Value). 

independent_advice(Player,Piece,Value) :-
	advice_tables(Tables), 
	independent_advice(Player,Piece,Value,Tables).



%--------------------------------------------------------------------------------
% Mediating independent advice 
%--------------------------------------------------------------------------------
% 
% Just like the advice meditation done dynamically, but here applying only to the
% static advisors.  

static_evaluation(Piece,Sq,Value,Position,Tables) :- 
	get_static_advices(Piece,Sq,Advice,Position,Tables),
	mediate_advices(Advice,Value,Tables).

	
get_static_advices(Piece,_Sq,Advices,_Position,Tables) :- 
	piece_index(Piece,_),
	findall(
		   Advice,
		   independent_advice(_Player,Piece,Advice,Tables),
		   Advices ).

get_static_advices(Piece,Advices) :- 
	find_advice_tables(Tables),
	get_static_advices(Piece,_Sq,Advices,_Position,Tables).

show_static_advices(Piece) :- 
	get_static_advices(Piece,As), ppl(As).


show_static_advices_total(Piece) :- 
	get_static_advices(Piece,As), ppl(As),
	mediate_advices(As,Total,_),
	format("Total (Mediated) for <~p>: ~p~n",[Piece,Total]).
	

advice_top(Color,Type) :- 
	piece(Piece,[Color,Type],[]),
	show_static_advices_total(Piece).

adviceold_top(Color,Type) :- 
	piece(Piece,[Color,Type],[]),
	show_static_advices(Piece).
	


%--------------------------------------------------------------------------------
% Piece has relative static value for each player, based on the independent
% values of pieces and certain possible relations between them.  
% One of these rels is capturing. 
%
%
% Giving points for pieces based on which other piece types they can capture, 
% perhaps with a measure of how effective they will be at making these
% captures.

% Value added to a piece for being able to capture a Victim.
% This is a *static* measure, which can be computed before the game
% ever starts! 
%
% At moment we aren't conditioning on different possible effects of the capture,
% which may be important. 
% 
cap_value(Piece,Player,Victim,Value,Tables) :-
	find_advice_tables_if(Tables), 
	piece_victim(Piece,Player,Victim,VPlayer,Effect),
	victim_value(Player,Victim,VPlayer,Value,Tables).
	
cap_value(Piece,Player,Victim,Value) :-
	advice_tables(Tables), 
	cap_value(Piece,Player,Victim,Value,Tables).


% How good for Player to be able to capture Piece owned by VPlayer.
% If Piece has Pos val for VPlayer, and we remove it, we make
% neg val for VPlayer.  Then we negate again if VPLAYER \== PLAYER,
% as the value will have been wrt the wrong perspective. 
%
% If this is optional, we don't force the player to take unacceptable
% pieces. 
victim_value(Player,Piece,VPlayer,Value,Tables) :-
	independent_advice(VPlayer,Piece,Advice,Tables),
	advice_victim_value(Advice,Val1,Tables),
	remove_option_value(Player,VPlayer,Val1,Value).

advice_victim_value(Advice,Val,Tables) :-
	weigh_advice(Advice,Val,Tables).


% Removal may be optional or forced.  When forced, he must accept
% whatever the consequences.  When optional, he can choose to accept
% only the positive ones. 
remove_option_value(Player,Owner,Val1,Value) :-
	( forced_remove(Player,Owner) ->
	  forced_remove_value(Player,Val1,Value)
	; optional_remove_value(Player,Val1,Value)
	).
	
% Removal may be forced when the game has a global must_capture rule,
% and the victim piece is owned by the enemy.  This is becasue he may throw
% his pieces at us and force us to absorb the value of them. 
forced_remove(Player,Owner) :- 
	current_game_must_capture,
	Player \== Owner.

% In this case, the player has no choice but to accept the loss of material. 
forced_remove_value(_Player,Val1,Value) :- 
	Value is -Val1.
	  
% Player has the option of removing Val1 points from the board.
% When this is optional, WHITE prefers to take 0 points over
% removing positive points, and BLACK prefers to take 0 points
% over removing negative points.  The result of this decision thus
% gives only not-unfavorable points for Player for having this option.
%
optional_remove_value(Player,Val1,Value) :- 
	RemoveVal is -Val1,
	negate_for_player(Player,RemoveVal,PVal),
	max(0,PVal,OptVal),
	negate_for_player(Player,OptVal,Value).
	


piece_victim(Piece,Victim) :-
	piece_victim(Piece,Victim,_Effect).

piece_victim(Piece,Victim,Effect) :-
	piece_victim(Piece,_Player,Victim,_VPlayer,Effect).

piece_victim(Piece,Player,Victim,VPlayer,Effect) :-
	piece_victim(Piece,Player,Victim,VPlayer,Effect,_Capture).

piece_victim(Piece,Player,Victim,VPlayer,Effect,Capture) :-
	piece_index(Piece,_PI),
	owns(Piece,Player),
	owns(Victim,VPlayer),
	current_game_for_player(Player,Game),
	game_piece_has_capture(Piece,Capture,Game),
	capture_type(Capture,Type),
	capture_effect(Capture,Effect),
	matches(Type,Victim).



% True when the effect of capturing the victim  is not opponent possesses. 
threat_piece_victim(Piece,Player,Victim,VPlayer,Effect,Capture) :-
	piece_victim(Piece,Player,Victim,VPlayer,Effect,Capture),
	\+ ( opposite_role(Player,VPlayer),
	     Effect = possess(VPlayer)
	   ).


victim_counts(Piece,VCount) :-
	unique_victims(Piece,Unique),
	length(Unique,VCount).
	       

unique_victims(Piece,Unique) :- 
	unique_victims(Piece,_Player,_Victim,_VPlayer,_Effect,Unique).

unique_victims(Piece,Player,Victim,VPlayer,Effect,Unique) :- 
	findall(
		   Victim,
		   threat_piece_victim(Piece,Player,Victim,VPlayer,Effect,_Cap),
		   Victims
	       ),
	remove_duplicates(Victims,Unique).


unique_victimizers(Piece,Unique) :- 
	unique_victimizers(Piece,_Player,_Victim,_VPlayer,_Effect,Unique).

unique_victimizers(Piece,Player,Victim,VPlayer,Effect,Unique) :- 
	findall(
		   Piece,
		   threat_piece_victim(Piece,Player,Victim,VPlayer,Effect,_Cap),
		   Victims
	       ),
	remove_duplicates(Victims,Unique).



giveaway_value(Player,Piece,Value) :- 
%	piece_type_count(Count),
	unique_victimizers(_CapPiece,Player,Piece,Player,_Effect,Unique),
	length(Unique,VCount),
	Value is VCount.


immunity_value(Player,Piece,Value) :- 
	piece_type_count(Count),
	opposite_role(Player,Opp),
	unique_victimizers(_CapPiece,Opp,Piece,Player,_Effect,Unique),
	length(Unique,VCount),
	Value is Count - VCount.



% True when Player has goal to eradicate piece Piece. 
player_eradicate_target(Player,Piece) :-
	game_player_has_goal(_,Player,Goal),
	eradicate_goal(Goal,Descr),
	matches(Descr,Piece).

% True when Player has goal to eradicate piece Piece,
% because that helps him to stalemate its owner.  
player_stalemate_target(Player,Piece) :-
	game_player_has_goal(_,Player,Goal),
	stalemate_goal(Goal,Owner),
	owns(Piece,Owner).


% True when Player has goal to arrive either Piece or something
% Piece could promote into without changing ownership.
player_arrive_generator(Player,Piece,Dist,Tables) :-
	game_player_has_goal(_,Player,Goal),
	arrive_goal(Goal,Descr,_Squares),
	piece_player_prom_distance(Piece,Player,PieceT,Dist,Tables),
	matches(Descr,PieceT).
	





max_static_mob(Piece,Val,Tables) :-
	findall(
		   Val1,
		   square_piece_mobility(_Sq,Piece,Val1,Tables),
		   Mobs),
        max(Mobs,Val).		   
	       


max_eventual_mob(Piece,Val,Tables) :-
	findall(
		   Val1,
		   square_piece_reachability(_Sq,Piece,Val1,Tables),
		   Mobs),
        max(Mobs,Val).		   




average_static_mob(Piece,Val,Tables) :-
	findall(
		   Val1,
		   square_piece_mobility(_Sq,Piece,Val1,Tables),
		   Mobs),
        average(Mobs,Val).		   
	       


average_eventual_mob(Piece,Val,Tables) :-
	findall(
		   Val1,
		   square_piece_reachability(_Sq,Piece,Val1,Tables),
		   Mobs),
        average(Mobs,Val).		   





%--------------------------------------------------------------------------------
% Mediating capture advice 
%--------------------------------------------------------------------------------
% 
% Just like the advice meditation done dynamically, but here applying only to the
% capture advisors.  

capture_evaluation(Piece,Sq,Value,Position,Tables) :- 
	get_capture_advices(Piece,Sq,Advice,Position,Tables),
	mediate_capture_advices(Advice,Value,Tables).

mediate_capture_advices(Advice,Value,_Tables) :-
	sumlist(Advice,Value).


get_capture_advices(Piece,_Sq,Advices,_Position,Tables) :- 
	piece_index(Piece,_),
	findall(
		   Advice,
		   cap_value(Piece,_Player,_Victim,Advice,Tables),
		   Advices ).

get_capture_advices(Piece,Advices) :- 
	find_advice_tables(Tables),
	get_capture_advices(Piece,_Sq,Advices,_Position,Tables).

show_capture_advices(Piece) :- 
	get_capture_advices(Piece,As), ppl(As).





%==============================================================================
% INDEPENDENT PIECE VALUE MATRIX
%==============================================================================

build_static_matrix(Matrix) :- 
	map_piece_table(static_matrix,[],Matrix).

build_static_matrix(Matrix,Tables) :- 
	map_piece_table(static_matrix,[Tables],Matrix).


print_static_matrix :- 
	print_static_matrix(_Piece,_Player,_StatVal).

print_static_matrix(Piece,Player,StatVal) :- 
	format("Player Piece Static Matrix:~n",[]),
	( piece_player_static(Piece,Player,StatVal),
	  format("~p: ~p -> ~p~n",[Player,Piece,StatVal]),
	  fail
        ; true
	).

pps(Player,Piece,StatVal) :- piece_player_static(Piece,Player,StatVal).

% PIECE_PLAYER_STATIC(?Piece,?Player,?StatVal)
% Very nice table indicating static value for Piece owned by Player. 
piece_player_static(Piece,Player,StatVal) :- 
	advice_tables(Tables),
	piece_player_static(Piece,Player,StatVal,Tables).

% PIECE_PLAYER_STATIC(?Piece,?Player,?StatVal,+Tables)
% Very nice table indicating static value for Piece owned by Player. 
piece_player_static(Piece,Player,StatVal,Tables) :- 
	piece_player_static(Piece,_PieceI,Player,_PlayerI,StatVal,Tables).

piece_player_static(Piece,PieceI,Player,_PlayerI,StatVal,Tables) :- 
	  piece_index(Piece,PieceI),
	  owns(Piece,Player),
	  piece_static_value(PieceI,StatVal,Tables).

piece_static_value(PieceI,StatVal,Tables) :- 
	static_matrix(Tables,M),
	piece_static_val1(PieceI,StatVal,M).

piece_static_val1(PieceI,StatVal,M) :- 
	pindex_table_entry(PieceI,M,StatVal).


static_matrix(Piece,PIndex,Value) :- 
	find_advice_tables(Tables),
	static_matrix(Piece,PIndex,Value,Tables).

% Tables not last so can be used with map_piece_table.
static_matrix(Piece,PIndex,Tables,Value) :- 
	piece_index(Piece,PIndex),
	static_evaluation(Piece,_Sq,Value,_Position,Tables),
	tracing_anal_format(tables,"Built independent matrix for <~p>~n",[Piece]).

