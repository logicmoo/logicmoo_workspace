%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% value.pl

%=============================================================================
% Loading evaluation files
%=============================================================================

% LOAD_EVAL(+GameName)
% Finds a file GameName.eval in a library directory.
% Then loads this file as the current evaluation function.
load_eval(Name) :-
	find_eval_file(Name,File),
	file_make_test_eval(File).

find_eval_file(Name,File) :- 
	find_suffixed_library_file(Name,eval,File).

% Maybe abolishing these is a bit strict, should
% just turn off redefinitions ...
file_make_test_eval(File) :- 
	abolish(piece_value/2),
	abolish(piece_square_value/2),
	compile(File).



%=============================================================================
% Search-Specific Predicates used in search engine
%=============================================================================

% ---------------------------------------
% SUCCESSOR_POS(Move,State,State2,Tables)
% ---------------------------------------
% External predicate used by alphabeta search engine.
% Backtracks over all successor STATE2 reachable from STATE.
% Move should be a unique name of this transition. 
% Uses evaluation tables. 

successor_pos(Move,State,State2,_Tables) :- 
	legal(Move,State,State2).


%-----------------------------------
% TERMINAL_POS_VALUE(Pos,Val,Tables)
%-----------------------------------
% External predicate for alphabeta code.  
% Returns a Val for a Pos if it is terminal, fails otherwise.  
%
terminal_pos_value(Pos,Val,_Tables) :- 
	terminal_game_outcome(Pos,Val).


% value-of-outcome defined in alphabeta.pl
%
terminal_game_outcome(Pos,Val) :-
	game_outcome(Outcome,Pos),
	value_of_outcome(Outcome,Val), !.



% -------------------------
% STATICVAL(Pos,Val,Tables)
% -------------------------
% This defines the external predicate for search engine.
% Determines what evaluation procedure will be used. 
%
staticval(Pos,Val,Tables) :- 
	evaluation(Val,Pos,Tables).

% ------------------
% staticval(Pos,Val)
% ------------------
staticval(Pos,Val) :- 
	evaluation(Val,Pos).



%=============================================================================
% Defining value function for Symmetric Chess-Like Games
%=============================================================================


% Each advisor is a separate rule making a comment about a position.
% They all contribute their advice to the position.
% These advices are then weighed appropriately (by the mediation routine),
% and then summed to give the final value of the position.
% [Note: the term "advices" is due to:
%        Professor Professor H. J. van den Herik.
% This may or may not be an official word in the English language.]

evaluation(Value,Position) :- 
	find_advice_tables(Tables),
	evaluation(Value,Position,Tables).

evaluation(Value,Position,Tables) :- 
	get_advices(Advice,Position,Tables),
	mediate_advices(Advice,Value,Tables).


% Like the global version, but just considers the evaluation if 
% Piece were to be on Sq in the Position.  
% It might be worth actually pretending to put the piece there
% to be more accurate, but that is somewhat more costly.  
local_evaluation(Piece,Sq,Value,Position,Tables) :- 
	get_local_advices(Piece,Sq,Advice,Position,Tables),
	mediate_advices(Advice,Value,Tables).

	
get_advices(Advices,Position) :- 
	find_advice_tables(Tables),
	get_advices(Advices,Position,Tables).
	
get_advices(Advices,Position,Tables) :- 
	findall(Advice,
	( add_dynamic_tables(Position,Tables),
	  value(Advice,Position,Tables)
	),
	   Advices).


get_local_advices(Piece,Sq,Advices,Position,Tables) :- 
%	bagof(Advice,
	findall(Advice,
	   value(Piece,Sq,Advice,Position,Tables),
	   Advices).

show_advices_name(PosName) :- 
	checkpoint(PosName,Pos), 
	show_advices(Pos).


show_advices(Pos) :- 
	get_advices(As,Pos), ppl(As).

show_advices(Pos,Tables) :- 
	get_advices(As,Pos,Tables),
	ppl(As).


show_local_advices(Piece,Sq,Pos) :- 
	find_advice_tables(Tables),
	show_local_advices(Piece,Sq,Pos,Tables).


show_local_advices(Piece,Sq,Pos,Tables) :- 
	on(Piece,Sq,Pos),
	get_local_advices(Piece,Sq,Advices,Pos,Tables),
	ppl(Advices).




mediate_advices(Advices,Value,Tables) :- 
	weigh_advices(Advices,Vals,Tables),
	sumlist(Vals,Value).

weigh_advices([],[],_).
weigh_advices([A|As],[V|Vals],Tables) :- 
	weigh_advice(A,V,Tables),
	weigh_advices(As,Vals,Tables).
	
weigh_advice(advice(Advisor,_Comment,V),Val,Tables) :- 
	advisor_weight(Advisor,Weight,Tables),
	Val is V*Weight. 

% Change this to use tables explicitly.
advisor_weight(Advisor,Weight,Tables) :- 
	parameter(Advisor,Weight).


advisor(A,B) :- dynamic_advisor(A,B).
advisor(A,B) :- static_advisor(A,B).


dynamic_advisor(gen_material,0).
dynamic_advisor(material,0).
dynamic_advisor(square,0).
dynamic_advisor(lthreat,0).
dynamic_advisor(potent,0).
dynamic_advisor(dynamic_mobility,0).

dynamic_advisor(gmovmob,0).
dynamic_advisor(gcapmob,0).
dynamic_advisor(gthreat,0).
dynamic_advisor(pthreat,0).

dynamic_advisor(static_mobility,0).
dynamic_advisor(eventual_mobility,0).
dynamic_advisor(arrive_distance,0).
dynamic_advisor(promote_distance,0).
dynamic_advisor(possess,0).
dynamic_advisor(initprom,0).
%dynamic_advisor(dominate,0).
dynamic_advisor(eradicate,0).
dynamic_advisor(vital,0). 

static_advisor(random,0).
static_advisor(static,0).
static_advisor(victims,0).
static_advisor(max_static_mob,0).
static_advisor(max_eventual_mob,0).
static_advisor(avg_static_mob,0).
static_advisor(avg_eventual_mob,0).
static_advisor(eradicate,0).
static_advisor(stalemate,0).
static_advisor(arrive,0).
static_advisor(giveaway,0).
static_advisor(immunity,0).
%advisor(,0).


initialize_advisors :- 
	whenever(advisor(Name,Default),
	   add_parameter(Name,Default)),
	assert(initialized_advisors).

active_parameter(P) :- advisor(P,_), parameter(P,Val), Val \== 0.

:- current_predicate(initialized_advisors,_) -> true ; initialize_advisors.


%----------------------------------------
% Advisor requirements
%----------------------------------------

movtable :- 
	( needs_movtable(Param),
	  active_parameter(Param) 
	-> true ).

needs_movtable(dynamic_mobility).
needs_movtable(gmovmob).


captable :- 
	( needs_captable(Param),
	  active_parameter(Param) 
	-> true ).

%needs_captable(threat).
needs_captable(gcapmob).
needs_captable(vital).
needs_captable(gthreat).
needs_captable(pthreat).
needs_captable(lthreat).
needs_captable(gcapmob).
needs_captable(eradicate).
needs_captable(potent).


%----------------------------------------
% VALUE/3
%----------------------------------------


% Global moving mobility
value(advice(gmovmob,Player,Value),Position,Tables) :- 
	active_parameter(gmovmob),
	gmobility(Player,Val,Position,Tables),
	negate_for_player(Player,Val,Value).
	

% Global capturing mobility
value(advice(gcapmob,Player,Value),Position,Tables) :- 
	active_parameter(gcapmob),
	gcapmobility(Player,Val1,Position,Tables),
	favor_control(Player,Val1,Val,Position),
	negate_for_player(Player,Val,Value).

% Global Threats
value(advice(gthreat,Player,Value),Position,Tables) :- 
	active_parameter(gthreat),
	gthreat(Player,Value,Position,Tables).

% Best Potent Threat
value(advice(pthreat,Player,Value),Position,Tables) :- 
	active_parameter(pthreat),
	pthreat(Player,Value,Position,Tables).


% One source of value comes from POSSESSING a piece.
value(Value,Position,Tables) :- 
	active_parameter(possess),
	in_hand(Piece,Player,Position),
	\+ still_assigning(Position),
	current_predicate(possess_value,possess_value(_,_,_,_,_)),
	possess_value(Piece,Player,Value,Position,Tables).

% One source of value comes from being able to 
% OPPONENT-PROMOTE a piece.
value(Value,Position,Tables) :- 
	active_parameter(initprom),
	current_predicate(initprom_value,initprom_value(_Value,_Position,_Tables)),
	initprom_value(Value,Position,Tables).

% VITAL 
% Losing points when our vital pieces (those enemy wants to remove and is near
% to achieving) are threatened.  
value(advice(vital,(Goal,Piece@Square),Value),Position,Tables) :- 
	active_parameter(vital),
	threatened_vital_piece_value(Piece,Square,_Player,Goal,Value,Position,Tables).



% RANDOM
% Augments a position's evaluation by a random number in the range
% [MIN,MAX] as set by the parameters RANDOM_MIN and RANDOM_MAX. 
value(advice(random,range(Min,Max),Value),_Position,_Tables) :- 
	active_parameter(random),
	random_eval(Min,Max,Value).


% One source of value comes from having a piece on a square.
% Using on/4 here ensures it is a piece struct instead of empty. 
value(Value,Position,Tables) :- 
	on(Piece,_Player,Square,Position),
	value(Piece,Square,Value,Position,Tables).




%----------------------------------------
% VALUE/5
%----------------------------------------

% General material: 1 point for player's piece, -1 for opponents.
%value(piece(Type,player),_Square,advice(gen_material,Type,1),_Position,_Tables).
%value(piece(Type,opponent),_Square,advice(gen_material,Type,-1),_Position,_Tables).
%	
value(Piece,_Sq,advice(gen_material,Type,Val),_Position,_Tables) :- 
	active_parameter(gen_material), 
	gen_material_value(Piece,_Player,Type,Val).



% INDEPENDENT STATIC PIECE VALUE
% This isn't the right way to do this (use table instead), but for now
% will cut down on extra eval stuff. 
value(Piece,_Square,advice(static,Piece,Value),_Pos,Tables) :- 
	active_parameter(static),
%	independent_advice(_Player,Piece,Advice).
	piece_player_static(Piece,_Player,Value,Tables).


% Dynamic Piece mobility
value(Piece,Square,advice(dynamic_mobility,Piece@Square,Value),Position,Tables) :- 
	active_parameter(dynamic_mobility),
	active_advisor(dynamic_mobility,Tables),
	dynamic_piece_mobility(Piece,Square,Value,Position,Tables).

% Static piece  mobility
value(Piece,Square,advice(static_mobility,Piece@Square,Value),Position,Tables) :- 
	active_parameter(static_mobility),
	static_piece_mobility(Piece,Square,Value,Position,Tables).

% Eventual piece  mobility
value(Piece,Square,advice(eventual_mobility,Piece@Square,Value),Position,Tables) :- 
	active_parameter(eventual_mobility),
%	current_predicate(eventual_piece_mobility,_),
	eventual_piece_mobility(Piece,Square,Value,Position,Tables).

% Piece attacks
% Now *requires* that the opponent have the tables constructed. 
value(Piece,Square,advice(lthreat,captures(Piece@Square,PieceV@SqV),Value),Position,Tables) :- 
	active_parameter(lthreat),
	active_advisor(threat,Tables),
 	local_threat_value(Piece,Square,PieceV,SqV,Value,Position,Tables).


% Potent Threats
value(Piece,Square,advice(potent,captures(Piece@Square,PieceV@SqV),Value),Position,Tables) :- 
	active_parameter(potent),
	active_advisor(threat,Tables),
 	potent_threat_mover(Piece,Square,PieceV,SqV,Value,Position,Tables).




% Arrive Distance
value(Piece,Square,advice(arrive_distance,(Goal,Piece@Square,SqT),Value),Position,Tables) :- 
	active_parameter(arrive_distance),
%	current_predicate(_,arrive_value(_,_,_,_,_,_)),
	arrive_value(Piece,Square,SqT,Goal,Value,Position,Tables).

% Promotion Distance
value(Piece,Square,advice(promote_distance,(Piece@Square,SqT),Value),Position,Tables) :- 
	active_parameter(promote_distance),
	active_advisor(prom,Tables),
%	current_predicate(_,prom_value(_,_,_,_,_)),
	prom_value(Piece,Square,SqT,Value,Position,Tables).


% Domination Value (not supported)
value(Piece,Square,advice(dominate,(Goal,Piece@Square,PieceV@SqV),Value),Position,Tables) :- 
	active_parameter(dominate),
%	current_predicate(_,dominate_value(_,_,_,_,_,_,_)),
	dominate_value(Piece,Square,PieceV,SqV,Goal,Value,Position,Tables).


% Eradicating enemy pieces
% Call this dominate advisor here, as we use static eradicate in step.pl
value(Piece,Square,advice(dominate,(Goal,Piece@Square),Value),Position,Tables) :- 
	active_parameter(dominate),
%	active_advisor(prom,Tables),
%	current_predicate(_,prom_value(_,_,_,_,_)),
	eradicate_safety(_Player,Piece,Square,Goal,Value,Position,Tables).


% Specific material: refers to table.  
value(Piece,_Square,advice(material,Piece,Value),_Position,_Tables) :- 
	active_parameter(material),
	current_predicate(_,piece_value(_,_)),
	piece_value(Piece,Value).


% Piece-Square tables
value(Piece,Square,advice(square,Piece@Square,Value),_Position,Tables) :- 
	active_parameter(square),
%	current_predicate(_,piece_square_value(_,_,_)),
	piece_square_value(Piece,Square,Value,Tables).


%============================================================================
% Specific Piece-value tables
%============================================================================
% To change this for another game, make a new file containing just
% facts like these but with piece names specialized for your new game.
% Then load this file separately.
% Examples files are games/chess.eval, and games/turncoat.eval

/*
piece_value(piece(king,player),15).
piece_value(piece(king,opponent),-15).
piece_value(piece(queen,player),9).
piece_value(piece(queen,opponent),-9).
piece_value(piece(rook,player),5).
piece_value(piece(rook,opponent),-5).
piece_value(piece(night,player),3).
piece_value(piece(night,opponent),-3).
piece_value(piece(bishop,player),3.25).
piece_value(piece(bishop,opponent),-3.25).
piece_value(piece(pawn,player),1).
piece_value(piece(pawn,opponent),-1).
*/

%============================================================================
% Piece-Square tables
%============================================================================
% This model is again for chess.  Make a separate file containing
% just rules of these types for your game, and load it.
% This could of course be the same file at that used for specific
% piece material values above.  

/*
% Piece-Square tables
piece_square_value(piece(night,player),square(4,4),2).
piece_square_value(piece(night,player),square(1,1),-5).

% Pawns given value as they move closer to their promotion
% rank.  
piece_square_value(piece(pawn,player),square(_X,Y),Val) :- 
	Val is (Y-1)/6. 
piece_square_value(piece(pawn,opponent),square(_X,Y),Val) :- 
	Val is (Y-8)/6. 
*/



%--------------------------------------------------------------------------------
% Support routines
%--------------------------------------------------------------------------------

% RANDOM_EVAL
% Augments a position's evaluation by a random number in the range
% [MIN,MAX] as set by the parameters RANDOM_MIN and RANDOM_MAX. 
random_eval(Min,Max,Value) :- 
	parameter(random_min,Min),
	parameter(random_max,Max),
	random(Min,Max,Value). 


% +1 if piece owned by white, -1 if owned by black. 
gen_material_value(Piece,Player,Type,Val) :- 
	owns(Piece,Player),
	piece_name(Piece,Type),
	negate_for_player(Player,1,Val).
	

% Dynamic Piece Mobility
% Only returns advice for pieces with non-0 value.
% To just count the moves a piece has, use dynamic_piece_mob.
dynamic_piece_mobility(Piece,Square,Value,Position,Tables) :- 
	owns(Piece,Player),
	dynamic_piece_mob(Piece,Player,Square,Val,Position,Tables),
	Val > 0, 
	negate_for_player(Player,Val,Value).


% Note:  this currently uses dynamic moving tables.  It will
% thus only return values for pieces actually on the board.
% (We know the tables exist by this point, because of the requires
% statement declaring that this advisor needs moving tables.
%
% What we should really do is:
% 1. Check if tables exist, and piece in current position, whenever call
%    things which might be used locally.  
% 2. If either not true, compute it ourself. 
% 
% However, this is not a problem in the current architecture, because:
% a. local eval only used in threat and promotion analysis.
% b. In threat anal, we know the piece is on the square already.
% c. In prom analysis, we shutdown dynamic mob, because don't have time! 
% So, if we ever change this for prom analysis, we must incorporate the
% change above.  This is easy, but slows things down a little bit. 
%
dynamic_piece_mob(Piece,Player,Square,Count,_Position,Tables) :- 
	moving_table(Tables,Moves),
	unique_moves(Player,Piece,Square,_SqT,Moves,Targets),
	length(Targets,Count),
	tracing_path_format(moves,
	    "Dynamic: <~p>: ~p -> ~p~n",[Piece,Square,Count]),
	true.




% Static Piece Mobility
static_piece_mobility(Piece,Square,Value,Position,Tables) :- 
	owns(Piece,Player),
	static_piece_mob(Piece,Player,Square,Val,Position,Tables),
	negate_for_player(Player,Val,Value).

static_piece_mob(Piece,_Player,Square,Value,_Position,Tables) :-  
	square_piece_mobility(Square,Piece,Value,Tables),
	tracing_path_format(static,
	    "In 1: <~p>: ~p -> ~p~n",[Piece,Square,Value]).


% Eventual Piece Mobility
eventual_piece_mobility(Piece,Square,Value,Position,Tables) :- 
	owns(Piece,Player),
	eventual_piece_mob(Piece,Player,Square,Val,Position,Tables),
	negate_for_player(Player,Val,Value).


eventual_piece_mob(Piece,_Player,Square,Value,_Position,Tables) :-  
	square_piece_reachability(Square,Piece,Value,Tables),
	tracing_path_format(eventual,
	    "In 4: <~p>: ~p -> ~p~n",[Piece,Square,Value]).


%================================================================================
% Generic 2-player-game Support Predicates
%================================================================================

initiative_offset(Control,Player,Offset) :- 
	( Control=Player ->
	    Offset = 0.9
	  ; Offset = 0.7).

	
favor_control(Player,Val1,Value,Position) :-
	control(Control,Position), 
	initiative_offset(Control,Player,Offset),
	Value is Val1*Offset. 
	
% The player will only execute the threat if doing so might net 
% him an increase in value. 
favorable_to_owner(player,Val) :- Val > 0.
favorable_to_owner(opponent,Val) :- Val < 0.


% How many moves extra it costs a player before it is his turn to
% move.  In 2-player game, just 0 or 1, of course. 
control_cost(Player,Cost,Position) :-
	control(Control,Position), 
	( Control=Player ->
	    Cost = 0
	; Cost = 1
	).

	

negate_if_same(Player1,Player2,Val1,Val) :-
	( Player1 \== Player2 ->
	  Val1 = Val
	; Val is -Val1
	).


negate_if_different(Player1,Player2,Val1,Val) :-
	( Player1 = Player2 ->
	  Val1 = Val
	; Val is -Val1
	).

negate_for_player(player,Val,Val).
negate_for_player(opponent,Val,Val1) :- 
	Val1 is -Val.

negate_advice_for_player(Player,advice(A,C,V),advice(A,C,V1)) :-
	negate_for_player(Player,V,V1).


% Of some list of values, PLAYER wants the max, OPPONENT wants the min. 
max_for_player(player,List,Best) :- max(List,Best).
max_for_player(opponent,List,Best) :- min(List,Best).

% Of some list of values, PLAYER wants the min, OPPONENT wants the max. 
min_for_player(player,List,Best) :- min(List,Best).
min_for_player(opponent,List,Best) :- max(List,Best).

	
%================================================================================
% Interface
%================================================================================

evalfile_top(Game) :- load_eval(Game).

evalfile_com(_,_,_,Game) :- load_eval(Game).

evaluate_com(_Move,SIn,_) :-
	timing(evaluation(Value,SIn)),
	format("Position's value (positive favors white):  ~p~n", [Value]).


advice_com(_Move,SIn,_) :-
	timing(get_advices(As,SIn)),
	ppl(As).


advice_com(_,SIn,_SOut,Row,Col) :-
	with_alpha_squares(
	  gsquare(Square,['(',Row,',',Col,')'],[])),
	  timing(show_local_advices(_Piece,Square,SIn)).
	  

advisor_weight(Adv,Weight) :- 
	advisor(Adv,_),
	advisor_weight(Adv,Weight,_).

show_advisors :- 
	format("Advisors: ~n",[]),
	whenever( advisor_weight(Adv,Weight),
	   format("<~p>: ~p~n",[Adv,Weight])
		).

alladvisors_com(_,_,_) :- show_advisors.
alladvisors_top :- show_advisors.


seta_com(_,_,_) :- 
	show_active_advisors.

seta_top :- show_active_advisors.
	
active_com(_,_,_) :- 
	show_active_advisors.

active_top :- show_active_advisors.


show_active_advisors :- 
	format("Active Advisors: ~n",[]),
	whenever( ( advisor_weight(Adv,Weight), Weight \== 0 ),
	   format("<~p>: ~p~n",[Adv,Weight])
		).
