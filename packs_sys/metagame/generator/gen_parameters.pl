%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

% gen_parameters.pl
% Defines the parameters used in the chess-like game generator.


% board_size:  Avg. size of square board. Chess has 8 squares.
% board_crowding: Fraction of board to use for placing initial arrays. 
%    Chess uses 1/2 the board
% row_crowding: Fraction of each row in init array to fill.  Chess fills
%    the rows entirely. 
% piece_variety: Of possible pieces (# used array locations), fraction which
%    will be unique piece types.  
%
% Thus, the number of unique piece-types used in a game will be:
%    (board_size^2/2)*board_crowding*row_crowding*piece_variety

:- dynamic(forced_gen_parameter/2).
:- dynamic(gen_parameter/2).

gen_parameter(board_size,
	range(
		 5,
		 6
	     )).

% board_crowding: Fraction of board to use for placing initial arrays. 
%    Chess uses 1/2 the board (4/8 rows)
%    Checkers uses 3/4 the board (6/8 rows)
% Correct to mean distribution later.
gen_parameter(board_crowding,
	distribution([
			 0.5=0.7,
			 0.3=0.15,
		         0.7=0.15
		     ])).

% row_crowding: what fraction of squares to place pieces on in initial array.
% Correct to mean distribution eventually.
gen_parameter(row_crowding,
	distribution([
			 1.0=0.7,
			 0.7=0.3
		     ])).


% Fraction of initial array rows used for promotion,
% where smaller fraction means farther distance to promotion.
gen_parameter(promotion_fraction,
	distribution([
			 1.0=0.3,
			 0.8=0.1,
			 0.6=0.1,
			 0.4=0.2,
		         0.2=0.2,
			 0 =0.1
		     ])).

% Of a possible number of pieces, what fraction should be unique.
%   (Checkers has low variety, chess has high, shogi even higher).
gen_parameter(piece_variety,
	distribution([
			 1.0=0.4,
			 0.8=0.2,
			 0.6=0.2,
			 0.4=0.2,
		         0.2=0.0
		     ])).


% Number of unique piece types, which will
% only be seen via promotion (i.e. they are not placed on 
% the initial board.
% Note this gen_parameter is independent of board size.
%
gen_parameter(promote_only_pieces,
	range(
		 1,
		 3
	     )).



% placement_method:  How the init. config is determined.
% Random --> randomly determined before each game.
% Arbitrary --> randomly determined before first game, then fixed.
% Player --> each player alternates deciding his placement.
% Opponent --> each player alternates deciding opponent's placement.
%
% So arbitrary is most pre-structured, random is least, decision 
% is midway. 

gen_parameter(placement_method,
	distribution([
			 random=0.2,
                         arbitrary=0.6,
			 player=0.1,
			 opponent=0.1
		     ])).


% Probability to choose planar vs. cylinder boards.
gen_parameter(board_type,
	distribution([planar=0.9,vertical_cylinder=0.1])).

% Probability to choose forward vs. diagonal board inversion.
gen_parameter(board_inversion,
	distribution([forward=0.5,diagonal=0.5])).




% Prob. of choosing each kind of symmetry (independent).
gen_parameter(symmetry(rotation),0.9).
gen_parameter(symmetry(forward),0.9).
gen_parameter(symmetry(side),0.9).

% Complexity of pieces

% Upper bound on fraction of board leapers should traverse
% each step.
gen_parameter(locality,
	range(0.1,0.8)).



% What fraction of movements should be leapers, riders, and hoppers.
gen_parameter(movement_type,
	distribution([
			 leaper=0.4,
			 rider=0.4,
			 hopper=0.2
		     ])).

% Whether riders must make the longest ride (continue riding as long as possible).
gen_parameter(must_ride,0.2).

% Whether to constrain part of the description of a hopper.
% (Each component determined independently)
gen_parameter(constrain(hopper(before,_)),0.5).
gen_parameter(constrain(hopper(over,_)),0.5).
gen_parameter(constrain(hopper(after,_)),0.5).

% A hopper can hop 0..Max squares before a piece.
gen_parameter(hopper(before,Max),
	range(0,Max)).

% A hopper can hop over 1..Max pieces.
gen_parameter(hopper(over,Max),
	range(1,Max)).

% A hopper can hop 1..Max squares after the last piece.
gen_parameter(hopper(after,Max),
	range(1,Max)).


% When need a comparison term, which one to use.
gen_parameter(comparative,
	distribution([
			 eq=0.5,
			 geq=0.2,
			 leq=0.3
		     ])).



% Prob. of continuing to add piece_definitions to a piece_type.
% So X% chance of piece having >1 method of moving/capturing.
gen_parameter(movement_complexity,0.2).
gen_parameter(capture_complexity,0.2).
gen_parameter(goal_complexity,0.6).  


% Independent Prob. of assigning each type of capturing method to
% a piece (they can have multiple powers).
% Hopping power can only be attached when the capture_movement 
% is hopping already.
 
gen_parameter(capture_method(retrieve),0.2).
gen_parameter(capture_method(clobber),0.9). 
gen_parameter(capture_method(hop),0.5).


gen_parameter(capture_effect,
	distribution([
			 remove=0.5,
			 possess(player)=0.3,
			 possess(opponent)=0.2
		     ])).


% Whether to use any_player or a particular
% player in general piece descriptions (ie. for capturing and hopping).

gen_parameter(player_generalization_level,
	distribution([
			 any=0.2,
			 specific=0.8
		     ])).

% Whether to use any_piece or a particular
% set of pieces in piece descriptions.
% The more general, the less constrained is the game,
% as all interactions apply to more objects.

gen_parameter(piece_generalization_level,
	distribution([
			 any=0.5,
			 specific=0.5
		     ])).

% If true, adds some pieces to set in a description.
% Thus, a high setting will have more genereral piece 
% descriptions  (when the specific option is selected
% by piece_generalization_level above).

gen_parameter(more_pieces,0.8).

gen_parameter(more_general_pieces,0.8).



% Whether a piece must capture if it can.

gen_parameter(must_capture,0.3).

% Whether you can continue capturing with a piece once you've captured
% something.  

gen_parameter(continue_captures,0.1).


% For additional goals besides stalemate, the probability of adding
% eradicate or arrive goals.

gen_parameter(goal_type,
  distribution([
		   eradicate=0.5,
		   arrive=0.5
	       ])).

% Whether an arrival goal is to arrive player's piece,
% opponent's piece, or either player's  piece on a square.
% In goals, having any_player too often makes the game a draw,
% as arrival or eradicate goals predicated on any_player 
% are always achieved for both players together.
% Thus these any_player options have now been removed.

gen_parameter(arrive_goal_player,
	distribution([
			 player=0.5,
			 opponent=0.5
		     ])).

% If true, adds more pieces to those which are in the 
% arrival goal for a player. 
% Thus, a high setting will make a given arrival goal
% easier to achieve.  

gen_parameter(more_arrival_pieces,0.8).


% Whether an eradicate goal is to eradicate player's piece
% or opponent's piece. 
gen_parameter(eradicate_goal_player,
	distribution([
			 player=0.2,
			 opponent=0.8
		     ])).

% If true, adds more pieces to those which are in the 
% eradicate goal for a player. 
% Thus, a high setting will make a given eradicate goal
% *harder* to achieve.  

gen_parameter(more_eradicate_pieces,0.8).


% Whether to use any_piece or a particular
% set of pieces in a given eradicate goal.
% The more general, the more pieces will need to be
% eradicated, and thus the goal becomes harder.

gen_parameter(eradicate_generalization_level,
	distribution([
			 any=0.2,
			 specific=0.8
		     ])).



% The probability of having a piece promote to exactly 1 type of piece,
% or having a piece promote as a decision of one of the players.

gen_parameter(specific_promotion,0.4).


% The method of promoting a given piece, if it is not arbitrary.

gen_parameter(promotion_method,
	distribution([
			 arbitrary=0.3,
			 player=0.5,
			 opponent=0.1
		     ])).


%================================================================================
% CHOOSE_PARAMETER(+Name,-Value)
% Still must include option for Mean,Std.
choose_parameter(Name,Value) :-
	forced_gen_parameter(Name,distribution(Dist)), !,
        sample_from_distribution(distribution(Dist),Value).
choose_parameter(Name,Value) :-
	gen_parameter(Name,distribution(Dist)), !,
        sample_from_distribution(distribution(Dist),Value).
choose_parameter(Name,Value) :-
	gen_parameter(Name,range(Min,Max)), !,
        sample_from_range(range(Min,Max),Value).
choose_parameter(Name,Value) :-
	gen_parameter(Name,Int),
	integer(Int), !,
        adjust_mean(Int,Value).
choose_parameter(Name,Value) :-
	gen_parameter(Name,Prob),
	sample_from_distribution(distribution([yes=Prob,no=1]),Value).


choose_parameter(Name) :-
	choose_parameter(Name,yes).


block_parameter(Name,Items) :-
	block_distribution(Name,Items,Dist),
	asserta(forced_gen_parameter(Name,distribution(Dist))).

unblock_parameter(Name) :-
	retract(forced_gen_parameter(Name,distribution(_Dist))).

reset_gen_parameters :- retractall(forced_gen_parameter(_,_)).


set_gen_parameter(P,V) :-
	( retract(gen_parameter(P,_)) -> assert(gen_parameter(P,V)) 
        ; otherwise -> trace_output('Unknown generator parameter ~p!~n',[P])
        ).



% Removes the possible events in ITEMS from 
% appearing in distribution for gen_parameter NAME,
% renormalizing, and saving as a blocked distribution.
block_distribution(Name,Items,Dist) :-
	gen_parameter(Name,distribution(Dist0)), !,
	remove_items(Items,Dist0,Prob,Dist1),
	P1 is 1/(1 - Prob),
	renormalize(Dist1,P1,Dist).

remove_items([],D,0,D).
remove_items([I|Is],Dist1,Prob,Dist) :-
	remove_item(I,Dist1,Prob1,Dist2),
	remove_items(Is,Dist2,Prob2,Dist),
	Prob is Prob1 + Prob2.

remove_item(I,[I=P|Dist],P,Dist).
remove_item(I,[H|T],P,[H|Dist]) :-
	remove_item(I,T,P,Dist).

renormalize([],_,[]).
renormalize([H=P|Rest],Ratio,[H=P1|Rest1]) :-
	P1 is P * Ratio,
	renormalize(Rest,Ratio,Rest1).


% For now, just return it.  
adjust_mean(Int,Int).


%==============================================================================
% Printing generator parameters
%==============================================================================

show_gen_parameters :- 
	whenever(gen_parameter(Name,Val),
	         ( portray_param(gen_parameter(Name,Val)),
		   nl
		 )
		),
	getrand(R),
	format('~nrandom seed = ~p~n',[R]).

portray_param(gen_parameter(Name,Val)) :-
	format("<~p> --> ~p~n",[Name,Val]).

portray_range(range(Min,Max)) :- 
	format("[~p .. ~p]",[Min,Max]).

portray_dist(distribution(Pairs)) :-
	portray_pairs(Pairs).

portray_pairs([]).
portray_pairs([Pair|Pairs]) :- 
	portray_pair(Pair),
	portray_pairs(Pairs).

portray_pair(Event=Val) :- 
	format("
     ~p: ~p",[Event,Val]).

:- add_portrayals([portray_param,portray_range,portray_dist]).

%==============================================================================
% Interface for changing generator parameters 
%==============================================================================

change_gen_param(Name) :- 
	read_new_gen_value(Name,New),
	set_gen_parameter(Name,New).

read_new_gen_value(Name,New) :- 
	gen_parameter(Name,Value),
	read_gen_value(Name,Value,New).

read_gen_value(Name,Value,New) :- 
	format("Enter new settings for parameter <~p>:~n",[Name]),
	format("Old parameter setting: ~p~n",[Value]),
	read_gen_value(Value,New),
	format("New parameter setting: ~p~n",[New]).


read_gen_value(distribution(Dist),distribution(New)) :- !,
	read_dist(Dist,New).
read_gen_value(range(Min1,Max1),range(Min,Max)) :- !, 
	read_range(Min1,Max1,Min,Max).
read_gen_value(Old,New) :- 
	read_new_val(new,Old,New).


read_dist([],[]).
read_dist([Event=Prob1|Rest1],[Event=Prob2|Rest2]) :- 
	read_event(Event,Prob1,Prob2),
	read_dist(Rest1,Rest2).

read_event(Event,Old,New) :- 
	format("Prob for <~p> (~p): ",[Event,Old]),
	read(New1),
	new_event_val(Event,Old,New1,New).

new_event_val(_Event,Old,New1,New) :- 
	new_val(Old,New1,New).
	
new_val(Old,New1,New) :- 
	( New1 = z ->
	  New = Old
	; New = New1
	).


read_range(Min1,Max1,Min,Max) :- 
	read_new_val(min,Min1,Min),
	read_new_val(max,Max1,Max).

read_new_val(Name,Old,New) :- 
	format("<~p> value (~p): ",[Name,Old]),
	read(New1),
	new_val(Old,New1,New).




