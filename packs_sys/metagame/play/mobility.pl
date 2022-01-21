%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% mobility.pl
%%% Defines a basic evaluation used by some players
%%% (like alpha_beta or evaluating player).
%%% Features are mobility and material (piece_count).

%========================================
% EVALUATING_CHOOSE(Player,Move,SIn,SOut)
%========================================
% Wins immediately if possible, else plays a random move 
% among those with highest value.
% Note doesn't search any more, so this might leave it 
% open for a loss.  Also semantics not clear if this move
% ends the game for either player.

evaluating_choose(_Player,Move,SIn,SOut) :-
	timing(evaluating_move(Move,SIn,SOut)), !,
	print_choice(Move,SIn,SOut).


evaluating_move(Move,SIn,SOut) :- victor_move(Move,SIn,SOut), !.
evaluating_move(Move,SIn,SOut) :- max_value_move(Move,SIn,SOut).

% Evaluates position relative to player who is to move.
% First choices have lower value, therefore WORSE for player to
% move.
% Thus, a player evaluating all possible successors will choose the
% lowest valued one, as this is the worst one for his opponent.

evaluated_move(Move,Val,SIn) :- 
	legal(Move,SIn,S1),
	evaluate(S1,Val).
	

max_value_move(Move,SIn,SOut) :- 
	setof(Val-Move,evaluated_move(Move,Val,SIn),Pairs),
	random_best(Val-Move,Pairs),
	format("The best move found has a value of: ~p~n",[Val]),
	legal(Move,SIn,SOut).
	
random_best(Best,L) :- 
	collect_init(L,BestFew),
	random_element(BestFew,Best).

collect_init([V-M|Rest],[V-M|BestFew]) :-
	collect_init(Rest,V,BestFew).

collect_init([],_V1,[]).
collect_init([V2-_|_],V1,[]) :- V1 < V2, !.
collect_init([Pair|Rest],Threshold,[Pair|R1]) :-
	collect_init(Rest,Threshold,R1).



evaluate_com(_Move,SIn,_) :-
	timing(evaluate(SIn,Value)),
	format("Position's value for player to move:  ~p~n", [Value]).


evaluate(S,Value) :-
	control(P,S),
	opposite_role(O,P),
	evaluate(P,S,PVal),
	evaluate(O,S,OVal),
	Value is PVal-OVal.


/*
evaluate(Player,S,Value) :-
	weight_vector(Weights),
	material_weight(Weights,WMat),
	mobility_weight(Weights,WMob),
	mobility(Player,S,Mob),
	material(Player,S,Mat),
	Value is Mob*WMob + Mat*WMat.
*/

evaluate(Player,S,Value) :-
	weight_vector(Weights),
	material_weight(Weights,MatW),
	mobility_weight(Weights,MobW),
	weighted_mobility(MobW,Player,S,WMob),
	weighted_material(MatW,Player,S,WMat),
	Value is WMob + WMat.

weighted_mobility(0,_,_,0) :- !.
weighted_mobility(MobW,Player,S,WMob) :-
	mobility(Player,S,Mob),
	WMob is MobW*Mob.


weighted_material(0,_,_,0) :- !.
weighted_material(MatW,Player,S,WMat) :-
	material(Player,S,Mat),
	WMat is MatW*Mat.



mobility(Player,S,M) :-
	put_control(Player,S,S1),
	count_bagof(Move,S2^legal_move(Move,Player,S1,S2),M).
	

material(Player,S,M) :-
	count_bagof(Piece,Sq^on(piece(Piece,Player),Sq,S),M).



% EVALUATE2
% Only considers opponent's mobility, and player's material,
% in a future position.  Not very good, or even that much faster.
evaluate2(S,Value) :-
	control(P,S),
	opposite_role(O,P),
	evaluate2(P,O,S,Value).

evaluate2(Player,Opponent,S,Value) :-
	mobility(Opponent,S,Mob),
	material(Player,S,Mat),
	weight(material,WMat),
	weight(mobility,WMob),
	Value is Mat*WMat - Mob*WMob .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Manipulating weights
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

toggle_alpha_beta_weights(Player,Old) :- 
	weight_vector(Player,Weights),
	change_parameter(weights,Old,Weights).
	

weight_vector(Player,Weights) :-
	parameter(weights(Player),Weights).

weight_vector(Weights) :-
	parameter(weights,Weights).


initialize_weights :-
	default_alpha_weights(W),
	add_parameter(weights,W).

default_alpha_weights(weights(Mat,Mob)) :- 
	default_weight(material,Mat),
	default_weight(mobility,Mob).

% General function, preferring mobility and material.
default_weight(material,4).
default_weight(mobility,1).

% Anti-material function, perferring mobility but not material.
%default_weight(material,-10).
%default_weight(mobility,1).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

material_weight(weights(Mat,_Mob),Mat).
mobility_weight(weights(_Mat,Mob),Mob).

set_player_weights(Player,Mat,Mob) :-
	material_weight(W,Mat),
	mobility_weight(W,Mob),
	( Player = default
	-> add_parameter(weights,W)
	;  add_parameter(weights(Player),W)
	).


show_player_weights :-
	show_player_weights(player),
	show_player_weights(opponent).

show_player_weights(Player) :-
	( weight_vector(Player,W) ->
	  Default = ''
	; weight_vector(W),
	  Default = '(by default)'
	),
	material_weight(W,Mat),
	mobility_weight(W,Mob),
	format("
<~p>'s weights are ~w:
       material:  ~p
       mobility:  ~p
",[Player,Default,Mat,Mob]).	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Menus
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% WEIGHT: Allows command:  "weight {player, opponent, default} Mat Mob"
weight_com(_,_,_,Player,Mat,Mob) :-
	set_player_weights(Player,Mat,Mob).

weight_top(Player,Mat,Mob) :-
	set_player_weights(Player,Mat,Mob).

show_com(_,_,_,weights,Player) :-
	show_player_weights(Player).

show_com(_,_,_,weights) :-
	show_player_weights.

show_top(weights,Player) :-
	show_player_weights(Player).

show_top(weights) :-
	show_player_weights.

% toggle player ==> sets default weights to be those of player.
toggle_com(_,_,_,Player) :- 
	toggle_alpha_beta_weights(Player,_Old).


