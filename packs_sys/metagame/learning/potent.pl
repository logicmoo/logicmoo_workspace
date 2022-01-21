%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%================================================================================
% Global Potent Threats 
%================================================================================

% The global value of the player to move having potent threats is the
% maximum of the value of all potent threats he has.
% We could add value for the other player, but not nearly as clear how
% to determine this.  

pthreat(Player,Value,Pos,Tables) :- 
	control(Player,Pos),
	potent_threats(Player,Evaled,Pos,Tables),
	pair_list(Vals,_Threats,Evaled),
	max_for_player(Player,Vals,Value).

potent_threats(Player,Evaled,Pos,Tables) :- 
	findall(Val-Threat,
	  ( potent_threat_mover(Player,Piece,Square,SqT,Victim,SqV,Effect,Val,Pos,Tables),
	    Threat = potent(Piece,Square,SqT,Victim,SqV,Effect,Val) ),
	  Evaled).

%================================================================================
% Local Potent Threats 
%================================================================================


% The value of having a potent threat on a piece is the amount which would be increased
% for the threatener if that piece disappeared (or 0 if that is negative). 
% Perhaps discounted by an offset based on whether the threat can be executed now or
% if the victim gets a chance to defend.  

% Threat is potent for mover if:
% 1. Capturing target (under the effect) has favorable value (V) for mover, and
% 2. Target undefended by non-mover (then V is net threat value) or
% 3. Target defended, but TargV (V) - AttackV (on current square) > 0.
%    (then this diff is net value).
% We are at present ignoring the effects (besides removal) that happen 
% if opponent has a defense against our attacker.  

potent_threat_mover(Piece,Square,PieceV,SqV,Value,Pos,Tables) :- 
	blank_state_if(Pos),
	find_advice_tables_if(Tables),
	add_capturing_table_if(Pos,Tables),
	control(Player,Pos),  % ensures mover has threat
%	on(Piece,Player,Square,Pos),
	potent_threat_mover(Player,Piece,Square,_SqT,PieceV,SqV,_Effect,Value,Pos,Tables).

% Weigh-by-effect ensures favorable to capture. 
% Assumes Player is in control. 
potent_threat_mover(Player,Piece,Square,SqT,Victim,SqV,Effect,Value,S,Tables) :- 
	capture_threat(Piece,Player,Square,SqT,Victim,SqV,Effect,Tables),
	effect_threat_evaluation(Effect,Player,Victim,SqV,Val,S,Tables),
	potency_value(Player,Piece,Square,SqT,Victim,SqV,Val,Value,S,Tables),
	tracing_anal_format(pthreat,
	    "<~p>: ~p -> ~p x ~p ~p (~p) [~p]~n",
	    [Piece,Square,SqT,Victim,SqV,Effect,Value]).


% POTENCY_VALUE(Player,Piece,Square,SqT,Victim,SqV,ValV,Value,S,Tables)
%
% ValV is the value for capturing the victim if undefended.
% If it is defended, player gets the net difference between the value
% of the capture, and losing his own piece.  If he wants to lose his piece
% however, it is as if the piece was undefended anyway! 
% 
% Should really use effect_threat_evaluation, since opponent gets value for capturing
% us back based on the effect of his recapture, given where our piece will be  now.
% For example, suppose we can remove-capture his bishop with our night, but he
% defends it with a possess-capture piece.  Then it would presently look like
% we have a real threat (bishop-night value), but actually we don't, as this is 
% just a small increment for us, while he winds up possessing a night 
% after the transaction, putting him at least a piece up! 
% 
% For the moment, just assume he wants to do it as long as the effect isn't to
% give it back to us.  
%
% If use this, don't need this min-for-player
% business, as it won't even be seen as a defense unless the enemy wins
% something for recapturing us.   
%
potency_value(Player,Piece,Square,SqT,Victim,SqV,ValV,Value,S,Tables) :- 
	( ( defended(Player,Piece,Square,SqT,Victim,SqV,Effect,S,Tables),
	    Effect \== possess(Player) ) 
	-> local_threat_evaluation(Piece,Square,ValA,S,Tables),
	   Net is ValV-ValA, 
	   min_for_player(Player,[ValV,Net],Value),
	   favorable_to_owner(Player,Value)
	;  Value = ValV
	).
	


% DEFENDED(Player,PieceA,SqA,SqT,PieceV,SqV,Effect,S,_Tables)
% General routine to check if some considered attack is defended.
% The attack PieceA @ SqA -> SqT x PieceV @ SqV  must be known applicable already.
% This threat is defended if there is some defender which could capture the 
% attacker if this considered capture were executed. 
%

defended(Player,PieceA,SqA,SqT,PieceV,SqV,Effect,S,_Tables) :- 
	opposite_role(Player,Opp),
	do_capture(PieceA,SqA,SqT,PieceV,SqV,S,PosOut),
	on(PieceD,Opp,SqD,PosOut),
	target_capture(PieceD,SqD,PieceA,SqT,_Capture,_Movement,_Dir,Effect,PosOut,_Tables).


% TARGET_CAPTURE(PieceA,SqA,PieceV,SqV,Capture,Movement,Dir,Effect,Pos,_Tables)
% ==============
%
% When the target piece is known, it is easier to check whether an attacker can
% actually capture it in the present position.  First check if it is even potentially
% possible (when the target is the right piece type, and in the right capture line). 
% If so, check that that capturing movement in that direction really works to create a capture. 
%
% This is a general routine, and can be very useful. 
target_capture(PieceA,SqA,PieceV,SqV,Capture,Movement,Dir,Effect,Pos,_Tables) :-
	owns(PieceA,Player),
	potential_capture(PieceA,SqA,PieceV,SqV,Capture,Movement,Dir,Effect,_Pos,_Tables),
	capturing_movement_for_piece(PieceA,SqA,_SqT,Player,Dir,Movement,Capture,Captured,Pos),
	captured_piece(PieceV,SqV,Captured).


% We have a potential capture if:
% The piece can capture victims of that type with some capture power.
% The effect is not to give it right back to oppenent.
% That capture power has some movement which is aligned with the target.
%
% Could take account of factors like min-ride, max-ride, hoppers, etc. 
potential_capture(PieceA,SqA,PieceT,SqT,Effect,_Pos,_Tables) :-
	potential_capture(PieceA,SqA,PieceT,SqT,_Capture,_Movement,_Dir,Effect,_Pos,_Tables).

potential_capture(PieceA,SqA,PieceT,SqT,Capture,Movement,Dir,Effect,_Pos,_Tables) :-
	threat_piece_victim(PieceA,Player,PieceT,VPlayer,Effect,Capture),
	opposite_role(Player,VPlayer),
	capture_has_movement(Capture,Movement),
	movement_sym_dir(Movement,Dir),
	capture_aligned(Capture,Movement,SqA,SqT,Dir).

capture_aligned(Capture,Movement,SqA,SqT,Dir) :- 
	capture_has_method(Capture,Method),
	( method_aligned(Method,Movement,SqA,SqT,Dir) -> true ). 

% For retrieve method, the attacker can move in a dir
% away from the target if the target is one leap behind. 
% For now ignore Movement, but could make use of movement 
% constraints as well. 
method_aligned(clobber,_Movement,SqA,SqT,Dir) :-
	aligned(SqA,SqT,Dir).
method_aligned(hop,_Movement,SqA,SqT,Dir) :-
	aligned(SqA,SqT,Dir).
method_aligned(retrieve,_Movement,SqA,SqT,Dir) :-
	connected(SqT,SqA,Dir).

	
% ALIGNED(S1,S2,Dir)
%
% True if  S2 is on the direction vector DIR from S1, given the
% current board size and types.  
% This would be more complicated if both axes could wraparound, but since we
% know Y doesn't we just find the # leaps there, then check that moving that
% many leaps with wrapping along X brings us back to X square.
% This of course checks that there is an integer, not fractional, number of leaps.
% For example:
% On vertical cylinder 5x6 board:
%    aligned(square(2,2),square(1,1),dir(-1,-1)).
%    aligned(square(1,5),square(1,3),dir(-5,-2)).
%    aligned(square(1,2),square(5,1),dir(-1,-1)).
% But not:
%    * aligned(square(2,2),square(3,3),dir(-1,-1)).
%
aligned(S1,S2,Dir) :-
	square(S1,Xf,Yf),
	square(S2,Xt,Yt),
	direction(Dir,DX,DY),
	current_board_type(T),
	x_given_y_leaps(T,Yf,Yt,DY,Xf,Xt,DX).

x_given_y_leaps(T,Yf,Yt,DY,Xf,Xt,DX) :- 
	DiffY is Yt-Yf,
	same_sign(DiffY,DY),
	( DY \== 0 
	-> 0 is DiffY mod abs(DY),
	   Leaps is DiffY // DY,
	   XNew is Xf + Leaps * DX, 
	   align_for_type(T,XNew,Xt)
	; T \== vertical_cylinder
	-> DiffX is Xt-Xf,
	   same_sign(DiffX,DX),
	   0 is (Xt-Xf) mod abs(DX)
	; valid_max_dir(DX,0,Max1),
	  Max is abs(Max1),
	  some_reaches(Max,T,DX,Xf,Xt)
	).

some_reaches(Leaps,T,DX,Xf,Xt) :- 
	Leaps > 0,
	XNew is Xf + Leaps * DX, 
	align_for_type(T,XNew,Xt).
some_reaches(Leaps,T,DX,Xf,Xt) :- 
	Leaps > 1,
	L1 is Leaps-1,
	some_reaches(L1,T,DX,Xf,Xt).


	


y_leaps(Yf,Yt,DY,Leaps) :-
	DiffY is Yt-Yf,
	same_sign(DiffY,DY),
	0 is DiffY mod abs(DY),
	Leaps is DiffY // DY. 


align_for_type(planar,X,X).
align_for_type(vertical_cylinder,X1,X) :- 
	current_board_size(XN,_YN),
	X is ( (X1 + XN - 1) mod XN ) + 1.



sign(X,Sign) :- 
	( X < 0 -> Sign = -1 ; 
	  X > 0 -> Sign = 1 ;
	  otherwise -> Sign = 0
	).


same_sign(X1,X2) :- 
	sign(X1,S),
	sign(X2,S).


test_potential_capture(PieceA,SqA,PieceT,SqT,Capture,Movement,Dir,Effect,S,_Tables) :-
     checkpoint(init,S),
     on(PieceA,_,SqA,S),
     on(PieceT,_,SqT,S),
     potential_capture(PieceA,SqA,PieceT,SqT,Capture,Movement,Dir,Effect,_Pos,_Tables).

test_target_capture(PieceA,SqA,PieceV,SqV,Capture,Movement,Dir,Effect,S,_Tables) :-
%     checkpoint(init,S),
     on(PieceA,_,SqA,S),
     on(PieceV,_,SqV,S),
     target_capture(PieceA,SqA,PieceV,SqV,Capture,Movement,Dir,Effect,S,_Tables).

