%============================================================
%		   METAGAME Game-Playing Workbench
%		  Copyright (c) 1992 Barney D. Pell
%============================================================

%%% invert.pl
%%% How to invert game component defs from perspective of
%%% other player.

%invert(Term,Player,InvertedTerm).
% If inverting for player, do nothing, otherwise flip.
invert(Term,Player,Term2) :- 
	Player = player 
        -> Term=Term2
        ;  invert(Term,Term2).


invert(opponent,player) :- !.
invert(player,opponent) :- !.
invert(Term,Term2) :- 
	atom(Term) -> Term=Term2;
        invert_struct(Term,Term2).


%invert_struct(square(X1,Y1),square(X2,Y2)) :- !,
invert_struct(square(X1,Y1),Sq) :- !,
	invert_square(square(X1,Y1),Sq).
invert_struct(dir(X1,Y1),Dir) :- !,
	invert_dir(dir(X1,Y1),Dir).
invert_struct(Pred,PredOut) :-
	functor(Pred,F,N),
	functor(PredOut,F,N),
	invert_args(N,Pred,PredOut).

invert_args(0,_,_) :- !.
invert_args(N,Pred,PredOut) :-
	arg(N,Pred,A),
	invert(A,A1),
	arg(N,PredOut,A1),
	N1 is N-1,
	invert_args(N1,Pred,PredOut).	


invert_square(Sq1,Sq) :- 
	current_board_size(XN,YN),
	current_board_inversion(Inv),
	invert_square_dim(Inv,XN,YN,Sq1,Sq).

invert_square_dim(diagonal,XN,YN,square(X1,Y1),square(X2,Y2)) :- 
	X2 is XN - X1 + 1,
	Y2 is YN - Y1 + 1.
invert_square_dim(forward,_XN,YN,square(X1,Y1),square(X2,Y2)) :- 
	X1=X2, 
	Y2 is YN - Y1 + 1.


invert_dir(dir(X1,Y1),dir(X2,Y2)) :- 
	current_board_inversion(Inv),
	inv_negate_dir(Inv,x,X1,X2),
	inv_negate_dir(Inv,y,Y1,Y2).

inv_negate_dir(diagonal,_Axis,X,XNeg) :- negates(X,XNeg).
inv_negate_dir(forward,Axis,X1,X2) :-
	( Axis = y -> 
	  negates(X1,X2)
	; X1 = X2
	). 

