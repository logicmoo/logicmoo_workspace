/* @(#)xgrun.pl	24.1 2/23/88 */

/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
:- mode terminal(?,+,?,+,?),
        gap(+),
        virtual(+,+,?).

terminal(T,S,S,x(_,terminal,T,X),X).
terminal(T,[T|S],S,X,X) :-
   gap(X).

gap(x(gap,_,_,_)).
gap([]).

virtual(NT,x(_,nonterminal,NT,X),X).
