/*

 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

/* Print term as a tree */

print_tree(T) :-
   numbervars80(T,1,_),
   pt0(T,0), nl, fail.
print_tree(_).

pt0(A,I) :-
   as_is(A), !,
   tab(I), write(A), nl.
pt0([T|Ts],I) :- !,
   pt0(T,I),
   pl(Ts,I).
pt0(T,I) :- !,
   T=..[F|As],
   tab(I), write(F), nl,
   I0 is I+3,
   pl(As,I0).

pl([],_) :- !.
pl([A|As],I) :- !,
   pt0(A,I),
   pl(As,I).

as_is(V):- var(V),!.
as_is(A) :- atomic(A), !.
as_is(F):- functor(F,'$VAR',_).
as_is('_'(_)) :- !.
as_is(X) :-
   quote(X).

