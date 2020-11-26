
/* @(#)ptree.pl	24.1 2/24/88 */

/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/
/* Print term as a tree */

 :- mode print_tree(+).
 :- mode pt(+,+).
 :- mode pl(+,+).
 :- mode as_is(+).

 :- public print_tree/1.

print_tree(T) :-
   numbervars(T,1,_),
   pt(T,0), nl, fail.
print_tree(_).

pt(A,I) :-
   as_is(A), !,
   tab(I), write(A), nl.
pt([T|Ts],I) :- !,
   pt(T,I),
   pl(Ts,I).
pt(T,I) :- !,
   T=..[F|As],
   tab(I), write(F), nl,
   I0 is I+3,
   pl(As,I0).

pl([],_) :- !.
pl([A|As],I) :- !,
   pt(A,I),
   pl(As,I).

as_is(A) :- atomic(A), !.
as_is('$VAR'(_)) :- !.
as_is(X) :-
   quote(X).

   
