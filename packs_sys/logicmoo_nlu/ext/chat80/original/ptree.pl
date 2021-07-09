/*

 _________________________________________________________________________
|       Copyright (C) 1982                                                |
|                                                                         |
|       David Warren,                                                     |
|               SRI International, 333 Ravenswood Ave., Menlo Park,       |
|               California 94025, USA;                                    |
|                                                                         |
|       Fernando Pereira,                                                 |
|               Dept. of Architecture, University of Edinburgh,           |
|               20 Chambers St., Edinburgh EH1 1JZ, Scotland              |
|                                                                         |
|       This program may be used, copied, altered or included in other    |
|       programs only for academic purposes and provided that the         |
|       authorship of the initial program is aknowledged.                 |
|       Use for commercial purposes without the previous written          |
|       agreement of the authors is forbidden.                            |
|_________________________________________________________________________|

*/

/* Print term as a tree */

:- if( \+ current_predicate(print_tree/1)). 
 print_tree(T):- print_tree80(T).
:- endif.

%:- listing(print_tree/1).

print_tree80(T) :- pprint_ecp_cmt(yellow,T),!.
print_tree80(T) :-
   numbervars80(T,3,_),
   pt_old(T,0), nl, fail.
print_tree80(_).

pt_old(A,I) :-
   as_is_old(A), !,
   tab(I), write(A), nl.
pt_old([T|Ts],I) :- !,
   pt_old(T,I),
   pl_old(Ts,I).
pt_old(T,I) :- !,
   T=..[F|As],
   tab(I), write(F), nl,
   I0 is I+3,
   pl_old(As,I0).

pl_old([],_) :- !.
pl_old([A|As],I) :- !,
   pt_old(A,I),
   pl_old(As,I).

as_is_old(A) :- \+ compound(A), !.
as_is_old('$VAR'(_)) :- !.
as_is_old('_'(_)) :- !.
as_is_old(X) :-
   quote80(X).

