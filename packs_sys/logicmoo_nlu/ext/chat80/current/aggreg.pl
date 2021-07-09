/* @(#)aggreg.pl	24.1 2/23/88 */
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
/* 
	Copyright 1986, Fernando C.N. Pereira and David H.D. Warren,

			   All Rights Reserved
*/


aggregate80(Fn,Set,Val) :-
   dimensioned(Set), !,
   u_aggr(Fn,Set,Val).
aggregate80(Fn,Set,Val) :-
   i_aggr(Fn,Set,Val).

i_aggr(total,Set,Val) :-
   i_total(Set,Val).
i_aggr(_,Set,_):- Set==[],!,fail.
i_aggr(average,Set,Val) :- 
   i_total(Set,T),
   length(Set,N), N>0,
   Val is T/N.
i_aggr(max,Set,Val) :-
   i_maxs(Set,List),
   one_of(List,Val).
i_aggr(min,Set,Val) :-
   i_mins(Set,List),
   one_of(List,Val).
i_aggr(maximum,[V0:O|S],V) :-
   i_maxs0(S,V0,[O],_,V).
i_aggr(minimum,[V0:O|S],V) :-
   i_mins0(S,V0,[O],_,V).

u_aggr(total,Set,Val) :-
   u_total(Set,Val).
u_aggr(_,Set,_):- Set==[],!,fail.
u_aggr(average,Set,V--U) :-
   u_total(Set,T--U),
   length(Set,N), N>0,
   V is T/N.
u_aggr(max,Set,Val) :-
   u_maxs(Set,List),
   one_of(List,Val).
u_aggr(min,Set,Val) :-
   u_mins(Set,List),
   one_of(List,Val).
u_aggr(maximum,[V0:O|Set],V) :-
   u_maxs0(Set,V0,[O],_,V).
u_aggr(minimum,[V0:O|Set],V) :-
   u_mins0(Set,V0,[O],_,V).

i_total([],0).
i_total([V:_|R],T) :-
   i_total(R,T0),
   T is V+T0.

i_maxs([V:X|Set],List) :-
   i_maxs0(Set,V,[X],List,_).

i_maxs0([],V,L,L,V).
i_maxs0([V0:X|R],V0,L0,L,V) :- !,
   i_maxs0(R,V0,[X|L0],L,V).
i_maxs0([U:X|R],V,_,L,W) :-
   U>V, !,
   i_maxs0(R,U,[X],L,W).
i_maxs0([_|R],V,L0,L,W) :-
   i_maxs0(R,V,L0,L,W).

i_mins([V:X|Set],List) :-
   i_mins0(Set,V,[X],List,_).

i_mins0([],V,L,L,V).
i_mins0([V:X|R],V,L0,L,W) :- !,
   i_mins0(R,V,[X|L0],L,W).
i_mins0([U:X|R],V,_,L,W) :-
   U<V, !,
   i_mins0(R,U,[X],L,W).
i_mins0([_|R],V,L0,L,W) :-
   i_mins0(R,V,L0,L,W).

u_total([],0--_U).
u_total([V:_|R],T) :-
   u_total(R,T0),
   u_sum(T0,V,T).

u_sum(X--U,Y--U,Z--U) :- !,
   Z is X+Y.
u_sum(X--U,Y--U1,Z--U) :-
   ratio_db(U,U1,M,M1), M>M1, !,
   Z is X + (Y*M1)/M.
u_sum(X--U1,Y--U,Z--U) :-
   ratio_db(U,U1,M,M1), M>M1, !,
   Z is (X*M1)/M + Y.

u_maxs([V:X|Set],List) :-
   u_maxs0(Set,V,[X],List,_).

u_maxs0([],V,L,L,V).
u_maxs0([V0:X|R],V0,L0,L,V) :- !,
   u_maxs0(R,V0,[X|L0],L,V).
u_maxs0([U:X|R],V,_,L,W) :-
   u_lt(V,U), !,
   u_maxs0(R,U,[X],L,W).
u_maxs0([_|R],V,L0,L,W) :-
   u_maxs0(R,V,L0,L,W).

u_mins([V:X|Set],List) :-
   u_mins0(Set,V,[X],List,_).

u_mins0([],V,L,L,V).
u_mins0([V:X|R],V,L0,L,W) :- !,
   u_mins0(R,V,[X|L0],L,W).
u_mins0([U:X|R],V,_,L,W) :-
   u_lt(U,V), !,
   u_mins0(R,U,[X],L,W).
u_mins0([_|R],V,L0,L,W) :-
   u_mins0(R,V,L0,L,W).

u_lt(A,X--U) :-
   Y is -X,
   u_sum(A,Y--U,Z--_),
   Z<0.

dimensioned([(_--_):_|_]).

one_of([X|_],X).
one_of([_|R],X) :-
   one_of(R,X).

ratio(N,M,R) :- R is (N*100)/M.

card(S,N) :- length(S,N).
