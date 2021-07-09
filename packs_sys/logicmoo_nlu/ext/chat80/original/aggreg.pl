/*  Copyright 1986-2020 David H. D. Warren, Fernando C. N. Pereira and
    Jan Wielemaker.

    Permission is hereby granted, free of charge, to any person obtaining a
    copy of this software and associated documentation files (the
    "Software"), to deal in the Software without restriction, including
    without limitation the rights to use, copy, modify, merge, publish,
    distribute, sublicense, and/or sell copies of the Software, and to
    permit persons to whom the Software is furnished to do so, subject to
    the following conditions:

    The above copyright notice and this permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
    OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

:- public aggregate80/3, one_of/2, ratio/3, card/2.


aggregate80(Fn,Set,Val) :-
   dimensioned(Set), !,
   u_aggr(Fn,Set,Val).
aggregate80(Fn,Set,Val) :-
   i_aggr(Fn,Set,Val).

i_aggr(average,Set,Val) :-
   i_total(Set,T),
   length(Set,N),
   Val is T/N.
i_aggr(total,Set,Val) :-
   i_total(Set,Val).
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

u_aggr(average,Set,V--U) :-
   u_total(Set,T--U),
   length(Set,N),
   V is T/N.
u_aggr(total,Set,Val) :-
   u_total(Set,Val).
u_aggr(max,Set,Val) :-
   u_maxs(Set,List),
   one_of(List,Val).
u_aggr(min,Set,Val) :-
   u_mins(Set,List),
   one_of(List,Val).
u_aggr(maximum,[V0:O|S],V) :-
   u_maxs0(S,V0,[O],_,V).
u_aggr(minimum,[V0:O|S],V) :-
   u_mins0(S,V0,[O],_,V).

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
u_total([V:_|R],T) :- !,
   u_total(R,T0),
   u_sum(T0,V,T).
u_total([V|R],T) :-
   u_total(R,T0),
   u_sum(T0,V,T).

u_sum(X--U,Y--U,Z--U) :- !,
   Z is X+Y.
u_sum(X--U,Y--U1,Z--U) :-
   ratio(U,U1,M,M1), M>M1, !,
   Z is X + (Y*M1)/M.
u_sum(X--U1,Y--U,Z--U) :-
   ratio(U,U1,M,M1), M>M1, !,
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
   Y is (-X),
%%% JPO: was:
%%% Y is -X,
   u_sum(A,Y--U,Z--_),
   Z<0.

dimensioned([(_--_):_|_]).

one_of([X|_],X).
one_of([_|R],X) :-
   one_of(R,X).

ratio(N,M,R) :- R is (N*100)/M.

card(S,N) :- length(S,N).


