%
%  utils.pl:  some low-level utility predicates.
%
%  Copyright 2014, Ryan Kelly
%
%  This file supplies some basic low-level utility predicates.
%

%
%  ismember(Elem,List)  -  like member/2, but does not bind variables or
%                          allow backtracking.
%

ismember(_, []) :- fail.
ismember(E, [H|T]) :-
  ( E == H ->
    true
  ;
    ismember(E, T)
  ).

%
%  findmember(Elem,List,Idx)  -  like nth0/2, but does not bind variables or
%                                allow backtracking.
%

findmember(E, L, N) :-
  findmember(E, L, 0, N).

findmember(_, [], _, _) :- fail.
findmember(E, [H|T], Acc, N) :-
  ( E == H ->
    N = Acc
  ;
    NextAcc is Acc + 1,
    findmember(E, T, NextAcc, N)
  ).

%
%  vdelete(List,Elem,Result) - like delete/3 but using equivalence rather
%                              than unification, so it can be used on lists
%                              of variables
%

vdelete([], _, []).
vdelete([H|T], E, Res) :-
  ( E == H ->
    vdelete(T, E, Res)
  ;
    Res = [H|T2],
    vdelete(T, E, T2)
  ).

vdelete_list(L, [], L).
vdelete_list(L, [H|T], L2) :-
  vdelete(L, H, L1),
  vdelete_list(L1, T, L2).

%
%  pairfrom(L,E1,E2,Rest)  -  E1 and E2 are a pair of (different) elements
%                             from L, wile Rest is the rest of the list
%
%  Like doing (member(E1,L), member(E2,L))  but more efficient, doesn't match
%  E1 and E2 to the same element, and doesnt generate equivalent permutations.
%

pairfrom(L, E1, E2, Rest) :-
  pairfrom_rec(L, [], E1, E2, Rest).

pairfrom_rec([H|T], Rest1, E1, E2, Rest) :-
  E1 = H, select(E2, T, Rest2), append(Rest1, Rest2, Rest)
  ;
  pairfrom_rec(T, [H|Rest1], E1, E2, Rest).

%
%  joinlist(+Op,+In,-Out) - join items in a list using given operator
%

joinlist(_, [H], H) :- !.
joinlist(O, [H|T], J) :-
  T \= [],
  J =.. [O, H, TJ],
  joinlist(O, T, TJ).

%
%  subs(Name,Value,Old,New) -  substitue values in a term
%
%  This predicate is true when New is equal to Old with all occurances
%  of Name replaced by Value - basically, a symbolic substitution
%  routine.  For example, it is usually used to produce a result such
%  as:
%
%      subs(now,S,fluent(now),fluent(S)).
%

subs(X, Y, T, Y) :- T == X, !.
subs(_, _, T, T) :- var(T), !.
subs(X, Y, T, Tr) :-
  T =.. [F|Ts],
  subs_list(X, Y, Ts, Trs),
  Tr =.. [F|Trs].

subs_list(_, _, [], []) :- !.
subs_list(X, Y, [T|Ts], [Tr|Trs]) :-
  subs(X, Y, T, Tr),
  subs_list(X, Y, Ts, Trs).


:- begin_tests(utils, [sto(rational_trees)]).


test(ismember) :-
  ismember(A, [A, B, c]),
  ismember(B, [A, B, c]),
  ismember(c, [A, B, c]),
  \+ ismember(A, [B, _, d]).

test(findmember) :-
  findmember(A, [A, B, c], 0),
  findmember(B, [A, B, c], 1),
  findmember(c, [A, B, c], 2),
  \+ findmember(_, [A, B, c], _),
  \+ findmember(e, [A, B, c], _).

:- end_tests(utils).
