:- module(ntabling, [tabling/2, tabling/3, tabling/4]).

% BUG: It performs an eager call of the Goal first.
:- meta_predicate tabling(0, 0).
tabling(Elem, Goal) :-
    \+ Elem,
    ( Goal,
      assertz(Elem),
      fail
    ; true
    ).

:- meta_predicate tabling(0, 0, -).
tabling(Elem, Goal, List) :-
    tabling(Elem, Goal, List, []).

:- meta_predicate tabling(0, 0, -, ?).
tabling(Elem, Goal, List, Tail) :-
    ( \+ Elem -> 
      findall(Elem, Goal, List, Tail)
    ; List = Tail
    ).
