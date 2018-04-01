:- module(map, [test/1,test_all/1]).

:- use_module(library(andorra/andorra)).

:- determinate(test_all(A),nonvar(A)).
:- determinate(next(A,B),(nonvar(A),nonvar(B))).
:- determinate(test(A),nonvar(A)).
:- determinate(goal(_,_,_,_,_,_),true).
:- determinate(goal_all(_,_,_,_,_,_),false).
:- determinate(ourdo,true).

test_all(A) :-
        A>0,
        goal_all(_B,_C,_D,_E,_F,_G),
        H is A-1,
        test_all(H).
test_all(0).

next(blue,yellow).
next(blue,red).
next(blue,green).
next(yellow,blue).
next(yellow,red).
next(yellow,green).
next(red,blue).
next(red,yellow).
next(red,green).
next(green,blue).
next(green,yellow).
next(green,red).

test(A) :-
        A>0,
        goal(_B,_C,_D,_E,_F,_G),
        H is A-1,
        test(H).
test(0).

goal(A,B,C,D,E,F) :-
        next(A,B),
        next(A,C),
        next(A,E),
        next(A,F),
        next(B,C),
        next(B,D),
        next(B,E),
        next(B,F),
        next(C,D),
        next(C,F),
        next(E,F).

goal_all(A,B,C,D,E,F) :-
        next(A,B),
        next(A,C),
        next(A,E),
        next(A,F),
        next(B,C),
        next(B,D),
        next(B,E),
        next(B,F),
        next(C,D),
        next(C,F),
        next(E,F),
        fail.
goal_all(_,_,_,_,_,_).

go :-
        goal_all(_A,_B,_C,_D,_E,_F).

map :-
        goal(_A, _B, _C, _D, _E, _F).
