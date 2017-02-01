:- module(rtchecks_example2,
          [pred1/2, pred2/2, pred3/1, pred4/2, aconcat/3,
           bad_concat/3, ppp1/0, test_all/2, test_atm/1]).

:- use_module(library(assertions)).
:- use_module(library(nativeprops)).

:- doc(author, "Edison Mera").

:- doc(module, "Examples of assertions for processing by the
        run-time checker.").

:- entry pred1/2 : (int * int).
:- entry pred1/2 : (atm * atm).

pred1(X, Y) :-
        display(pred1(X, Y)),
        nl.

pred2(X, Y) :-
        pred1(X, Y).

:- entry pred3(X) : int(X).

pred3(X) :-
        display(X),
        nl.

:- check comp pred4/2 : int * int + (not_fails, not_fails).
:- check comp pred4/2 : atm * atm + not_fails.

pred4(X, Y) :-
        display(p(X, Y)),
        nl.

:- entry pred5/2.

:- export(pred5/2).

pred5(a, b).

:- check success concat(A, B, X) : (A = [1, 2], B = [3]) => (X == [1, 2, 4]).

:- check exit aconcat/3 : (list * list * var) => (list * list * list).

aconcat([],    X, X).
aconcat([X|Y], Z, [X|T]) :-
        aconcat(Y, Z, T).

:- check exit bad_concat/3
        : (list * list * var) => (list * list * list).

bad_concat(_A, _X, a).

:- pred test_all(A, B) :: int(A) : int(A) => int(B) + not_fails.
:- pred test_all(A, B) :: atm(A) : atm(A) => atm(B) + not_fails.

test_all(A, A).

:- pred test_atm(A) : atm(A).
test_atm(A) :-
        test_atm2(A).

:- pred test_atm2(A) : atm(A).
test_atm2(_) :- fail.
test_atm2(A) :-
        test_atm3(A),
        display(done),
        nl.

:- pred test_atm3(A) : int(A).
test_atm3(A) :-
        display(A),
        nl.

:- check comp ppp1/0 + not_fails.

ppp1.
