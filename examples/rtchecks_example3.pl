:- module(rtchecks_example3, [nullasr/2,
                              square/2,
                              test1/0,
                              animal/1,
                              family/1,
                              fullasr/2,
                              p/1]).

:- use_module(library(assertions)).
:- use_module(library(basicprops)).
:- use_module(library(nativeprops)).
:- use_module(library(plprops)).
:- use_module(library(rtchecks)).

:- pred nullasr/2.

nullasr(_, _).

square(X, X2) :-
        X2 is X * X,
        check(X2 > 0).

% this should generate a runtime-check error:
test1 :-
        square(0, X2),
        display(X2),
        nl.

:- prop animal/1 is type.

animal(A) :- atm(A).
animal(A) :- int(A).

:- prop family/1 is type.

family(B) :- atm(B).

:- entry fullasr(A, B) : (animal(A), var(B)).

:- pred fullasr(A, _) :: atm(A) : atm(A) => atm(A).
:- pred fullasr(A, B) :: atm(A) : (animal(A), atm(A)) => family(B) + not_fails.
:- pred fullasr(A, B) :: atm(A) : animal(A) => family(B) + is_det.
:- pred fullasr(A, B) :: (num(A), int(A)) : animal(A) => family(B) + is_det.
:- calls fullasr(A, _) :: str(A).
:- success fullasr(A, _) : int(A) => nnegint(A).

:- rtchecked
    fullasr/2.

fullasr(X, X).

:- pred p/1 is semidet.

p(_) :-
    r.

:- pred r/0 is det.
r :-
    display(2),
    qq,
    display(1).

:- pred qq/0 + not_fails.
:- pred qq/0 + fails.

qq :- fail.
