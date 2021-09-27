
%:- include(sanity_tests).

:- use_module('../prolog/logicmoo_common').
:- use_module(library(plunit)).
:- use_module(library(test_cover)).
:- use_module(library(biocham_junit)).
:- at_halt(biocham_junit:run_junit_tests).

:- use_module(library(loop_check)).

loop_inf0 :- loop_check(loop_inf0).

loop_inf1 :- loop_check(loop_inf2,true).
loop_inf1 :- loop_inf0.

loop_inf2 :- loop_inf1.

loop_inf3 :- loop_inf1.
loop_inf3.


loop_inf4(X) :- loop_check(loop_inf4(X),X=1).
loop_inf4(2).

:- listing.

test(loop_inf0):- must(\+ loop_inf0).

test(loop_inf1):- must( loop_inf1),!.


test(loop_inf2):- must( loop_inf2),!.

test(loop_inf3):- must(   loop_inf3).

test(loop_inf4):- must((   loop_inf4(X),!,X=1)).

test(loop_inf4a):- must((   loop_inf4(X),X=2,!)).

%:- break.


