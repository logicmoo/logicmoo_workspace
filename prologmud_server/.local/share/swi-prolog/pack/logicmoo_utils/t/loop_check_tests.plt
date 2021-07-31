
:- include(sanity_tests).

:- use_module(library(loop_check)).

loop_inf0 :- loop_check(loop_inf0).

loop_inf1 :- loop_check(loop_inf2).
loop_inf1 :- loop_inf0.
loop_inf2 :- loop_inf1.

loop_inf3 :- loop_inf1.
loop_inf3.


test(loop_inf0):- must(\+ loop_inf0).

test(loop_inf1):- must(\+ loop_inf1).

test(loop_inf2):- must(\+ loop_inf2).

test(loop_inf3):- must(   loop_inf3).

