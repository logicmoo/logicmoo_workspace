
:- include('../ec_test_incl').

:- module(ec).

do_test_cakes(N) :- abdemo_special(loops,[holds_at(num_cakes(N),t)],R).

%do_test(1) :- do_test_cakes(1).
%do_test(0) :- do_test_cakes(0).
do_test(4) :- do_test_cakes(4).
do_test(5) :- do_test_cakes(5).
do_test(6) :- \+ do_test_cakes(6).
do_test(3) :- do_test_cakes(3).

fluent(num_cakes(_)).

executable(eat_cakes(_)).

initially(num_cakes(5)).

axiom( initiates(eat_cakes(Eat),num_cakes(Remaining),T), 
 [ holds_at(num_cakes(Start), T),
   call((   
   plus(Remaining, Eat, Start),
   Start>=0,Remaining>=0,Eat>=0)),
   holds_at(num_cakes(Start), T)]).

axiom(terminates(eat_cakes(Eat),num_cakes(N),T), [call((number(E),Eat>0)),holds_at(num_cakes(N),T)]).

/* Abduction policy */

abducible(dummy).


:- listing(ec_current_domain_db).


:- run_tests.




