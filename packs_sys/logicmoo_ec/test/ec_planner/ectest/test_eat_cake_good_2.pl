
:- include('../ec_test_incl').

:- module(ec).
/*
   Test queries:

*/

%do_test(num_cakes(0)).
%do_test(num_cakes(1)).

do_test(G) :- G= neg(num_cakes(0)), ec_prove(G).
do_test(G) :- G= neg(num_cakes(1)), ec_prove(G).

do_test(G) :- G= eat_cakes(0), ec_prove(G).
do_test(G) :- G= eat_cakes(1), ec_prove(G).

%do_test(G) :- G= [happens(eat_cakes(1),t),holds_at(eat_cakes(0),t)], fail_solve_goal(G,R).
%do_test(G) :- G= [happens(eat_cakes(1),t),holds_at(eat_cakes(1),t)], ec_prove(G).

do_test(G) :-  G= [ b(start, t), b(t, end), happens(eat_cakes(1), t), holds_at(num_cakes(0), end) ], 
  ec_prove(G).

do_test(G) :- G= {eat_cakes(1),num_cakes(0)}, ec_prove(G).

do_test(G) :- G= {happens(eat_cakes(1),t),holds_at(num_cakes(1),start)}, ec_prove(G).

do_test(G) :- G= {happens(eat_cakes(1),t),holds_at(num_cakes(1),t-1)}, ec_prove(G).

do_test(G) :- G= {happens(eat_cakes(1),t),holds_at(num_cakes(0),end)}, ec_prove(G).

% rus out of stack but should just fail
% do_test(G) :- G= {happens(eat_cakes(1),start),holds_at(num_cakes(1),end)}, ec_prove(G).

fluent(num_cakes(_Integer)).

axiom(initially(num_cakes(5))).

axiom( initiates(eat_cakes(Eat),num_cakes(Remaining),T), [holds_at(num_cakes(Start),T),call((plus(Remaining, Eat, Start),Start>=0,Remaining>=0,Eat>=0))]).
axiom(terminates(eat_cakes(Eat),num_cakes(N),T), [call((number(E),Eat>0)),holds_at(num_cakes(N),T)]).

axiom( initiates(make_cakes(Made),num_cakes(Remaining),T), [holds_at(num_cakes(Start),T),call((plus(Start, Made, Remaining),Start>=0,Remaining>=0,Made>=0))]).
axiom(terminates(make_cakes(Made),num_cakes(N),T), [call((number(E),Made>0)),holds_at(num_cakes(N),T)]).

%axiom(initiates(eat_cakes(0),num_cakes(0),T), [holds_at(num_cakes(0),T)]).
%axiom(initiates(eat_cakes(0),num_cakes(1),T), [holds_at(num_cakes(1),T)]).
%axiom(initiates(eat_cakes(0),num_cakes(N),T), [holds_at(num_cakes(N),T)]).

%axiom(initiates(imagine_initiates(Holds),Holds,T), [holds_at(neg(Holds),T),holds_at(hypothesizing(Holds),T)]).
%axiom(terminates(imagine_terminates(Holds),Holds,T), [holds_at(Holds,T),holds_at(hypothesizing(Holds),T)]).

/*
*/
%axiom(initiates(immagine_initiates(Holds),Holds,T), [holds_at(neg(Holds),T)]).
%axiom(terminates(immagine_terminates(Holds),Holds,T), [holds_at(Holds,T)]).
%axiom(releases(immagine_releases(Holds),Holds,T), [holds_at(Holds,T)]).

%axiom(holds_at(num_cakes(0),T), [holds_at(neg(num_cakes(1)),T)]).

%axiom(holds_at(neg(num_cakes(0)),T),
%     [holds_at(num_cakes(1),T)]).

%axiom(holds_at(num_cakes(1),T),[holds_at(neg(num_cakes(0)),T)]).

% Why causes loops?
%axiom(holds_at(neg(num_cakes(1)),T),
%     [holds_at(num_cakes(0),T)]):- fail.

/* Abduction policy */

abducible(dummy).

%executable(imagine_terminates(_)).
%executable(imagine_initiates(_)).
executable(make_cake(_)).
executable(eat_cakes(_)).
%executable(ignore_cakes(_)).



