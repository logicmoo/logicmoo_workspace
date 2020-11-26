:- include('../ec_test_incl').

/*

%   Formulae for the shopping example from Russell and Norvig

*/

%   Here's a test query.

   do_test(shopping1) :- abdemo_special(easy,[holds_at(have(drill),t), holds_at(have(milk),t),
        holds_at(have(banana),t), holds_at(at(home),t)],R).

%   Here's a simpler one.

   do_test(shopping2) :- abdemo_special(easy,[holds_at(have(drill),t), holds_at(have(milk),t)],R).



axiom(initiates(go(X),at(X),T),[]).

axiom(terminates(go(X),at(Y),T),[diff(X,Y)]).

axiom(initiates(buy(X),have(X),T),[sells(Y,X), holds_at(at(Y),T)]).

axiom(sells(hws,drill),[]).

axiom(sells(sm,milk),[]).

axiom(sells(sm,banana),[]).



/* Abduction policy */

abducible(dummy).

executable(go(X)).

executable(buy(X)).


