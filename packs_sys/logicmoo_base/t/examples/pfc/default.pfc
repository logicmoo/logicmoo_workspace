:- use_module(library(logicmoo_user)).

% -*-Prolog-*-
% here is an example which defines default facts and rules.  Will it work?

:- begin_pfc.

(default(P)/mpred_literal(P))  ==>  (~( ~P) ==> P).

default((P ==> Q))/mpred_literal(Q) ==> (P, \+( ~Q) ==> Q).

% birds fly by default.
==> default((bird(X) ==> fly(X))).

% here's one way to do an zisa hierarchy.
% zisa = subclass.

zisa(C1,C2) ==>
  {P1 =.. [C1,X],
    P2 =.. [C2,X]},
  (P1 ==> P2).

==> zisa(canary,bird).
==> zisa(penguin,bird).

% penguins do not fly.
penguin(X) ==> ( ~fly(X)).

% chilly is a penguin.
==> penguin(chilly).

% tweety is a canary.
==> canary(tweety).


:- mpred_test(~fly(chilly)).

:- mpred_test(fly(tweety)).


