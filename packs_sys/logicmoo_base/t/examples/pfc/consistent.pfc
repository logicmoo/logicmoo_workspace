:- use_module(library(logicmoo_user)).

% -*-Prolog-*-
% here is an interesting rule!

~P, P ==> contradiction(P).

contradiction(P) ==> 
  {format('~n% contradiction - both ~w and ( ~ ~w) added.~n',[P,P])}.

% this means that both P and Q can't be true.
disjoint(P,Q)
  ==>
  (P ==> ~Q),
  (Q ==> ~P).

==> disjoint(male(P), female(P)).

==> male(shirley).

==> mother(shirley,mary).

mother(X,_Y) ==> female(X).
