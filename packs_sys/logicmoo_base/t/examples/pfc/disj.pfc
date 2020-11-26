:- use_module(library(logicmoo_user)).

% -*-Prolog-*-

:- op(1050,xfx,('===>')).

(P ===> Q) ==> 
  (P ==> Q),
  ( ~Q ==>  ~P).


or(P,Q) ==> 
  ( ~P ==> Q),
  ( ~Q ==> P).
		
prove_by_contradiction(P) :- P.
prove_by_contradiction(P) :-
  \+ ( ~P ; P),
  add( ~P),
  P -> rem( ~P)
    ; (rem( ~P),fail).

==> or(p,q).
==> (p ===> x).
==> (q ===> x).


% try :- prove_by_contradiction(x).

  