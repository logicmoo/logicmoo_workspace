:- module(pqr,[]).


:- use_module(library(logicmoo_base)).

:- begin_pfc.

==> p(1).
p(X) ==> q(X).
q(X) ==> r(X).

:- mpred_test(r(1)).

==> p(2).

:- mpred_test(r(2)).

  
