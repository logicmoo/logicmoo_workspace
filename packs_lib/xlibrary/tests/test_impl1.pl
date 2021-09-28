:- module(test_impl1, []).

:- use_module(library(interface)).

:- implements(test_intf).

intf_pred(A, B, C) :-
    C is A rdiv B.

intf_meta(A, A).
