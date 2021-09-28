:- module(test_impl2, []).

:- use_module(library(interface)).

:- implements(test_intf).

intf_pred(A, B, C) :-
    C is A / B.

intf_meta(A, A).
