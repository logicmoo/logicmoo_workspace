:- begin_tests(interface).

:- use_module(library(interface)).

:- use_module(test_intf).
:- use_module(test_impl1, []).
:- use_module(test_impl2, []).

test(binding1) :-
    bind_interface(test_intf, test_impl1),
    intf_pred(3, 4, X),
    assertion(X==3r4).

test(binding2) :-
    bind_interface(test_intf, test_impl2),
    intf_pred(3, 4, X),
    assertion(X==0.75).

:- end_tests(interface).
