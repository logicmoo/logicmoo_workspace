:- module(checkers_hooks, []).

checkable_predicate:application_predicate(M:_) :-
    application_module(M).

application_module(cwda).
application_module(cwdb).
