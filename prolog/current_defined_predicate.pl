:- module(current_defined_predicate, [current_defined_predicate/1]).

current_defined_predicate(MPI) :-
    MPI = M:F/A,
    current_module(M),
    M \= user,
    module_property(M, class(user)),
    current_predicate(MPI),
    functor(H, F, A),
    \+ predicate_property(M:H, imported_from(_)).
