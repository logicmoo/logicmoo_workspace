:- module(current_defined_predicate, [current_defined_predicate/1]).

current_defined_predicate(Module:F/A) :-
    MPI = Module:F/A,
    current_module(Module),
    Module \= user,
    module_property(Module, class(user)),
    current_predicate(MPI).

