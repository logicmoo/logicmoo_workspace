:- module(implementation_module, [implementation_module/2]).

:- meta_predicate implementation_module(?,?).
% BUG: At compile time, if there exist other predicate with the same
% name in the libraries, and it is called before such predicate be
% defined, can report the wrong implementation module pointing at the
% libraries. Work around: Never use names that are already being used
% in the libraries. --EMM
implementation_module(MGoal, Module) :-
    predicate_property(MGoal, imported_from(Module)),
    !.
% Asume that if not imported, it is defined here:
implementation_module(Module:_, Module).

