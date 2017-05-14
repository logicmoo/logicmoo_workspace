:- module(ai_extra_clauses, []).

:- use_module(library(implementation_module)).
:- use_module(library(extra_location)).
:- use_module(library(abstract_interpreter), []).

abstract_interpreter:extra_clauses(Goal, CM, CM:true, From) :-
    predicate_property(CM:Goal, dynamic),
    implementation_module(CM:Goal, M),
    loc_dynamic(Goal, M, dynamic(def, _, _), From).
