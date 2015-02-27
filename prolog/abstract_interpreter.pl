:- module(abstract_interpreter, [abstract_interpreter/2,
				 head_abstraction/3]).

:- use_module(library(extra_location)).
:- use_module(library(implementation_module)).

:- dynamic inferred/3.		% Kind of manual tabling

:- meta_predicate abstract_interpreter(0,3).

abstract_interpreter(M:Goal, Abstraction) :-
    abstract_interpreter(Goal, M, Abstraction).

abstract_interpreter(Goal, M, _) :-
    (var(Goal) ; var(M)), !.
abstract_interpreter(true, _, _) :- !.
abstract_interpreter(fail, _, _) :- !, fail.
abstract_interpreter(M:Goal, _, Abs) :-
    abstract_interpreter(Goal, M, Abs).
abstract_interpreter(call(Goal), M, Abs) :-
    abstract_interpreter(Goal, M, Abs).
abstract_interpreter(\+ A, M, Abs) :-
    \+ abstract_interpreter(A, M, Abs).
abstract_interpreter((A, B), M, Abs) :-
    abstract_interpreter(A, M, Abs),
    abstract_interpreter(B, M, Abs).
abstract_interpreter((A;B), M, Abs) :-
    ( abstract_interpreter(A, M, Abs)
    ; abstract_interpreter(B, M, Abs)
    ).
abstract_interpreter(A->B, M, Abs) :-
    abstract_interpreter(A, M, Abs), % loose of precision
    abstract_interpreter(B, M, Abs).

abstract_interpreter(Goal, M, Abs) :-
      % Here we have to loose precision, but ensure termination
    ( predicate_property(M:Goal, interpreted)
    ->call(Abs, Goal, M, Body),
      ( predicate_property(M:Goal, transparent)
      ->CM = M
      ; predicate_property(M:Goal, imported_from(IM))
      ->CM = IM
      ; CM = M
      ),
      abstract_interpreter(Body, CM, Abs)
    ; true % Nothing to said
    ).

head_abstraction(Goal, M, true) :-
    head_body_abstraction(Goal, M, _).

head_body_abstraction(Goal, M, Body) :-
    ( predicate_property(M:Goal, dynamic),
      dyn_defined(Goal, M, Body)
    ; clause(M:Goal, Body)
    ).

dyn_defined(Goal, CM, true) :-
    implementation_module(CM:Goal, M),
    extra_location(Goal, M, dynamic(def, _, _), _).
