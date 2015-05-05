:- module(assrt_meta, []).

:- use_module(library(location_utils)).
:- use_module(library(assertions/assrt_lib)).

:- create_prolog_flag(assrt_meta_pred, check, [type(atom)]).

% Extends assertion_db/11 to get assertions from meta predicate declarations.

assrt_lib:assertion_db(Head, M, Status, (pred), Comp, Call, Succ, Glob, "", [], Pos) :-
    current_prolog_flag(assrt_meta_pred, Status),
    Status \= none,
    Pred = M:Head,
    ( var(Head)
    ->current_predicate(M:F/A),
      functor(Head, F, A)
    ; true
    ),
    \+ predicate_property(Pred, imported_from(_)),
    '$predicate_property'(meta_predicate(Spec), Pred),
    % predicate_property(Pred, meta_predicate(Spec)),
    ( property_from(M:Spec, meta_predicate, Pos) -> true
    ; predicate_from(Pred, Pos)
    ),
    assertion(nonvar(Pos)),
    normalize_assertion_head(Spec, M, _, Pred, Comp, Call, Succ, Glob, _).
    % current_normalized_assertion(Status pred Spec, M, _, Pred, Status, Type,
    % 			 Comp, Call, Succ, Glob, Comm, _).
