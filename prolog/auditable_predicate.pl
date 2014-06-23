:- module(auditable_predicate, [auditable_predicate/1]).

% An application predicate is a predicate that have at least one clause in the
% application side. We distinguish application from libraries, to extend the
% unused or the wrong dynamic analysis to exported predicates.

:- multifile application_predicate/1.

not_auditable_predicate(P) :-
    predicate_property(P, built_in),
    \+ predicate_property(P, multifile),
    \+ predicate_property(P, dynamic).
not_auditable_predicate(P) :-
    predicate_property(P, exported),
    \+ application_predicate(P).
not_auditable_predicate(P) :-
    predicate_property(P, dynamic),
    \+ application_predicate(P).
not_auditable_predicate(P) :-
    predicate_property(P, imported_from(_)).
not_auditable_predicate(P) :-
    predicate_property(P, foreign).
not_auditable_predicate(P) :-
    predicate_property(P, volatile).

:- meta_predicate auditable_predicate(?).
auditable_predicate(P) :-
    \+ not_auditable_predicate(P).
