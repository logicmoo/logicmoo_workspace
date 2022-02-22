%%
%% Predicates for manipulating logical forms
%%

:- external past/1, future/1, can/1, may/1, should/1, would/1, must/1.

%% modalized(?LF, ?Tense, ?Aspect, ?Modalized) is det
%  Modalized is LF inflected with Tense and Aspect

modalized(P, present, simple, P) :-
   !.
modalized(P, past, _, past(P)) :-
   !.
modalized(P, future, _, future(P)) :-
   !.

%% lf_main_predicate(:LF, -Core)
%  Strips ancillary conjuncts from LF and returns its main predicate,
%  including modals and/or question operators.
lf_main_predicate((P, _), C) :-
   !,
   lf_main_predicate(P, C).
lf_main_predicate(P,P).

%% lf_core_predicate(?LF, ?Predication)
%  If LF is bound, strips its conjuncts and modals to return just the predication of the main verb.

lf_core_predicate(S, _) :-
   var(S), !.  % do nothing if we don't know what the sentence LF is.
lf_core_predicate(_:S, P) :-
   !,
   lf_core_predicate(S, P).
lf_core_predicate(explanation(S,_), P) :-
   !,
   lf_core_predicate(S, P).
lf_core_predicate(not(S), P) :-
   !,
   lf_core_predicate(S, P).
lf_core_predicate(may(S), P) :-
   !,
   lf_core_predicate(S, P).
lf_core_predicate(should(S), P) :-
   !,
   lf_core_predicate(S, P).
lf_core_predicate(can(S), P) :-
   !,
   lf_core_predicate(S, P).
lf_core_predicate(must(S), P) :-
   !,
   lf_core_predicate(S, P).
lf_core_predicate(would(S), P) :-
   !,
   lf_core_predicate(S, P).
lf_core_predicate(S, S).


%% lf_subject(?LF, ?Subject)
%  If LF is bound, binds Subject to the term representing the subject of the main verb in LF.

% Don't do anything if S is uninstantiated
lf_subject(LF, _) :-
   var(LF), !.
lf_subject(LF, Subject) :-
   lf_core_predicate(LF, P),
   lf_core_predicate_subject(P, Subject).

lf_core_predicate_subject(be(Subject), Subject) :-
   !.
lf_core_predicate_subject(related(Subject, _, _), Subject):-
   !.
lf_core_predicate_subject(S, Subject) :-
   iv(past_participle, _, Subject^S, _, _, _, _).
lf_core_predicate_subject(S, Subject) :-
   tv(past_participle, _, Subject^_^S, _, _, _, _).
lf_core_predicate_subject(S, Subject) :-
   dtv(past_participle, _, Subject^_^_^S, _, _, _, _).
lf_core_predicate_subject(S, Subject) :-
   adjective(Subject^S, _, []).
lf_core_predicate_subject(S, Subject) :-
   modal_verb(_, _, Subject^_Complement^S, _, _).
lf_core_predicate_subject(S, Subject) :-
   verb_with_clausal_complement(_, _, Subject, _Complement, S, _, _, _).
lf_core_predicate_subject(S, Subject) :-
   verb_with_clausal_complement(_, _, Subject, _Complement, _, S, _, _).
lf_core_predicate_subject(S, Subject) :-
   verb_with_object_and_clausal_complement(_, _, Subject, _Object, _Complement, S, _, _, _).
lf_core_predicate_subject(S, Subject) :-
   verb_with_object_and_clausal_complement(_, _, Subject, _Object, _Complement, _, S, _, _).
lf_core_predicate_subject(S, Subject) :-
   turn_phrasal_verb(_, Subject, _, S).