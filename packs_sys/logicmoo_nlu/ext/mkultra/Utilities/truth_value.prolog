%%%
%%% Meta-interpreter for reasoning about truth values
%%%
%%% Used when characters answer questions.  This allows characters
%%% to distinguish between something being false and something
%%% being unknown.  Also allows information in the global KB to
%%% be "hidden" from individual characters by declaring that they
%%% don't know it.
%%%

:- public truth_value/2, admitted_truth_value/3, know_that/1.
:- external know_whether/1, pretend_truth_value/3.
:- external know_property/3, know_relation/3,
            know_about_kind/1.

%% know_whether(?Predicate)
%  True when this character has knowledge about the truth of
%  Predicate.

%% truth_value(:P, ?TruthValue)
%  The truth value (true, false, or unknown) of P is for this character
%  is TruthValue.  Current version is very simplistic.
truth_value(P, unknown) :-
   \+ know_whether(P),
   !.
truth_value(P, true) :-
   P.
truth_value(P, false) :-
   \+ P.

%% know_that(:P)
%  P is true and the character knows it.
know_that(P) :-
   know_whether(P),
   P.

know_whether(is_a(_, entity)).
know_whether(is_a(Object, _Kind)) :-
   !,
   know_about_object(Object).

know_whether(property_value(Object, Property, Value)) :-
   !,
   know_property(Property, Object, Value).

know_whether(related(Object, Relation, Relatum)) :-
   !,
   know_relation(Relation, Object, Relatum).

know_about_object($me).
know_about_object(Person) :-
   related($me, know, Person).

know_about_object(illuminati).

know_whether(_).

know_relation(_Relation, Object, _Relatum) :-
   know_about_object(Object).
know_property(_Property, Object, _Value) :-
   know_about_object(Object).

know_about_object(Object) :-
   atomic(Object),
   is_a(Object, Kind),
   know_about_kind(Kind).

%% pretend_truth_value(+Listener, :P, ?TValue)
%  If Listener asks about P, pretend its truth value is TValue.
:- external pretend_truth_value/3.

%% admitted_truth_value(+Listener, :P, ?TValue)
%  The truth value for P that the character should admit to
%  Listener is TValue.
admitted_truth_value(Listener, (P1, P2), Value) :-
   !,
   admitted_truth_value(Listener, P1, Value1),
   admitted_truth_value(Listener, P2, Value2),
   and_truth_values(Value1, Value2, Value).
admitted_truth_value($me, P, Value) :-
   !,
   truth_value(P, Value).
admitted_truth_value(Listener, P, Value) :-
   pretend_truth_value(Listener, P , Value).
admitted_truth_value(Listener, P, Value) :-
   truth_value(P, Value),
   consistent_with_pretend_truth_value(Listener, P, Value).

consistent_with_pretend_truth_value(Listener, P, Value) :-
   pretend_truth_value(Listener, P, PretendValue) ->
       (Value=PretendValue)
       ;
       true.

and_truth_values(true, true, true) :-
   !.
and_truth_values(false, _, false) :-
   !.
and_truth_values(_, false, false) :-
   !.
and_truth_values(_, _, unknown).