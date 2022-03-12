%%%
%%% Meta-interpreter for reasoning about truth values
%%%
%%% Used when characters answer questions.  This allows characters
%%% to distinguish between something being false and something
%%% being unknown.  Also allows information in the global KB to
%%% be "hidden" from individual characters by declaring that they
%%% don't know it.
%%%
:- style_check(-discontiguous).

:- multifile(closed_word_naf/1).

:- public truth_value/2, admitted_truth_value/3,
   know_true/1, know_false/1,
   closed_word_naf/1,
   thinks/2, knows_if/2.
:- external know_whether/1, pretend_truth_value/3.
:- dynamic know_property/3, know_relation/3,
   know_about_kind/1, closed_word_naf/1.
:- external believes/2, know/2, knows_value/2.



%=autodoc
%% thinks( ?Me, ?Proposition) is semidet.
%
% Thinks.
%
thinks($me, Proposition) :-
   truth_value(Proposition, true).


%=autodoc
%% believes( ?Me, ?Proposition) is semidet.
%
% Believes.
%
believes($me, Proposition) :-
   truth_value(Proposition, true).


%=autodoc
%% know( ?Me, ?Proposition) is semidet.
%
% Know.
%
know($me, Proposition) :-
   truth_value(Proposition, true).


%=autodoc
%% knows_if( ?Me, ?Proposition) is semidet.
%
% Knows If.
%
knows_if($me, Proposition) :-
   truth_value(Proposition, true).

%% know_whether(?Predicate)
%  True when this character has knowledge about the truth of
%  Predicate.



%=autodoc
%% informed_about( ?Me, ?P) is semidet.
%
% Informed About.
%
informed_about($me, P) :- know_whether(P).

%% truth_value(:P, ?TruthValue)
%  The truth value (true, false, or unknown) of P is for this character
%  is TruthValue.  Current version is very simplistic.

truth_value(P, Value) :-
   var(Value), !,
   determine_truth_value(P, Value).
truth_value(P, unknown) :-
   determine_truth_value(P, unknown).
truth_value(P, true) :-
   know_true(P).
truth_value(P, false) :-
   know_false(P).

%% determine_truth_value(:Sentence, -TruthValue)
%  Returns the truth value for Sentence for this character
determine_truth_value((P, Q), Value) :-
   !,
   determine_truth_value(P, LeftValue),
   continue_conjunction(Q, LeftValue, Value).


%=autodoc
%% continue_conjunction( ?ARG1, ?Unknown2, ?Unknown3) is semidet.
%
% Continue Conjunction.
%
continue_conjunction(_, false, false).
continue_conjunction(_, unknown, unknown).
continue_conjunction(Q, LValue, Value) :-
   LValue \= false,
   LValue \= unknown,
   determine_truth_value(Q, Value).
determine_truth_value((P ; Q), Value) :-
   !,
   determine_truth_value(P, LeftValue),
   continue_disjunction(Q, LeftValue, Value).


%=autodoc
%% continue_disjunction( ?ARG1, ?ARG2, ?ARG3) is semidet.
%
% Continue Disjunction.
%
continue_disjunction(_, true, true).
continue_disjunction(Q, LValue, Value) :-
   LValue \= true,
   determine_truth_value(Q, Value).

determine_truth_value(~P, Value) :-
   !,
   determine_truth_value(P, NotValue),
   invert_truth_value(NotValue, Value).


%=autodoc
%% invert_truth_value( ?Unknown1, ?Unknown2) is semidet.
%
% Invert Truth Value.
%
invert_truth_value(true, false).
invert_truth_value(false, true).
invert_truth_value(unknown, unknown).

determine_truth_value(P, unknown) :-
   ~informed_about($me, P).
determine_truth_value(P, true) :-
   informed_about($me, P),
   P.
determine_truth_value(P, false) :-
   informed_about($me, P),
   ~P.
determine_truth_value(P, false) :-
   closed_word_naf(P),
   informed_about($me, P),
   \+ P.
determine_truth_value(P, unknown) :-
   \+ closed_word_naf(P),
   \+ P,
   \+ ~P.



%=autodoc
%% closed_word_naf( ?ARG1) is semidet.
%
% Closed Word Negation-by-faliure.
%
closed_word_naf(iz_a(_,_)).

%% know_true(:Sentence)
%  This character knows that Sentence is true.
know_true((P, Q)) :-
   !,
   know_true(P),
   know_true(Q).
know_true((P; Q)) :-
   !,
   (know_true(P) ; know_true(Q)).
know_true(~P) :-
   !,
   know_false(P).
know_true(P) :-
   informed_about($me, P),
   P.

%% know_false(:Sentence)
%  This character knows that Sentence is false.
know_false((P, Q)) :-
   !,
   (know_false(P) ; know_false(Q)).
know_false((P; Q)) :-
   !,
   (know_false(P), know_false(Q)).
know_false(~P) :-
   !,
   know_true(P).
know_false(P) :-
   informed_about($me, P),
   (~P ; (closed_word_naf(P), \+ P)).

%%%%%%%%%%%%%%%%%%%%%% OLD CODE %%%%%%%%%%%%%%%%%%%%%%%%
% truth_value(P, unknown) :-
%    \+ know_whether(P),
%    !.
% truth_value(P, true) :-
%    P.
% truth_value(P, false) :-
%    \+ P.

$global_root/configuration/omniscent_characters.

know_whether(_) :-
   $global_root/configuration/omniscent_characters.
know_whether(iz_a(_, entity)).
know_whether(iz_a(Object, _Kind)) :-
   !,
   know_about_object(Object).

know_whether(property_value(Object, Property, Value)) :-
   !,
   know_property(Property, Object, Value).

know_whether(related(Object, Relation, Relatum)) :-
   !,
   know_relation(Relation, Object, Relatum).



%=autodoc
%% know_about_object( ?Theclub1) is semidet.
%
% Know About Object.
%
know_about_object($me).
know_about_object(Person) :-
   related($me, know, Person);know($me, Person).

know_about_object(theclub).

know_whether(_).



%=autodoc
%% know_relation( ?Relation, ?Object, ?Relatum) is semidet.
%
% Know Relation.
%
know_relation(_Relation, Object, _Relatum) :-
   know_about_object(Object).


%=autodoc
%% know_property( ?Property, ?Object, ?Value) is semidet.
%
% Know Property.
%
know_property(_Property, Object, _Value) :-
   know_about_object(Object).

know_about_object(Object) :-
   atomic(Object),
   iz_a(Object, Kind),
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
admitted_truth_value(player, P, Value) :-
   !,
   admitted_truth_value($pc, P, Value).
admitted_truth_value(Listener, P, Value) :-
   pretend_truth_value(Listener, P , Value),
   !,
   emit_grain("untrue", 100),
   affective_reaction(0, 0, 1, 0.2).
admitted_truth_value(Listener, P, Value) :-
   truth_value(P, Value),
   consistent_with_pretend_truth_value(Listener, P, Value).



%=autodoc
%% consistent_with_pretend_truth_value( ?Listener, ?P, ?Value) is semidet.
%
% Consistent Using Pretend Truth Value.
%
consistent_with_pretend_truth_value(Listener, P, Value) :-
   pretend_truth_value(Listener, P, PretendValue) ->
       (Value=PretendValue)
       ;
       true.



%=autodoc
%% and_truth_values( ?ARG1, ?ARG2, ?Unknown3) is semidet.
%
% And Truth Values.
%
and_truth_values(true, true, true) :-
   !.
and_truth_values(false, _, false) :-
   !.
and_truth_values(_, false, false) :-
   !.
and_truth_values(_, _, unknown).