test_file(completion(s, _), "NL/base_grammar_test").
test_file(generate(s, _), "NL/base_grammar_test").

sentence(S, Mood, Polarity, Tense, Aspect) -->
   { input_from_player },
   [X],
   { X=='(',
     !,
     begin(bind(speaker, player),
	   bind(addressee, $me)) },
   s(S, Mood, Polarity, Tense, Aspect),
   opt_stop(Mood),
   [')'].

sentence(S, Mood, Polarity, Tense, Aspect) -->
   [ Name, ',' ],     
   { input_from_player },
   { bind_indexicals_for_addressing_character_named(Name) },
   s(S, Mood, Polarity, Tense, Aspect),
   opt_stop(Mood).

sentence(S, Mood, Polarity, Tense, Aspect) -->
   { bind_discourse_variables(S, Core) },
   s(Core, Mood, Polarity, Tense, Aspect),
   opt_stop(Mood).

bind_discourse_variables(Var, Var) :-
   var(Var),
   !.
bind_discourse_variables( (Core, Other), Core) :-
   !,
   bind_discourse_variables(Other).
bind_discourse_variables(S, S).

bind_discourse_variables( (X, Y)) :-
   !,
   bind_discourse_variables(X),
   bind_discourse_variables(Y).
bind_discourse_variables(is_a(Var, Kind)) :-
   !,
   bind(discourse_variables, [is_a(Var, Kind) | $discourse_variables]).
bind_discourse_variables(_).

%% discourse_variable_type(Var, Kind)
%  Var is bound in $discourse_variables using is_a(Var,Kind).
discourse_variable_type(Var, Kind) :-
   member(is_a(Var, Kind), $discourse_variables).

%% bound_discourse_variable(Var)
%  Var is an uninstantiated variable that is bound to a type in $discourse_variables.
bound_discourse_variable(Var) :-
   var(Var),
   discourse_variable_type(Var, _).

opt_stop(interrogative) --> [ '?' ].
opt_stop(_Mood) --> [ ].
opt_stop(Mood) --> [ '.' ], { Mood \= interrogative }.
opt_stop(Mood) --> [ '!' ], { Mood \= interrogative }.

%% s(?S, ?Mood, ?Polarity, ?Tense, ?Aspect)
%  Sentences

:- randomizable s/7.

%%%%
%%%% CLAUSES
%%%%

%%%
%%% Finite clauses
%%%

content_clause(ComplementLF, DeclarativePredicate, InterrogativePredicate, Predicate) -->
   complementizer(DeclarativePredicate, InterrogativePredicate, Predicate),
   { Predicate \= null },
   s(ComplementLF, indicative, affirmative, present, simple).
content_clause((Wh:(S, is_a(Wh, Kind))), _, InterrogativePredicate, InterrogativePredicate) -->
   { InterrogativePredicate \= null },
   whpron(Kind),
   np((NP^S1)^S, subject, Agreement, nogap, nogap),
   vp(base, X^X, NP^S1, _Tense, Agreement, np(Wh)).
content_clause(Object:(be(Subject, Object), is_a(Subject, Kind)), _, InterrogativePredicate, InterrogativePredicate) -->
   { InterrogativePredicate \= null },
   whpron(Kind),
   np((Subject^S)^S, subject, Agreement, nogap, nogap),
   aux_be(present, Agreement).
content_clause(Container:contained_in(Subject, Container), _, InterrogativePredicate, InterrogativePredicate) -->
   { InterrogativePredicate \= null },
   [where],
   np((Subject^S)^S, subject, Agreement, nogap, nogap),
   aux_be(present, Agreement).
content_clause(ComplementLF, _, InterrogativePredicate, InterrogativePredicate) -->
   { InterrogativePredicate \= null },
   s(ComplementLF, interrogative, affirmative, present, simple).

%% complementizer(+DeclarativePredicate, +InterrogativePredicate, -Predicate)
%  Matches a complementizer (that, if, whether, null), chooses which version of the
%  Predicate should be used based on whether this is a declarative or interrogative
%  content clause.
complementizer(_, IPredicate, IPredicate) --> [whether].
complementizer(_, IPredicate, IPredicate) --> [if].
complementizer(Predicate, _, Predicate) --> [that].
complementizer(Predicate, _, Predicate) --> [].


%%%
%%% Infinitival clauses
%%%

infinitival_clause(EnclosingSubject, S) -->
   { lf_subject(S, NP) },
   np((NP^S1)^S, subject, _Agreement, nogap, nogap),
   { NP \= EnclosingSubject },
   infinitival_vp(NP^S1).

infinitival_clause(EnclosingSubject, S) -->
   infinitival_vp(EnclosingSubject^S).

%%%%
%%%% SENTENCES
%%%%

%%%
%%% Indicative mood
%%%

s(S, indicative, Polarity, Tense, Aspect) -->
   { lf_subject(S, NP) },
   np((NP^S1)^S, subject, Agreement, nogap, nogap),
   aux_vp(NP^S1, Polarity, Agreement, Tense, Aspect).

% NP is [not] Adj
s(S, indicative, Polarity, Tense, simple) -->
   { lf_subject(S, Noun) },
   np((Noun^S)^S, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   ap(Noun^S).

% NP is [not] KIND
s(is_a(Noun, Kind), indicative, Polarity, Tense, simple) -->
   np((Noun^_)^_, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   [a],
   kind_noun(Kind, singular).

% NP is [not] NP
s(be(S, O), indicative, Polarity, Tense, simple) -->
   np((S^_)^_, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   np((O^_)^_, object, _, nogap, nogap).

% NP is [not] in NP
s(contained_in(S, Container), indicative, Polarity, Tense, simple) -->
   np((S^_)^_, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   [in],
   np((Container^_)^_, object, _, nogap, nogap),
   { is_a(Container, enclosing_container) }.

% NP is [not] on NP
s(contained_in(S, Container), indicative, Polarity, Tense, simple) -->
   np((S^_)^_, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   [on],
   np((Container^_)^_, object, _, nogap, nogap),
   { is_a(Container, work_surface) }.

% Character has  NP
s(contained_in(Object, Character), indicative, Polarity, Tense, simple) -->
   np((Character^_)^_, subject, Agreement, nogap, nogap),
   { character(Character) },
   aux_have(Tense, Agreement),
   opt_not(Polarity),
   np((Object^_)^_, object, _, nogap, nogap).

% NP's Property is [not] PropertyValue
s(property_value(Noun, Property, Value), indicative, Polarity, Tense, simple) -->
   { once(valid_property_value(Property, Value)),
     % Prefer other forms of realization, when available
     % But always let the user type this version if they want.
     ( input_from_player
       ;
       ( \+ adjectival_property(Property),
	 \+ nominal_property(Property) ) ) },
   np((Noun^_)^_, genitive, _Agreement, nogap, nogap),
   property_name(Property),
   copula(simple, Tense, third:singular),
   opt_not(Polarity),
   property_value_np(Property, Value).

% NP is [not] PropertyValue
s(property_value(Noun, Property, Value), indicative, Polarity, Tense, simple) -->
   { once(valid_property_value(Property, Value)),
     adjectival_property(Property) },
   np((Noun^_)^_, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   [Value].

% NP is [not] a PropertyValue
s(property_value(Noun, Property, Value), indicative, Polarity, Tense, simple) -->
   { once(valid_property_value(Property, Value)),
     nominal_property(Property) },
   np((Noun^_)^_, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   [a, Value].

% NP's Relation is [not] Relatum
s(related(Noun, Relation, Relatum), indicative, Polarity, Tense, simple) -->
   np((Noun^_)^_, genitive, _Agreement, nogap, nogap),
   genitive_form_of_relation(Relation, singular),
   copula(simple, Tense, third:singular),
   opt_not(Polarity),
   np((Relatum^_)^_, object, _Agreement, nogap, nogap).

%%%
%%% Imperative mood
%%%
s(S, imperative, Polarity, present, simple) -->
   { lf_subject(S, $addressee) },
   aux_vp($addressee^S, Polarity, second:singular, present, simple).
s(S, imperative, Polarity, present, simple) -->
   [let, us],
   { lf_subject(S, $dialog_group) },
   aux_vp($dialog_group^S, Polarity, first:singular, present, simple).

%%%
%%% Interrogative mood
%%%

% Yes/no question generated by subject-aux inversion
s(S, interrogative, Polarity, Tense, Aspect) -->
   { var(S) ; S \= (_:_) },  % don't try if we already know it's a wh-question.
   inverted_sentence(S, Polarity, Tense, Aspect).

% Normal inverted sentence
inverted_sentence(S, Polarity, Tense, Aspect) -->
   { lf_subject(S, NP) },
   aux(np((NP^S1)^S, subject, Agreement),
       Polarity, Agreement, Tense, Aspect, Form, Modality),
   vp(Form, Modality, NP^S1, Tense, Agreement, nogap).

% is NP a KIND?
s(is_a(Noun, Kind), interrogative, affirmative, Tense, simple) -->
   aux_be(Tense, Agreement),
   np((Noun^_)^_, subject, Agreement, nogap, nogap),
   [a],
   kind_noun(Kind, singular).

% Wh-questions about normal verbs

% This seems redundant with the "questions about the subject" rule below.
% % What Verbs?
% s((Wh:(S, is_a(Wh, Kind))), interrogative, affirmative, Tense, Aspect) -->
%    whpron(Kind),
%    aux_vp(Wh^S, affirmative, third:singular, Tense, Aspect).

% What did Subject Verb?
s((Wh:(S, is_a(Wh, Kind))), interrogative, affirmative, Tense, simple) -->
   whpron(Kind),
   aux_do(Tense, Agreement),
   np((NP^S1)^S, subject, Agreement, nogap, nogap),
   vp(base, X^X, NP^S1, Tense, Agreement, np(Wh)).


% Wh-questions about properties

% Whose property is Value?
s(Noun:property_value(Noun, Property, Value),
  interrogative, affirmative, Tense, simple) -->
   { valid_property_value(Property, Value) },
   [ whose ],
   property_name(Property),
   copula(simple, Tense, third:singular),
   property_value_np(Property, Value).

:- register_lexical_item(whose).

% NP's Property is [not] PropertyValue
s((Value:(property_value(Noun, Property, Value), is_a(Value, Kind))),
  interrogative, affirmative, Tense, simple) -->
   whpron(Kind),
   copula(simple, Tense, third:singular),
   np((Noun^_)^_, genitive, _Agreement, nogap, nogap),
   property_name(Property).

% Wh-questions about relations

% Whose relation is Value?
s(Noun:related(Noun, Relation, Value),
  interrogative, affirmative, Tense, simple) -->
   [ whose ],
   genitive_form_of_relation(Relation, Number),
   copula(simple, Tense, Person:Number),
   np((Value^_)^_, subject, Person:Number, nogap, nogap).

% NP's Relation is [not] RelationValue
s((Value:(related(Noun, Relation, Value), is_a(Value, Kind))),
  interrogative, affirmative, Tense, simple) -->
   whpron(Kind),
   copula(simple, Tense, third:Number),
   np((Noun^_)^_, genitive, _Agreement, nogap, nogap),
   genitive_form_of_relation(Relation, Number).

% Wh-questions about the subject.
s(Subject:(S, is_a(Subject, Kind)), interrogative, Polarity, Tense, Aspect) -->
   { lf_subject(S, Subject), var(Subject) },
   whpron(Kind),
   aux_vp(Subject^S, Polarity, _Agreement, Tense, Aspect).

% Wh-questions about the object.
s(Object:(S, is_a(Object, Kind)), interrogative, Polarity, Tense, Aspect) -->
   { lf_subject(S, NP) },
   whpron(Kind),
   aux(nogap, Polarity, Agreement, Tense, Aspect, Form, Modality),
   np((NP^S1)^S, subject, Agreement, nogap, nogap),
   vp(Form, Modality, NP^S1, Tense, Agreement, np(Object)).

% Who is/what is Subject
s(Object:(be(Subject, Object), is_a(Subject, Kind)), interrogative, affirmative, present, simple) -->
   whpron(Kind),
   aux_be(present, Agreement),
   np((Subject^S)^S, subject, Agreement, nogap, nogap).

% How is Subject?
s(Manner:manner(be(Subject), Manner), interrogative, affirmative, present, simple) -->
   [ how ],
   aux_be(present, Agreement),
   np((Subject^be(Subject))^be(Subject), subject, Agreement, nogap, nogap).

% How does Subject Predicate?
s(Method:method(S, Method), interrogative, affirmative, Tense, simple) -->
   [ how ],
   { lf_subject(S, NP) },
   aux_do(Tense, Agreement),
   np((NP^S1)^S, subject, Agreement, nogap, nogap),
   vp(base, M^M, NP^S1, present, Agreement, nogap).

:- register_lexical_item(how).

% Is he Adjective?
s(S, interrogative, Polarity, present, simple) -->
   aux_be(present, Agreement),
   opt_not(Polarity),
   np((Noun^_)^_, subject, Agreement, nogap, nogap),
   ap(Noun^S).

% Is X in/on Container?
s(contained_in(S, Container), interrogative, Polarity, Tense, simple) -->
   aux_be(Tense, Agreement),
   np((S^_)^_, subject, Agreement, nogap, nogap),
   opt_not(Polarity),
   [in],
   np((Container^_)^_, object, _, nogap, nogap),
   { is_a(Container, enclosing_container) }.

s(contained_in(S, Container), interrogative, Polarity, Tense, simple) -->
   aux_be(Tense, Agreement),
   np((S^_)^_, subject, Agreement, nogap, nogap),
   opt_not(Polarity),
   [on],
   np((Container^_)^_, object, _, nogap, nogap),
   { is_a(Container, work_surface) }.

% Is Subject a PROPERTYVALUE?
inverted_sentence(property_value(Subject, Property, Value), Polarity, Tense, simple) -->
   copula(simple, Tense, Agreement),
   opt_not(Polarity),
   np((Subject^_)^_, subject, Agreement, nogap, nogap),
   { nominal_property(Property),
     valid_property_value(Property, Value) },
   [ a, Value ].

% why did he X?
s(X:explanation(S, X), interrogative, Polarity, Tense, Aspect) -->
   [why],
   inverted_sentence(S, Polarity, Tense, Aspect).

:- register_lexical_item(why).

% where is NP
s(Container:contained_in(S, Container), interrogative, Polarity, Tense, simple) -->
   [where],
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   np((S^S)^S, subject, Agreement, nogap, nogap).

:- register_lexical_item(where).

% Who has  NP
s((Character:contained_in(Object, Character), is_a(Character, person)), interrogative, Polarity, Tense, simple) -->
   [who],
   aux_have(Tense, third:singular),
   opt_not(Polarity),
   np((Object^S)^S, object, _, nogap, nogap).

% what is on the X
s(S:contained_in(S, Container), interrogative, Polarity, Tense, simple) -->
   [what],
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   [on],
   { impose_selectional_constraint(Container, work_surface) },
   np((Container^Ignored)^Ignored, subject, Agreement, nogap, nogap),
   { is_a(Container, work_surface) }.

% Who/what is in the X
s(S:(contained_in(S, Container), is_a(S, Kind)), interrogative, Polarity, Tense, simple) -->
   whpron(Kind),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   [in],
   { impose_selectional_constraint(Container, enclosing_container) },
   np((Container^Ignored)^Ignored, subject, Agreement, nogap, nogap),
   { is_a(Container, enclosing_container), \+ character(Container) }.

% what does Character have?
s(S:contained_in(S, Character), interrogative, Polarity, Tense, simple) -->
   [what],
   aux_do(Tense, Agreement),
   opt_not(Polarity),
   np((Character^Ignored)^Ignored, subject, Agreement, nogap, nogap),
   { character(Character) },
   aux_have(Tense, Agreement).

%%%
%%% Adjectival phrases
%%% Seems silly to make a whole new file for one clause...
%%%

ap(Meaning) -->
   adjective(Meaning).