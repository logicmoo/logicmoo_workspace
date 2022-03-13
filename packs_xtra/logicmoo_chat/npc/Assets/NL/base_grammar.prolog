

:- multifile(test_file/2).
%% test_file( ?TestMask, ?Filename) is semidet.
%
% Test File.
%
test_file(completion(s, _), "NL/base_grammar_test").
test_file(generate(s, _), "NL/base_grammar_test").




%% sentence( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is semidet.
%
% Sentence.
%
sentence(S, Mood, Polarity, Tense, Aspect) -->  
  ( {input_from_player}  ,
    theTextM1('('),  !, 
    with_bind( speaker, 
      $user, 
      with_bind( addressee, 
        $me, 
        s(S, Mood, Polarity, Tense, Aspect), opt_stop(Mood))), 
    theTextM1(')')).

sentence(S, Mood, Polarity, Tense, Aspect) -->
   [ Name, ',' ],     
   { input_from_player },
   bind_indexicals_for_addressing_character_named(Name,
    (s(S, Mood, Polarity, Tense, Aspect),
     opt_stop(Mood))).

sentence(S, Mood, Polarity, Tense, Aspect) -->
   bind_discourse_variables(S, Core,
   (s(Core, Mood, Polarity, Tense, Aspect),
    opt_stop(Mood))).




%% bind_discourse_variables( ?ARG1, ?ARG2, ?ARG3) is semidet.
%
% Bind Discourse Variables.
%
bind_discourse_variables(Var, Var, G) --> {var(Var)}, !, G.
bind_discourse_variables( (Core, Other), Core, G) --> !, bind_discourse_variables(Other, G).
bind_discourse_variables(S, S, G) --> G.




%% bind_discourse_variables( ?ARG1, ?ARG2) is semidet.
%
% Bind Discourse Variables.
%
bind_discourse_variables( (X, Y), G) -->  !,
   bind_discourse_variables(X,
    bind_discourse_variables(Y, G)).
bind_discourse_variables(iz_a(Var, Kind), G) -->
   { must_getvar(discourse_variables,DV) }, !,
     with_bind(discourse_variables, [iz_a(Var, Kind) | DV], G).
bind_discourse_variables(_, G) --> G.

%% discourse_variable_type(Var, Kind)
%  Var is bound in $discourse_variables using iz_a(Var,Kind).
discourse_variable_type(Var, Kind) :-
    must_getvar(discourse_variables,DV),
    DV \== null,
   member(iz_a(Var, Kind), DV).

%% bound_discourse_variable(Var)
%  Var is an uninstantiated variable that is bound to a type in $discourse_variables.
bound_discourse_variable(Var) :-
   var(Var),
   discourse_variable_type(Var, _).




%% opt_stop( ?ARG1) is semidet.
%
% Opt Stop.
%
opt_stop(interrogative)-->theTextM1(?).
opt_stop(_Mood) --> [ ].
opt_stop(Mood)-->theTextM1('.'), {Mood\=interrogative}.
opt_stop(Mood)-->theTextM1(!), {Mood\=interrogative}.

%% s(?S, ?Mood, ?Polarity, ?Tense, ?Aspect)
%  Sentences

:- randomizable s/7.
:- randomizable ss/7.


:- discontiguous s//5.
:- discontiguous ss//5.
/*
vvpp( Form, PredicateModalIn, SubjectS, Tense, Agreement, GapInfo) -->  theTextM1(not),
  {negatePM(PredicateModalIn,PredicateModalOut)},
  vp( Form, PredicateModalOut, SubjectS, Tense, Agreement, GapInfo), 
  {negatePM(PredicateModalIn,PredicateModalOut)}.
*/

vvpp( Form, PredicateModal, SubjectS, Tense, Agreement, GapInfo) -->  
  vp( Form, PredicateModal, SubjectS, Tense, Agreement, GapInfo).

negatePM(PredicateModalIn,PredicateModalOut):- nonvar(PredicateModalIn),PredicateModalIn=(P^M),
  PredicateModalOut=PP^MM,negatePM(P,PP),=(M,MM).
negatePM(PredicateModalOut,PredicateModalIn):- nonvar(PredicateModalIn),PredicateModalIn=(P^M),
  PredicateModalOut=PP^MM,negatePM(P,PP),=(M,MM).
negatePM(PredicateModalIn,PredicateModalOut):- nonvar(PredicateModalIn),nonvar(PredicateModalOut),!.
negatePM(PredicateModalIn,PredicateModalOut):- nonvar(PredicateModalIn),!, PredicateModalOut = not(PredicateModalIn).
negatePM(PredicateModalOut,PredicateModalIn):- nonvar(PredicateModalIn),!, PredicateModalOut = not(PredicateModalIn).
/*
vp( Form, 
  Predicate^not(Modal), Subject^S, Tense, 
  Agreement, GapInfo) -->  theTextM1(not),
  vp( Form, 
  Predicate^Modal, Subject^S, Tense, 
  Agreement, GapInfo).
*/

%%%%
%%%% CLAUSES
%%%%

%%%
%%% Finite clauses
%%%




%% content_clause( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is semidet.
%
% Content Clause.
%
content_clause(ComplementLF, DeclarativePredicate, InterrogativePredicate, Predicate) -->
   complementizer(DeclarativePredicate, InterrogativePredicate, Predicate),
   { Predicate \= null },
   s(ComplementLF, indicative, affirmative, present, simple).
content_clause((Wh:(S, iz_a(Wh, Kind))), _, InterrogativePredicate, InterrogativePredicate) -->
   { InterrogativePredicate \= null },
   whpron(Kind),
   np((NP^S1)^S, subject, Agreement, nogap, nogap),
   vvpp(base, X^X, NP^S1, _Tense, Agreement, np(Wh)).
content_clause(Object:(be(Subject, Object), iz_a(Subject, Kind)), _, InterrogativePredicate, InterrogativePredicate) -->
   { InterrogativePredicate \= null },
   whpron(Kind),
   np((Subject^S)^S, subject, Agreement, nogap, nogap),
   aux_be(present, Agreement).
content_clause( Container:t(contained_in, Subject, Container), 
  _, InterrogativePredicate, InterrogativePredicate) -->  
  ( {InterrogativePredicate\=null}  ,
    theTextM1(where), 
    np((Subject^S)^S, subject, Agreement, nogap, nogap), 
    aux_be(present, Agreement)).
content_clause(ComplementLF, _, InterrogativePredicate, InterrogativePredicate) -->
   { InterrogativePredicate \= null },
   s(ComplementLF, interrogative, affirmative, present, simple).

%% complementizer(+DeclarativePredicate, +InterrogativePredicate, -Predicate)
%  Matches a complementizer (that, if, whether, null), chooses which version of the
%  Predicate should be used based on whether this is a declarative or interrogative
%  content clause.
complementizer(_, IPredicate, IPredicate)-->theTextM1(whether).
complementizer(_, IPredicate, IPredicate)-->theTextM1(if).
complementizer(Predicate, _, Predicate)-->theTextM1(that).
complementizer(Predicate, _, Predicate) --> [].


%%%
%%% Infinitival clauses
%%%




%% infinitival_clause( ?ARG1, ?ARG2) is semidet.
%
% Infinitival Clause.
%
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




%% a_an is semidet.
%
% A An.
%
a_an-->theTextM1(a);theTextM1(an);[].




%% swizzle_lf( ?UPARAM1, ?LF) is semidet.
%
% Swizzle Lf.
%
swizzle_lf(LF,LF).
swizzle_lf(t(location, X, Y), t(contained_in, X, Y)).
swizzle_lf(t(location, X, Y), t(contained_in, X, Y)).
%swizzle_lf(located(X,Y),contained_in(X,Y)).
%swizzle_lf(LF,SLF):- nonvar(SLF),LF=SLF.
%swizzle_lf(LF,SLF):- var(LF),!,freeze(SLF,swizzle_lf(LF,SLF)).




%% s( ?LF, +Indicative, ?Polarity, ?Tense, ?Aspect, ?S, ?E) is semidet.
%
% S.
%
s(LF, Indicative, Polarity, Tense, Aspect, S, E) :- 
  % {enforce_args(S)},
   swizzle_lf(LF,SLF),
   limit(1000, ss(SLF, Indicative, Polarity, Tense, Aspect, S, E)).




%% ss( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is semidet.
%
% Ss.
%
ss(S, indicative, Polarity, Tense, Aspect) -->
   { lf_subject(S, NP) },
   np((NP^S1)^S, subject, Agreement, nogap, nogap),
   aux_vp(NP^S1, Polarity, Agreement, Tense, Aspect).

% NP is [not] Adj
ss(S, indicative, Polarity, Tense, simple) -->
   { lf_subject(S, Noun) },
   np((Noun^S)^S, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   ap(Noun^S).

% NP is [not] KIND
ss(iz_a(Noun, Kind), indicative, Polarity, Tense, simple) -->
   np((Noun^_)^_, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   a_an,
   kind_noun(Kind, singular).

% NP is [not] NP
ss(be(S, O), indicative, Polarity, Tense, simple) -->
   np((S^_)^_, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   np((O^_)^_, object, _, nogap, nogap).




%% x_is_cont_in( ?Contained_in1, ?On2, ?Work_surface3) is semidet.
%
% X If Is A Cont In.
%
x_is_cont_in(contained_in, in, enclosing_container).
x_is_cont_in(contained_in, on, work_surface).

% NP is [not] in NP
ss(t(Contained_in, S, Container), indicative, Polarity, Tense, simple) -->  
  {x_is_cont_in(Contained_in, In, Enclosing_container)},
  ( np((S^_)^_, subject, Agreement, nogap, nogap)  ,
    aux_be(Tense, Agreement), 
    opt_not(Polarity), 
    theTextM1(In), 
    np((Container^_)^_, object, _, nogap, nogap), 
    {iz_a(Container, Enclosing_container)}).
/*
% NP is [not] on NP
ss(t(contained_in, S, Container), indicative, Polarity, Tense, simple) -->  
  ( np((S^_)^_, subject, Agreement, nogap, nogap)  ,
    aux_be(Tense, Agreement), 
    opt_not(Polarity), 
    theTextM1(on), 
    np((Container^_)^_, object, _, nogap, nogap), 
    {iz_a(Container, work_surface)}).
*/

% Character has  NP
ss(t(contained_in, Object, Character), indicative, Polarity, Tense, simple) -->  
  ( np((Character^_)^_, subject, Agreement, nogap, nogap)  ,
    {character(Character)}, 
    aux_have(Tense, Agreement), 
    opt_not(Polarity), 
    np((Object^_)^_, object, _, nogap, nogap)).

% NP's Property is [not] PropertyValue
ss(t(Property, Noun, Value), indicative, Polarity, Tense, simple) -->
   { % DMILES %%%%%%%%%%%%%%% 
     at_least_once(valid_property_value(Property, Value)),
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
ss(t(Property, Noun, Value), indicative, Polarity, Tense, simple) -->
   { % DMILES %%%%%%%%%%%%%%% 
     at_least_once(valid_property_value(Property, Value)),
     adjectival_property(Property) },
   np((Noun^_)^_, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   [Value].

% NP is [not] a PropertyValue
ss(t(Property, Noun, Value), indicative, Polarity, Tense, simple) -->
   { % DMILES %%%%%%%%%%%%%%% 
     at_least_once(valid_property_value(Property, Value)),
     nominal_property(Property) },
   np((Noun^_)^_, subject, Agreement, nogap, nogap),
   aux_be(Tense, Agreement),
   opt_not(Polarity),
   [a, Value].

% NP's Relation is [not] Relatum
ss(t(Relation, Noun, Relatum), indicative, Polarity, Tense, simple) -->  
  ( np((Noun^_)^_, genitive, _Agreement1, nogap, nogap)  ,
    genitive_form_of_relation(Relation, singular), 
    copula(simple, Tense, third:singular), 
    opt_not(Polarity), 
    np((Relatum^_)^_, object, _Agreement2, nogap, nogap)).



%% at_least_once( ?G) is semidet.
%
% When Least Once.
%
at_least_once(G):- call(G).

%%%
%%% Imperative mood
%%%
ss(S, imperative, Polarity, present, simple) -->
   { lf_subject(S, $addressee) },
   aux_vp($addressee^S, Polarity, second:singular, present, simple).
ss(S, imperative, Polarity, present, simple) -->  
  ( theTextM([let, us])  ,
    {lf_subject(S, $dialog_group)}, 
    aux_vp($dialog_group^S, Polarity, first:singular, present, simple)).

%%%
%%% Interrogative mood
%%%

% Yes/no question generated by subject-aux inversion
ss(S, interrogative, Polarity, Tense, Aspect) -->
   { var(S) ; S \= (_:_) },  % don't try if we already know it's a wh-question.
   inverted_sentence(S, Polarity, Tense, Aspect).

:- discontiguous inverted_sentence//4.
% Normal inverted sentence
inverted_sentence(S, Polarity, Tense, Aspect) -->
   { lf_subject(S, NP) },
   aux(np((NP^S1)^S, subject, Agreement),
       Polarity, Agreement, Tense, Aspect, Form, Modality),
   vvpp(Form, Modality, NP^S1, Tense, Agreement, nogap).


%% inverted_sentence( ?ARG1, ?ARG2, ?ARG3, ?ARG4) is semidet.
%
% Inverted Sentence.
%


% is NP a KIND?
ss(iz_a(Noun, Kind), interrogative, affirmative, Tense, simple) -->
   aux_be(Tense, Agreement),
   np((Noun^_)^_, subject, Agreement, nogap, nogap),
   a_an,
   kind_noun(Kind, singular).

% Wh-questions about normal verbs

% This seems redundant with the "questions about the subject" rule below.
% % What Verbs?
% ss((Wh:(S, iz_a(Wh, Kind))), interrogative, affirmative, Tense, Aspect) -->
%    whpron(Kind),
%    aux_vp(Wh^S, affirmative, third:singular, Tense, Aspect).

% What did Subject Verb?
ss((Wh:(S, iz_a(Wh, Kind))), interrogative, affirmative, Tense, simple) -->
   whpron(Kind),
   aux_do(Tense, Agreement),
   np((NP^S1)^S, subject, Agreement, nogap, nogap),
   vvpp(base, X^X, NP^S1, Tense, Agreement, np(Wh)).


% Wh-questions about properties

% Whose property is Value?
ss( Noun:t(Property, Noun, Value), 
  interrogative, affirmative, Tense, simple) -->  
  ( {valid_property_value(Property, Value)}  ,
    theTextM1(whose), 
    property_name(Property), 
    copula(simple, Tense, third:singular), 
    property_value_np(Property, Value)).

:- register_lexical_item(whose).

% NP's Property is [not] PropertyValue
ss( Value:(t(Property, Noun, Value), iz_a(Value, Kind)), 
  interrogative, affirmative, Tense, simple) -->  
  ( whpron(Kind)  ,
    copula(simple, Tense, third:singular), 
    np((Noun^_)^_, genitive, _Agreement, nogap, nogap), 
    property_name(Property)).

% Wh-questions about relations

:- dynamic(genitive_form_of_relation//2).

% Whose relation is Value?
ss( Noun:t(Relation, Noun, Value), 
  interrogative, affirmative, Tense, simple) -->  
  ( theTextM1(whose)  ,
    genitive_form_of_relation(Relation, Number), 
    copula(simple, Tense, Person:Number), 
    np((Value^_)^_, subject, Person:Number, nogap, nogap)).

% NP's Relation is [not] RelationValue
ss( Value:(t(Relation, Noun, Value), iz_a(Value, Kind)), 
  interrogative, affirmative, Tense, simple) -->  
  ( whpron(Kind)  ,
    copula(simple, Tense, third:Number), 
    np((Noun^_)^_, genitive, _Agreement, nogap, nogap), 
    genitive_form_of_relation(Relation, Number)).

% Wh-questions about the subject.
ss(Subject:(S, iz_a(Subject, Kind)), interrogative, Polarity, Tense, Aspect) -->
   { lf_subject(S, Subject), var(Subject) },
   whpron(Kind),
   aux_vp(Subject^S, Polarity, _Agreement, Tense, Aspect).

% Wh-questions about the object.
ss(Object:(S, iz_a(Object, Kind)), interrogative, Polarity, Tense, Aspect) -->
   { lf_subject(S, NP) },
   whpron(Kind),
   aux(nogap, Polarity, Agreement, Tense, Aspect, Form, Modality),
   np((NP^S1)^S, subject, Agreement, nogap, nogap),
   vvpp(Form, Modality, NP^S1, Tense, Agreement, np(Object)).

% Who is/what is Subject
ss(Object:(be(Subject, Object), iz_a(Subject, Kind)), interrogative, affirmative, present, simple) -->
   whpron(Kind),
   aux_be(present, Agreement),
   np((Subject^S)^S, subject, Agreement, nogap, nogap).

% How is Subject?
ss(Manner:manner(be(Subject), Manner), interrogative, affirmative, present, simple) -->  
  ( theTextM1(how)  ,
    aux_be(present, Agreement), 
    np((Subject^be(Subject))^be(Subject), subject, Agreement, nogap, nogap)).

% How does Subject Predicate?
ss(Method:method(S, Method), interrogative, affirmative, Tense, simple) -->  
  ( theTextM1(how)  ,
    {lf_subject(S, NP)}, 
    aux_do(Tense, Agreement), 
    np((NP^S1)^S, subject, Agreement, nogap, nogap), 
    vvpp(base, M^M, NP^S1, present, Agreement, nogap)).

:- register_lexical_item(how).

% Is he Adjective?
ss(S, interrogative, Polarity, present, simple) -->
   aux_be(present, Agreement),
   opt_not(Polarity),
   np((Noun^_)^_, subject, Agreement, nogap, nogap),
   ap(Noun^S).

% Is X in/on Container?
ss(t(contained_in, S, Container), interrogative, Polarity, Tense, simple) -->  
  ( aux_be(Tense, Agreement)  ,
    np((S^_)^_, subject, Agreement, nogap, nogap), 
    opt_not(Polarity), 
    theTextM1(in), 
    np((Container^_)^_, object, _, nogap, nogap), 
    {iz_a(Container, enclosing_container)}).

ss(t(contained_in, S, Container), interrogative, Polarity, Tense, simple) -->  
  ( aux_be(Tense, Agreement)  ,
    np((S^_)^_, subject, Agreement, nogap, nogap), 
    opt_not(Polarity), 
    theTextM1(on), 
    np((Container^_)^_, object, _, nogap, nogap), 
    {iz_a(Container, work_surface)}).

% Is Subject a PROPERTYVALUE?
inverted_sentence(t(Property, Subject, Value), Polarity, Tense, simple) -->  
  ( copula(simple, Tense, Agreement)  ,
    opt_not(Polarity), 
    np((Subject^_)^_, subject, Agreement, nogap, nogap), 
    {nominal_property(Property), valid_property_value(Property, Value)}, 
    theTextM([a, Value])).

% why did he X?
ss( X:explanation(S, X), 
  interrogative, Polarity, Tense, Aspect) -->  
  theTextM1(why), inverted_sentence(S, Polarity, Tense, Aspect).

:- register_lexical_item(why).

% where is NP
ss( Container:t(contained_in, S, Container), 
  interrogative, Polarity, Tense, simple) -->  
  ( theTextM1(where)  ,
    aux_be(Tense, Agreement), 
    opt_not(Polarity), 
    np((S^S)^S, subject, Agreement, nogap, nogap)).

:- register_lexical_item(where).

% Who has  NP
ss( Character:t(contained_in, Object, Character), 
  iz_a(Character, person), interrogative, Polarity, Tense, 
  simple) -->  
  ( theTextM1(who)  ,
    aux_have(Tense, third:singular), 
    opt_not(Polarity), 
    np((Object^S)^S, object, _, nogap, nogap)).


%% ss( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5, ?ARG6) is semidet.
%
% Ss.
%


% what is on the X
ss( S:t(contained_in, S, Container), 
  interrogative, Polarity, Tense, simple) -->  
  ( theTextM1(what)  ,
    aux_be(Tense, Agreement), 
    opt_not(Polarity), 
    theTextM1(on), 
    {impose_selectional_constraint(Container, work_surface)}, 
    np((Container^Ignored)^Ignored, subject, Agreement, nogap, nogap), 
    {iz_a(Container, work_surface)}).

% Who/what is in the X
ss( S:(t(contained_in, S, Container), iz_a(S, Kind)), 
  interrogative, Polarity, Tense, simple) -->  
  ( whpron(Kind)  ,
    aux_be(Tense, Agreement), 
    opt_not(Polarity), 
    theTextM1(in), 
    {impose_selectional_constraint(Container, enclosing_container)}, 
    np((Container^Ignored)^Ignored, subject, Agreement, nogap, nogap), 
    {iz_a(Container, enclosing_container), \+character(Container)}).

% what does Character have?
ss( S:t(contained_in, S, Character), 
  interrogative, Polarity, Tense, simple) -->  
  ( theTextM1(what)  ,
    aux_do(Tense, Agreement), 
    opt_not(Polarity), 
    np((Character^Ignored)^Ignored, subject, Agreement, nogap, nogap), 
    {character(Character)}, 
    aux_have(Tense, Agreement)).

%%%
%%% Adjectival phrases
%%% Seems silly to make a whole new file for one clause...
%%%

%% ap( ?ARG1) is semidet.
%
% Adjectival phrases
%
ap(Meaning) -->
   adjective(Meaning).