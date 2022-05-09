%%%                  Verb Phrases



%=autodoc
%% test_file( ?ARG1, ?NL/base_grammar_test2) is semidet.
%
% Test File.
%
test_file(generate(vp, _), "NL/vp_tests").
test_file(complete(vp, _), "NL/vp_tests").
test_file(parse(vp, _), "NL/vp_tests").

%% aux_vp(LF, Person, Number, Tense, Progressive, Perfect)
%  Verb phrases, optionally augmented with auxilliaries and/or
%  negative particle.

:- randomizable aux_vp/7.



%=autodoc
%% copula( ?ARG1, ?ARG2, ?ARG3) is semidet.
%
% Copula.
%
copula(past_participle, _, _)-->theTextM1(been).
copula(present_participle, _, _)-->theTextM1(being).
copula(base, _, _)-->theTextM1(be).
copula(Form, Tense, Agreement) -->
   { \+ memberchk(Form, [base, past_participle, present_participle]) },
   aux_be(Tense, Agreement).


start_verb(_Agreement, present, Info)--> theTextMLR(2,1,Info). 
complete_verb(SInfo, Subject, Object, do_v(Subject, SInfo, Object, Info))--> theTextMLR(1,0,Info). 

theTextMLR(N,_,Info) -->  theTextML(N,Info).
theTextMLR(_,R,Info) -->  theTextML(R,Info).

theTextML(1,Info)--> !, theTextM1W(Info).
theTextML(2,[Info,Info2])--> !, theTextM1W(Info), theTextM1W(Info2).
theTextML(0,[])--> !,[].

theTextM1W(Info)--> theTextM1(Info), {atom(Info), \+ (upcase_atom(Info,InfoU), Info==InfoU)}.


%=autodoc
%% aux_vp( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is semidet.
%
% Aux Verb Phrase.
%
aux_vp(VP, Polarity, Agreement, Tense, Aspect) --> 
   aux(nogap, Polarity, Agreement, Tense, Aspect, Form, M),
   vp(Form, M, VP, Tense, Agreement, nogap).

%% vp(?Form, ?Modal, ?Meaning, ?Tense, ?Agreement ?Gap)

:- randomizable vp/8.

/*
vp(_, Predicate^Modal, Subject^Modal, Tense, Agreement, Gap) -->
   modal_verb(Tense, Agreement, Subject^Complement^Predicate),
   vp(_, Predicate, Complement, Tense, Agreement, Gap).
*/


%test_modal_vp(LF) :-
%   vp(_, X^can(X), LF, _, _, nogap, [halt], [ ]).
vp(Form, Predication^Modal, Subject^S, Tense, Agreement, Gap) -->
   { lf_core_predicate(S, Predication) },
   iv(Form, Agreement, Subject^Predication, Tense, ForcePPs),
   opt_pp(ForcePPs, Predication, Gap, Modal, S).

%=autodoc
%% vp( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5, ?ARG6) is semidet.
%
% Verb Phrase.
%


vp(Form, Predication^Modal, Subject^S3, Tense, Agreement, GapInfo) -->
   { lf_core_predicate(S3, Predication) },
   dtv(Form, Agreement,
       Subject^IndirectObject^DirectObject^Predication,
       Tense,
       ForcePPs), 
   np_chat((IndirectObject^Modal)^S1, object, _, nogap, nogap),
   np_chat((DirectObject^S1)^S2, object, _, GapInfo, GapOut),
   opt_pp(ForcePPs, Predication, GapOut, S2, S3).

vp(Form, Predication^Modal, Subject^S2, Tense, Agreement, GapInfo) -->
   { lf_core_predicate(S2, Predication) },
   tv(Form, Agreement, Subject^Object^Predication, Tense, ForcePPs), 
   np_chat((Object^Modal)^S1, object, _, GapInfo, GapOut),
   opt_pp(ForcePPs, Predication, GapOut, S1, S2).

%%%
%%% Infinitival VPs
%%%



%=autodoc
%% infinitival_vp( ?ARG1) is semidet.
%
% Infinitival Verb Phrase.
%
infinitival_vp(LF) -->  
  %LF = EnclosingSubject^S
  (theTextM1(to);[]), vp(base, S^S, LF, present, first:singular, nogap).

%%%
%%% Special case verbs
%%%

%% Turn phrasal verbs
%% Someday this should be data driven
vp( _Form, 
  Predicate^Modal, Subject^S, Tense, 
  Agreement, GapInfo) -->  
  ( turn_verb(Agreement, Tense)  ,
    theTextM1(TurnAdverb), 
    {turn_phrasal_verb(TurnAdverb, Subject, Object, Predicate)}, 
    np_chat((Object^Modal)^S, object, _, GapInfo, nogap)).
vp( _Form, 
  Predicate^Modal, Subject^S, Tense, 
  Agreement, GapInfo) -->  
  ( turn_verb(Agreement, Tense)  ,
    np_chat((Object^Modal)^S, object, _, GapInfo, nogap), 
    theTextM1(TurnAdverb), 
    {turn_phrasal_verb(TurnAdverb, Subject, Object, Predicate)}).

%% turn_verb( ?ARG1, ?ARG2) is semidet.
%
% Turn Verb.
%
turn_verb(_, Tense) -->  
  theTextM1(Turned), {turn_word(Turned, Tense)}.



%=autodoc
%% turn_word( ?Switched1, ?Past2) is semidet.
%
% Turn Word.
%
turn_word(turn,present).
turn_word(turned,past).
turn_word(Switch,present):- Switch=switch.
turn_word(Switched,past):- Switched=switched.
:- forall(turn_word(W,_),register_lexical_items([W])).



%=autodoc
%% turn_phrasal_verb( ?On, ?S, -O, ?S) is semidet.
%
% Turn Phrasal Verb.
%
turn_phrasal_verb(on, S, O, switch(S, O, power,on)).
turn_phrasal_verb(off, S, O, switch(S, O, power,off)).
turn_phrasal_verb(True, S, O, switch(S, O, power,on)):- true == True.
turn_phrasal_verb(False, S, O, switch(S, O, power,off)):- false == False.

:- register_lexical_item(turn),
   forall(turn_phrasal_verb(Word, _, _, _),
	  register_lexical_item(Word)).

%% modal verbs

vp(_, Predicate^Modal, Subject^Modal, Tense, Agreement, nogap) -->
   modal_verb(Tense, Agreement, Subject^Complement^Predicate),
   infinitival_clause(Subject, Complement).



%=autodoc
%% modal_verb( ?ARG1, ?ARG2, ?ARG3) is semidet.
%
% Modal Verb.
%
modal_verb(present, third:singular, S^C^wants(S, C)) -->  
  theTextM1(wants).
modal_verb(present, Agreement, S^C^wants(S, C)) -->  
  theTextM1(want), {dif(Agreement, third:singular)}.
modal_verb(past, _Agreement, S^C^wants(S, C)) -->  
  theTextM1(wanted).

modal_verb(present, third:singular, S^C^needs(S, C)) -->  
  theTextM1(needs).
modal_verb(present, Agreement, S^C^needs(S, C)) -->  
  theTextM1(need), {dif(Agreement, third:singular)}.
modal_verb(past, _Agreement, S^C^needs(S, C)) -->  
  theTextM1(needed).

modal_verb(present, third:singular, S^C^likes(S, C)) -->  
  theTextM1(likes).
modal_verb(present, Agreement, S^C^likes(S, C)) -->  
  theTextM1(like), {dif(Agreement, third:singular)}.
modal_verb(past, _Agreement, S^C^likes(S, C)) -->  
  theTextM1(liked).

modal_verb(Present, ThirdSingular, S^C^ not(WSC)) -->
  nonvar(theTextM1(not);WSC),
  modal_verb(Present, ThirdSingular, S^C^WSC).

modal_verb(present, _Agreement, _S^C^ not(C)) -->
  theTextM1(not).


nonvar(DCG;WSE,[Not|S],E):- nonvar(WSE), !, phrase(DCG,[Not|S],E).
nonvar(DCG;_,[Not|S],E):- !, nonvar(Not), !, phrase(DCG,[Not|S],E).
nonvar(DCG,[Not|S],E):- nonvar(Not), !, phrase(DCG,[Not|S],E).


:- forall(modal_verb(_, _, _, Phrase, []),
	  register_lexical_items(Phrase)).

%% Other verbs with clausal complements

vp(_, Predicate^Modal, Subject^Modal, Tense, Agreement, nogap) -->
   verb_with_clausal_complement(Tense, Agreement, Subject, Complement, DeclarativePredicate, InterrogativePredicate),
   content_clause(Complement, DeclarativePredicate, InterrogativePredicate, Predicate).



%% verb_with_clausal_complement( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5, ?ARG6) is semidet.
%
% Verb Using Clausal Complement.
%
verb_with_clausal_complement( present, 
  third : single, Subject, Complement, 
  believes(Subject, Complement), null) -->  
  theTextM1(believes).
verb_with_clausal_complement( past, 
  _, Subject, Complement, 
  believes(Subject, Complement), null) -->  
  theTextM1(believed).
verb_with_clausal_complement( present, 
  Agreement, Subject, Complement, 
  believes(Subject, Complement), null) -->  
  theTextM1(believe), {dif(Agreement, third:single)}.

verb_with_clausal_complement( present, 
  third : single, Subject, Complement, 
  thinks(Subject, Complement), null) -->  
  theTextM1(thinks).
verb_with_clausal_complement( past, 
  _, Subject, Complement, 
  thinks(Subject, Complement), null) -->  
  theTextM1(thought).
verb_with_clausal_complement( present, 
  Agreement, Subject, Complement, 
  thinks(Subject, Complement), null) -->  
  theTextM1(think), {dif(Agreement, third:single)}.

verb_with_clausal_complement( present, 
  third : single, Subject, Complement, 
  know(Subject, Complement), 
  knows_value(Subject, Complement)) -->  
  theTextM1(knows).
verb_with_clausal_complement( past, 
  _, Subject, Complement, 
  know(Subject, Complement), 
  knows_value(Subject, Complement)) -->  
  theTextM1(knew).
verb_with_clausal_complement( present, 
  Agreement, Subject, Complement, 
  know(Subject, Complement), 
  knows_value(Subject, Complement)) -->  
  theTextM1(know), {dif(Agreement, third:single)}.

:- forall(verb_with_clausal_complement(_, _, _, _, _, _, Phrase, []),
	  register_lexical_items(Phrase)).

vp(_, Predicate^Modal, Subject^Modal, Tense, Agreement, nogap) -->
   verb_with_object_and_clausal_complement(Tense, Agreement, Subject, Object, Complement, DeclarativePredicate, InterrogativePredicate),
   np_chat((Object^_)^_, object, _, nogap, nogap),
   content_clause(Complement, DeclarativePredicate, InterrogativePredicate, Predicate).



%=autodoc
%% verb_with_object_and_clausal_complement( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5, ?ARG6, ?ARG7) is semidet.
%
% Verb Using Object And Clausal Complement.
%
verb_with_object_and_clausal_complement( present, 
  third : single, Subject, Object, 
  Complement, 
  tell(Subject, Object, Complement), 
  tell_value(Subject, Object, Complement)) -->  
  theTextM1(tells).
verb_with_object_and_clausal_complement( past, 
  _, Subject, Object, 
  Complement, 
  tell(Subject, Object, Complement), 
  tell_value(Subject, Object, Complement)) -->  
  theTextM1(told).
verb_with_object_and_clausal_complement( present, 
  Agreement, Subject, Object, 
  Complement, 
  tell(Subject, Object, Complement), 
  tell_value(Subject, Object, Complement)) -->  
  theTextM1(tell), {dif(Agreement, third:single)}.

verb_with_object_and_clausal_complement( present, 
  third : single, Subject, Object, 
  Complement, 
  ask(Subject, Object, Complement), 
  ask_value(Subject, Object, Complement)) -->  
  theTextM1(asks).
verb_with_object_and_clausal_complement( past, 
  _, Subject, Object, 
  Complement, 
  ask(Subject, Object, Complement), 
  ask_value(Subject, Object, Complement)) -->  
  theTextM1(asked).
verb_with_object_and_clausal_complement( present, 
  Agreement, Subject, Object, 
  Complement, 
  ask(Subject, Object, Complement), 
  ask_value(Subject, Object, Complement)) -->  
  theTextM1(ask), {dif(Agreement, third:single)}.

:- forall(verb_with_object_and_clausal_complement(_, _, _, _, _, _, _, Phrase, []),
	  register_lexical_items(Phrase)).


vp( _Form, 
  Predicate^Modal, Subject^S, Tense, 
  Agreement, GapInfo) -->  {fail},
  ( start_verb(Agreement, Tense, Info)  ,
    np_chat((Object^Modal)^S, object, _, GapInfo, nogap), 
    complete_verb(Info, Subject, Object, Predicate)).



