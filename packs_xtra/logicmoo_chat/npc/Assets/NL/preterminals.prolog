%:- public det/3.

%det(LF) --> [D], {det(D, LF)}.

:- randomizable proper_name/4, proper_name/2.

%% proper_name(?Object, ?Name) is nondet
%  Object has proper name Name (a list of words)
proper_name(Object, Name) :-
   proper_name(Object, _, Name, [ ]).

proper_name(Object, Number) -->
   proper_name_without_A(Object, Number).

proper_name(Object, Number) -->
   [the],
   proper_name_with_the(Object, Number).

:- multifile(proper_name_without_the//2).
:- dynamic(proper_name_without_the//2).
proper_name_without_A(Object,_,Left, More):- 
 object_words(pn,Object,Words), 
 append(Words,More,Left).

proper_name_with_the(Object, Number)--> proper_name_without_the(Object, Number).
proper_name_with_the(Object,_,Left, More):- 
 object_words(a,Object,Words), 
 append(Words,More,Left).

art_the(Art):- atom(Art), member(Art,[a,an,the]).
carefull_words([Art|X],Y):- art_the(Art),!,carefull_words(X,Y).
carefull_words([X],[X]):-!.
carefull_words([],_):-!,fail.
carefull_words(FIT,Out):- include(not_lower,FIT,NotLow), (NotLow==[] -> Out=FIT ; Out = NotLow).

not_lower(X):- \+ (atom(X), downcase_atom(X,X)).

object_words(pn,Object,CWords):- object_words0(pn,Object, Words),carefull_words(Words,CWords).
object_words(a,Object,CWords):- object_words0(a,Object,Words), \+ object_words0(pn,Object, Words),carefull_words(Words,CWords).

object_words0(pn,Object, Words):- declare_value(Object,given_name,String),tokenize_atom(String,Words).
object_words0(a,#(Kind),[Kind]):- atom(Kind),!.
object_words0(a,Object,[Kind]) :- (var(Kind);atom(Kind)),atom(Object),atom_concat('unknown_',Kind,Object).
object_words0(a,Object,Words):- 
 atom(Object), \+ atom_concat('unknown_',_,Object),
 (atom_contains(Object,' ')-> atomic_list_concat(Words,' ',Object);atomic_list_concat(Words,'_',Object)).

pronoun(Case, Person:Number, (E^S)^S) -->
   [PN],
   { \+ bound_discourse_variable(E),
     pronoun_word(PN, Case, Person, Number, E) }.

%relpron --> [RP], {relpron(RP)}.
whpron(Kind) -->
   { Kind \== person,		% Use who for persons
     Kind \== entity },		% Just say what, rather than "what entities"
   [what],
   kind_noun(Kind, _).

whpron(Kind) --> [WH], {whpron(WH, Kind)}.

%%
%% Verb conjugations
%%

:- randomizable iv//5.

load_special_csv_row(_RowNumber,
                     intransitive_verb(Base, TPS, Past, PastP, PresentP,
				       ForcePPs, LF)) :-
   assert_phrase_rule(iv(simple, third:singular, LF, present, ForcePPs),
		      TPS),
   assert_phrase_rule(iv(simple, Agreement, LF, present, ForcePPs),
		      Base,
		      Agreement \= third:singular),
   assert_phrase_rule(iv(simple, _Agreement1, LF, past, ForcePPs),
		      Past),
   assert_phrase_rule(iv(simple, _Agreement2, LF, future, ForcePPs),
		      Base),
   % Used only in the construction X does not BASEFORM.
   assert_phrase_rule(iv(base, _Agreement3, LF, present, ForcePPs),
		      Base),
   assert_phrase_rule(iv(base, _Agreement4, LF, past, ForcePPs),
		      Base),
   assert_phrase_rule(iv(past_participle, _Agreement5, LF, _Tense5, ForcePPs),
		      PastP),
   assert_phrase_rule(iv(present_participle, _Agreement6, LF, _Tense6, ForcePPs),
		      PresentP).
		     
end_csv_loading(intransitive_verb) :-
   check_lexicon_typing(LF^iv(past_participle, _, LF, _, _, _, _)).

					
:- randomizable tv//5.

load_special_csv_row(_RowNumber,
                     transitive_verb(Base, TPS, Past, PastP, PresentP,
				       ForcePPs, LF)) :-
   assert_phrase_rule(tv(simple, third:singular, LF, present, ForcePPs),
		      TPS),
   assert_phrase_rule(tv(simple, Agreement, LF, present, ForcePPs),
		      Base,
		      Agreement \= third:singular),
   assert_phrase_rule(tv(simple, _, LF, past, ForcePPs),
		      Past),
   assert_phrase_rule(tv(simple, _, LF, future, ForcePPs),
		      Base),
   % Used only in the construction X does not BASEFORM.
   assert_phrase_rule(tv(base, _, LF, present, ForcePPs),
		      Base),
   assert_phrase_rule(tv(base, _, LF, past, ForcePPs),
		      Base),
   assert_phrase_rule(tv(past_participle, _, LF, _, ForcePPs),
		      PastP),
   assert_phrase_rule(tv(present_participle, _, LF, _, ForcePPs),
		      PresentP).

end_csv_loading(transitive_verb) :-
   check_lexicon_typing(LF^tv(past_participle, _, LF, _, _, _, _)).

:- dynamic(copular_relation//1).
%
% Relations and properties disguised as transitive verbs
%

% Uninverted sentence
tv(Form, Agreement, S^O^related(S, Relation, O), Tense, [ ]) -->
   {Form \== present_participle ; Tense \== present },
   copula(Form, Tense, Agreement),
   copular_relation(Relation).
% Inverted sentence
tv(present_participle, _, S^O^related(S, Relation, O), present, [ ])
   -->
   copular_relation(Relation).

%tv(Form, Agreement, S^O^property_value(S, Property, O), Tense, [ ]) -->
%   property_verb(Property, Form, Agreement, Tense).


:- randomizable dtv//5.

load_special_csv_row(_RowNumber,
                     ditransitive_verb(Base, TPS, Past, PastP, PresentP,
				       ForcePPs, LF)) :-
   assert_phrase_rule(dtv(simple, third:singular, LF, present, ForcePPs),
		      TPS),
   assert_phrase_rule(dtv(simple, Agreement, LF, present, ForcePPs),
		      Base,
		      Agreement \= third:singular),
   assert_phrase_rule(dtv(simple, _, LF, past, ForcePPs),
		      Past),
   assert_phrase_rule(dtv(simple, _, LF, future, ForcePPs),
		      Base),
   % Used only in the construction X does not BASEFORM.
   assert_phrase_rule(dtv(base, _, LF, present, ForcePPs),
		      Base),
   assert_phrase_rule(dtv(base, _, LF, past, ForcePPs),
		      Base),
   assert_phrase_rule(dtv(past_participle, _, LF, _, ForcePPs),
		      PastP),
   assert_phrase_rule(dtv(present_participle, _, LF, _, ForcePPs),
		      PresentP).

end_csv_loading(ditransitive_verb) :-
   check_lexicon_typing(LF^dtv(past_participle, _, LF, _, _, _, _)).

check_lexicon_typing(LF^Generator) :-
   forall(Generator,
	  check_lexical_entry_type(LF)).

check_lexical_entry_type([X]):- check_lexical_entry_type(X).
check_lexical_entry_type(_Arg^LF) :-
   !,
   check_lexical_entry_type(LF).
check_lexical_entry_type(LF) :-
   LF =.. [Functor | Args],
   length(Args, N),
   length(BlankArgs, N),
   BlankLF =.. [Functor | BlankArgs],
   predicate_type(_, BlankLF),
   !.
check_lexical_entry_type(LF) :-
   log(no_type_specified_for(LF)).

:- dynamic(adjective//1).

load_special_csv_row(_RowNumber,
                     adjective(Phrase, Meaning)) :-
   assert_phrase_rule(adjective(Meaning), Phrase).

end_csv_loading(adjective) :-
   forall(adjective(_, Phrase, []),
	  register_lexical_items(Phrase)).