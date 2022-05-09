%:- public det/3.

%det(LF) --> [D], {det(D, LF)}.

:- randomizable proper_name/4, proper_name/2.


%transitive_interface_verb(Deactivate, Deactivates, Deactivated, Deactivated, Deactivating, _, ABDAB).
%transitive_interface_verb(D, es(D), ed(D), ed2(D), ing(D), _, LF):- D = craz, LF = A^B^tv_call(D,A,B).

iface(iv, Write, Writes, Wrote, Written, Writing, _, A^t(Write,A)):-
  current_predicate(talkdb_talk_db/6),
  talkdb_talk_db(intransitive,Write,Writes,Wrote,Writing,Written).

iface(tv, Write, Writes, Wrote, Written, Writing, _, (A^(B^t(Write,A,B)))):-
  current_predicate(talkdb_talk_db/6),
  talkdb_talk_db(transitive,Write,Writes,Wrote,Writing,Written).

iface(dtv, Write, Writes, Wrote, Written, Writing, _, (A^(B^(C^t(Write,A,B,C))))):-
  current_predicate(talkdb_talk_db/6),
  talkdb_talk_db(ditransitive,Write,Writes,Wrote,Writing,Written).
%iface(iv,Deactivate, Deactivates, Deactivated, Deactivated2, Deactivating, _, ABDAB):-
%  talkdb_talk_db(_Transitive,Write,Writes,_Wrote,_Writing,_Written)

phrase_rule_gen(PR,Phrase,Guard):-
 ARG = ppg(PR,Phrase,Guard),
 arg(_,
  v(ppg(iv(simple, third:singular, ABDAB, present, []), [Deactivates], true),
    ppg(iv(simple, E, ABDAB, present, []),[Deactivate], E\=third:singular),
    ppg(iv(simple, _, ABDAB, past, []),   [Deactivated], true),
    ppg(iv(simple, _, ABDAB, future, []), [Deactivate], true),
    ppg(iv(base, _, ABDAB, present, []),  [Deactivate], true),
    ppg(iv(base, _, ABDAB, past, []),     [Deactivate], true),
    ppg(iv(past_participle,_,ABDAB,_,[]), [Deactivated2], true),
    ppg(iv(present_participle,_,ABDAB,_,[]), [Deactivating], true)),
  ARG),
 iface(iv,Deactivate, Deactivates, Deactivated, Deactivated2, Deactivating, _, ABDAB).

phrase_rule_gen(PR,Phrase,Guard):-
 arg(_,
  v(ppg(tv(simple, third:singular, ABDAB, present, []), [Deactivates], true),
    ppg(tv(simple, E, ABDAB, present, []),[Deactivate], E\=third:singular),
    ppg(tv(simple, _, ABDAB, past, []),   [Deactivated], true),
    ppg(tv(simple, _, ABDAB, future, []), [Deactivate], true),
    ppg(tv(base, _, ABDAB, present, []),  [Deactivate], true),
    ppg(tv(base, _, ABDAB, past, []),     [Deactivate], true),
    ppg(tv(past_participle,_,ABDAB,_,[]), [Deactivated2], true),
    ppg(tv(present_participle,_,ABDAB,_,[]), [Deactivating], true)),
  ppg(PR,Phrase,Guard)),
 iface(tv,Deactivate, Deactivates, Deactivated, Deactivated2, Deactivating, _, ABDAB).

phrase_rule_gen(PR,Phrase,Guard):- 
 arg(_,
  v(ppg(dtv(simple, third:singular, ABDAB, present, []), [Deactivates], true),
    ppg(dtv(simple, E, ABDAB, present, []),[Deactivate], E\=third:singular),
    ppg(dtv(simple, _, ABDAB, past, []),   [Deactivated], true),
    ppg(dtv(simple, _, ABDAB, future, []), [Deactivate], true),
    ppg(dtv(base, _, ABDAB, present, []),  [Deactivate], true),
    ppg(dtv(base, _, ABDAB, past, []),     [Deactivate], true),
    ppg(dtv(past_participle,_,ABDAB,_,[]), [Deactivated2], true),
    ppg(dtv(present_participle,_,ABDAB,_,[]), [Deactivating], true)),
  ppg(PR,Phrase,Guard)),
 iface(dtv,Deactivate, Deactivates, Deactivated, Deactivated2, Deactivating, _, ABDAB).


 
phrase_rule(PR,Phrase,Guard):- % fail,
 phrase_rule_gen(PR,Phrase,Guard).

 


%=autodoc
%% limit_var( ?Four, ?ObjectName, :GoalGoal) is semidet.
%
% Limit Variable.
%
limit_var(_Four,ObjectName,Goal):- ground(ObjectName),!,once(Goal).
limit_var(Four,_ObjectName,Goal):-
  limit(Four,Goal).

%% proper_name(?Object, ?Name) is nondet
%  Object has proper name Name (a list of words)
proper_name(Object, Name) :-   
   limit_var(4,Object:Name,proper_name(Object, _, Name, [ ])).

proper_name(Object, Number) -->
   proper_name_without_A(Object, Number).

proper_name(Object, Number) -->  
  theTextM1(the), proper_name_with_the(Object, Number).

:- multifile(proper_name_without_the//2).
:- dynamic(proper_name_without_the//2).
:- dynamic(phrase_rule/3).



%=autodoc
%% proper_name_without_A( ?Object, ?ARG2, ?Left, ?More) is semidet.
%
% Proper Name Without A.
%
proper_name_without_A(Object,_,Left, More):- 
 object_words(pn,Object,Words), 
 append(Words,More,Left).



%=autodoc
%% proper_name_with_the( ?ARG1, ?ARG2) is semidet.
%
% Proper Name Using The.
%
proper_name_with_the(Object, Number)--> proper_name_without_the(Object, Number).


%=autodoc
%% proper_name_with_the( ?Object, ?Number, ?Left, ?More) is semidet.
%
% Proper Name Using The.
%
proper_name_with_the(Object,_,Left, More):- 
 object_words(a,Object,Words), 
 append(Words,More,Left).



%=autodoc
%% kind_noun( ?ARG1, ?ARG2) is semidet.
%
% Kind Noun.
%
kind_noun(Kind, Number) --> 
  {limit_var(1000,Kind,phrase_rule(kind_noun(Kind, Number), Words, Guard))},
  Words,{Guard}.

proper_name(Object, Number) --> 
  {phrase_rule(proper_name(Object, Number), Words, Guard)},
  Words,{Guard}.



%=autodoc
%% proper_name_without_the( ?ARG1, ?ARG2) is semidet.
%
% Proper Name Without The.
%
proper_name_without_the(Kind, Number) --> 
  {phrase_rule(proper_name_without_the(Kind, Number), Words, Guard)},
  Words,{Guard}.

proper_name_without_the(Kind, Number) --> 
  {phrase_rule(proper_name_without_the(Kind, Number), Words, Guard)},
  Words,{Guard}.



%=autodoc
%% genitive_form_of_relation( ?ARG1, ?ARG2) is semidet.
%
% Genitive Form Of Relation.
%
genitive_form_of_relation(Kind, Number) --> 
  {phrase_rule(genitive_form_of_relation(Kind, Number), Words, Guard)},
  Words,{Guard}.



%=autodoc
%% property_name( ?ARG1) is semidet.
%
% Property Name.
%
property_name(Kind) --> 
  {phrase_rule(property_name(Kind), Words, Guard)},
  Words,{Guard}.



%=autodoc
%% copular_relation( ?ARG1) is semidet.
%
% Copular Relation.
%
copular_relation(Kind) --> 
  {phrase_rule(copular_relation(Kind), Words, Guard)},
  Words,{Guard}.



%=autodoc
%% adjective( ?ARG1) is semidet.
%
% Adjective.
%
adjective(Kind) --> 
  {phrase_rule(adjective(Kind), Words, Guard)},
  Words,{Guard}.



%=autodoc
%% dtv( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is semidet.
%
% Dtv.
%

dtv(Form, Agreement, SLF, Tense, ForcePPs, [S|SS], E) :- SLF = (_^(_^(_^LF))), once(nonvar(S);nonvar(LF)),
  phrase_rule(dtv(Form, Agreement, SLF, Tense, ForcePPs), [S|Words], Guard),
  append(Words,E,SS),
  call(Guard).




%=autodoc
%% art_the( ?Art) is semidet.
%
% Art The.
%
art_the(Art):- atom(Art), member(Art,[a,an,the]).


%=autodoc
%% carefull_words( ?Art, ?Y) is semidet.
%
% Carefull Words.
%
carefull_words([Art|X],Y):- art_the(Art),!,carefull_words(X,Y).
carefull_words([X],[X]):-!.
carefull_words([],_):-!,fail.
carefull_words(FIT,Out):- include(not_lower,FIT,NotLow), (NotLow==[] -> Out=FIT ; Out = NotLow).



%=autodoc
%% not_lower( ?X) is semidet.
%
% Not Lower.
%
not_lower(X):- \+ (atom(X), downcase_atom(X,X)).



%=autodoc
%% object_words( ?Pn, ?Object, ?CWords) is semidet.
%
% Object Words.
%
object_words(pn,Object,CWords):- object_words0(pn,Object, Words),carefull_words(Words,CWords).
object_words(a,Object,CWords):- object_words0(a,Object,Words), \+ object_words0(pn,Object, Words),carefull_words(Words,CWords).

object_words0(pn, Object, Words) :- 
  t(given_name, Object, String), 
  tokenize_atom(String, Words).
%object_words0(a,#(Kind),[Kind]):- atom(Kind),!.
object_words0(a,#(VarName),Words):- atom(VarName),
  (atom_contains(VarName,' ')
   -> atomic_list_concat(Words,' ',VarName)
   ;  atomic_list_concat(Words,'_',VarName)).
object_words0(a,Object,[Kind]) :- (var(Kind);atom(Kind)),atom(Object),atom_concat('unknown_',Kind,Object).
object_words0(a,Object,Words):- 
 atom(Object), \+ atom_concat('unknown_',_,Object),
 (atom_contains(Object,' ')-> atomic_list_concat(Words,' ',Object);atomic_list_concat(Words,'_',Object)).



%=autodoc
%% pronoun( ?ARG1, ?ARG2, ?ARG3) is semidet.
%
% Pronoun.
%
pronoun(Case, Person:Number,  (E^S)^S) -->  
  ( theTextM1(PN), 
    { \+bound_discourse_variable(E), 
      pronoun_word(PN, Case, Person, Number, E) }).

%relpron --> [RP], {relpron(RP)}.
whpron(Kind) -->
   { Kind \== person,		% Use who for persons
     Kind \== entity },		% Just say what, rather than "what entities"
   [what],
   kind_noun(Kind, _).

%=autodoc
%% whpron( ?ARG1) is semidet.
%
% Whpron.
%


whpron(Kind)-->theTextM1(WH), {whpron(WH, Kind)}.

%%
%% Verb conjugations
%%

:- randomizable iv//5.



%=autodoc
%% iv( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is semidet.
%
% Iv.
%
iv(Form, Agreement, SLF, Tense, ForcePPs, [S|SS], E) :- SLF = _^LF, once(nonvar(S);nonvar(LF)),
  phrase_rule(iv(Form, Agreement, SLF, Tense, ForcePPs), [S|Words], Guard),
  append(Words,E,SS),
  call(Guard).



%=autodoc
%% load_special_csv_row( ?RowNumber, ?Base) is semidet.
%
% Load Special Csv Row.
%
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
		     


%=autodoc
%% end_csv_loading( ?Intransitive_verb) is semidet.
%
% End Csv Loading.
%
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

:- dynamic(transitive_verb/7).
transitive_verb([deactivate], [deactivates], [deactivated], [deactivated], [deactivating], [], A^B^deactivate(A, B)).
transitive_verb([know], [knows], [knew], [knew], [knowing], [], A^B^know(A, B)).
transitive_verb([love], [loves], [loved], [loved], [loving], [], A^B^related(A, loves, B)).
transitive_verb([like], [likes], [liked], [liked], [liking], [], A^B^related(A, likes, B)).
transitive_verb([dislike], [dislikes], [disliked], [disliked], [disliking], [], A^B^related(A, dislikes, B)).
transitive_verb([give], [gives], [gave], [gave], [giving], [to], A^B^give(A, _, B)).
transitive_verb([type], [types], [typed], [typed], [typing], [], A^B^comm(keystrokes, A, B)).
transitive_verb([do], [does], [did], [did], [doing], [], A^B^do(A, B)).
transitive_verb([grab], [grabs], [grabbed], [grabbed], [grabbing], [], A^B^take(A, B, _)).
transitive_verb([take], [takes], [took], [taken], [taking], [from], A^B^take(A, B, _)).
transitive_verb([put], [puts], [put], [put], [putting], [in], A^B^put(A, B, _)).
transitive_verb([move], [moves], [moved], [moved], [moving], [from, to], A^B^move(A, B, _, _)).
transitive_verb([tell], [tells], [told], [told], [telling], [about], A^B^tell_about(A, B, _)).
transitive_verb([ask], [asks], [asked], [asked], [asking], [about], A^B^ask_about(A, B, _)).
transitive_verb([eat], [eats], [ate], [ate], [eating], [], A^B^eat(A, B)).
transitive_verb([drink], [drinks], [drank], [drank], [drinking], [], A^B^drink(A, B)).
transitive_verb([search], [searches], [searched], [searched], [searching], [for], A^B^search_for(A, B, _)).
transitive_verb([check], [checks], [checked], [checked], [checking], [for], A^B^search_for(A, B, _)).
transitive_verb([look, around], [looks, around], [looked, around], [looked, around], [looking, around], [for], A^B^search_for(A, B, _)).
transitive_verb([look], [looks], [looked], [looked], [looking], [for], A^_^search_for(A, _, _)).
transitive_verb([look, at], [looks, at], [looked, at], [looked, at], [looking, at], [], A^B^examine(A, B)).
transitive_verb([examine], [examines], [examined], [examined], [examining], [], A^B^examine(A, B)).
transitive_verb([read], [reads], [read], [read], [reading], [], A^B^read(A, B)).
transitive_verb([bring], [brings], [brought], [brought], [bringing], [to], A^B^bring(A, _, B)).
transitive_verb([get, in], [gets, in], [got, in], [got, in], [getting, in], [], A^B^move(A, A, B)).
transitive_verb([enter], [enters], [entered], [entered], [entering], [], A^B^move(A, A, B)).
transitive_verb([push], [pushes], [pushed], [pushed], [pushing], [], A^B^press(A, B)).
transitive_verb([press], [presses], [pressed], [pressed], [pressing], [], A^B^press(A, B)).
transitive_verb([find], [finds], [found], [found], [finding], [], A^B^find(A, B)).
transitive_verb([get], [gets], [got], [got], [getting], [], A^B^get(A, B)).
transitive_verb([look, for], [looks, for], [looked, for], [looked, for], [looking, for], [in], A^B^look_for(A, B)).
transitive_verb([leave], [leaves], [left], [left], [leaving], [], A^B^leave(A, B)).
transitive_verb([exit], [exits], [exited], [exited], [exiting], [], A^B^leave(A, B)).
transitive_verb([operate], [operates], [operated], [operated], [operating], [], A^B^operate(A, B)).


:- dynamic(copular_relation//1).
%
% Relations and properties disguised as transitive verbs
%

% Uninverted sentence
tv( Form, 
  Agreement, 
  S^O^t(Relation, S, O), 
   Tense, []) -->  
  ( {Form\==present_participle;Tense\==present}  ,
    copula(Form, Tense, Agreement), 
    copular_relation(Relation)).

%=autodoc
%% tv( ?ARG1, ?ARG2, ?ARG3, ?ARG4, ?ARG5) is semidet.
%
% Tv.
%

% Inverted sentence
tv( present_participle, 
  _, 
  S^O^t(Relation, S, O), 
   present, []) -->  
  copular_relation(Relation).

tv(Form, Agreement, SLF, Tense, ForcePPs, [S|SS], E) :- SLF= (_^(_^LF)), once(nonvar(S);nonvar(LF)),
  phrase_rule(tv(Form, Agreement, SLF, Tense, ForcePPs), [S|Words], Guard),
  append(Words,E,SS),
  call(Guard).

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



%=autodoc
%% check_lexicon_typing( ?LF) is semidet.
%
% Check Lexicon Typing.
%
check_lexicon_typing(LF^Generator) :-
   forall(Generator,
	  check_lexical_entry_type(LF)).



%=autodoc
%% check_lexical_entry_type( ?X) is semidet.
%
% Check Lexical Entry Type.
%
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