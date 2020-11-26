% ===================================================================
% File 'e2c.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog  
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $

% ===================================================================
:-module(e2c,[
	e2c/1,
	e2c/2,
	testE2C/0]).

:- style_check(-singleton).
:- style_check(-discontiguous).
:- style_check(-atom).
:- if(false).
:- style_check(-string). 
:- endif.

%:-use_module(library(opencyc)).  

%:-[cyc].



% Semantic Interpretation
/* from Bratko chapter 17 page 455.
   This comes from Pereira and Warren paper, AI journal, 1980 */

/* 
   when using sentence we need to pass 3 arguments, 
   the first will match CycL in the head of the DGC clause
   the second is the list containing the words in the sentence
   the third is the empty list.
*/

e2c(English):-
      e2c(English,CycLOut),
      format('~w~n',[CycLOut]).


e2c(English,CycLOut):-atomic(English),
   atom_junct(English,[Eng|Lish]),!,
   e2c([Eng|Lish],CycLOut).

e2c([Eng|Lish],CycLOut):-atom(Eng),!,
   try_e2c([Eng|Lish],CycL),
   toCycApiExpression(CycLIn,CycLOut).
   
   
try_e2c(English,CycL):-sentence(CycL,English,[]),!.
try_e2c(English,CycL):-noun_phrase('?Var','?SomeRelation',CycL,English,[]),!.


% =======================================================
% sentence(CycL, [every,man,that,paints,likes,monet],[]) 
% =======================================================

sentence(CycL) --> declaritive_sentence(CycL).
sentence(CycL) --> imparitive_sentence(CycL).
sentence(CycL) --> inquiry(CycL).
	
imparitive_sentence(CycL) --> verb_phrase('?TargetAgent','?ImparitiveEvent',CycL).
declaritive_sentence(CycL) --> noun_phrase(Subj,CycL1,CycL),verb_phrase(Subj,Event,CycL1).
declaritive_sentence(CycL) --> 
      [the],trans2_verb(Subj,Event,Obj,VProp),possible_prep,
      noun_phrase(Obj,VProp,CycL1),[is],noun_phrase(Subj,CycL1,CycL).

declaritive_sentence(CycL) --> 
      noun_phrase(Obj,VProp,CycL1),
      [the],trans2_verb(Subj,Event,Obj,VProp),possible_prep,
      noun_phrase(Obj,VProp,CycL1),[is],noun_phrase(Subj,CycL1,CycL).

possible_prep-->[of];[].

inquiry('#$thereExists'(Actor,CycL)) --> wh_pronoun(Actor), verb_phrase(Actor,'?QuestionEvent',CycL),[?].
inquiry(CycL) --> inv_sentence(CycL),[?].

inv_sentence(CycL) --> aux, sentence(CycL).

wh_pronoun('?Who') --> [who].
wh_pronoun('?What') --> [what].

aux --> [does].

% =======================================================
% Quantification (DET Phrases)
% =======================================================

quant_phrase(Subj,Prop,CycL1,'#$thereExists'(Subj,'#$and'(Prop , CycL1))) --> existential_words.
quant_phrase(Subj,Prop,CycL1,'#$forAll'(Subj,'#$implies'(Prop , CycL1))) --> universal_words.

existential_words --> existential_word,existential_word.
existential_words --> existential_word.
existential_word --> ["a"];["an"];["the"];["some"];["there","is"];["there","are"];["there","exists"].

universal_words --> universal_word,universal_word.
universal_words --> universal_word.
universal_word --> ["every"];["all"];["forall"];["each"];["for","all"].

% =======================================================
% Adjective Phrases
% =======================================================

adjectives_phrase(Subj,IsaDoes,'#$and'(IsaDoes, AttribProp)) --> adj_phrase(Subj,AttribProp).
adjectives_phrase(_,IsaDoes,IsaDoes) --> [].

adj_phrase(Subj,Formula) -->  [A,B,C],{phrase_meaning_adj([A,B,C],Subj,Formula)}.
adj_phrase(Subj,Formula) -->  [A,B],{phrase_meaning_adj([A,B],Subj,Formula)}.
adj_phrase(Subj,Formula) -->  [A],{phrase_meaning_adj([A],Subj,Formula)}.


%'#$adjSemTrans'('#$Cloud-TheWord', 0, '#$RegularAdjFrame', ['#$weather', ':NOUN', '#$Cloudy']).
phrase_meaning_adj(Phrase,Subj,CycL):-
	 pos(CycWord,Phrase,'#$Adjective',Form),      
	 '#$adjSemTrans'(CycWord, _, _, Formula),
	 Repl='#$CollectionOfFn'(Subj),
	 %posMeans(Phrase,'#$Verb',POS,WordMeaning),
	 subst(Formula,':SUBJECT',Subj,Formula21),
	 subst(Formula21,':REPLACE',Repl,Formula2),
	 subst(Formula2,':NOUN',Subj,Formula3),
	 subst(Formula3,':ACTION','?ACTION',Formula4),
	 subst(Formula4,':OBJECT','?OBJECT',Formula6),
	 list_to_term(Formula6,CycL).

phrase_meaning_adj(Phrase,Subj,'#$hasAttributeOrCollection'(Subj,CycL)):-
      posMeans(Phrase,'#$Adjective',Form,CycL),not(lowerCasePred(CycL)).


% =======================================================
% Nouns Phrases
% =======================================================

noun_phrase(Subj,CycLIn,CycLOut) --> noun_expression(Subj,Isa,CycLOut),rel_clause(Subj,CycLIn,Isa).
%noun_phrase(Subj,CycL1,CycL) --> noun_expression(Subj,CycL1,CycL),[and],noun_phrase(Subj,CycL1,CycL)
   
noun_expression(PN,CycL,CycL) --> pronoun(PN).
noun_expression(PN,CycL,CycL) --> proper_noun_phrase(PN).
noun_expression(Subj,CycLVerb,CycLOut) -->  
   quant_phrase(Subj,AttribIsa,CycLVerb,CycLOut),
   adjectives_phrase(Subj,Isa,AttribIsa),
   collection_noun_isa(Subj,Isa).
noun_expression(Subj,CycLVerb,('#$thereExists'(Subj,'#$and'(AttribIsa,CycLVerb)))) -->  
   adjectives_phrase(Subj,AttribIsa1,AttribIsa),
   collection_noun_isa(Subj,Isa),
   adjectives_phrase(Subj,Isa,AttribIsa1).

%noun_expression(Subj,CycL1,CycL) -->  quant_phrase(Subj,Prop12,CycL1,CycL),collection_noun_isa(Subj,Prop1),rel_clause(Subj,Prop1,Prop12).
%noun_expression(Subj,CycL1,CycL) -->  quant_phrase(Subj,Prop1,CycL1,CycL),collection_noun_isa(Subj,Prop1).

% =======================================================
% Conjunctions
% =======================================================

conj_word --> [X],{conj_word(X)}.
disj_word --> [X],{disj_word(X)}.

conj_word(that). conj_word(who). conj_word(and). conj_word(also). disj_word(or).

% =======================================================
% Rel Clauses
% =======================================================

rel_clause(_,Isa,Isa) --> [].
rel_clause(Subj,Isa,'#$and'(Isa, HowDoes)) --> [that],adverbs_phrase(Event,Does,HowDoes),verb_phrase(Subj,Event,Does).


% =======================================================
% Qualified Noun
% =======================================================
collection_noun_isa(Subj,'#$isa'(Subj,CycLCollection)) --> collection_noun(Subj,CycLCollection).

collection_noun(Subj,CycLCollection) --> [A,B,C,D],{phraseNoun([A,B,C,D],Form,Subj,CycLCollection)}.
collection_noun(Subj,CycLCollection) --> [A,B,C],{phraseNoun([A,B,C],Form,Subj,CycLCollection)}.
collection_noun(Subj,CycLCollection) --> [A,B],{phraseNoun([A,B],Form,Subj,CycLCollection)}.
collection_noun(Subj,CycLCollection) --> [A],{phraseNoun([A],Form,Subj,CycLCollection)}.
collection_noun(Subj,'#$AdultMalePerson') --> [man].

phraseNoun(Eng,Form,Subj,CycLCollection):-
      phraseNoun_each(Eng,Form,CycLCollction),
      eng_subj(Eng,Subj).

eng_subj(Eng,Subj):-var(Subj),getVarAtom(Subj,Atom),concat_atom([?|Eng],'',T),atom_concat(T,Atom,Subj).
eng_subj(Eng,Subj):-!.


phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'#$SimpleNoun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'#$MassNoun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'#$AgentiveNoun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'#$Noun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'#$QuantifyingIndexical',_,CycL).
							 

% =======================================================
% Pronoun Phrases
% =======================================================
%'#$nounPrep'('#$Address-TheWord', '#$Of-TheWord', ['#$pointOfContactInfo', ':OBLIQUE-OBJECT', '#$ContactLocation', '#$addressText', ':NOUN']).


pronoun('?Speaker') --> ["I"];["i"];["me"].
pronoun('?TargetAgent') --> ["you"];["You"].
pronoun(CycLTerm) --> [A,B,C],{lex_pronoun([A,B,C],CycLTerm)}.
pronoun(CycLTerm) --> [A,B],{lex_pronoun([A,B],CycLTerm)}.
pronoun(CycLTerm) --> [A],{lex_pronoun([A],CycLTerm)}.

lex_pronoun(W,Fixed):-
      lex_pronoun2(W,T),
      fix_pronoun(W,T,Fixed).

fix_pronoun(W,[null],Fixed):-!,concat_atom(['?'|W],Fixed).
fix_pronoun(W,Fixed,Fixed).

lex_pronoun2(Words,CycLTerm):-posMeans(Words,'#$WHPronoun-Subj',_,CycLTerm).
lex_pronoun2(Words,CycLTerm):-posMeans(Words,'#$Pronoun',_,CycLTerm).
lex_pronoun2(Words,CycLTerm):-posMeans(Words,'#$ObjectPronoun',_,CycLTerm).

% =======================================================
% Proper Noun Phrases
% =======================================================

proper_noun_phrase(CycLTerm) --> [the],proper_noun(CycLTerm).
proper_noun_phrase(CycLTerm) --> proper_noun(CycLTerm).

proper_noun(CycLTerm) --> [A,B,C],{lex_proper_noun_cached( [A,B,C],CycLTerm)}.
proper_noun(CycLTerm) --> [A,B],{lex_proper_noun_cached( [A,B],CycLTerm)}.
proper_noun(CycLTerm) --> [A],{lex_proper_noun_cached( [A],CycLTerm)}.

lex_proper_noun_cached(Words,CycLTerm):-posMeans(Words,'#$ProperNoun',_,CycLTerm).

% =======================================================
% Verbs/Verb Phrases
% =======================================================


verb_phrase(Subj,Event,CycL) --> 
	adverbs_phrase(Event,VProp,CycL), 
	intrans_verb(Subj,Event,VProp).

verb_phrase(Subj,Event,CycL) -->
      adverbs_phrase(Event,VProp,EventProp),
      trans2_verb(Subj,Event,Obj,VProp),
      noun_phrase(Obj,EventProp,CycL).

verb_phrase(Subj,Event,CycL) -->
      adverbs_phrase(Event,VProp,EventProp),
      trans3_verb(Subj,Event,Obj,Prep,Target,VProp),
      noun_phrase(Obj,EventProp,ObjPropEvent),
      prepositional_noun_phrase(Target,ObjPropEvent,Prep,CycL).


trans3_verb(Subj,Event,Obj,Prep,Target,VProp) --> [A,B,C], {lex_trans3_verb([A,B,C],Subj,Event,Obj,Prep,Target,VProp)}.
trans3_verb(Subj,Event,Obj,Prep,Target,VProp) --> [A,B], {lex_trans3_verb([A,B],Subj,Event,Obj,Prep,Target,VProp)}.
trans3_verb(Subj,Event,Obj,Prep,Target,VProp) --> [A], {lex_trans3_verb([A],Subj,Event,Obj,Prep,Target,VProp)}.

lex_trans3_verb(VerbPhrase,Subj,Event,Obj,Prep,Target,CycL):-
      verb_frame(VerbPhrase,CycWord,3,CycPred,Formula),
      apply_frame(Formula,Subj,Event,Obj,Target,CycL).

lex_trans3_verb(VerbPhrase,Subj,Event,Obj,Prep,Target,VProp):-
      posMeans(VerbPhrase,'#$Verb',Form,CycLPred),
     nonvar(CycLPred),
      ignore(Event='?ACTION'),
      lex_trans3_verb2(VerbPhrase,CycLPred,Subj,Event,Obj,Prep,Target,VProp).
      
lex_trans3_verb2(VerbPhrase,CycLPred,Subj,Event,Obj,Prep,Target,VProp):-
      lowerCasePred(CycLPred) -> 
      VProp=..[CycLPred,Subj,Obj,Target] ; 
      VProp = '#$and'('#$isa'(Event,CycLPred),'#$doneBy'(Event,Subj),'#$eventOccursAt'(Event,Obj),'#$constituentInSituation'(Event,Target)).

   
      

      
% =======================================================
% Proposition
% =======================================================

prepositional_noun_phrase(Target,ObjPropEvent,Prep,CycL) -->
      proposition(Prep),noun_phrase(Target,ObjPropEvent,CycL).
prepositional_noun_phrase(Target,ObjPropEvent,'NIL',CycL) -->
      noun_phrase(Target,ObjPropEvent,CycL).


proposition(Prep) --> [PrepWord],{proposition_lex(PrepWord,Prep)}.

proposition_lex(X,X):-proposition_lex(X).
proposition_lex(to). proposition_lex(from). proposition_lex(of).


% =======================================================
% Adverbs
% =======================================================

adverbs_phrase(Event,IsaDoes,'#$and'(IsaDoes, AttribProp)) --> adv_phrase(Event,AttribProp).
adverbs_phrase(_,IsaDoes,IsaDoes) --> [].

adv_phrase(Event,Formula) -->  [A,B,C],{lex_adverb([A,B,C],Event,Formula)}.
adv_phrase(Event,Formula) -->  [A,B],{lex_adverb([A,B],Event,Formula)}.
adv_phrase(Event,Formula) -->  [A],{lex_adverb([A],Event,Formula)}.

lex_adverb(Phrase,Event,'#$hasAttributeOrCollection'(Event,Trait)):-
      posMeans(Phrase,'#$Adverb',Form,Trait).

% =======================================================
% Transitive 2 Verbs
% =======================================================

%trans2_verb(Subj,Y,like(Subj,Y)) --> [likes].
trans2_verb(Subj,Event,Obj,CycL) --> [A,B,C,D,E],{lex_verb_meaning([A,B,C,D,E],CycL,Subj,Event,Obj)}.
trans2_verb(Subj,Event,Obj,CycL) --> [A,B,C,D],{lex_verb_meaning([A,B,C,D],CycL,Subj,Event,Obj)}.
trans2_verb(Subj,Event,Obj,CycL) --> [A,B,C],{lex_verb_meaning([A,B,C],CycL,Subj,Event,Obj)}.
trans2_verb(Subj,Event,Obj,CycL) --> [A,B],{lex_verb_meaning([A,B],CycL,Subj,Event,Obj)}.
trans2_verb(Subj,Event,Obj,CycL) --> [A],{lex_verb_meaning([A],CycL,Subj,Event,Obj)}.
trans2_verb(Subj,admire(Subj,Obj),Obj,admire(Subj,Obj)) --> [admires].

% =======================================================
% Intransitive Verbs
% =======================================================

intrans_verb(Subj,Event,'#$and'('#$bodilyDoer'(Subj,Event),'#$isa'(Event,actOf(paint)))) --> [paints].

% ============================================================================
% Verb CycL Tense
% ============================================================================

%   '#$verbSemTrans'('#$Fancy-TheWord', 0, '#$TransitiveNPCompFrame', ['#$likesObject', ':SUBJECT', ':OBJECT']).
lex_verb_meaning(Phrase,MeaningTerm,Subj,Event,Obj):-
   tensed_lex_verb_meaning(Phrase,MeaningTerm,Subj,Event,Obj,Tense).

lex_verb_meaning(Phrase,MeaningTerm,Subj,Event,Obj):-
   lex_trans2_verb2(Phrase,MeaningTerm,Subj,Event,Obj).

% rewrites
tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,now):-
   atom(Words),atom_concat(Phrase,'s',Words),
   lex_trans2_verb2([Phrase],MeaningTerm,Subj,Event,Obj).

tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,past):-
   atom(Words),atom_concat(Phrase,'d',Words),
   lex_trans2_verb2([Phrase],MeaningTerm,Subj,Event,Obj).

tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,past):-
   atom(Words),atom_concat(Phrase,'ed',Words),
   lex_trans2_verb2([Phrase],MeaningTerm,Subj,Event,Obj).

tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,nowing):-
   atom(Words),atom_concat(Phrase,"ing",Words),
   lex_trans2_verb2([Phrase],MeaningTerm,Subj,Event,Obj).


% ============================================================================
% lex_trans2_verb2 Templates
% ============================================================================

lex_trans2_verb2(VerbPhrase,CycL,Subj,Event,Obj):-
      verb_frame(VerbPhrase,CycWord,2,CycPred,Formula),
      apply_frame(Formula,Subj,Event,Obj,'?OBLIQUE-OBJECT',CycL).

verb_frame([is,the,subclass,of],CycWord,Arity,CycPred,['#$genls',':SUBJECT',':OBJECT']).
verb_frame([is,a,subclass,of],CycWord,Arity,CycPred,['#$genls',':SUBJECT',':OBJECT']).
verb_frame([is,a],CycWord,Arity,CycPred,['#$isa',':SUBJECT',':OBJECT']).

verb_frame(VerbPhrase,CycWord,Arity,CycPred,Formula):-
      pos(CycWord,VerbPhrase,'#$Verb',Form),      
      '#$verbSemTrans'(CycWord, _, '#$TransitiveNPCompFrame', Formula),
      (contains_obliqe(Formula) -> Arity=3;Arity=2).

verb_frame([is,the,Verb,Phrase],CycWord1,2,CycPred,Formula2):-!,
      pos(CycWord1,[Verb],_,_),      
      pos(CycWord2,[Phrase],_,_),
      '#$nounPrep'(CycWord1,CycWord2, Formula),
      subst(Formula,':NOUN',':SUBJECT',Formula1),
      subst(Formula1,':OBLIQUE-OBJECT',':OBJECT',Formula2).

verb_frame([is,Verb,Phrase],CycWord1,2,CycPred,Formula2):-!,
      pos(CycWord1,[Verb],_,_),      
      pos(CycWord2,[Phrase],_,_),
      '#$nounPrep'(CycWord1,CycWord2, Formula),
      subst(Formula,':NOUN',':SUBJECT',Formula1),
      subst(Formula1,':OBLIQUE-OBJECT',':OBJECT',Formula2).


/*
the start of Obleec is Noun

Obleec is start of noun

'#$nounPrep'('#$Address-TheWord', '#$Of-TheWord', ['#$pointOfContactInfo', ':OBLIQUE-OBJECT', '#$ContactLocation', '#$addressText', ':NOUN']).
'#$nounPrep'('#$Retail-TheWord', '#$Of-TheWord', ['#$sellsProductType', ':NOUN', ':OBLIQUE-OBJECT']).
'#$nounPrep'('#$Market-TheWord', '#$Of-TheWord', ['#$sellsProductType', ':NOUN', ':OBLIQUE-OBJECT']).
'#$nounPrep'('#$Start-TheWord', '#$Of-TheWord', ['#$startingPoint', ':OBLIQUE-OBJECT', ':NOUN']).
*/

apply_frame(Formula,Subj,Event,Obj,Target,CycL):-
      ignore(Event='?ACTION'),
      subst(Formula,':SUBJECT',Subj,Formula2),
      subst(Formula2,':ACTION',Event,Formula3),
      subst(Formula3,':OBJECT',Obj,Formula4),
      subst(Formula4,':EVENT',Event,Formula5),
      subst(Formula5,':OBLIQUE-OBJECT',Target,Formula6),
      subst(Formula6,':ARG1',Subj,Formula7),
      subst(Formula7,':ACTION',Event,Formula8),
      subst(Formula8,':ARG2',Obj,Formula9),
      subst(Formula9,':EVENT',Event,Formula10),
      subst(Formula10,':ARG3',Target,Formula11),
      list_to_term(Formula11,CycL).

contains_obliqe(Formula):-flatten(Formula,Flat),member(':OBLIQUE-OBJECT',Flat).


lex_trans2_verb2(VerbPhrase,CycL,Subj,Event,Obj):-
      posMeans(VerbPhrase,'#$Verb',Form,CycLPred),
      ignore(Event='?ACTION'),
      atom(CycLPred),
      (lowerCasePred(CycLPred) -> 
	 CycL =..[CycLPred,Subj,Obj] ;
	 CycL = '#$and'('#$isa'(Event,CycLPred),'#$doneBy'(Event,Subj),'#$eventOccursAt'(Event,Obj))).


% uses genFormat
lex_trans2_verb2(Phrase,MeaningTerm,Subj,Event,Obj):-
   nonvar(Phrase),
   append(Phrase,['~a'],Rest),!,
   not(memberchk('~a',Rest)),
   '#$genFormat'(Pred,['~a'|Rest],Format),
   do_genformat(Format,Pred,Subj,Obj,MeaningTerm).

do_genformat(['NIL'],Pred,Subj,Obj,MeaningTerm):-MeaningTerm =..[Pred,Subj,Obj].
do_genformat([P1,P2],Pred,Subj,Obj,MeaningTerm):-fp(P1,1),fp(P2,2),!, MeaningTerm =..[Pred,Subj,Obj].
do_genformat([P2,P1],Pred,Subj,Obj,MeaningTerm):-fp(P1,1),fp(P2,2),!, MeaningTerm =..[Pred,Obj,Subj].

fp(N,N).
fp([N|_],N).



%'#$nonCompositionalVerbSemTrans'('#$End-TheWord', '#$Agreement', ['#$and', ['#$isa', ':ACTION', '#$EndingAnAgreement'], ['#$performedBy', ':ACTION', ':SUBJECT'], ['#$objectActedOn', ':ACTION', ':OBJECT']]).

%'#$lightVerb-TransitiveSemTrans'('#$Do-TheWord', '#$CommercialActivity', ['#$and', ['#$isa', ':ACTION', '#$CommercialActivity'], ['#$performedBy', ':ACTION', ':SUBJECT']]).
%'#$multiWordStringDenotesArgInReln'([service], '#$Provide-TheWord', '#$AgentiveNoun', '#$providerOfService', 2).
%'#$nounSemTrans'('#$Hire-TheWord', 0, '#$RegularNounFrame', ['#$and', ['#$isa', '?HIRE', '#$EmployeeHiring'], ['#$objectActedOn', '?HIRE', ':NOUN']]).
%'#$multiWordStringDenotesArgInReln'([service], '#$Provide-TheWord', '#$AgentiveNoun', '#$providerOfService', 2).
%'#$headMedialString'([intended], '#$Recipient-TheWord', [of, communication], '#$SimpleNoun', '#$communicationTarget').
%'#$agentiveNounSemTrans'('#$Emit-TheWord', 0, '#$RegularNounFrame', ['#$emitter', '?X', ':NOUN']).
%'#$adjSemTrans'('#$Cloud-TheWord', 0, '#$RegularAdjFrame', ['#$weather', ':NOUN', '#$Cloudy']).
%'#$relationIndicators'('#$abbreviationForMultiWordString', '#$Form-TheWord', '#$Verb').
%'#$genNatTerm-compoundString'('#$TransportViaFn', '#$Transport-TheWord', [via], '#$MassNoun', '#$singular').
%'#$genTemplate'('#$transferredThing', ['#$ConcatenatePhrasesFn', ['#$TermParaphraseFn-NP', ':ARG2'], ['#$BestHeadVerbForInitialSubjectFn', '#$Be-TheWord'], ['#$BestNLPhraseOfStringFn', 'transferred in'], ['#$TermParaphraseFn-NP', ':ARG1']]).
%'#$lightVerb-TransitiveSemTrans'('#$Do-TheWord', '#$CommercialActivity', ['#$and', ['#$isa', ':ACTION', '#$CommercialActivity'], ['#$performedBy', ':ACTION', ':SUBJECT']]).
%'#$genTemplate-Constrained'('#$isa', ['#$quotedCollection', ':ARG2'], ['#$NPIsNP-NLSentenceFn', ['#$BestCycLPhraseFn', ':ARG1'], ['#$BestDetNbarFn-Indefinite', ['#$TermParaphraseFn', ':ARG2']]]).



% ============================================================================
% posMeans
% ============================================================================

posMeans(Phrase,POS,Form,CycL):-
      posm_cached,!,posm_cached(Phrase,POS,Form,CycL).

:-index(posm_cached(1,1,1,1)).

posMeans(Phrase,POS,Form,CycL):-
      cache_the_posm,
      asserta(posm_cached),
      posMeans(Phrase,POS,Form,CycL).



cache_the_posm:-
      posm_c(Phrase,POS,Form,CycL),
      assertz_if_new(posm_cached(Phrase,POS,Form,CycL)),%write(.),flush,
      %format('~q~n',[posm_cached(CycWord,Phrase,POS,Form,CycL)]),
      fail.
   
cache_the_posm.



% ============================================================================
% General Parts Of Speech and Meanings
% ============================================================================

%'#$multiWordString'([health, care], '#$Organize-TheWord', '#$SimpleNoun', '#$MedicalCareOrganization').
posm_c(Phrase,POS,Form,CycL):-'#$multiWordString'(Words, CycWord, POS, CycL),
	 pos(CycWord,Eng,_POS,Form),append(Words,Eng,Phrase).

%'#$genPhrase'('#$MedicalCareProvider', '#$AgentiveNoun', '#$agentive-Sg', [health, care, provider]).
posm_c(Phrase,POS,Form,CycL):-'#$genPhrase'(CycL, POS,Form, Phrase).

%'#$headMedialString'([dimensionless], '#$Unit-TheWord', [of, measure], '#$SimpleNoun', '#$DimensionlessUnitOfMeasure').
posm_c(Phrase,POS,Form,CycL):-'#$headMedialString'(WordsBef,CycWord,WordsAft,POS, CycL),
	 pos(CycWord,Eng,_POS,Form),append(WordsBef,Eng,PhrasingLeft),append(PhrasingLeft,WordsAft,Phrase).

%'#$compoundString'('#$Movement-TheWord', [of, fluid], '#$MassNoun', '#$FluidFlowEvent').
posm_c(Phrase,POS,Form,CycL):-'#$compoundString'(CycWord,Words,POS, CycL),
	 pos(CycWord,Eng,_POS,Form),append(Eng,Words,Phrase).

%'#$prepCollocation'('#$Beset-TheWord', '#$Adjective', '#$By-TheWord').      
posm_c(Phrase,POS,Form2,'#$PrepCollocationFn'(CycWord1,POS,CycWord2)):-'#$prepCollocation'(CycWord1,POS, CycWord2),
	 pos(CycWord1,Eng1,_POS,Form1),pos(CycWord2,Eng2,_POS,Form2),append(Eng1,Eng2,Phrase).


%TODO '#$abbreviationForString'([scatology], [scat]).  '#$abbreviationForMultiWordString'([political], '#$Science-TheWord', '#$massNumber', [poli, sci]).
%'#$abbreviationForLexicalWord'('#$Kilogram-TheWord', '#$singular', [kg]).


%'#$initialismString'('#$CodeOfConduct', [coc]).
posm_c(Term,'#$SimpleNoun',normal,Proper) :- 
      '#$initialismString'(Proper,Term);
      '#$formerName'(Proper, Term);
      '#$scientificName'(Proper, Term);
      '#$termStrings-GuessedFromName'(Proper, Term);
      '#$nameString'(Proper, Term).

%'#$abbreviationString-PN'('#$India', ['IND']).
posm_c(Term,'#$ProperNoun',normal,Proper) :- 
      '#$initialismString'('#$CodeOfConduct',Term);
      '#$abbreviationString-PN'(Proper, Term);
      '#$preferredNameString'(Proper, Term);
      '#$countryName-LongForm'(Proper, Term);
      '#$countryName-ShortForm'(Proper, Term).

posm_c(Eng,POS,Form,CycL):-
	 pos(CycWord,Eng,POS,Form),
	 posm_build(CycWord,Eng,POS,Form,CycL).

% posm_c(Eng,POS,Form,CycL):-posTT(CycWord,Eng,POS,Form),'#$TTPred-denotation'(CycWord, POS, _, CycL).


%'#$denotation'('#$Capacity-TheWord', '#$SimpleNoun', 0, '#$Volume').
posm_build(CycWord,Eng,POS,Form,CycL):-'#$denotation'(CycWord, POS, _, CycL).

%'#$preferredGenUnit'('#$on-Physical', '#$Preposition-Directional-Telic', '#$On-TheWord').
posm_build(CycWord,Eng,POS,Form,CycL):-'#$preferredGenUnit'(CycL, POS, CycWord).

%'#$denotationRelatedTo'('#$Can-TheWord', '#$Verb', 0, '#$PreservingFood').
%posm_build(CycWord,Eng,POS,Form,'#$DenotationRelatedToFn'(CycL)):-'#$denotationRelatedTo'(CycWord, POS, _, CycL).
posm_build(CycWord,Eng,POS,Form,(CycL)):-'#$denotationRelatedTo'(CycWord, POS, _, CycL).

posm_build(CycWord,Eng,POS,Form,meaningOfWord(CycWord)):-
   not('#$denotation'(CycWord, _, _, CycL)),
   not('#$denotationRelatedTo'(CycWord, _, _, CycL)),
   not('#$preferredGenUnit'(CycL, _, CycWord)).
   
%'#$relationIndicators'('#$catalyst', '#$Catalyst-TheWord', '#$Verb').


pos(CycWord,Phrase,POS,Form):-'#$lex'(Form, CycWord, Phrase),'#$lexMap'(PosForms, CycWord, POS).
pos(CycWord,Phrase,POS,_) :- '#$partOfSpeech'(CycWord,POS, Phrase).
%'#$abbreviationForLexicalWord'('#$Kilogram-TheWord', '#$singular', [kg])
pos(CycWord,Phrase,POS,_) :- '#$abbreviationForLexicalWord'(CycWord,POS, Phrase).


% '#$prepCollocation'('#$Beset-TheWord', '#$Adjective', '#$By-TheWord').
posTT(CycWord,Phrase,POS,Form:PosForms):-'#$TT-lex'(Form, CycWord, Phrase),not('#$lex'(_, _, Phrase)),
      '#$TT-lexMap'(PosForms, CycWord, POS).

%'#$termStrings-GuessedFromName'('#$GenlsFormat', 'Genls Format').
%   '#$nounPrep'('#$Offspring-TheWord', '#$Of-TheWord', ['#$children', ':NOUN', ':OBLIQUE-OBJECT']),



%:-at_initialization(convertCycKb).
:-dynamic(posm_cached).
:-dynamic(posm_cached/4).
:-dynamic(real_posm_cached/4).
:-dynamic(real_posm_cachedTT/4).


  
:-catch([foo],_,true).

%:-posMeans(CycWord,Phrase,POS,Form,CycL).


clean_posm_cache:-
      retractall(posm_cached(CycWord,Phrase,POS,Form,[null])),
      retractall(posm_cached(CycWord,[],POS,Form,CycL)),
      retractall(real_posm_cached(CycWord,_,POS,Form,CycL)),
      retractall(real_posm_cachedTT(CycWord,_,POS,Form,CycL)),
      posm_cached(CycWord,Phrase,POS,Form,CycL),
      once(partition_cache(CycWord,Phrase,POS,Form,CycL)),
      fail.

clean_posm_cache:-tell(foo2),
   listing(real_posm_cached),
   listing(real_posm_cachedTT),
   told.

save_posm_cache
   :-tell(foo),
   listing(posm_cached),
   told.




partition_cache(CycWord,Phrase,POS,Form:'#$posForms',CycL):-!,partition_cache(CycWord,Phrase,POS,Form,CycL).

partition_cache(CycWord,Phrase,POS,Form,CycL):-
   atom(CycWord),
      atom_concat('#$TT',_,CycWord),!,
      partition_cacheTT(CycWord,Phrase,POS,Form,CycL).


% ======================================================
% Partitinion CycNL
% ======================================================

%posm_cached('#$Skill-TheWord', [skilled], '#$MassNoun', '#$regularDegree':'#$posForms', meaningOfWord('#$Skill-TheWord')).

partition_cache(CycWord,Phrase,POS,Form,meaningOfWord(CycWord)):-!,
   posm_cached(CycWord,Phrase,_,_,CycL),not(CycL=meaningOfWord(_)),
   assertz_if_new(real_posm_cached(CycWord,Phrase,POS,Form,CycL)).
      
   %real_posm_cached('#$Type-TheWord', [of, geographical, entity, classified, by, hierarchy], '#$SimpleNoun', form, '#$GeographicalEntityByHierarchy').
partition_cache(CycWord,Phrase,POS,form,CycL):-!,
      posm_cached(CycWord,BPhraseing,POS,Not_form,OMeaning),not(Not_form=form),
      append(BPhraseing,Phrase,OPhrasing),
      partition_cache(CycWord,OPhrasing,POS,Not_form,CycL).
      

partition_cache(CycWord,Phrase,POS,Form,CycL):-!,
   assertz_if_new(real_posm_cached(CycWord,Phrase,POS,Form,CycL)).

% ======================================================
% Partitinion TT CycNL
%posm_cached('#$TTWord-RATP', ['RATP'], '#$Noun', '#$TTPred-inflNounFemininePluralUnchecked', '#$TT-company-RATP')
% ======================================================

% Delete copies of cycNL from TT
partition_cacheTT(CycWord,Phrase,POS,Form,CycL):-
   posm_cached(OCycWord,Phrase,_,_,_),
   atom(OCycWord),
   not(atom_concat('#$TT',_,OCycWord)),!.

partition_cacheTT(CycWord,Phrase,POS,Form,meaningOfWord(CycWord)):-
   posm_cached(CycWord,Phrase,_,_,CycL),not(CycL=meaningOfWord(_)),!,
   assertz_if_new(real_posm_cachedTT(CycWord,Phrase,POS,Form,CycL)).

partition_cacheTT(CycWord,Phrase,POS,Form,CycL):-!,
   assertz_if_new(real_posm_cachedTT(CycWord,Phrase,POS,Form,CycL)).




%:-clean_posm_cache.

/*
   
cycPred('#$subcatFrameKeywords').
cycPred('#$nounPrep').
cycPred('#$verbPrep-Transitive').
cycPred('#$prepReln-Action').
cycPred('#$prepReln-Obj').
cycPred('#$preferredGenUnit').
cycPred('#$prepSemTrans').
cycPred('#$subcatFrame').
cycPred('#$verbSemTrans').
cycPred('#$genTemplate').
cycPred('#$multiWordSemTrans').
cycPred('#$denotationPlaceholder').
cycPred('#$denotesArgInReln').
cycPred('#$nounSemTrans').
cycPred('#$multiWordStringDenotesArgInReln').
cycPred('#$headMedialString').
cycPred('#$verbPrep-TransitiveTemplate').
cycPred('#$agentiveNounSemTrans').
cycPred('#$nonCompositionalVerbSemTrans').
cycPred('#$abbreviationForMultiWordString').
cycPred('#$abbreviationForLexicalWord').
cycPred('#$formalityOfWS').
cycPred('#$relationIndicators-Strong').
cycPred('#$verbSemTransPartial').
cycPred('#$implies').
cycPred('#$verbPrep-Passive').
cycPred('#$compoundStringDenotesArgInReln').
cycPred('#$genNatTerm-multiWordString').
cycPred('#$hyphenString').
cycPred('#$adjSemTrans').
cycPred('#$properNounSemTrans').
cycPred('#$morphologicallyDerivedFrom').
cycPred('#$politenessOfWS').
cycPred('#$genNatTerm-compoundString').
cycPred('#$genPhrase').
cycPred('#$abbreviationForCompoundString').
cycPred('#$posForms').
cycPred('#$plural').
cycPred('#$synonymousExternalConcept').
cycPred('#$lightVerb-TransitiveSemTrans').
cycPred('#$genTemplate-Constrained').
cycPred('#$expansion').
cycPred('#$compoundSemTrans').
cycPred('#$genStringAssertion-Old').
cycPred('#$compoundVerbSemTrans').
cycPred('#$adjSemTrans-Restricted').
cycPred('#$massNounSemTrans').
cycPred('#$morphologicalComposition').
cycPred('#$determinerAgreement').
cycPred('#$TT-lexMap').
cycPred('#$TT-lex').
cycPred('#$TTPred-denotation').
cycPred('#$TTPred-thetaRoleFeat-Frequent').
cycPred('#$TTPred-thetaRoleFeat-Trademark').
cycPred('#$TTPred-thetaRoleFeat-Informal').
cycPred('#$TTPred-thetaRoleFeat-LiteraryTechnical').
cycPred('#$TTPred-thetaRoleFeat-DefiniteArticle').
cycPred('#$TTPred-thetaRole').
cycPred('#$TTPred-thetaRoleFeat-ZeroArticle').
cycPred('#$TTPred-thetaRoleFeat-Slang').
cycPred('#$TTPred-thetaRoleFeat-Dated').
cycPred('#$TTPred-thetaRoleFeat-MassNoun').
cycPred('#$TTPred-thetaRoleFeat-Clause2Only').
cycPred('#$TTPred-thetaRoleFeat-Coordinator').
cycPred('#$TTPred-thetaRoleFeat-Abstract').
cycPred('#$TTPred-thetaRoleFeat-Plural').
cycPred('#$TTPred-thetaRoleFeat-CommonInflection').
cycPred('#$TTPred-thetaRoleFeat-AmericanEnglish').
cycPred('#$TTPred-thetaRoleFeat-BritishEnglish').
cycPred('#$TTPred-thetaRoleFeat-Infrequent').
cycPred('#$TTPred-thetaRoleSubcat-Indicative').
cycPred('#$TTPred-thetaRoleFeat-OtherRegionalDialect').
cycPred('#$TTPred-thetaRoleFeat-Feminine').
cycPred('#$TTPred-thetaRoleSubcat-Infinitive').
cycPred('#$TTPred-thetaRoleSubcat-PresentParticiple').
cycPred('#$TTPred-thetaRoleFeat-Humorous').
cycPred('#$TTPred-thetaRoleFeat-Masculine').
cycPred('#$TTPred-thetaRoleFeat-DoNotReorder').
cycPred('#$TTPred-thetaRoleFeat-ProgressiveNontaker').
cycPred('#$TTPred-thetaRoleFeat-BEEBlock').
cycPred('#$TTPred-thetaRoleFeat-ZBZBlock').
cycPred('#$TTPred-thetaRoleFeat-BWWBlock').
cycPred('#$TTPred-thetaRoleFeat-WBWBlock').
cycPred('#$TTPred-thetaRoleFeat-BZZBlock').
cycPred('#$TTPred-thetaRoleFeat-TranslationOnly').
cycPred('#$TTPred-thetaRoleFeat-PreposedAdjective').
cycPred('#$TTPred-thetaRoleFeat-Singular').
cycPred('#$TTPred-thetaRoleFeat-Derogatory').
cycPred('#$TTPred-thetaRoleFeat-Attributive').
cycPred('#$TTPred-thetaRoleSubcat-Subjunctive').
cycPred('#$TTPred-thetaRoleFeat-Predicative').
cycPred('#$TTPred-thetaRoleFeat-Canadian').
cycPred('#$TTPred-thetaRoleFeat-Tutoiement').
cycPred('#$TTPred-thetaRoleFeat-Etre').
cycPred('#$speechPartPreds').
cycPred('#$genlPreds').
cycPred('#$afterRemoving').
cycPred('#$isa').
cycPred('#$quotedArgument').
cycPred('#$notAssertible').
cycPred('#$genKeyword').
cycPred('#$basicSpeechPartPred').
cycPred('#$genFormat').
cycPred('#$arg2Format').
cycPred('#$argFormat').
cycPred('#$comparativeDegree').
cycPred('#$regularSuffix').
cycPred('#$paraphraseCoercionAllowedFrom').
cycPred('#$arg1Format').
cycPred('#$posPredForTemplateCategory').
cycPred('#$superlativeDegree').
cycPred('#$relationAllInstance').
cycPred('#$backchainForbidden').
cycPred('#$typedGenlPreds').
cycPred('#$transitiveViaArg').
cycPred('#$functionalInArgs').
cycPred('#$ncRuleLabel').
cycPred('#$keRequirementPreds').
cycPred('#$ncRuleTemplate').
cycPred('#$ncRuleConstraint').
cycPred('#$defaultCorrespondingRoles').
cycPred('#$headsPhraseOfType').
cycPred('#$posOfPhraseType').
cycPred('#$argGenl').
cycPred('#$barLevelOfPhraseType').
cycPred('#$transitiveViaArgInverse').
cycPred('#$nlPhraseTypeForTemplateCategory').
cycPred('#$denotatumArg').
cycPred('#$placeName-ShortForm').
cycPred('#$abnormal').
cycPred('#$scientificName').
cycPred('#$keWeakSuggestionPreds').
cycPred('#$sharedNotes').
cycPred('#$nameString').
cycPred('#$termStrings').
cycPred('#$interArgReln1-3').
cycPred('#$termStrings-GuessedFromName').
cycPred('#$acronymString').
cycPred('#$genFormat-Precise').
cycPred('#$completeExtentKnown').
cycPred('#$preferredTermStrings').
cycPred('#$initialismString').
cycPred('#$abbreviationString-PN').
cycPred('#$formerName').
cycPred('#$preferredNameString').
cycPred('#$countryName-LongForm').
cycPred('#$countryName-ShortForm').
cycPred('#$keStrongSuggestionPreds').
cycPred('#$arg5Isa').
cycPred('#$semTransArg').
cycPred('#$assertTemplate-Reln').
cycPred('#$interArgIsa3-4').
cycPred('#$interArgIsa4-5').
cycPred('#$arg4Format').
cycPred('#$termPOS-Strings').
cycPred('#$adjSemTransTemplate').
cycPred('#$salientAssertions').
cycPred('#$verbSemTransTemplate').
cycPred('#$semTransPredForPOS').
cycPred('#$arg5Format').
cycPred('#$not').
cycPred('#$phraseTemplateArg').
cycPred('#$ist').
cycPred('#$requiredArg1Pred').
cycPred('#$genPreferredKeyword').
cycPred('#$genQuestion').
cycPred('#$genExpansion').
cycPred('#$genStringAssertion').
cycPred('#$genFormat-ArgFixed').
cycPred('#$genStringAssertion-Precise').
cycPred('#$genFormat-NP').
cycPred('#$genNatTerm-ArgLast').
cycPred('#$genCodeSupport').
cycPred('#$relationAll').
cycPred('#$unitOfMeasurePrefixString').
cycPred('#$generateQuantOverArg').
cycPred('#$interArgIsa1-3').
cycPred('#$generateArgWithOutsideScope').
cycPred('#$interArgIsa1-2').
cycPred('#$formalityOfWS-New').
cycPred('#$languageOfLexicon').
cycPred('#$arg6Isa').
cycPred('#$abbreviationForString').
cycPred('#$instancesDontNeedLexification').
cycPred('#$lexicalWordTypeForLanguage').
cycPred('#$psRuleTemplateBindings').
cycPred('#$genlFuncs').
cycPred('#$resultIsaArgIsa').
cycPred('#$reformulatorEquiv').
cycPred('#$reformulatorEquals').
cycPred('#$reformulationPrecondition').
cycPred('#$subcatFrameDependentConstraint').
cycPred('#$subcatFrameArity').
cycPred('#$subcatFrameDependentKeyword').
cycPred('#$subcatFrameExample').
cycPred('#$prefixString').
cycPred('#$derivedUsingPrefix').
cycPred('#$relationAllExists').
cycPred('#$baseForm').
cycPred('#$derivedUsingSuffix').
cycPred('#$negationInverse').
cycPred('#$relationExistsAll').
cycPred('#$posBaseForms').
cycPred('#$suffixString').
cycPred('#$phoneticVariantOfPrefix').
cycPred('#$phoneticVariantOfSuffix').
cycPred('#$relationAllExistsMin').
cycPred('#$derivationalAffixBasePOS').
cycPred('#$affixRuleArity').
cycPred('#$affixRuleCategorialConstraint').
cycPred('#$derivationalAffixResultPOS').
cycPred('#$affixRuleTypeMorphemePosition').
cycPred('#$etymologicalVariantOfSuffix').
cycPred('#$variantOfSuffix').
cycPred('#$relationInstanceAll').
cycPred('#$genls').
cycPred('#$disjointWith').
cycPred('#$coExtensional').
cycPred('#$partitionedInto').
cycPred('#$notAssertibleCollection').
cycPred('#$typeGenls').
cycPred('#$resultIsaArg').
cycPred('#$resultGenlArg').
cycPred('#$arityMin').
cycPred('#$arityMax').
cycPred('#$argAndRestIsa').
cycPred('#$functionCorrespondingPredicate-Canonical').
cycPred('#$argsIsa').
cycPred('#$evaluationDefn').
cycPred('#$psRuleArity').
cycPred('#$psRuleSyntacticHeadDtr').
cycPred('#$psRuleTemplateDtr').
cycPred('#$psRuleConstraint').
cycPred('#$psRuleCategory').
cycPred('#$psRuleExample').
cycPred('#$psRuleSemanticsHandler').
cycPred('#$keConsiderationPreds').
cycPred('#$genlAttributes').
cycPred('#$negationAttribute').
cycPred('#$TTPred-thoughtTreasureToCyc').
cycPred('#$rewriteOf').
cycPred('#$TTPred-cycToThoughtTreasure').
cycPred('#$psRuleSemanticsFromDtr').
cycPred('#$posForTemplateCategory').
cycPred('#$backchainRequired').
cycPred('#$defnIff').
cycPred('#$completeCollectionExtent').
cycPred('#$quotedCollection').
cycPred('#$keClarifyingCollection').
cycPred('#$relationAllExistsCount').
cycPred('#$rolesForEventType').
cycPred('#$defaultReformulationDirectionInModeForPred').
cycPred('#$keStrongConsiderationPreds').
cycPred('#$keStrongSuggestion').
cycPred('#$requiredActorSlots').
cycPred('#$collectionUnion').
cycPred('#$keCommonQueryPredForInstances').
cycPred('#$subjectRoles').
cycPred('#$trueRule').
cycPred('#$TTPred-processor-of').
cycPred('#$TTPred-create').
cycPred('#$TTPred-eng-aux-verb-of').
cycPred('#$TTPred-cpart-of').
cycPred('#$TTPred-capital-of').
cycPred('#$TTPred-polity-of').
cycPred('#$TTPred-first-surname-of').
cycPred('#$TTPred-performed-in').
cycPred('#$TTPred-part-of').
cycPred('#$TTPred-processors-of').
cycPred('#$TTPred-owner-of').
cycPred('#$TTPred-cost-of').
cycPred('#$TTPred-first-name-of').
cycPred('#$TTPred-population-of').
cycPred('#$TTPred-antonym-of').
cycPred('#$TTPred-time-range-of').
cycPred('#$TTPred-event02-of').
cycPred('#$TTPred-fr-infl-tense-of').
cycPred('#$TTPred-role07-of').
cycPred('#$TTPred-stereo').
cycPred('#$TTPred-NTSC').
cycPred('#$TTPred-phone-prefix-of').
cycPred('#$TTPred-variant-of').
cycPred('#$TTPred-r1').
cycPred('#$TTPred-time-off').
cycPred('#$TTPred-diminutive-of').
cycPred('#$TTPred-value-of').
cycPred('#$TTPred-result-of').
cycPred('#$TTPred-eng-aux-tense-of').
cycPred('#$TTPred-fr-subjunctive-of').
cycPred('#$TTPred-street-of').
cycPred('#$TTPred-feed-of').
cycPred('#$TTPred-role03-script-of').
cycPred('#$TTPred-atomic-weight-of').
cycPred('#$TTPred-political-affiliation-of').
cycPred('#$TTPred-dark-brown').
cycPred('#$TTPred-violet').
cycPred('#$TTPred-SECAM').
cycPred('#$TTPred-fr').
cycPred('#$TTPred-product-of').
cycPred('#$TTPred-computer-bus-of').
cycPred('#$TTPred-event03-of').
cycPred('#$TTPred-attend-twelfth-grade').
cycPred('#$TTPred-inside').
cycPred('#$TTPred-street-number-of').
cycPred('#$TTPred-period-of').
cycPred('#$TTPred-role02-of').
cycPred('#$TTPred-MIPS-of').
cycPred('#$TTPred-attr-occultist').
cycPred('#$TTPred-canonical-factor-of').
cycPred('#$TTPred-diploma-of').
cycPred('#$TTPred-event04-of').
cycPred('#$TTPred-duration-of').
cycPred('#$TTPred-specialty-of').
cycPred('#$TTPred-attr-rel-value').
cycPred('#$TTPred-eng-infl-mood-of').
cycPred('#$TTPred-fr-size-of').
cycPred('#$TTPred-us').
cycPred('#$TTPred-producer-of').
cycPred('#$TTPred-second-name-of').
cycPred('#$TTPred-emotion-of').
cycPred('#$TTPred-event01-of').
cycPred('#$TTPred-silk').
cycPred('#$TTPred-max-value-of').
cycPred('#$TTPred-preppy').
cycPred('#$TTPred-frequency-of').
cycPred('#$TTPred-attend-kindergarten').
cycPred('#$TTPred-politically-ultraconservative').
cycPred('#$TTPred-other-language-of').
cycPred('#$TTPred-SPECintRate92-of').
cycPred('#$TTPred-role02-script-of').
cycPred('#$TTPred-event05-of').
cycPred('#$TTPred-slots-of').
cycPred('#$TTPred-attr-South-Korean').
cycPred('#$TTPred-coordinates-of').
cycPred('#$TTPred-goal-of').
cycPred('#$TTPred-incorporated-in').
cycPred('#$TTPred-green').
cycPred('#$TTPred-male').
cycPred('#$TTPred-clothing-middle').
cycPred('#$TTPred-female').
cycPred('#$TTPred-href').
cycPred('#$TTPred-red').
cycPred('#$TTPred-r8').
cycPred('#$TTPred-many-to-one').
cycPred('#$TTPred-clothing-bottom').
cycPred('#$TTPred-role05-of').
cycPred('#$TTPred-dark-gray').
cycPred('#$TTPred-nationality-of').
cycPred('#$TTPred-activated-emotion-of').
cycPred('#$TTPred-brown').
cycPred('#$TTPred-headquarters-of').
cycPred('#$TTPred-attr-Liverpudlian').
cycPred('#$TTPred-attr-polytheist').
cycPred('#$TTPred-event09-of').
cycPred('#$TTPred-actor-of').
cycPred('#$TTPred-antiparticle-of').
cycPred('#$TTPred-baryon-number-of').
cycPred('#$TTPred-role01-of').
cycPred('#$TTPred-thin-stripes').
cycPred('#$TTPred-fr-infl-mood-of').
cycPred('#$TTPred-gray').
cycPred('#$TTPred-clothing-top').
cycPred('#$TTPred-max-value1-of').
cycPred('#$TTPred-first-OS-of').
cycPred('#$TTPred-row-distance-of').
cycPred('#$TTPred-event11-of').
cycPred('#$TTPred-religion-of').
cycPred('#$TTPred-SPECint92-of').
cycPred('#$TTPred-Dacron').
cycPred('#$TTPred-r3').
cycPred('#$TTPred-postal-code-of').
cycPred('#$TTPred-event22-of').
cycPred('#$TTPred-r2').
cycPred('#$TTPred-role04-of').
cycPred('#$TTPred-symmetric').
cycPred('#$TTPred-official-language-of').
cycPred('#$TTPred-leadto1').
cycPred('#$TTPred-height-of').
cycPred('#$TTPred-sphere').
cycPred('#$TTPred-white').
cycPred('#$TTPred-luminance-of').
cycPred('#$TTPred-acrylic').
cycPred('#$TTPred-clothing-ankle').
cycPred('#$TTPred-fr-aux-tense-of').
cycPred('#$TTPred-event06-of').
cycPred('#$TTPred-text-ref').
cycPred('#$TTPred-us-size-of').
cycPred('#$TTPred-min-value-of').
cycPred('#$TTPred-length-of').
cycPred('#$TTPred-atomic-number-of').
cycPred('#$TTPred-affiliation-of').
cycPred('#$TTPred-humanmade').
cycPred('#$TTPred-made-in').
cycPred('#$TTPred-attr-ENA').
cycPred('#$TTPred-virtual-memory-of').
cycPred('#$TTPred-nonencrypted').
cycPred('#$TTPred-MFLOPS-of').
cycPred('#$TTPred-contralto').
cycPred('#$TTPred-SPECmark89-of').
cycPred('#$TTPred-has-ceiling').
cycPred('#$TTPred-travel-max-speed-of').
cycPred('#$TTPred-event12-of').
cycPred('#$TTPred-fr-tense-0-of').
cycPred('#$TTPred-triangles').
cycPred('#$TTPred-travel-cargo-capacity-of').
cycPred('#$TTPred-rhs-assertion-of').
cycPred('#$TTPred-eng-main-tense-of').
cycPred('#$TTPred-role10-of').
cycPred('#$TTPred-phone-number-of').
cycPred('#$TTPred-attend-day-care').
cycPred('#$TTPred-event08-of').
cycPred('#$TTPred-event17-of').
cycPred('#$TTPred-attr-rel-range').
cycPred('#$TTPred-topic-of').
cycPred('#$TTPred-inverse-of').
cycPred('#$TTPred-occupation-of').
cycPred('#$TTPred-isospin-of').
cycPred('#$TTPred-fr-main-tense-of').
cycPred('#$TTPred-canonical-of').
cycPred('#$TTPred-original-run').
cycPred('#$TTPred-event15-of').
cycPred('#$TTPred-first-author-of').
cycPred('#$TTPred-weight-of').
cycPred('#$TTPred-clock-frequency-of').
cycPred('#$TTPred-residence-of').
cycPred('#$TTPred-intelligent').
cycPred('#$TTPred-post-title-of').
cycPred('#$TTPred-shirtlayer').
cycPred('#$TTPred-RAM-of').
cycPred('#$TTPred-blue').
cycPred('#$TTPred-topological-genus-of').
cycPred('#$TTPred-saturation-of').
cycPred('#$TTPred-arch-of').
cycPred('#$TTPred-electric-charge-of').
cycPred('#$TTPred-r4').
cycPred('#$TTPred-vitamin-B2').
cycPred('#$TTPred-cotton').
cycPred('#$TTPred-blue-green').
cycPred('#$TTPred-wool').
cycPred('#$TTPred-attr-Anglican').
cycPred('#$TTPred-diameter-of').
cycPred('#$TTPred-eng-infl-adverb-of').
cycPred('#$TTPred-studio-of').
cycPred('#$TTPred-strangeness-of').
cycPred('#$TTPred-spin-of').
cycPred('#$TTPred-lhs-class-of').
cycPred('#$TTPred-role03-of').
cycPred('#$TTPred-famous').
cycPred('#$TTPred-underlying-of').
cycPred('#$TTPred-case-of').
cycPred('#$TTPred-counter-tenor').
cycPred('#$TTPred-gen-max-of').
cycPred('#$TTPred-unwalkable').
cycPred('#$TTPred-entry-condition-of').
cycPred('#$TTPred-politically-radical-socialiste').
cycPred('#$TTPred-event07-of').
cycPred('#$TTPred-FPU-of').
cycPred('#$TTPred-magenta').
cycPred('#$TTPred-avoid').
cycPred('#$TTPred-orientation-of').
cycPred('#$TTPred-third-name-of').
cycPred('#$TTPred-baritone').
cycPred('#$TTPred-fine-weave').
cycPred('#$TTPred-light-gray').
cycPred('#$TTPred-rhs-feat-of').
cycPred('#$TTPred-clothing-foot').
cycPred('#$TTPred-affiliate-of').
cycPred('#$TTPred-schizophrenia').
cycPred('#$TTPred-col-distance-of').
cycPred('#$TTPred-attr-Baptist').
cycPred('#$TTPred-frying-of').
cycPred('#$TTPred-comment').
cycPred('#$TTPred-stripes').
cycPred('#$TTPred-mother-of').
cycPred('#$TTPred-role13-of').
cycPred('#$TTPred-host-of').
cycPred('#$TTPred-adult').
cycPred('#$TTPred-min-value2-of').
cycPred('#$TTPred-tt-ticker-of').
cycPred('#$TTPred-vertical-polarization').
cycPred('#$TTPred-skeptical').
cycPred('#$TTPred-ca').
cycPred('#$TTPred-seat-of').
cycPred('#$TTPred-preposition-of').
cycPred('#$TTPred-eng-infl-tense-of').
cycPred('#$TTPred-fr-tense-neg-4-of').
cycPred('#$TTPred-lhs-pos-of').
cycPred('#$TTPred-fr-literary-subjunctive-of').
cycPred('#$TTPred-litigious').
cycPred('#$TTPred-role06-of').
cycPred('#$TTPred-cardioid').
cycPred('#$TTPred-omnidirectional').
cycPred('#$TTPred-fr-tense-pos-2-of').
cycPred('#$TTPred-black').
cycPred('#$TTPred-clothing-hand').
cycPred('#$TTPred-optimistic').
cycPred('#$TTPred-ap').
cycPred('#$TTPred-event19-of').
cycPred('#$TTPred-walkable').
cycPred('#$TTPred-SPECfpRate92-of').
cycPred('#$TTPred-level-of').
cycPred('#$TTPred-eng-tense-0-of').
cycPred('#$TTPred-rhs-pos-of').
cycPred('#$TTPred-figure-8').
cycPred('#$TTPred-serve-meal').
cycPred('#$TTPred-bigoted').
cycPred('#$TTPred-middle-aged-adult').
cycPred('#$TTPred-role18-of').
cycPred('#$TTPred-anchor-of').
cycPred('#$TTPred-pdg-of').
cycPred('#$TTPred-lighter-than-air').
cycPred('#$TTPred-attr-Canadian').
cycPred('#$TTPred-fr-aux-verb-of').
cycPred('#$TTPred-manufacturer-of').
cycPred('#$TTPred-eng-tense-neg-5-of').
cycPred('#$TTPred-believe').
cycPred('#$TTPred-sister-of').
cycPred('#$TTPred-superior-of').
cycPred('#$TTPred-attr-Irish').
cycPred('#$TTPred-exchange-ticker-of').
cycPred('#$TTPred-amber').
cycPred('#$TTPred-role08-of').
cycPred('#$TTPred-ideal-sleep-of').
cycPred('#$TTPred-Caucasian').
cycPred('#$TTPred-politically-subversive').
cycPred('#$TTPred-attr-Chinese').
cycPred('#$TTPred-eat').
cycPred('#$TTPred-tweed').
cycPred('#$TTPred-one-to-one').
cycPred('#$TTPred-pink').
cycPred('#$TTPred-waveform-of').
cycPred('#$TTPred-light-blue').
cycPred('#$TTPred-fanciful').
cycPred('#$TTPred-success-emotion-of').
cycPred('#$TTPred-aunt-of').
cycPred('#$TTPred-unique-author-of').
cycPred('#$TTPred-event27-of').
cycPred('#$TTPred-barrier-isa').
cycPred('#$TTPred-politically-Leninist').
cycPred('#$TTPred-event14-of').
cycPred('#$TTPred-free-object').
cycPred('#$TTPred-gen-min-of').
cycPred('#$TTPred-travel-max-distance-of').
cycPred('#$TTPred-currency-of').
cycPred('#$TTPred-tie-dye').
cycPred('#$TTPred-attr-Kashmiri').
cycPred('#$TTPred-isbn-of').
cycPred('#$TTPred-dots').
cycPred('#$TTPred-unit-of').
cycPred('#$TTPred-overlayer').
cycPred('#$TTPred-event10-of').
cycPred('#$TTPred-taped').
cycPred('#$TTPred-event16-of').
cycPred('#$TTPred-cycle-time-of').
cycPred('#$TTPred-stored-in').
cycPred('#$TTPred-r9').
cycPred('#$TTPred-sold-at').
cycPred('#$TTPred-event29-of').
cycPred('#$TTPred-event13-of').
cycPred('#$TTPred-attr-Swiss').
cycPred('#$TTPred-live').
cycPred('#$TTPred-surface-area-of').
cycPred('#$TTPred-eng-tense-pos-3-of').
cycPred('#$TTPred-fr-translation-of').
cycPred('#$TTPred-attr-African').
cycPred('#$TTPred-similar').
cycPred('#$TTPred-attr-Menton').
cycPred('#$TTPred-related-concept-of').
cycPred('#$TTPred-language-of').
cycPred('#$TTPred-closed-captioned').
cycPred('#$TTPred-eng-tense-neg-4-of').
cycPred('#$TTPred-used-for').
cycPred('#$TTPred-infant').
cycPred('#$TTPred-psychosis').
cycPred('#$TTPred-film-converted-to-NTSC').
cycPred('#$TTPred-attend-eighth-grade').
cycPred('#$TTPred-light-brown').
cycPred('#$TTPred-stitch-strings').
cycPred('#$TTPred-politically-Owenite').
cycPred('#$TTPred-model-number-of').
cycPred('#$TTPred-professional-product').
cycPred('#$TTPred-used-at').
cycPred('#$TTPred-attr-Neuilly').
cycPred('#$TTPred-polyester').
cycPred('#$TTPred-event18-of').
cycPred('#$TTPred-role11-of').
cycPred('#$TTPred-role09-of').
cycPred('#$TTPred-politically-Republican').
cycPred('#$TTPred-response-of').
cycPred('#$TTPred-leadto12').
cycPred('#$TTPred-r7').
cycPred('#$TTPred-min-value1-of').
cycPred('#$TTPred-power-of').
cycPred('#$TTPred-attr-North-Korean').
cycPred('#$TTPred-rhs-class-of').
cycPred('#$TTPred-yellow').
cycPred('#$TTPred-fr-tense-literary-neg-4-of').
cycPred('#$TTPred-event25-of').
cycPred('#$TTPred-fr-tense-pos-1-of').
cycPred('#$TTPred-SPECfp92-of').
cycPred('#$TTPred-politically-Social-Democratic').
cycPred('#$TTPred-mezzo-soprano').
cycPred('#$TTPred-computer-chassis-of').
cycPred('#$TTPred-eng-translation-of').
cycPred('#$TTPred-politically-phalansterian').
cycPred('#$TTPred-attend-graduate-school').
cycPred('#$TTPred-time-on').
cycPred('#$TTPred-attend-seventh-grade').
cycPred('#$TTPred-uk').
cycPred('#$TTPred-print').
cycPred('#$TTPred-role06-script-of').
cycPred('#$TTPred-underlayer').
cycPred('#$TTPred-skill-of-play').
cycPred('#$TTPred-attr-Scottish').
cycPred('#$TTPred-eng-progressive-of').
cycPred('#$TTPred-eng-tense-pos-2-of').
cycPred('#$TTPred-politically-nazi').
cycPred('#$TTPred-UK-eng-subjunctive-of').
cycPred('#$TTPred-attr-libertine').
cycPred('#$TTPred-travel-crew-of').
cycPred('#$TTPred-fr-tense-neg-3-of').
cycPred('#$TTPred-attr-Czech').
cycPred('#$TTPred-diagonal-length-of').
cycPred('#$TTPred-crosses').
cycPred('#$TTPred-cyan').
cycPred('#$TTPred-Ceefax').
cycPred('#$TTPred-unit2-of').
cycPred('#$TTPred-fruit-of').
cycPred('#$TTPred-last-OS-of').
cycPred('#$TTPred-attend-ninth-grade').
cycPred('#$TTPred-very-old-adult').
cycPred('#$TTPred-annoying').
cycPred('#$TTPred-champagne').
cycPred('#$TTPred-gestation-period-of').
cycPred('#$TTPred-groggy').
cycPred('#$TTPred-steel').
cycPred('#$TTPred-thin-checker').
cycPred('#$TTPred-cylinder').
cycPred('#$TTPred-create-paint').
cycPred('#$TTPred-vitamin-A').
cycPred('#$TTPred-clothing-calf').
cycPred('#$TTPred-minitel-number-of').
cycPred('#$TTPred-vestlayer').
cycPred('#$TTPred-charm-of').
cycPred('#$TTPred-inexperienced').
cycPred('#$TTPred-illegal').
cycPred('#$TTPred-leadto2').
cycPred('#$TTPred-coatlayer').
cycPred('#$TTPred-attr-Episcopalian').
cycPred('#$TTPred-child').
cycPred('#$TTPred-creator-of').
cycPred('#$TTPred-gold').
cycPred('#$TTPred-attr-Zen-Buddhist').
cycPred('#$TTPred-r6').
cycPred('#$TTPred-max-value2-of').
cycPred('#$TTPred-attr-British').
cycPred('#$TTPred-event32-of').
cycPred('#$TTPred-role16-of').
cycPred('#$TTPred-attr-Greek-Orthodox').
cycPred('#$TTPred-fr-tense-neg-7-of').
cycPred('#$TTPred-unit1-of').
cycPred('#$TTPred-snobby').
cycPred('#$TTPred-attr-Burgundian').
cycPred('#$TTPred-attend-school').
cycPred('#$TTPred-width-of').
cycPred('#$TTPred-video-channel-of').
cycPred('#$TTPred-soprano').
cycPred('#$TTPred-attr-Protestant').
cycPred('#$TTPred-next-state-of').
cycPred('#$TTPred-cowardly').
cycPred('#$TTPred-OS-of').
cycPred('#$TTPred-attr-doer').
cycPred('#$TTPred-apolitical').
cycPred('#$TTPred-event20-of').
cycPred('#$TTPred-wife-of').
cycPred('#$TTPred-composer-of').
cycPred('#$TTPred-fixed-object').
cycPred('#$TTPred-white-wine').
cycPred('#$TTPred-neurosis').
cycPred('#$TTPred-small-squares').
cycPred('#$TTPred-radio-station-of').
cycPred('#$TTPred-Shetland-wool').
cycPred('#$TTPred-politically-Maoist').
cycPred('#$TTPred-prejudiced').
cycPred('#$TTPred-attr-Sorbonne').
cycPred('#$TTPred-issuer-of').
cycPred('#$TTPred-r5').
cycPred('#$TTPred-computer-monitor-of').
cycPred('#$TTPred-role04-script-of').
cycPred('#$TTPred-politically-progressive').
cycPred('#$TTPred-calfskin').
cycPred('#$TTPred-bass-baritone').
cycPred('#$TTPred-clothing-thigh').
cycPred('#$TTPred-example').
cycPred('#$TTPred-attr-Lutheran').
cycPred('#$TTPred-calfskin-velvet').
cycPred('#$TTPred-politically-fascist').
cycPred('#$TTPred-worsted').
cycPred('#$TTPred-crosshatch').
cycPred('#$TTPred-translation-of').
cycPred('#$TTPred-attr-East-Ender').
cycPred('#$TTPred-event23-of').
cycPred('#$TTPred-publish').
cycPred('#$TTPred-clothing-wrist').
cycPred('#$TTPred-event21-of').
cycPred('#$TTPred-supercardioid').
cycPred('#$TTPred-lazy').
cycPred('#$TTPred-attr-Vedaic').
cycPred('#$TTPred-glove-leather').
cycPred('#$TTPred-fine-stitch').
cycPred('#$TTPred-vitamin-B5').
cycPred('#$TTPred-politically-anticapitalist').
cycPred('#$TTPred-attr-shamanist').
cycPred('#$TTPred-corduroy').
cycPred('#$TTPred-olive-green').
cycPred('#$TTPred-eng-tense-pos-1-of').
cycPred('#$TTPred-first-editor-of').
cycPred('#$TTPred-humorous').
cycPred('#$TTPred-mono').
cycPred('#$TTPred-attend-eleventh-grade').
cycPred('#$TTPred-RIC-of').
cycPred('#$TTPred-khaki-color').
cycPred('#$TTPred-politically-liberal').
cycPred('#$TTPred-role12-of').
cycPred('#$TTPred-roommate-of').
cycPred('#$TTPred-fr-tense-neg-6-of').
cycPred('#$TTPred-old-adult').
cycPred('#$TTPred-role17-of').
cycPred('#$TTPred-attr-Hongkong').
cycPred('#$TTPred-attr-anti-religious').
cycPred('#$TTPred-pin-stripe').
cycPred('#$TTPred-rayon').
cycPred('#$TTPred-leather').
cycPred('#$TTPred-encrypted').
cycPred('#$TTPred-wide-angle-cardioid').
cycPred('#$TTPred-politically-collectivist').
cycPred('#$TTPred-official-residence-of').
cycPred('#$TTPred-politically-right-wing').
cycPred('#$TTPred-attend-technical-school').
cycPred('#$TTPred-attr-Jaina').
cycPred('#$TTPred-drivable').
cycPred('#$TTPred-politically-conservative').
cycPred('#$TTPred-attend-junior-college').
cycPred('#$TTPred-attr-Shintoist').
cycPred('#$TTPred-trade-arbitrage').
cycPred('#$TTPred-politically-Tory').
cycPred('#$TTPred-acetate').
cycPred('#$TTPred-second-author-of').
cycPred('#$TTPred-thirty-something').
cycPred('#$TTPred-architect-of').
cycPred('#$TTPred-attr-rel-proportional').
cycPred('#$TTPred-nonborn').
cycPred('#$TTPred-delayed').
cycPred('#$TTPred-fabric-linen').
cycPred('#$TTPred-politically-Marxist-Leninist').
cycPred('#$TTPred-entertaining').
cycPred('#$TTPred-unsuccessful').
cycPred('#$TTPred-attr-Danish').
cycPred('#$TTPred-cone').
cycPred('#$TTPred-abstinent').
cycPred('#$TTPred-young-adult').
cycPred('#$TTPred-nonreligious').
cycPred('#$TTPred-dark-blue').
cycPred('#$TTPred-eng-tense-literary-neg-4-of').
cycPred('#$TTPred-create-write-music').
cycPred('#$TTPred-attr-Taoist').
cycPred('#$TTPred-attr-Mormon').
cycPred('#$TTPred-politically-radical').
cycPred('#$TTPred-cusip-of').
cycPred('#$TTPred-politically-royalist').
cycPred('#$TTPred-overlayerpost').
cycPred('#$TTPred-attr-Finnish').
cycPred('#$TTPred-create-draw').
cycPred('#$TTPred-eng-tense-neg-7-of').
cycPred('#$TTPred-vitamin-B9').
cycPred('#$TTPred-attr-New-York').
cycPred('#$TTPred-hypercardioid').
cycPred('#$TTPred-liquid').
cycPred('#$TTPred-employer-of').
cycPred('#$TTPred-attr-agnostic').
cycPred('#$TTPred-super-100-CycL').
cycPred('#$TTPred-fr-tense-neg-5-of').
cycPred('#$TTPred-attend-first-grade').
cycPred('#$TTPred-attr-Orthodox-Eastern-Church').
cycPred('#$TTPred-travel-passengers-of').
cycPred('#$TTPred-secretary-of').
cycPred('#$TTPred-key-of').
cycPred('#$TTPred-eng-tense-neg-3-of').
cycPred('#$TTPred-sick').
cycPred('#$TTPred-rare').
cycPred('#$TTPred-Hispanic').
cycPred('#$TTPred-bottomness-of').
cycPred('#$TTPred-suburban').
cycPred('#$TTPred-US-eng-subjunctive-of').
cycPred('#$TTPred-coupon-of').
cycPred('#$TTPred-floral').
cycPred('#$TTPred-attr-kharidjite').
cycPred('#$TTPred-attr-Christian').
cycPred('#$TTPred-bits-of').
cycPred('#$TTPred-fr-tense-neg-2-of').
cycPred('#$TTPred-nylon').
cycPred('#$TTPred-attr-monotheist').
cycPred('#$TTPred-chine-cotton').
cycPred('#$TTPred-attend-junior-high-school').
cycPred('#$TTPred-tenor').
cycPred('#$TTPred-market-of').
cycPred('#$TTPred-attr-Korean').
cycPred('#$TTPred-underlayerpre').
cycPred('#$TTPred-politically-yippie').
cycPred('#$TTPred-researcher-of').
cycPred('#$TTPred-politically-individualistic').
cycPred('#$TTPred-natural-parent-of').
cycPred('#$TTPred-vitamin-B12').
cycPred('#$TTPred-politically-anarcho-syndicalist').
cycPred('#$TTPred-blue-gray').
cycPred('#$TTPred-checker').
cycPred('#$TTPred-attr-Episcopal').
cycPred('#$TTPred-attr-Asian').
cycPred('#$TTPred-event31-of').
cycPred('#$TTPred-ivory-colored').
cycPred('#$TTPred-circumference-of').
cycPred('#$TTPred-politically-left-wing-radical').
cycPred('#$TTPred-attr-X').
cycPred('#$TTPred-second-editor-of').
cycPred('#$TTPred-eng-tense-neg-6-of').
cycPred('#$TTPred-market-manipulation').
cycPred('#$TTPred-insider-trading').
cycPred('#$TTPred-role01-script-of').
cycPred('#$TTPred-spinoff-of').
cycPred('#$TTPred-embryonic').
cycPred('#$TTPred-attr-Calvinist').
cycPred('#$TTPred-copper').
cycPred('#$TTPred-politically-Stalinist').
cycPred('#$TTPred-attr-Catholic').
cycPred('#$TTPred-Panda1').
cycPred('#$TTPred-circadian-rhythm-of').
cycPred('#$TTPred-iodine').
cycPred('#$TTPred-nerdy').
cycPred('#$TTPred-PAL').
cycPred('#$TTPred-small-dots').
cycPred('#$TTPred-attend-third-grade').
cycPred('#$TTPred-attr-Jewish').
cycPred('#$TTPred-poor').
cycPred('#$TTPred-bars').
cycPred('#$TTPred-event28-of').
cycPred('#$TTPred-role14-of').
cycPred('#$TTPred-distillation-of').
cycPred('#$TTPred-attr-Dutch').
cycPred('#$TTPred-attr-Swedish').
cycPred('#$TTPred-politically-Democratic').
cycPred('#$TTPred-attr-Afghan').
cycPred('#$TTPred-attr-Mennonite').
cycPred('#$TTPred-create-write-literature').
cycPred('#$TTPred-viscose').
cycPred('#$TTPred-addiction-of').
cycPred('#$TTPred-squares').
cycPred('#$TTPred-rubber').
cycPred('#$TTPred-attr-Muslem').
cycPred('#$TTPred-attr-wrongdoing').
cycPred('#$TTPred-politically-Saint-Simonian').
cycPred('#$TTPred-bullying').
cycPred('#$TTPred-attend-secondary-school').
cycPred('#$TTPred-attr-Indian').
cycPred('#$TTPred-carrier-of').
cycPred('#$TTPred-attr-Russian-Orthodox').
cycPred('#$TTPred-dead').
cycPred('#$TTPred-talkative').
cycPred('#$TTPred-writer-of').
cycPred('#$TTPred-attr-Confucianist').
cycPred('#$TTPred-create-arrange').
cycPred('#$TTPred-noble').
cycPred('#$TTPred-attend-medical-school').
cycPred('#$TTPred-unkind').
cycPred('#$TTPred-Sanforized').
cycPred('#$TTPred-fly').
cycPred('#$TTPred-second-surname-of').
cycPred('#$TTPred-eng-tense-literary-0-of').
cycPred('#$TTPred-skin-material-resin').
cycPred('#$TTPred-can-lift').
cycPred('#$TTPred-attend-doctoral-program').
cycPred('#$TTPred-ellipsoid').
cycPred('#$TTPred-introverted').
cycPred('#$TTPred-attr-Tokyoite').
cycPred('#$TTPred-ceo-of').
cycPred('#$TTPred-father-of').
cycPred('#$TTPred-polka-dot').
cycPred('#$TTPred-politically-reactionary').
cycPred('#$TTPred-event24-of').
cycPred('#$TTPred-herringbone').
cycPred('#$TTPred-orange').
cycPred('#$TTPred-attr-German').
cycPred('#$TTPred-vitamin-E').
cycPred('#$TTPred-clothing-forearm').
cycPred('#$TTPred-attr-Buddhist').
cycPred('#$TTPred-male-chauvinist').
cycPred('#$TTPred-politically-capitalist').
cycPred('#$TTPred-politically-socialist').
cycPred('#$TTPred-attr-Polish').
cycPred('#$TTPred-eng-tense-neg-2-of').
cycPred('#$TTPred-politically-moderate').
cycPred('#$TTPred-dg-adjoint-of').
cycPred('#$TTPred-attr-chafiite').
cycPred('#$TTPred-smooth').
cycPred('#$TTPred-splotches').
cycPred('#$TTPred-silent').
cycPred('#$TTPred-VRAM-of').
cycPred('#$TTPred-director-of').
cycPred('#$TTPred-attr-hanafite').
cycPred('#$TTPred-combed-cotton').
cycPred('#$TTPred-clothing-knee').
cycPred('#$TTPred-BW').
cycPred('#$TTPred-fr-tense-neg-1-of').
cycPred('#$TTPred-black-leather').
cycPred('#$TTPred-attr-Slovakian').
cycPred('#$TTPred-attr-pantheist').
cycPred('#$TTPred-nubuck').
cycPred('#$TTPred-attr-Spanish').
cycPred('#$TTPred-bass').
cycPred('#$TTPred-political').
cycPred('#$TTPred-attr-Londoner').
cycPred('#$TTPred-amplitude-modulation-of').
cycPred('#$TTPred-construction-membrane').
cycPred('#$TTPred-attr-ENS').
cycPred('#$TTPred-orange-red').
cycPred('#$TTPred-heavier-than-air').
cycPred('#$TTPred-attr-American').
cycPred('#$TTPred-narrator-of').
cycPred('#$TTPred-small-chevrons').
cycPred('#$TTPred-extroverted').
cycPred('#$TTPred-vitamin-B3').
cycPred('#$TTPred-politically-Fabian').
cycPred('#$TTPred-attr-Biarritz').
cycPred('#$TTPred-Black').
cycPred('#$TTPred-failure-emotion-of').
cycPred('#$TTPred-brother-of').
cycPred('#$TTPred-attr-Hindu').
cycPred('#$TTPred-event26-of').
cycPred('#$TTPred-politically-state-socialist').
cycPred('#$TTPred-jacketlayer').
cycPred('#$TTPred-Nagravision').
cycPred('#$TTPred-politically-Bolshevist').
cycPred('#$TTPred-attend-sixth-grade').
cycPred('#$TTPred-attr-Italian').
cycPred('#$TTPred-attr-Shiite').
cycPred('#$TTPred-attr-materialist').
cycPred('#$TTPred-rural').
cycPred('#$TTPred-atom-nickel').
cycPred('#$TTPred-politically-Marxist').
cycPred('#$TTPred-politically-progressiste').
cycPred('#$TTPred-fr-tense-pos-3-of').
cycPred('#$TTPred-light-violet').
cycPred('#$TTPred-iron').
cycPred('#$TTPred-blackcurrant-liqueur').
cycPred('#$TTPred-attend-nursery-school').
cycPred('#$TTPred-politically-worker').
cycPred('#$TTPred-trade-speculate').
cycPred('#$TTPred-attr-Taiwanese').
cycPred('#$TTPred-levels-of').
cycPred('#$TTPred-attr-Methodist').
cycPred('#$TTPred-event30-of').
cycPred('#$TTPred-gas').
cycPred('#$TTPred-attend-elementary-school').
cycPred('#$TTPred-bronze').
cycPred('#$TTPred-attr-New-Jersey').
cycPred('#$TTPred-fluent-language-of').
cycPred('#$TTPred-politically-left-wing').
cycPred('#$TTPred-fr-tense-literary-neg-7-of').
cycPred('#$TTPred-department-head-of').
cycPred('#$TTPred-attr-Northern-Irish').
cycPred('#$TTPred-role15-of').
cycPred('#$TTPred-urban').
cycPred('#$TTPred-attr-Roman-Catholic').
cycPred('#$TTPred-ptrans-swim').
cycPred('#$TTPred-politically-nationalist').
cycPred('#$TTPred-underlayerpost').
cycPred('#$TTPred-politically-syndicalist').
cycPred('#$TTPred-circles').
cycPred('#$TTPred-teenager').
cycPred('#$TTPred-politically-nihilist').
cycPred('#$TTPred-attend-law-school').
cycPred('#$TTPred-unintelligent').
cycPred('#$TTPred-attr-Czechoslovakian').
cycPred('#$TTPred-gullible').
cycPred('#$TTPred-ovals').
cycPred('#$TTPred-J17').
cycPred('#$TTPred-bouncy').
cycPred('#$TTPred-tail-length-of').
cycPred('#$TTPred-vitamin-B1').
cycPred('#$TTPred-burgundy').
cycPred('#$TTPred-denim').
cycPred('#$TTPred-attr-Anglo-Catholic').
cycPred('#$TTPred-attr-European').
cycPred('#$TTPred-attr-Welsh').
cycPred('#$TTPred-good').
cycPred('#$TTPred-eng-tense-neg-1-of').
cycPred('#$TTPred-radius-of').
cycPred('#$TTPred-weave').
cycPred('#$TTPred-attr-Zoroastrian').
cycPred('#$TTPred-product-release').
cycPred('#$TTPred-attend-fifth-grade').
cycPred('#$TTPred-lucky').
cycPred('#$TTPred-teach').
cycPred('#$TTPred-pacifist').
cycPred('#$TTPred-clumsy').
cycPred('#$TTPred-attr-Californian').
cycPred('#$TTPred-edition-of').
cycPred('#$TTPred-attend-second-grade').
cycPred('#$TTPred-attr-atheist').
cycPred('#$TTPred-do-postdoctoral-work').
cycPred('#$TTPred-fr-progressive-of').
cycPred('#$TTPred-rich').
cycPred('#$TTPred-attr-good-Samaritan').
cycPred('#$TTPred-politically-Titoist').
cycPred('#$TTPred-politically-neonazi').
cycPred('#$TTPred-attr-Japanese').
cycPred('#$TTPred-technical').
cycPred('#$TTPred-politically-Trotskyite').
cycPred('#$TTPred-attend-college').
cycPred('#$TTPred-attr-Martinique').
cycPred('#$TTPred-executive-producer-of').
cycPred('#$TTPred-fetal').
cycPred('#$TTPred-racist').
cycPred('#$TTPred-attr-English').
cycPred('#$TTPred-sky-blue').
cycPred('#$TTPred-husband-of').
cycPred('#$TTPred-vitamin-B6').
cycPred('#$TTPred-rollable').
cycPred('#$TTPred-attr-witch').
cycPred('#$TTPred-attend-preschool').
cycPred('#$TTPred-attr-animist').
cycPred('#$TTPred-not').
cycPred('#$TTPred-attend-MBA-program').
cycPred('#$TTPred-attend-fourth-grade').
cycPred('#$TTPred-attr-Ile-de-France').
cycPred('#$TTPred-politically-revolutionary').
cycPred('#$TTPred-in-color').
cycPred('#$TTPred-politically-communist').
cycPred('#$TTPred-attr-Seventh-Day-Adventist').
cycPred('#$TTPred-yellow-green').
cycPred('#$TTPred-attend-tenth-grade').
cycPred('#$TTPred-attr-malekite').
cycPred('#$TTPred-politically-Castroite').
cycPred('#$TTPred-large-stripes').
cycPred('#$TTPred-politically-Marxist-revisionist').
cycPred('#$TTPred-politically-extremist').
cycPred('#$TTPred-attr-Douarnenez').
cycPred('#$TTPred-attr-Sunnite').
cycPred('#$TTPred-tielayer').
cycPred('#$TTPred-attr-Adventist').
cycPred('#$TTPred-content-of').
cycPred('#$TTPred-Asian').
cycPred('#$TTPred-can-hold').
cycPred('#$TTPred-politically-anarchist').
cycPred('#$TTPred-fan-of').
cycPred('#$TTPred-unique-translator-of').
cycPred('#$TTPred-large-crosshatch').
cycPred('#$TTPred-attr-Thai').
cycPred('#$TTPred-attr-French').
cycPred('#$TTPred-unique-editor-of').
cycPred('#$TTPred-vitamin-C').
cycPred('#$TTPred-sing').
cycPred('#$TTPred-sentient').
cycPred('#$TTPred-role09-script-of').
cycPred('#$TTPred-alto').
cycPred('#$TTPred-attr-Parisian').
cycPred('#$TTPred-event33-of').
cycPred('#$TTPred-spaced-out').
cycPred('#$TTPred-attr-Quaker').
cycPred('#$TTPred-attend-high-school').
cycPred('#$TTPred-attr-Christian-Scientist').
cycPred('#$TTPred-attr-Sikh').
cycPred('#$genlMt').

*/

testE2C:-make,halt.

