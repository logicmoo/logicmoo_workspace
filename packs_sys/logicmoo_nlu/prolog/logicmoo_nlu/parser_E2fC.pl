f% ===================================================================
% File 'parser_e2c.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog  
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_e2c.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================


:-module(parser_e2fC,[]
        % e2c/1,
         % e2c/2,
         % do_renames_e2c/2
         % idGen/1
         ).

:- throw(module(parser_e2c)).

:- use_module(library(multimodal_dcg)).  
:- use_module(library(logicmoo_util_body_textstr)).
:- use_module(library(logicmoo_util_body_reorder)).
:- use_module(library(logicmoo_plarkc)).

:- install_constant_renamer_until_eof.
:- set_prolog_flag(do_renames_sumo,never).


% expand_file_search_path(pldata(assertions),From),[module(baseKB),derived_from(From)]

:- baseKB:kb_shared((('abbreviationString-PN'/2),
        ('adjSemTrans-Restricted'/5),
        ('countryName-LongForm'/2),
        ('countryName-ShortForm'/2),
        ('lightVerb-TransitiveSemTrans'/3),
        ('prepReln-Action'/4),
        ('prepReln-Object'/4),
        ('termPOS-Strings'/3),
        ('termStrings-GuessedFromName'/2),
        ('verbPrep-Passive'/3),
        ('verbPrep-Transitive'/3),
        ('verbPrep-TransitiveTemplate'/3),
        (abbreviationForCompoundString/4),
        (abbreviationForLexicalWord/3),
        (abbreviationForMultiWordString/4),
        (abbreviationForString/2),
        (acronymString/2),
        (adjSemTrans/4),
        (adjSemTransTemplate/3),
        (balanceBinding/2),
        (compoundSemTrans/5),
        (compoundString/4),
        (compoundVerbSemTrans/4),
        (denotation/4),
        (denotationPlaceholder/4),
        (denotationRelatedTo/4),
        (derivationalAffixBasePOS/2),
        (formerName/2),
        (genFormat/3),
        (genPhrase/4),
        (getSurfaceFromChars/3),
        (headMedialString/5),
        (hyphenString/4),
        (initialismString/2),
        (multiWordSemTrans/5),
        (multiWordString/4),
        (nicknames/2),
        (nonCompositionalVerbSemTrans/3),
        (nounPrep/3),
        (partOfSpeech/3),
        (phoneticVariantOfPrefix/3),
        (phoneticVariantOfSuffix/3),
        (posBaseForms/2),
        (posForms/2),
        (preferredGenUnit/3),
        (preferredNameString/2),
        (preferredTermStrings/2),
        (prepSemTrans/4),
        (prettyName/2),
       %(reduceWordList/2),
        (regularSuffix/3),
        (scientificName/2),
        (stringToWords/2),
        (termStrings/2),
        (unitOfMeasurePrefixString/2),
        (verbSemTrans/4),
       % cyckb_t_e2c/3,
       % cyckb_t_e2c/4,
        (verbSemTransTemplate/3))).

/*
:- require((
 stringArg/2,
 stringArgUC/3,
 balanceBinding/2,
 reduceWordList/2,
 strings_match/0,
 dictionary/3,
 cycStringToString/2,
 dcgStartsWith1/3,
 call_tabled/1,
 theText//1,
 reorderBody/2,
 reorderBody/3,
 reorderBody/4,
 t_l:noreorder/0,
 idioms/3,
 do_body_reorder/4,
 dstringify/2,
 module/2)).
*/

:- baseKB:kb_shared(((
 abbreviationForString/2,
 abbreviationStringPn/2,
 acronymString/2,
 adjSemTrans/4,
 adjSemTransRestricted/5,
 adjSemTransTemplate/3,
 argIsa/3,
 compoundSemTrans/5,
 compoundString/4,
 compoundVerbSemTrans/4,
 countryNameLongform/2,
 countryNameShortform/2,
 denotation/4,
 denotationPlaceholder/4,
 denotationRelatedTo/4,
 derivationalAffixBasePOS/2,
 genFormat/3,
 genlPreds/2,
 genPhrase/4,
 getSurfaceFromChars/3,
 headMedialString/5,
 hyphenString/4,
 initialismString/2,
 lightVerbTransitivesemtrans/3,
 multiWordSemTrans/5,
 multiWordString/4,
 nameString/2,
 nicknames/2,
 nonCompositionalVerbSemTrans/3,
 nounPrep/3,
 phoneticVariantOfPrefix/3,
 phoneticVariantOfSuffix/3,
 posBaseForms/2,
 posForms/2,
 preferredGenUnit/3,
 preferredNameString/2,
 preferredTermStrings/2,
 prepRelnAction/4,
 prepRelnObject/4,
 prepSemTrans/4,
 prettyName/2,
 regularSuffix/3,
 scientificName/2,
 stringToWords/2,
 termPOSStrings/3,
 termStrings/2,
 termStringsGuessedfromname/2,
 unitOfMeasurePrefixString/2,
 verbPrepPassive/3,
 verbPrepTransitive/3,
 verbPrepTransitivetemplate/3,
 verbSemTrans/4,
 verbSemTransTemplate/3, 
 wnS/6))).

:- thread_local(t_l:noreorder/0).

disable_current_module_expansion(M):-
  system:forall((member(F/A,[term_expansion/2, term_expansion/4,goal_expansion/2, goal_expansion/4]),
            functor(P,F,A),
            predicate_property(M:P,clause_count(N)),
            N>0,
         \+ predicate_property(M:P,imported_from(_)),
         \+ predicate_property(M:P,static)),
     (( writeq(M:F/A),nl,M:multifile(M:F/A),
        M:dynamic(M:F/A),
        M:call(asserta,((P :- (!,fail))),Ref),
        call(asserta,on_end(erase(Ref)))))).

:- if(current_prolog_flag(logicmoo_simplify_te,true)).
:- disable_current_module_expansion(baseKB).
:- forall(current_module(M),disable_current_module_expansion(M)).
:- endif.


:- (current_prolog_flag(qcompile,PrevValue)->true;PrevValue=false),
   call(assert,on_fin(set_prolog_flag(qcompile,PrevValue))),
   set_prolog_flag(qcompile,large).


%:- set_prolog_flag(logicmoo_virtualize,true).


/*
:- (rtrace,kb_shared(prefixString/2)).
:-kb_shared('suffixString'/2).
:-kb_shared('variantOfSuffix'/2).
*/

/*
:-dynamic('prefixString'/2).
:-dynamic('suffixString'/2).
:-dynamic('variantOfSuffix'/2).
*/

% ==============================================================================
%:- kb_shared kbp_t_list_prehook/2.

% do_renames_e2c(_,_):- !,fail.
do_renames_e2c(I,O):- is_ftVar(I),!,I=O.
do_renames_e2c(I,O):- atomic(I),do_renames(I,O),!.
do_renames_e2c(A,B):- compound_name_arguments(A,P,ARGS),maplist(do_renames_e2c,[P|ARGS],[T|L]),compound_name_arguments(B,T,L).
 
%:- rtrace.
cyckb_t_e2c(P,A,B):- !,call_u(t(P,A,B)).
%:- break.
cyckb_t_e2c(P,A,B):- maplist(do_renames_e2c,[P,A,B],[P1,A1,B1]),!,call_u(t(P1,A1,B1)).
cyckb_t_e2c(P,A,B,C):- !,call_u(t(P,A,B,C)).
cyckb_t_e2c(P,A,B,C):-  maplist(do_renames_e2c,[P,A,B,C],[P1,A1,B1,C1]),!,call_u(t(P1,A1,B1,C1)).

% :- assert_until_eof((term_expansion(I,O):- ( I\=(:- _),do_renames_e2c(I,O)->I\=@=O))).

% :- kb_shared thglobal:use_cyc_database/0.

% :- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_all.pl')).

:- meta_predicate do_dcg(?,?,?,?,?).
:- meta_predicate isPOS(?,?,?,?,?).

% :- register_module_type(utility).

:-thread_local t_l:allowTT/0.
:-thread_local t_l:omitCycWordForms/0.

/*

 (parse-a-question-completely "Did George W. Bush fall off a bicycle?" #$RKFParsingMt 
 '(:wff-check? t)
 )

*/

% idGen(X):-flag(idGen,X,X+1).

%:- ensure_loaded(logicmoo(mpred/mpred_loader)).

% :- file_begin(pl).

% :- retractall(prevent_transform_moo_preds).

% Semantic Interpretation
/* from Bratko chapter 17 page 455.
   This comes from Pereira and Warren paper, AI journal, 1980 */


% ===================================================================

to_simple_wl(L,[L]):-var(L),!.
to_simple_wl([L|T],[L|T]):-!.
to_simple_wl(L,[L]):-!.

:- export(lmfs:e2c_in_file/4).

lmfs:e2c_in_file(Head,Vars,In,Out):- 
     do_body_textstr(Head,Vars,In,Mid),
     do_body_reorder(Head,Vars,Mid,Out).     

enable_e2c :- enable_in_file(e2c_in_file).
disable_e2 :- disable_in_file(e2c_in_file).
%:- enable_body_reorder.
:- use_module(library(logicmoo_util_body_textstr)).
:- enable_body_textstr.
% :- enable_e2c.


notground(V):-notrace(not(ground(V))).
term_atoms(Term,Vs):-findall(A,(arg(_,Term,A),non_blankWord(A),A\=@=[],atom(A)),Vs).

words_append(L,R,LRS):-stringToWords(L,LS),stringToWords(R,RS),stringToWords(LRS,LRLIST),append(LS,RS,LRLIST).
words_concat(PreAffix,Affix,String):- reduceWordList(PreAffix,A),reduceWordList(Affix,B),reduceWordList(String,C),!,
   term_atoms(atom_concat(A,B,C),Vs),length(Vs,L),!,L > 1,atom_concat_er(A,B,C), !,non_blankWord(A),!,non_blankWord(B),non_blankWord(C).

atom_concat_er(A,B,C):-atom(A),atom(B),atom_concat(Other,B,A),C=A,non_blankWord(Other),!.
atom_concat_er(A,B,C):-atom(A),atom(B),atom_concat(A,Other,B),C=B,non_blankWord(Other),!.
atom_concat_er(A,B,C):-atom(A),atom(B),atom_concat(A,B,C),!.


get_pl_type(Term,var):-var(Term),!.
get_pl_type([],list(nil)):-!.
get_pl_type(Term,atom):-atom(Term),!.
get_pl_type(Term,string):-string(Term),!.
get_pl_type(Term,number(int)):-integer(Term),!.
get_pl_type(Term,number(float)):-float(Term),!.
get_pl_type(Term,number(other)):-number(Term),!.
get_pl_type(List,list(proper)):-is_list(List),!.
get_pl_type([_|_],list(improper)):-!.
get_pl_type(C,compound(F,A)):-functor(C,F,A).

:-dynamic(textCached/2).


:- include(pldata(posm_cached_data)).

% :- break.

/*
'nonCompositionalVerbSemTrans'('End-TheWord', 'Agreement', ['and', ['isa', ':ACTION', 'EndingAnAgreement'], ['performedBy', ':ACTION', ':SUBJECT'], ['objectActedOn', ':ACTION', ':OBJECT']]).

'lightVerb-TransitiveSemTrans'('Do-TheWord', 'CommercialActivity', ['and', ['isa', ':ACTION', 'CommercialActivity'], ['performedBy', ':ACTION', ':SUBJECT']]).
'multiWordStringDenotesArgInReln'([service], 'Provide-TheWord', 'AgentiveNoun', 'providerOfService', 2).
'nounSemTrans'('Hire-TheWord', 0, 'RegularNounFrame', ['and', ['isa', '?HIRE', 'EmployeeHiring'], ['objectActedOn', '?HIRE', ':NOUN']]).
'multiWordStringDenotesArgInReln'([service], 'Provide-TheWord', 'AgentiveNoun', 'providerOfService', 2).
'headMedialString'([intended], 'Recipient-TheWord', [of, communication], 'SimpleNoun', 'communicationTarget').
'agentiveNounSemTrans'('Emit-TheWord', 0, 'RegularNounFrame', ['emitter', '?X', ':NOUN']).
'adjSemTrans'('Cloud-TheWord', 0, 'RegularAdjFrame', ['weather', ':NOUN', 'Cloudy']).
'relationIndicators'('abbreviationForMultiWordString', 'Form-TheWord', 'Verb').
%'genNatTerm-compoundString'('TransportViaFn', 'Transport-TheWord', [via], 'MassNoun', 'singular').
%'genTemplate'('transferredThing', ['ConcatenatePhrasesFn', ['TermParaphraseFn-NP', ':ARG2'], ['BestHeadVerbForInitialSubjectFn', 'Be-TheWord'], ['BestNLPhraseOfStringFn', 'transferred in'], ['TermParaphraseFn-NP', ':ARG1']]).
%'lightVerb-TransitiveSemTrans'('Do-TheWord', 'CommercialActivity', ['and', ['isa', ':ACTION', 'CommercialActivity'], ['performedBy', ':ACTION', ':SUBJECT']]).
%'genTemplate-Constrained'('isa', ['quotedCollection', ':ARG2'], ['NPIsNP-NLSentenceFn', ['BestCycLPhraseFn', ':ARG1'], ['BestDetNbarFn-Indefinite', ['TermParaphraseFn', ':ARG2']]]).

*/
:-dynamic(e2c_result/1).
:-export(e2c_result/1).
:-export((dm1/0,dm2/0,dm3/0)).

dm1:-
  % mmake,
   e2c("I am happy when I am seeing two books sitting on a shelf",F),portray_clause(F),
   forall(clause(e2c_result(Text),O),retractall((e2c_result(Text):-O))),
   forall(descriptionTest(_,Texts),forall(member(Text,Texts),doE2CTest(Text))),
   findall(((was(LEN,TEXT):-PROPS)),(is_wordage_cache(TEXT, wordage(_,PROPS)),length(PROPS,LEN)),LIST),sort(LIST,SET),reverse(SET,RSET),
   forall(member(R,RSET),portray_clause(R)).

doE2CTest(Text):- atomic(Text),atom_contains(Text,'  '),!.
doE2CTest(Text):- clause(e2c_result(Text),_),!.
doE2CTest(Text):- e2c(Text,F),call(asserta,(e2c_result(Text):- F)),portray_clause(F),!.

% dm1:-e2c("I"),e2c("I am happy"),e2c("I saw wood"),e2c("I see two books"), e2c("I see two books sitting on a shelf").
dm2:-e2c("AnyTemplate1 affects the NPTemplate2").
dm3:-e2c("AnyTemplate1 at AnyTemplate2").

mostSpec(TTT,'NLWordForm',TTT).
mostSpec(_TTT,TT,TT).

nodeTrans(v,'Verb').
nodeTrans(a,'Adjective').
nodeTrans(n,'Noun').
nodeTrans(p,'Pronoun').
nodeTrans(pn,'ProperNoun').
nodeTrans('-','NLWordForm').
nodeTrans(np,'NounPhrase').
nodeTrans(v,'Verb').
nodeTrans(vp,'VerbPhrase').
nodeTrans(state,'TemporallyExistingThing').
nodeTrans(property,'Role').
nodeTrans(event,'Situation').
nodeTrans(action,'Action').
nodeTrans(thing,'SpatialThing').
nodeTrans(person,'Person').
nodeTrans(location,'SubcollectionOfWithRelationFromTypeFn'('EnduringThing-Localized',toLocation,'Translocation')).
nodeTrans(destination,'Location-Underspecified').


nodeTrans(ss,'TheSentenceSubject').
nodeTrans(pp,'PrepositionalPhrase').

nodeTrans(s,'NLSentence').
nodeTrans(whnp,'WHPronoun').
%:- rtrace.
nodeTrans(P,S):-dstringify(P,S),!.
nodeTrans(P,string([P])).



%%	phrase(:RuleSet, ?List).
%%	phrase(:RuleSet, ?List, ?Rest).
%
%	Interface to DCGs
/*
:- meta_predicate
	phrase_orig(//, ?),
	phrase_orig(//, ?, ?).
:- noprofile((phrase_orig/2,
	      phrase_orig/3)).

phrase_orig(RuleSet, Input) :-
	phrase(RuleSet, Input, []).
phrase_orig(RuleSet, Input, Rest) :-
	phrase_input(Input),
	phrase_input(Rest),
	(   strip_module(RuleSet, M, Plain),
	    nonvar(Plain),
	    dcg_special(Plain)
	->  dcg_body(Plain, _, q(M,M,_), S0, S, Body, _),
	    Input = S0, Rest = S,
	    call(M:Body)
	;   call(RuleSet, Input, Rest)
	).
*/

% ===================================================================
% posm_cached(CycWord, String,POS,Form,CycL)
:-dynamic lex/3,  lexMap/3.
%:-at_initialization(convertCycKb).
:-dynamic(posm_cached).
:-dynamic(posm_cached/5).
:-dynamic(real_posm_cached/5).
:-dynamic(real_posm_cachedTT/5).

dont_cache.
posm_cached.

posm_cached(CycWord,String,POS,Form,CycL):-posm_c_gen(String,POS,Form,CycL),fillInPosAndForm(String,CycWord,Form,POS).

% ===================================================================

% :- enable_body_textstr.
% :- enable_body_reorder.

% ============================================================================
% posMeans
% ============================================================================

posMeans(String,POS,Form,CycL):- dont_cache,!, posm_c_gen( String,POS,Form,CycL).
posMeans(String,POS,Form,CycL):- posm_cached,!, posm_cached(_CycWord,String,POS,Form,CycL).


posMeans(String,POS,Form,CycL):-
      cache_the_posms,
      call(asserta,posm_cached),
      posMeans(String,POS,Form,CycL).


:-export(cache_the_posms/0).
cache_the_posms:-!.
cache_the_posms:- t_l:noreorder,
 locally(thglobal:use_cyc_database,
     (retractall(posm_cached(_CW, _Phrase,POS, _Form, _CycL)),
      posm_c_gen( String,POS,Form,CycL),
      push_to_cache(posm_c_gen( String,POS,Form,CycL)), 
      fail)).
   
cache_the_posms:-!.


push_to_cache(posm_c_gen(String,POS,Form,CycL)):- CycWord = _ ,
   once((correctArgIsas(posm_cached(CycWord,String,POS,Form,CycL),vv('CycWord','CharacterString','POS','Form','CycL'),Out),
      (clause(Out,true);(dmsg(Out),assert_if_new(Out))))).  

correctArgIsas(posm_cached(CycWord,String,POS,Form,CycL), _ ,posm_cached(CycWord,String,POS,Form,CycL)):- stringToWords(String,String),!.
correctArgIsas(X, _ ,X).
% ============================================================================
% General Parts Of Speech and Meanings
% ============================================================================

:-disable_body_textstr.
% :- enable_body_textstr.

multiStringMatch([W|List],[W|_]):-is_list(List),!.
multiStringMatch(W,[W|_]):-atom(W),!.
multiStringMatch([W|_],[W|_]):-atom(W),!.
multiStringMatch([W|_],[W|_]).
multiStringMatch(W,[W|_]).


% ============================================================================
% POSM_GEN_CACHE MULTI
% ============================================================================
posm_c_gen_multi(String,POS,Form,CycL):-no_repeats(posm_c_gen_multi_0(String,POS,Form,CycL)).


%'multiWordString'([health, care], 'Organize-TheWord', 'SimpleNoun', 'MedicalCareOrganization').
posm_c_gen_multi_0(String,POS,Form,CycL):- 
   reorderBody('multiWordString'(Words, CycWord,POS, CycL), fillInPosAndForm(Eng,CycWord,Form,POS), String^words_append(Words,Eng,String)).

%'genPhrase'('MedicalCareProvider', 'AgentiveNoun', 'agentive-Sg', [health, care, provider]).
posm_c_gen_multi_0( String,POS,Form,CycL):- 'genPhrase'(CycL,POS,Form, String).

%'compoundString'('Movement-TheWord', [of, fluid], 'MassNoun', 'FluidFlowEvent').
posm_c_gen_multi_0( String,POS,Form,CycL):- reorderBody('compoundString'(CycWord,Words,POS, CycL),Eng^fillInPosAndForm(Eng,CycWord,Form,POS),String^words_append(Eng,Words,String)).

%'prepCollocation'('Beset-TheWord', 'Adjective', 'By-TheWord').      
posm_c_gen_multi_0( String,POS,Form2,'PrepCollocationFn'(CycWord1,POS,CycWord2)):- 
      reorderBody(POS^'prepCollocation'(CycWord1,POS, CycWord2),
	 stringToCycWord(Eng1,CycWord1),Form2^meetsForm(Eng2,CycWord2,Form2), String^words_append(Eng1,Eng2,String)).

%'headMedialString'([dimensionless], 'Unit-TheWord', [of, measure], 'SimpleNoun', 'DimensionlessUnitOfMeasure').
posm_c_gen_multi_0( String,POS,Form,CycL):- reorderBody('headMedialString'(WordsBef,CycWord,WordsAft,POS, CycL),
	 fillInPosAndForm(Eng,CycWord,Form,POS),PhrasingLeft^words_append(WordsBef,Eng,PhrasingLeft),String^words_append(PhrasingLeft,WordsAft,String)).


% ============================================================================
% POSM_GEN_CACHE PLUS MULTI
% ============================================================================

%TODO 'abbreviationForString'([scatology], [scat]).  
%TODO 'abbreviationForMultiWordString'([political], 'Science-TheWord', 'massNumber', [poli, sci]).
%TODO 'abbreviationForLexicalWord'('Kilogram-TheWord', 'singular', [kg]).


%'initialismString'('CodeOfConduct', [coc]).
posm_c_gen( StrVar,'SimpleNoun',normal,Proper) :- 
  stringArgUC(StrVar,CycStr,
      (('initialismString'(Proper,CycStr);
      'formerName'(Proper, CycStr);
      'scientificName'(Proper, CycStr);
      'termStrings-GuessedFromName'(Proper, CycStr);
      'nameString'(Proper, CycStr)))).

%'abbreviationString-PN'('India', ['IND']).
posm_c_gen( StrVar,'ProperNoun',normal,Proper) :- 
  stringArgUC(StrVar,CycStr,
      (('initialismString'(Proper,CycStr);
      'abbreviationString-PN'(Proper, CycStr);
      'preferredNameString'(Proper, CycStr);
      'countryName-LongForm'(Proper, CycStr);
      'countryName-ShortForm'(Proper, CycStr)))).

posm_c_gen(String,POS,Form,CycL):- nonvar(String), String=[_|_],!,posm_c_gen_multi(String,POS,Form,CycL).

posm_c_gen(Eng,POS,Form,CycL):-posm_c_gen_unify(_CycWord,Eng,POS,Form,CycL).

posm_c_gen_unify(CycWord,Eng,POS,Form,CycL):- cwposm_build(CycWord,POS,CycL),
   ignore((var(Form)->meetsForm(Eng,CycWord,Form);true)),
   once(meetsPos(Eng,CycWord,POS);((var(Eng)->stringToCycWord_0(Eng,CycWord);fail))).
   
% posm_c_gen( Eng,POS,Form,CycL):-posTT(CycWord,Eng,POS,Form),tthold('denotation',CycWord,POS, _Num, CycL).

% ============================================================================
% POSM_GEN_CACHE CYCWORDS
% ============================================================================

%'denotation'('Capacity-TheWord', 'SimpleNoun', 0, 'Volume').
cwposm_build(CycWord, POS,CycL):-'denotation'(CycWord,POS, _Num, CycL).

%'preferredGenUnit'('on-Physical', 'Preposition-Directional-Telic', 'On-TheWord').
cwposm_build(CycWord, POS,CycL):-'preferredGenUnit'(CycL,POS, CycWord).

%'denotationRelatedTo'('Can-TheWord', 'Verb', 0, 'PreservingFood').
%cwposm_build(CycWord, POS,'DenotationRelatedToFn'(CycL)):-'denotationRelatedTo'(CycWord,POS, _ , CycL).
cwposm_build(CycWord, POS,CycL):-'denotationRelatedTo'(CycWord,POS, _ , CycL).

cwposm_build(CycWord,_MISSING_POS, meaningOfWord(CycWord)):-
   not('denotation'(CycWord, _ , _ , CycL)),
  not('denotationRelatedTo'(CycWord, _ , _ , CycL)), not('preferredGenUnit'(CycL, _ , CycWord)).
   
%'relationIndicators'('catalyst', 'Catalyst-TheWord', 'Verb').

%'lex'(Form, CycWord, String):- ttholds('TT-lex',Form, CycWord, String).
%'lexMap'(PosForms, CycWord,POS):- ttholds('TT-lexMap',PosForms, CycWord,POS).



% 'prepCollocation'('Beset-TheWord', 'Adjective', 'By-TheWord').
posTT(_CycWord, _Phrase, _POS, _Form:_PosForms):- fail.
     % 'TT-lex'(Form, CycWord, String),not('lex'(_ , _ , String)),
     %  'TT-lexMap'(PosForms, CycWord,POS).

%'termStrings-GuessedFromName'('GenlsFormat', 'Genls Format').
%   'nounPrep'('Offspring-TheWord', 'Of-TheWord', ['children', ':NOUN', ':OBLIQUE-OBJECT']),



%:-disable_body_textstr.
% % :- enable_body_textstr.

:- listing(posm_c_gen/4).

% ==========================================================
% String FORM and POS DCG
% ==========================================================
isForm(Form) --> isForm(Form,_).
isForm(Form,CycWord) --> isForm(Form,CycWord,_).
isForm(Form,CycWord,[String]) --> theText([String]),{notrace(meetsForm(String,CycWord,Form)),!}.
isForm(Form,CycWord,[S,W|String]) --> theText([S,W|String]),{notrace(meetsForm([S,W|String],CycWord, Form))},theText(String).


isPOS(POS) --> isPOS(POS,_, _).
isPOS(POS,CycWord) --> isPOS(POS,CycWord, _).
isPOS(POS,CycWord,[String]) --> theText([String]),{notrace(meetsPos([String],CycWord,POS))}.
isPOS(POS,CycWord,[S,W|String]) --> theText([S,W]),{notrace(meetsPos([S,W|String],CycWord,POS))},theText(String).


theGenPhraseTemplate(Template,CycL)--> {!, fail, dmsg(warn(phraseToTemplate(Template,CycL))),!,fail }.


pos_cycl(POS,CycL) --> {'genPhrase'(CycL,POS,_Form,Template)},theGenPhraseTemplate(Template,CycL).

pos_cycl(POS,CycL) --> {'termPOS-Strings'(CycL,POS,String)},theText(String).

pos_cycl(POS,CycL) --> isCycWord(CycWord), {'compoundString'(CycWord,String,POS,CycL)}, theText(String).

pos_cycl(POS,CycL) --> theText([S]),{'multiWordString'([S|String], CycWord,POS,CycL)}, theText(String), isCycWord(CycWord).

pos_cycl(POS,CycL) --> theText([S]),{'headMedialString'([S|String], CycWord,POS,Right,CycL)}, theText(String),isCycWord(CycWord), theText(Right).

pos_cycl(POS,CycL) -->  theText([String]), {concat_atom([Left,Right],'-',String), 'hyphenString'([Left], RightWord,POS,CycL), phrase(isCycWord(RightWord),[Right])}.


% ==========================================================
% PhraseType / POS
% ==========================================================

goodStart(verb_phrase,'Verb').
goodStart(verb_phrase,'BeAux').

goodStart(noun_phrase,'Adjective').
goodStart(noun_phrase,'Determiner').
goodStart(noun_phrase,'Noun').
goodStart(noun_phrase,'Pronoun').
goodStart(noun_phrase,'SubjectPronoun').

canStart(PhraseType,POS):-goodStart(PhraseType,POS),!.

cantStart(PhraseType,POS):-canStart(PhraseType,POS),!,fail.
cantStart(PhraseType,POS):-goodStart(OtherPhraseType,POS),OtherPhraseType\=@=PhraseType,!.
cantStart(PhraseType,POS):-dmsg(cantStart(PhraseType,POS)),!.

% ==========================================================
% String / Word
% ==========================================================

% TODO  cycWordForISA
cycWordForISA(_CycWord,_EventIsa):-fail.  

genlPreds_different(Child,Form):-genlPreds(Child,Form), Child\=@=Form.

% peace atal beh - 695-1297
%isCycWord(CycWord) --> {var(CycWord),!,trace}.
isCycWord(CycWord) --> {notrace(stringToCycWord(String,CycWord))},literal(String).


% ==========================================================
% meetsPos
% ==========================================================
meetsPos(String,CycWord,POS):-  sanify_string(String,Sane),no_repeats(meetsPos_0(Sane,CycWord,POS)).

meetsPos_0(String,CycWord,POS):- stack_check,one_must(meetsPos_1(String,CycWord,POS),
     one_must(meetsPos_2(String,CycWord,POS),
     one_must(meetsPos_3(String,CycWord,POS),
     one_must(meetsPos_4(String,CycWord,POS),
     meetsPos_5(String,CycWord,POS))))).
meetsPos_0(_String,_CycWord,POS):- var(POS),!,fail.
meetsPos_0(String,CycWord,FormNotPOS):- is_speechPartPred_any(FormNotPOS),!,meetsForm(String,CycWord,FormNotPOS).
meetsPos_0(String,CycWord,POS):-'genls'(Child,POS),Child\=@=POS, meetsPos_0(String,CycWord,Child).

meetsPos_1(String,CycWord,POS):- has_wordage(String),!,is_wordage_prop(String, pos(_, CycWord, POS)).

meetsPos_2(String,CycWord,POS):- stringArgUC(String,CycString,'partOfSpeech'(CycWord,POS,CycString)).
meetsPos_2(String,CycWord,POS):- reorderBody(meetsForm(String,CycWord,Form),POS^speechPartPreds_transitive(POS, Form)).

meetsPos_3([String],CycWord,POS):- atom(String),stringAtomToPOS([String],CycWord,POS).

meetsPos_4(String,CycWord,POS):- locally(t_l:allowTT,meetsPos_2(String,CycWord,POS)).

meetsPos_5(String,CycWord,POS):-  member(POS,['Noun','Adjective','Verb','Adverb']), stringArg(String,'wnS'(CycWord, _ , String,POS, _ , _)).
meetsPos_5(String,CycWord,'Adjective'):- 'wnS'(CycWord, _ , String, 'AdjectiveSatellite', _ , _). 
 

stringAtomToPOS(String,CycWord,'Verb'):- meetsPosVerb(String,CycWord).

stringAtomToPOS(String,nartR('WordWithPrefixFn', CycAffix, CycWord),POS):- 
     'derivationalAffixBasePOS'(CycAffix,POS),
     'prefixString'(CycAffix, Affix),
      words_concat(Affix, BaseString ,String),
      stringToCycWord([BaseString],CycWord).

stringAtomToPOS(String,nartR('WordWithSuffixFn', CycWord, CycAffix),POS):-
     reorderBody(
     'denotation'(nartR('WordWithSuffixFn', CycWord, CycAffix), POS,_,_),
      String^('suffixString'(CycAffix,Affix),words_concat(BaseString,Affix,String),stringToCycWord([BaseString],CycWord))).
     
'suffixString'(CycAffix,Affix):-'variantOfSuffix'(CycAffix,Affix).
'variantOfSuffix'(CycAffix,Affix):- 'phoneticVariantOfSuffix'(CycAffix,Affix,_BaseType).

'prefixString'(CycAffix,Affix):-'variantOfPrefix'(CycAffix,Affix).
'variantOfPrefix'(CycAffix,Affix):- 'phoneticVariantOfPrefix'(CycAffix,Affix,_BaseType).

%meetsPos(String,CycWord,'Noun'):-atom(String),meetsPosNoun(String,CycWord).

% TODO require infinitive
meetsPosVerb(String,CycWord):- stringArgUC(String,CycString,meetsPosVerb_0(CycString,CycWord)).
meetsPosVerb_0(String,CycWord):-reorderBody(String^atom_concat(S,'d',String),atom_concat(_,'e',S),CycWord^meetsPos([S],CycWord,'Verb')).
meetsPosVerb_0(String,CycWord):-reorderBody(String^atom_concat(S,'ed',String),CycWord^meetsPos([S],CycWord,'Verb')). 
meetsPosVerb_0(String,CycWord):-reorderBody(String^atom_concat(S,'ded',String),CycWord^meetsPos([S],CycWord,'Verb')). % like embedded
meetsPosVerb_0(String,CycWord):-reorderBody(String^atom_concat(S,'s',String),CycWord^meetsPos([S],CycWord,'Verb')). % TODO add or require intransitivity and infinitive
% ==========================================================
% **61*+18056377249*11*30#

% Wordnet
wordToWNPOS(CycWord,WNWord,POS):- 'denotationPlaceholder'(CycWord,POS, _ , WNWord).
%'synonymousExternalConcept'('AbandoningSomething', 'WordNet-1995Version', 'V01269572', 'WordNetMappingMt', v(v('AbandoningSomething', 'WordNet-1995Version', 'V01269572'), A)).	 

% ==========================================================
% String to WordsList - Form /POS
% ==========================================================
stringToWordsListForm(String,[CycWord|Words],FormOrPOS):- 
       stringArgUC(String,CycString,'abbreviationForCompoundString'(CycWord,WordList,FormOrPOS,CycString)),
	 stringToWords(WordList,Words).

stringToWordsListPOS(String,CycWords,POS):- 
      stringArgUC(String,CycString,'abbreviationForMultiWordString'(List,CycWord,POS,CycString)),
      stringToWordsListForm(List,Words, _),
      append(Words,[CycWord],CycWords).


% ==========================================================
% String to CycWord 
% ==========================================================
cycWordToPossibleStrings(CycWord,StringAtom):- call_tabled(cycWordToPossibleStrings_0(CycWord,StringAtom)).
cycWordToPossibleStrings_0(_,[StringAtom]):- nonvar(StringAtom),StringAtom='',!,fail. 
cycWordToPossibleStrings_0(CycWord,String):- cyckb_t_e2c(Form,CycWord,CycString),is_speechPartPred_any(Form),cycStringToString(CycString,String).
cycWordToPossibleStrings_0(CycWord,StringAtom):- atom(CycWord),stringToCycWord_0(StringAtom2,CycWord),[StringAtom] = StringAtom2.

stringToCycWord_never([String],_):-String=='',!,fail.
stringToCycWord_never([String],CycWord):-ground(stringToCycWord_never(String,CycWord)),
   once(cycWordToPossibleStrings(CycWord,_)),!,
   not(((cycWordToPossibleStrings(CycWord,StringSample),equals_icase(StringSample,String)))),!.

is_stringWord(String):-stringToCycWord(String,_CycWord).

stringToCycWord([EMPTY],_CycWord):- is_blankWord(EMPTY),!,fail.
stringToCycWord(String,CycWord):-
 not(stringToCycWord_never(String,CycWord)),
  locally(t_l:allowTT,
   one_must(stringToCycWord_0(String,CycWord),
      one_must(stringToCycWord_1(String,CycWord),
         stringToCycWord_2(String,CycWord)))),notPrefixOrSuffix(CycWord).


stringToCycWord_0(String,CycWord):-  meetsForm_1(String,CycWord,_).
stringToCycWord_0(String,CycWord):-  meetsPos_2(String,CycWord,_).

stringToCycWord_1([String],CycWord):- atom(String),not(compound(CycWord)),once(toPropercase(String,UString)),atom_concat(UString,'-TheWord',CycWord),cyckb_t_e2c(_,CycWord,_),!.

stringToCycWord_2(String,CycWord):- var(String),nonvar(CycWord),cycWordToPossibleStrings(CycWord,String).

cycWordPosForm_Likely(POS,CycWord,Form):-        
	 (('preferredGenUnit'(_CycL,POS, CycWord);
	 'posBaseForms'(CycWord,POS);
	 'posForms'(CycWord,POS);
	 'denotation'(CycWord,POS, _Arg, _CycL))),      
	 speechPartPreds_transitive(POS, Form).

sanify_string(String,Sane):- if_defined(isT(nameString(isa,Y)),fail),atom(Y), \+ atom(String),!,trace_or_throw(sanify_string(String,Sane)).
sanify_string(String,String):- \+ compound(String).
sanify_string(String,String):- sanify_string_list(String).

% sanify_string([_|Tring]):-length(Tring,RLen),!,between(0,1,RLen).
sanify_string_list([_]).
sanify_string_list([_,_]).
sanify_string_list([_,_,_]).

%pos(String,CycWord,Form,POS):- 'lex'(Form, CycWord, String),'lexMap'(_PosForms, CycWord,POS).
fillInPosAndForm(String,CycWord,Form,POS):- ignore(pos_0(String,CycWord,Form,POS)).


% pos_0(String,CycWord,Form,POS):-  pos_4(String,CycWord,Form,POS).

pos_0(String,CycWord,Form,POS):- (nonvar(String);nonvar(CycWord)),!,meetsForm_1(Form,CycWord,String),speechPartPreds_transitive(POS, Form).
pos_0(String,CycWord,Form,POS):- speechPartPreds_transitive(POS, Form), meetsForm_1(Form,CycWord,String).

% ==========================================================
% speechPartPreds HACKS
% ==========================================================
is_speechPartPred_tt_ever(Form):- atom(Form),atom_concat(infl,_,Form),
  call_tabled(findall_nodupes(F,((el_holds(isa,F,'Predicate','iThoughtTreasureMt',
    [amt('iThoughtTreasureMt')|_]),atom(F),atom_concat(infl,_,F))),Forms)),!,member(Form,Forms).

:-export(is_speechPartPred_tt/1).
is_speechPartPred_tt(Form):- t_l:allowTT,!,is_speechPartPred_tt_ever(Form).

speechPartPreds_transitive(POS,Form):-speechPartPreds_asserted(POS,Form).
speechPartPreds_asserted(POS, Form):- is_speechPartPred_tt(Form),posName(POS),atom_contains(Form,POS).
speechPartPreds_asserted(POS, Form):- cyckb_t_e2c('basicSpeechPartPreds',POS, Form).
speechPartPreds_asserted(POS, Form):- cyckb_t_e2c('mostSpecificSpeechPartPreds',POS, Form).
speechPartPreds_asserted(POS, Form):- cyckb_t_e2c('speechPartPreds',POS, Form).
%speechPartPreds_asserted(SPOS, SForm):- speechPartPreds_transitive0(SPOS, SForm),must_det((nonvar(SForm),nonvar(SPOS))).
%speechPartPreds_asserted(SPOS, SForm):- reorderBody('speechPartPreds'(POS, Form),SPOS^pred_or_same('genls',POS,SPOS),SForm^pred_or_same('genlPreds',Form,SForm)).

pred_or_same(_,POS,POS).
pred_or_same(Pred,POS,SPOS):- (nonvar(SPOS);nonvar(POS)),!,cyckb_t_e2c(Pred,POS,SPOS).
% pred_or_same(Pred,POS,SPOS):-cyckb_t_e2c(Pred,POS,MPOS),cyckb_t_e2c(Pred,MPOS,SPOS).

:- expire_tabled_list(all).

is_speechPartPred(Form):-is_speechPartPred_nontt(Form).
is_speechPartPred(Form):-is_speechPartPred_tt(Form).

is_speechPartPred_any(Form):-is_speechPartPred_nontt_ever(Form).
is_speechPartPred_any(Form):-is_speechPartPred_tt_ever(Form).

is_speechPartPred_nontt(Form):- not(t_l:omitCycWordForms),!,is_speechPartPred_nontt_ever(Form).
is_speechPartPred_nontt_ever(Form):- call_tabled(no_repeats(Form,(is_speechPartPred_0(Form),not(is_speechPartPred_tt_ever(Form))))).

is_speechPartPred_0(Form):-is_speechPartPred_1(Form).
is_speechPartPred_0(Form):-is_speechPartPred_1(FormP),cyckb_t_e2c(genlPreds,Form,FormP).

is_speechPartPred_1('baseForm').
is_speechPartPred_1(Form):-speechPartPreds_asserted(_,Form).
is_speechPartPred_1(Form):-cyckb_t_e2c(isa,Form,'SpeechPartPredicate').
is_speechPartPred_1(Form):-cyckb_t_e2c(genlPreds,Form,'wordStrings').
is_speechPartPred_1(Form):-argIsa(Form,1, 'LexicalWord'),cyckb_t_e2c(arity,Form,2).
is_speechPartPred_1(Form):-argIsa(Form,1, 'EnglishWord'),cyckb_t_e2c(arity,Form,2).

:- listing(is_speechPartPred_1/1).
/*
is_speechPartPred_1(baseForm).
is_speechPartPred_1(A) :-
        speechPartPreds_asserted(_, A).
is_speechPartPred_1(A) :-
        cyckb_t_e2c(isa, A, rtSpeechPartPredicate).
is_speechPartPred_1(A) :-
        cyckb_t_e2c(genlPreds, A, wordStrings).
is_speechPartPred_1(A) :-
        argIsa(A, 1, xtLexicalWord),
        cyckb_t_e2c(arity, A, 2).
is_speechPartPred_1(A) :-
        argIsa(A, 1, xtEnglishWord),
        cyckb_t_e2c(arity, A, 2).
*/

is_SpeechPart(POS):-cyckb_t_e2c(isa,POS,'SpeechPart').


:- baseKB:ain((argIsa(Form,1, 'LexicalWord') :- loop_check(parser_e2c:is_speechPartPred_tt(Form)))).
:- baseKB:ain((argIsa(Form,2, 'CharacterString'):- loop_check(parser_e2c:is_speechPartPred_tt(Form)))).

% ==========================================================
% meetsForm(String,CycWord,Form)
% ==========================================================

%baseKB:ain((meetsForm80(String,RootString,form80(MainPlusTrans,main+tv)):-fail,nop((String,RootString,form80(MainPlusTrans,main+tv))))).

% ==========================================================
% meetsForm(String,CycWord,Form)
% ==========================================================

% cycstring(_).

meetsForm(String,CycWord,Form):-  no_repeats(meetsForm_0(String,CycWord,Form)).

meetsForm_0(String,CycWord,Form):- one_must( meetsForm_1(String,CycWord,Form),meetsForm_2(String,CycWord,Form)).
meetsForm_0(_String,_CycWord,Form):- var(Form),!,fail.
meetsForm_0(String,CycWord,Form):- is_SpeechPart(Form),!,meetsPos(String,CycWord,Form).
meetsForm_0(String,CycWord,Form):- genlPreds_different(Child,Form),meetsForm_1(String,CycWord,Child).

meetsForm_1(String,CycWord,Form):- has_wordage(String),is_wordage_prop(String, form(_, CycWord, _)),!,is_wordage_prop(String, form(_, CycWord, Form)).
% meetsForm_1(String,CycWord,Form):-nonvar(String),stringListToWordForm(String,CycWord,Form).
%'abbreviationForLexicalWord'('Kilogram-TheWord', 'singular', [kg])
meetsForm_1(String,CycWord,Form):-  stringArgUC(String, CycString,'abbreviationForLexicalWord'(CycWord,Form,CycString)).
meetsForm_1(String,CycWord,Form):- 
                      stringArgUC(String, CycString, (((once(nonvar(CycWord);atom(CycString))),                        
                                 cyckb_t_e2c(Form,CycWord,CycString),is_speechPartPred(Form),notPrefixOrSuffix(CycWord)))).
meetsForm_1([String],CycWord,Form):- stringAtomToWordForm([String],CycWord,Form).

meetsForm_2(String,CycWord,POS):- locally(t_l:allowTT,meetsForm_1(String,CycWord,POS)).

stringAtomToWordForm([String],CycWord,Form):- nonvar(CycWord),!,stringAtomToWordForm([String],NewCycWord,Form),!,CycWord=NewCycWord.
stringAtomToWordForm([String],CycWord,Form):- nonvar(String),!,           
	    'regularSuffix'(Form, Before, Affix), Affix\=@='',
	    words_concat(BaseString,Affix,[String]),
	    meetsForm([BaseString],CycWord,Before).
stringAtomToWordForm([String],CycWord,Form):- 
	    'regularSuffix'(Form,Before,Affix),  Affix\=@='',
            meetsForm([BaseString],CycWord,Before),
	    words_concat(BaseString,Affix,[String]).

  
	    
% speechPartPreds_transitive(POS, Form),meetsPos(String,CycWord,POS)

%'suffixString'('Y_AdjectiveProducing-TheSuffix', y, 'GeneralEnglishMt', v(v('Y_AdjectiveProducing-TheSuffix', y), A)).

%'variantOfSuffix'('Able-TheSuffix', ible, 'GeneralEnglishMt', v(v('Able-TheSuffix', ible), A)).
%'variantOfSuffix'('Al_AdjectiveProducing-TheSuffix', ual, 'GeneralEnglishMt', v(v('Al_AdjectiveProducing-TheSuffix', ual), A)).

%'compoundStringDenotesArgInReln'('Actor-TheWord', [remaining, afterwards], 'CountNoun', 'postActors', 2, 'GeneralEnglishMt', v(v('Actor-TheWord', 'CountNoun', 'postActors', afterwards, remaining), A)).
%'multiWordStringDenotesArgInReln'([unchanged], 'Actor-TheWord', 'SimpleNoun', 'unchangedActors', 2, 'GeneralEnglishMt', v(v('Actor-TheWord', 'SimpleNoun', 'unchangedActors', unchanged), A)).



/*
% Adjectives
meetsForm_1(String,CycWord,'regularDegree'):-'regularDegree'(CycWord,String).
meetsForm_1(String,CycWord,'comparativeDegree'):-'comparativeDegree'(CycWord,String).
meetsForm_1(String,CycWord,'superlativeDegree'):-'superlativeDegree'(CycWord,String).
meetsForm_1(String,CycWord,'nonGradableAdjectiveForm'):-'nonGradableAdjectiveForm'(CycWord,String).

% Nouns
meetsForm_1(String,CycWord,'singular'):-'singular'(CycWord,String).
meetsForm_1(String,CycWord,'plural'):-'plural'(CycWord,String).
%meetsForm_1(String,CycWord,'nonPlural-Generic'):-'nonPlural-Generic'(CycWord,String).

meetsForm_1(String,CycWord,'agentive-Mass'):-'agentive-Mass'(CycWord,String).
meetsForm_1(String,CycWord,'agentive-Pl'):-'agentive-Pl'(CycWord,String).
meetsForm_1(String,CycWord,'agentive-Sg'):-'agentive-Sg'(CycWord,String).
%meetsForm_1(String,CycWord,'singular-Feminine'):-'singular-Feminine'(CycWord,String).
%meetsForm_1(String,CycWord,'singular-Masculine'):-'singular-Masculine'(CycWord,String).
%meetsForm_1(String,CycWord,'singular-Neuter'):-'singular-Neuter'(CycWord,String).
meetsForm_1(String,CycWord,'massNumber'):-'massNumber'(CycWord,String).
meetsForm_1(String,CycWord,'pnSingular'):-'pnSingular'(CycWord,String).
meetsForm_1(String,CycWord,'pnMassNumber'):-'pnMassNumber'(CycWord,String).


% Adverbs
meetsForm_1(String,CycWord,'regularAdverb'):-'regularAdverb'(CycWord,String).
meetsForm_1(String,CycWord,'superlativeAdverb'):-'superlativeAdverb'(CycWord,String).

% Verbs
meetsForm_1(String,CycWord,'infinitive'):-'infinitive'(CycWord,String).
meetsForm_1(String,CycWord,'perfect'):-'perfect'(CycWord,String).
meetsForm_1(String,CycWord,'presentParticiple'):-'presentParticiple'(CycWord,String).
meetsForm_1(String,CycWord,'pastTense-Universal'):-'pastTense-Universal'(CycWord,String).
meetsForm_1(String,CycWord,'presentTense-Universal'):-'presentTense-Universal'(CycWord,String).
meetsForm_1(String,CycWord,'firstPersonSg-Present'):-'firstPersonSg-Present'(CycWord,String).
meetsForm_1(String,CycWord,'secondPersonSg-Present'):-'secondPersonSg-Present'(CycWord,String).
meetsForm_1(String,CycWord,'nonThirdSg-Present'):-'nonThirdSg-Present'(CycWord,String).
meetsForm_1(String,CycWord,'thirdPersonSg-Present'):-'thirdPersonSg-Present'(CycWord,String).
*/



% ==========================================================

varnameIdea(X,Y):-varnameIdea2(X,Y),!.
varnameIdea2([String|_],Subj):-!,varnameIdea2(String,Subj).
varnameIdea2('?TargetAgent','?TargetAgent').
varnameIdea2('TargetAgent','?TargetAgent').
varnameIdea2('?Speaker','?Speaker').
varnameIdea2(String,Subj):-atom(String),var(Subj),atom_concat('?',String,Sym),gensym(Sym,Subj).
varnameIdea2(_String,_Subj).
  

%:-posMeans(CycWord,String,POS,Form,CycL).


clean_posm_cache:-
      t_l:noreorder,
      retractall(posm_cached(CycWord,String,POS,Form,[null])),
      retractall(posm_cached(CycWord,[] ,   POS,Form,CycL)),
      retractall(real_posm_cached(CycWord, _ ,POS,Form,CycL)),
      retractall(real_posm_cachedTT(CycWord, _ ,POS,Form,CycL)),
      posm_cached(CycWord,String,POS,Form,CycL),
      once(partition_cache(CycWord,String,POS,Form,CycL)),
      fail.

clean_posm_cache:-tell(foo2),
   t_l:noreorder,
   listing(real_posm_cached),
   listing(real_posm_cachedTT),
   told.

save_posm_cache
   :-tell(pldata(posm_cached_data)),
   listing(posm_cached),
   told.




partition_cache(CycWord,String,POS,Form:'posForms',CycL):-!,partition_cache(CycWord,String,POS,Form,CycL).

partition_cache(CycWord,String,POS,Form,CycL):-
      t_l:noreorder,
   atom(CycWord),
      atom_concat('TT', _ ,CycWord),!,
      partition_cacheTT(CycWord,String,POS,Form,CycL).


% ======================================================
% Partitinion CycNL
% ======================================================

% will look like.. posm_cached('Skill-TheWord', [skilled], 'MassNoun', 'regularDegree':'posForms', meaningOfWord('Skill-TheWord')).

partition_cache(CycWord,String,POS,Form,meaningOfWord(CycWord)):-!,t_l:noreorder,
   posm_cached(CycWord,String, _ , _ ,CycL),not(CycL=meaningOfWord(_)),
   assert_if_new(real_posm_cached(CycWord,String,POS,Form,CycL)).
      
   %real_posm_cached('Type-TheWord', [of, geographical, entity, classified, by, hierarchy], 'SimpleNoun', form, 'GeographicalEntityByHierarchy').
partition_cache(CycWord,String,POS,form,CycL):-!,
  t_l:noreorder,
      posm_cached(CycWord,BPhraseing,POS,Not_form, _Old_Meaning),not(Not_form=form),
      append(BPhraseing,String,OPhrasing),
      partition_cache(CycWord,OPhrasing,POS,Not_form,CycL).
      

partition_cache(CycWord,String,POS,Form,CycL):-!,
   assert_if_new(real_posm_cached(CycWord,String,POS,Form,CycL)).

% ======================================================
% Partitinion TT CycNL
% posm_cached('TTWord-RATP', ['RATP'], 'Noun', 'TTPred-inflNounFemininePluralUnchecked', 'TT-company-RATP')
% ======================================================

% Delete copies of cycNL from TT
partition_cacheTT(_CycWord,String,_POS, _Form, _CycL):-
   posm_cached(OCycWord,String, _ , _ , _ ),
   atom(OCycWord),
   not(atom_concat('TT', _ ,OCycWord)),!.

partition_cacheTT(CycWord,String,POS,Form,meaningOfWord(CycWord)):-
   posm_cached(CycWord,String, _ , _ ,CycL),not(CycL=meaningOfWord(_)),!,
   assert_if_new(real_posm_cachedTT(CycWord,String,POS,Form,CycL)).

partition_cacheTT(CycWord,String,POS,Form,CycL):-!,
   assert_if_new(real_posm_cachedTT(CycWord,String,POS,Form,CycL)).



%:-clean_posm_cache.


% testE2C:-make,halt.
codesToForms(Codes,[],Codes):-!.
codesToForms(Codes,[List|More],Out):-!,
      codesToForms(Codes,List,M),!,
      codesToForms(M,More,Out),!.

codesToForms(Codes,lowercase,Out):-!,toLowercase(Codes,Out).
codesToForms(Codes,uppercase,Out):-!,toUppercase(Codes,Out).
codesToForms(Codes,cyclist,Out):-!,getSurfaceFromChars(Codes,Out, _).
codesToForms(Codes,cyclistvars,Out:V):-!,getSurfaceFromChars(Codes,Out,V).
codesToForms(Codes,cycl,Out):-!,getSurfaceFromChars(Codes,O,_V),balanceBinding(O,Out).
codesToForms(Codes,cyclvars,Out:V):-!,getSurfaceFromChars(Codes,O,V),balanceBinding(O,Out).
codesToForms(Codes,words,Out):-!,into_lexical_segs(Codes,Out).
codesToForms(Codes,idioms(D),Out):-!,idioms(D,Codes,Out).
codesToForms(Codes,Pred,Out):-atom(Pred),!,Call=..[Pred,Codes,Out],!,Call.

dirrect_order([start,tomcat]).
%dirrect_order([start,tomcat]):-shell('/opt/tomcat/bin/startup.sh'),fmt([ok,done]).

% =======================================================
%'semTransPredForPOS'('Verb', 'verbSemTrans', 'EnglishMt', v(v('Verb', 'verbSemTrans'), A)).
% =================================================================
apply_frame(Formula,Subj,Event,Obj,Target,CycL):-
      varnameIdea('ACTION',Event),
      varnameIdea('SUBJECT',Subj),
      varnameIdea('OBJECT',Obj),
      varnameIdea('OBLIQUE',Target),
      vsubst(Formula,':SUBJECT',Subj,Formula1),
      vsubst(Formula1,':NOUN',Subj,Formula2),
      vsubst(Formula2,':ACTION',Event,Formula3),
      vsubst(Formula3,':OBJECT',Obj,Formula4),
      vsubst(Formula4,':EVENT',Event,Formula5),
      vsubst(Formula5,':OBLIQUE-OBJECT',Target,Formula6),
      vsubst(Formula6,':ARG1',Subj,Formula7),
      vsubst(Formula7,':VERB',Event,Formula8),
      vsubst(Formula8,':ARG2',Obj,Formula9),
      vsubst(Formula9,':EVENT',Event,Formula10),
      vsubst(Formula10,':ARG3',Target,CycL).

contains_obliqe(Formula):-flatten(Formula,Flat),member(':OBLIQUE-OBJECT',Flat).

%:- rtrace.

:- op(500,xfy,'&'). 
:- op(510,xfy,('=>')).
:- op(100,fx,('`')).

	 
:-export((fdelete/3)).

% ===============================================================================================
	             	 	
fdelete([],_,[]):-!.

fdelete([Replace|Rest],[H|T],Out):-
	functor(Replace,F, _),memberchk(F,[H|T]),!,
       fdelete(Rest,[H|T],Out),!.

fdelete([Replace|Rest],[H|T],[Replace|Out]):-!,
       fdelete(Rest,[H|T],Out),!.

fdelete([Replace|Rest],F,Out):-
	functor(Replace,F, _),!,%F=FF,
       fdelete(Rest,F,Out),!.

fdelete([Replace|Rest],F,[Replace|Out]):-
       fdelete(Rest,F,Out),!.

%:-ensure_loaded(opencyc_chatterbot_data).





/* Prediate:  descriptionTest/2 
interpreted.
file('c:/development/opensim4opencog/bin/cynd/startrek/mudreader.pl').
line_count(33).
number_of_clauses(1).
Pattern: descriptionTest(_G1470,_G1471). 
 */
descriptionTest('NpcCol1000-Geordi684',["Lieutenant","Commander","Geordi","LaForge","Geordi LaForge","Lieutenant Commander Geordi LaForge is standing here","Geordi is the Chief Engineer of the Enterprise","He's blind, so he wears a special VISOR that lets him see things","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Geordi","Geordi LaForge","Lieutenant Commander Geordi LaForge is standing here","Geordi is the Chief Engineer of the Enterprise","He's blind, so he wears a special VISOR that lets him see things"]).
descriptionTest('NpcCol1002-Worf720',["Lieutenant","Worf","Klingon","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 2","mudMaxHitPoints: 12d12+3400","#$PunchingSomething mudBareHandDamage: 9d9+60","Worf","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong"]).
descriptionTest(vacuum(1),["Lieutenant","Commander","Data","Android"]).
descriptionTest(v12,["Data","Lieutenant Commander Data is here, trying to be more human","Data is the only android on the Enterprise, and the only android in all of Starfleet","He stowed super-human strength, and is extremely tough","ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON","NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 1","mudMaxHitPoints: 18d18+4000","#$PunchingSomething mudBareHandDamage: 10d10+75","Data","CycLBot","CycBot","CycBot1","Data","Lieutenant Commander Data is here, trying to be more human","Data is the only android on the Enterprise, and the only android in all of Starfleet","He stowed super-human strength, and is extremely tough"]).
descriptionTest(explorer(player1),["Lieutenant","Commander","Human","Player",
            "Explorer Player",
            "ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON",
            "NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 1","mudMaxHitPoints: 18d18+4000",
            "#$PunchingSomething mudBareHandDamage: 10d10+75","Player","Player","Human",
            "Logged on player character"]).
descriptionTest('NpcCol1002-Worf720',["Lieutenant","Worf","Klingon","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 2","mudMaxHitPoints: 12d12+3400","#$PunchingSomething mudBareHandDamage: 9d9+60","Worf","Lieutenant Worf","Lieutenant Worf is here, looking pretty mean","Worf is the first Klingon to have joined Starfleet","He's Chief of Security of the Enterprise, and he's plenty strong"]).
descriptionTest('NpcCol1003-Dr-Crusher677',["Doctor","Beverly","Crusher","Doctor Crusher","Lieutenant Beverly Crusher is here, looking for someone to heal","Doctor Crusher is the Enterprise's Chief Medical Officer","Wesley is her son","Her husband was killed years ago in an accident on another starship which was also commanded by Captain Picard","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Dr. Crusher","Doctor Crusher","Lieutenant Beverly Crusher is here, looking for someone to heal","Doctor Crusher is the Enterprise's Chief Medical Officer","Wesley is her son","Her husband was killed years ago in an accident on another starship which was also commanded by Captain Picard"]).
descriptionTest('NpcCol1004-Troi712',["Counselor","Deanna","Troi","Counselor Troi","Counselor Deanna Troi is here","Counselor Troi is the ship's main counselor","She's half betazoid, which means that she can read people's minds","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 3","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+42","Troi","Counselor Troi","Counselor Deanna Troi is here","Counselor Troi is the ship's main counselor","She's half betazoid, which means that she can read people's minds"]).
descriptionTest('NpcCol1005-Riker707',["Commander","William","Riker","Commander Riker","Commander William Riker is here, staring at you","Commander Riker is the Enterprise's first officer","He's in charge of keeping the crew in line","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 2","mudMaxHitPoints: 12d12+3200","#$PunchingSomething mudBareHandDamage: 9d9+52","Riker","Commander Riker","Commander William Riker is here, staring at you","Commander Riker is the Enterprise's first officer","He's in charge of keeping the crew in line"]).
descriptionTest('NpcCol1006-Picard701',["Captain","Jean","Luc","Jean-Luc","Picard","Captain Picard","Captain Jean-Luc Picard is standing here, watching you","Captain Picard is a very important man","He's in charge of Starfleet's flagship, the Enterprise","He's very smart, and very wise","Don't mess with him!","ACT_NICE_THIEF","AWARE","NOBACKSTAB","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOSUMMON","NOSLEEP","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_SANCTUARY","NPC_NOTRACK","+mudToHitArmorClass0: 0","mudMaxHitPoints: 20d20+5000","#$PunchingSomething mudBareHandDamage: 12d12+75","Picard","Captain Picard","Captain Jean-Luc Picard is standing here, watching you","Captain Picard is a very important man","He's in charge of Starfleet's flagship, the Enterprise","He's very smart, and very wise","Don't mess with him!"]).
descriptionTest('NpcCol1007-Guinan689',["Guinan","Guinan","Guinan is here, tending the bar","Guinan is a strange being","She's lived for thousands of years and experienced many things, but now she's decided to work on the Enterprise as a bartender","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 4","mudMaxHitPoints: 12d12+2600","#$PunchingSomething mudBareHandDamage: 9d9+36","Guinan","Guinan","Guinan is here, tending the bar","Guinan is a strange being","She's lived for thousands of years and experienced many things, but now she's decided to work on the Enterprise as a bartender"]).
descriptionTest('NpcCol1008-OBrien696',["Chief","O'Brien","Transporter","Chief O'Brien","Chief O'Brien is here, waiting to teleport you somewhere","Chief O'Brien is the transporter chief on the Enterprise","It's his job to make sure everyone arrives(and leaves) in one piece, instead of trillions of atoms","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 4","mudMaxHitPoints: 12d12+2600","#$PunchingSomething mudBareHandDamage: 9d9+36","O'Brien","Chief O'Brien","Chief O'Brien is here, waiting to teleport you somwhere","Chief O'Brien is the transporter chief on the Enterprise","It's his job to make sure everyone arrives(and leaves) in one piece, instead of trillions of atoms"]).
descriptionTest('NpcCol1009-Wesley716',["Wesley","Crusher","Wesley","Wesley Crusher is here, eagerly trying to earn your praise","Wesley Crusher is not even an official officer, but he serves as an acting Ensign on the bridge","He got this position only because Captain Picard feels guilty about killing his father","ACT_STAY_ZONE","ACT_WIMPY","wimpy mobile will try to flee when it gets low on hit points. A mobile which is both aggressive and wimpy will not attack a player that is awake","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+1400","#$PunchingSomething mudBareHandDamage: 9d9+24","Wesley","Wesley","Wesley Crusher is here, eagerly trying to earn your praise","Wesley Crusher is not even an official officer, but he serves as an acting Ensign on the bridge","He got this position only because Captain Picard feels guilty about killing his father"]).
descriptionTest('NpcCol1010-Livingston726',["Livingston","fish","Livingston","Livingston the fish is here, swimming about in his tank","Livingston is Captain Picard's pet fish","He's some sort of exotic breed, and he's expensive to feed and keep alive","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+800","#$PunchingSomething mudBareHandDamage: 9d9+14","Livingston","Livingston","Livingston the fish is here, swimming about in his tank","Livingston is Captain Picard's pet fish","He's some sort of exotic breed, and he's expensive to feed and keep alive"]).
descriptionTest('NpcCol1011-Spot727',["spot","the","cat","Spot","Spot, Data's pet cat, is sitting here looking at you","Spot is Data's orange coloured cat","Data is always trying to become more human, so he thinks that having a pet might help him achieve his goal","ACT_SENTINEL","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 12d12+800","#$PunchingSomething mudBareHandDamage: 9d9+14","Spot","Spot","Spot, Data's pet cat, is sitting here looking at you","Spot is Data's orange coloured cat","Data is always trying to become more human, so he thinks that having a pet might help him achieve his goal"]).
descriptionTest('NpcCol1012-Ensign728',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign732',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign736',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign740',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign744',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign748',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1012-Ensign752',["ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Ensign","the ensign","A nervous looking ensign is standing here, watching you","These ensigns make up the backbone of the Enterprise","They clean things, do jobs the higher ups won't even consider doing, and get yelled at all the time"]).
descriptionTest('NpcCol1013-Alexander671',["alexander","rozhenko","alexander rozhenko","Alexander Rozhenko is here, practicing laughing hour","Alexander Rozhenko is Worf's son","His mother was half human and half Klingon, so Alexander is 3/4 Klingon","He's quite small, but since he's a Klingon he's very strong","ACT_STAY_ZONE","MEMORY","HELPER","ACT_FRIEND","NOCHARM","NOBASH","NOBLIND","NPC_DETECT_INVIS","NPC_NOTRACK","+mudToHitArmorClass0: 6","mudMaxHitPoints: 8d8+1600","#$PunchingSomething mudBareHandDamage: 8d8+26","Alexander","alexander rozhenko","Alexander Rozhenko is here, practicing laughing hour","Alexander Rozhenko is Worf's son","His mother was half human and half Klingon, so Alexander is 3/4 Klingon","He's quite small, but since he's a Klingon he's very strong"]).
descriptionTest('ArtifactCol1000-Phaser676',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
descriptionTest('ArtifactCol1000-Phaser776',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
descriptionTest('ArtifactCol1000-Phaser700',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
descriptionTest('ArtifactCol1000-Phaser724',["standard","issue","starfleet","phaser","a standard issue phaser","A standard issue Starfleet phaser has been left here","damageNumberDice 5","damageSizeDice 5","WeaponBlasting","These phasers are the standard weapon of Starfleet officers. It offers decent damage for its fairly small size","Phaser","a standard issue phaser"]).
descriptionTest('ArtifactCol1001-5-Phaser-Rifle705',["phaser","rifle","a phaser rifle","A large phaser rifle is lying here","damageNumberDice 7","damageSizeDice 6","WeaponBlasting","This phaser rifle looks pretty powerful. These weapons are used mainly on assault type missions, where power is important","5 Phaser Rifle","a phaser rifle"]).
descriptionTest('ArtifactCol1002-Red-Uniform704',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
descriptionTest('ArtifactCol1002-Red-Uniform710',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
descriptionTest('ArtifactCol1002-Red-Uniform719',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
descriptionTest('ArtifactCol1002-Red-Uniform739',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
descriptionTest('ArtifactCol1002-Red-Uniform743',["burgandy","starfleet","command","uniform","a burgandy Starfleet command uniform","A neatly folded burgandy Starfleet command uniform is lying here","armorLevel: 10","These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good","Red Uniform","a burgandy Starfleet command uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform675',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform775',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform687',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform699',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform723',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform731',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1003-Gold-Uniform735',["gold","starfleet","engineering","uniform","a gold Starfleet engineering uniform","A neatly folded gold Starfleet engineering uniform is lying here","armorLevel: 10","These uniforms are worn by engineering officers on Federation starships. It's kind of tight, but it looks pretty good","Gold Uniform","a gold Starfleet engineering uniform"]).
descriptionTest('ArtifactCol1004-Blue-Uniform680',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
descriptionTest('ArtifactCol1004-Blue-Uniform715',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
descriptionTest('ArtifactCol1004-Blue-Uniform747',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
descriptionTest('ArtifactCol1004-Blue-Uniform751',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
descriptionTest('ArtifactCol1004-Blue-Uniform755',["blue","starfleet","medical","uniform","a blue Starfleet medical uniform","A neatly folded blue Starfleet medical uniform is lying here","armorLevel: 10","These uniforms are worn by medical officers on Federation starships. It's kind of tight, but it looks pretty good","Blue Uniform","a blue Starfleet medical uniform"]).
descriptionTest('ArtifactCol1005-Boots673',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).



descriptionTest('ArtifactCol1005-Boots773',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots678',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots685',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots697',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots702',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots708',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots713',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots717',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots721',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots729',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots733',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots737',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots741',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots745',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots749',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1005-Boots753',["starfleet","black","boots","a pair of Starfleet black boots","A pair of Starfleet black boots are sitting here","armorLevel: 5","These boots must be worn by all Starfleet officers while on duty. They're quite light, and offer good protection for the feet","Boots","a pair of Starfleet black boots"]).
descriptionTest('ArtifactCol1006-Comm-Badge674',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge774',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge679',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge686',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1",
                     "These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge698',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge703',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge709',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge714',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge718',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge722',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge730',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge734',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge738',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge742',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge746',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge750',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1006-Comm-Badge754',["starfleet","comm","com","communication","badge","a Starfleet communication badge","A Starfleet communication badge is lying here","armorLevel: 1","These communication badges must be worn by all officers while on a starship. It looks like a silver arrow head on top of a golden coloured oval: ____/____ / /   | /  | _/ _/_ _/ // \\ ","Comm Badge","a Starfleet communication badge"]).
descriptionTest('ArtifactCol1007-Sash725',["worf's","worf","sash","Worf's sash","Worf's silver chain sash has been left here","armorLevel: 8","Worf's sash is some sort of Klingon clothing. Worf always wears it, which makes you wonder how you managed to get a hold of it..","Sash","Worf's sash"]).
descriptionTest('ArtifactCol1008-VISOR688',["geordi","geordi's","visor","Geordi's VISOR","Geordi's VISOR is lying here","armorLevel: 2","Geordi's VISOR was made specially for him, because he's blind. This piece of equipment allows him to see things, but differently than normal eyes. I wonder how Geordi is managing, now that you've stolen his only way of seeing?","VISOR","Geordi's VISOR"]).
descriptionTest('ArtifactCol1009-Medical-Tricorder681',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1009-Medical-Tricorder682',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1009-Medical-Tricorder683',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Medical Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1009-Tricorder759',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1009-Tricorder760',["medical","tricorder",
                     "a medical Tricorder","A medical Tricorder is lying here, ready to be used",
                     "mudLevelOf: 10",
                     "chargeCapacity: 5",
                     "chargeRemaining: 5",
                     "This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it",
                     "Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1009-Tricorder761',["medical","tricorder","a medical Tricorder","A medical Tricorder is lying here, ready to be used","mudLevelOf: 10","chargeCapacity: 5","chargeRemaining: 5","This medical Tricorder is used to heal small wounds and cuts. While it isn't made for major injuries, it can help you limp home. To use, hold it and then use it","Tricorder","a medical Tricorder"]).
descriptionTest('ArtifactCol1010-Dilithium-Crystal756',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships. This particular crystal is glowing brightly, and gives off a blue-ish tinge","Dilithium Crystal","a dilithium crystal"]).
descriptionTest('ArtifactCol1010-Dilithium-Crystal757',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships. This particular crystal is glowing brightly, and gives off a blue-ish tinge","Dilithium Crystal","a dilithium crystal"]).
descriptionTest('ArtifactCol1010-Dilithium-Crystal758',["dilithium","crystal","a dilithium crystal","A shard of dilithium crystal is lying here","maybe a #$LightingDevice","Dilithium crystals are used to power warp cores of starships. This particular crystal is glowing brightly, and gives off a blue-ish tinge","Dilithium Crystal","a dilithium crystal"]).
descriptionTest('ArtifactCol1011-5-Picards-Flute706',["picard","picard's","flute","Picard's flute","Captain Picard's wooden flute is sitting here","Captain Picard recieved this flute when he lost his memory and was stuck on some strange world. Now, he plays it to relieve stress","5 Picard's Flute","Picard's flute"]).
descriptionTest('ArtifactCol1012-Trombone711',["riker","riker's","trombone","Riker's trombone","Commander Riker's trombone has been placed here","Commander Riker considers himself to be a talented jazz musician. He practices on this trombone all the time","Trombone","Riker's trombone"]).
descriptionTest('ArtifactCol1020-Tea690',["tea","cup","a small cup","A small cup of tea is sitting here","Tea","a small cup"]).
descriptionTest('ArtifactCol1021-Synthehol691',["wine","bottle","synthehol","a synthehol","A bottle of synthehol is standing here","Synthehol","a synthehol"]).
descriptionTest('ArtifactCol1022-Ferengi-Ale692',["ale","ferengi","bottle","a Ferengi bottle","A bottle of Ferengi ale is sitting here","Ferengi Ale","a Ferengi bottle"]).
descriptionTest('ArtifactCol1023-Romulan-Whisky693',["whisky","whiskey","romulan","bottle","a Romulan bottle","A bottle of Romulan whiskey is sitting here","Romulan Whisky","a Romulan bottle"]).
descriptionTest('ArtifactCol1024-Lemonade-Prune-Juice694',["lemonade","prune","juice","glass","a small glass","A small glass of prune juice is sitting here","Lemonade","Prune Juice","a small glass"]).
descriptionTest('ArtifactCol1025-Vulcan-Beer695',["beer","vulcan","bottle","a Vulcan bottle","A bottle of Vulcan beer is standing here","Vulcan Beer","a Vulcan bottle"]).
descriptionTest('iArea1000',["Main Engineering","You find yourself in the middle of main engineering","The room is longer than it is wide, and it has fairly low ceilings","Computer terminals cover all the walls, and a large table built into the floor sits in the middle of the room","At the far end of the room you see the warp core, a large pulsating vertical tube"]).
descriptionTest('iArea1002',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
descriptionTest('iArea1001',["Geordi's Quarters","You're in the middle of Geordi's quarters","The room is sparsely decorated, due to the fact that Geordi is blind","A small personal computer sits on a desk against the western wall, in between two windows that look out into space","A neatly made bed has been placed against the northern wall"]).
descriptionTest('iArea1005',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
descriptionTest('iArea1003',["Data's Quarters","You're in the middle of Data's quarters","Some easils and paintings have been left scattered around the southern part of the room, while a huge computer screen showing a cross section of the Enterprise covers the entire northern wall","In front of the screen is a large desk, which is covered in computer controls","You can't see a bed in this room, but you figure it's because Data doesn't sleep"]).
descriptionTest('iArea1004',["The Brig","You're in the dimly lit Brig","This is where all the criminals and prisoners are kept while on board the Enterprise","Three fairly large cells can been seen in the southern part of the room, and they're all empty","A computer control panel is situated in the northwestern corner of the room, which is where the force fields for the cells are controlled",'The panel says:

***************************************************
*                                                 *
*            NCC-1701-D - ENTERPRISE              *
*                                                 *
*              *****                              *
*      **********************                     *
*      ***********************  _________         *
*              *****        ***(___  ____(        *
*                            ***** \\ \\*           *
*                             **********          *
*                                                 *
*          You are currently on deck 1            *
*                                                 *
***************************************************
']).
descriptionTest('iArea1008',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You see the holodeck's control panel beside the holodeck door, and it has some information on it"]).
descriptionTest('iArea1006',["Transporter Room","You're in the Enterprise transporter room","A computer terminal is sitting near the southern wall, where the transporter chief can control the transporters","Eight round transport pads have been arranged in a circle, on a raised platform against the northern wall"]).
descriptionTest('iArea1042',["Transporter Beam","You find yourself in a transporter beam","All you can see is blue flashing light","It feels as though your body is racing around at high speeds","As you try to look down at your body, you realize that there's nothing there!"]).
descriptionTest('iArea1007',["School","You step through the doors and find yourself in a large school room","Various tables and chairs are set up all around the room, and many paintings and drawings have been attached to the walls","Several computer consoles with a children's interface on them can be seen on the tables"]).
descriptionTest('iArea1010',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
descriptionTest('iArea1009',["Holodeck 2","You're now on Holodeck 2","The room is just a large cube, with jet black walls and a yellow grid painted on the floors, the walls, and the ceiling","This is where different programs can be loaded and experienced, which seem totally real","Right now, this holodeck is not functioning",'
***************************************************
*                                                 *
*            NCC-1701-D - "ENTERPRISE"            *
*                    HOLODECK 2                   *
*                                                 *
*              STATUS : Inactive                  *
*     CURRENT PROGRAM : N/A                       *
*            SAFETIES : N/A                       *
*                                                 *
*    NOTE: Starfleet is not responsible for       *
*          any injuries incurred while on this    *
*          holodeck!                              *
*                                                 *
* WARNING: While the safeties are disabled, you   *
*          CAN be injured, or even killed.        *
*                                                 *
***************************************************']).
descriptionTest('iArea1011',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can accessthe other decks on the Enterprise"]).
descriptionTest('iArea1013',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
descriptionTest('iArea1032',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
descriptionTest('iArea1012',["Cargo Bay 1","You're in the main cargo bay of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling"]).
descriptionTest('iArea1016',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You see the holodeck's control panel beside the holodeck door, and it has some information on it"]).
descriptionTest('iArea1014',["Riker's Quarters","You've arrived in Riker's quarters","The room is very neat and tidy, with a couch and several chairs aranged around a coffee table by the eastern wall","A small partition at the northern part of the room seperates his sleeping area with the rest of the room"]).
descriptionTest('iArea1015',["Sick Bay","You're in the middle of the Enterprise's Sick Bay","About a dozen beds are arranged along the walls of the room, while several carts covered with medical supplies are scattered around the room","A large glass window in the northern part of the room separates the doctor's office with the rest of the room",'
***************************************************
*                                                 *
*            NCC-1701-D - "ENTERPRISE"            *
*                    HOLODECK 4                   *
*                                                 *
*              STATUS : Active                    *
*     CURRENT PROGRAM : Sherlock Holmes (19th     *
*                       century London)           *
*            SAFETIES : Disabled                  *
*                                                 *
*    NOTE: Starfleet is not responsible for       *
*          any injuries incurred while on this    *
*          holodeck!                              *
*                                                 *
* WARNING: While the safeties are disabled, you   *
*          CAN be injured, or even killed.        *
*                                                 *
*             ---ENTER WHEN READY---              *
*                                                 *
*************************************************** ']).
descriptionTest('iArea1019',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
descriptionTest('iArea1017',["Holodeck 4 Entrance - A Narrow Alley","You emerge into a dark narrow alley","Tall dark brick buildings block your way north and south","You can see that the windows on the buildings are fairly high, and some have been boarded up","The smell from the rotting food and garbage mixing with the foul water on the ground is unbearable","You can hear the sounds of a bustling marketpace to the east","The archway leading out of the holodeck is west"]).
descriptionTest('iArea1018',["Crusher's Quarters","You're in Doctor Crusher's quarters","Several different paintings are attached to the walls, and you also notice a few sculptures","A neatly made bed is located by the northern wall, in between two large windows looking out into space"]).
descriptionTest('iArea1021',["Ten Forward","You're now in Ten Forward, the entertainment room of the Enterprise","The entire northern wall is covered with windows looking out into space, while two large wooden doors with the Starfleet insignia stamped on them face south","Many round metal tables are scattered around the room, surrounded by metal chairs","A long bar spans almost the entire length of the southern part of the room, and about two dozen bar stools are sitting in front of it","It's very noisy in here, due to all the talking and laughing"]).
descriptionTest('iArea1020',["Enterprise Security","You're standing in the dimly lit Enterprise Security","Weapons lockers cover all of the walls, except along the northern wall, where a large glass window protects dozens of different phasors, blaster rifles, and other high tech weapons","Three long tables surrounded by chairs stretch across the room"]).
descriptionTest('iArea1022',["Shuttle Bay","You're in the main shuttle bay of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see three different shuttle crafts sitting here, waiting to be flown","A large grey door leads into space"]).
descriptionTest('iArea1024',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape","You notice a tiny computer panel embedded into the wall"]).
descriptionTest('iArea1039',["Outer Space by the Enterprise","You're floating in outer space right beside the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold","A large grey door leads into the Enterprise's Shuttle Bay"]).
descriptionTest('iArea1023',["Troi's Quarters","You're in Counselor Deanna Troi's quarters","Several different paintings have been hung from the walls, and a small couch and a recliner are positioned around a coffee table","A neatly made bed is partially hidden behind a curtain at the northern part of the room"]).
descriptionTest('iArea1027',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape",
"
***************************************************
*                                                 *
*            NCC-1701-D - ENTERPRISE            *
*                                                 *
*              *****                              *
*      **********************                     *
*      ***********************  _________         *
*              *****        ***(___  ____(        *
*                            ***** \\ \\*           *
*                             **********          *
*                                                 *
*          You are currently on deck 3            *
*                                                 *
***************************************************
"]).
descriptionTest('iArea1025',["Worf's Quarters","You're in Worf's quarters","A small table is sitting in the southeastern corner, and on it is a small potted plant","An impressive selection of Klingon weapons have been mounted on the northern wall, and a partition splits this room with Worf's bedroom to the east"]).
descriptionTest('iArea1026',["Enterprise Gym","You emerge into the Enterprise gym","The room is quite large, with a soft grey floor","A set of lockers against the southern wall contain all of the necessary equipment needed for using the gym","A thick stack of mats have been piled high in one corner, which can be used for different activities","Captain Picard likes to come here to practice his fencing"]).
descriptionTest('iArea1030',["A Corridor","You find yourself in the middle of a well lit corridor on the Enterprise","It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape"]).
descriptionTest('iArea1028',["Picard's Quarters","You find yourself standing by the door of Captain Picard's quarters","He isn't very fond of visitors, but you decide to stay and have a look around","You can see several different ancient artifacts on tables and small pedestals, and a large wooden wardrobe is facing south","A comfortable looking recliner with a matching footrest sits beside the door, along with a bright reading lamp and end table","Two large windows offer a great view of space","A small partition at the northern part of the room contains Picard's sleeping area"]).
descriptionTest('iArea1029',["Science Lab","You're in the Enterprise science lab","A strange looking machine sits in the middle of the room, up on a slightly raised platform","It looks as though something(or someone) could be placed inside, hooked up to the multitude of wires and cables, and have scientific tests performed on it(or them)","A complex looking computer console is facing this machine","Around the rest of the room are counterops with with the odd computer terminal"]).
descriptionTest('iArea1031',["Cargo Bay 2","You're in the cargo bay 2 of the Enterprise","It's quite a large room, with a very high ceiling and a lot of floor space","You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling"]).
descriptionTest('iArea1033',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
descriptionTest('iArea1034',["Turbolift","You're in the turbolift","The turbolift walls have been rounded off, making it in the shape of a tube","Several vertical rows of lights make this place very well lit","From here, you can access the other decks on the Enterprise"]).
descriptionTest('iArea1036',["Main Bridge - Upper Half","You find yourself on the upper half of the main bridge of the USS Enterprise","Directly in front of you is a thick railing that contains many different computer panels used for the tactical systems of the ship","The entire southern wall is covered with computer consoles, where the ship's main systems are controlled","Two small curved ramps on either side of the room lead north to the lower part of the bridge, and a large circular skylight shows the space outside the ship"]).
descriptionTest('iArea1035',["Picard's Ready Room","You're standing in Captain Picard's ready room","A long couch has been placed beside the door, while a large U shaped desk is located by the northern wall","A small computer screen is sitting on the desk, as well as several other papers and documents","A single high window beside the desk looks into space, and a fish tank is located in the northwestern corner of the room","This is where the Captain makes all of his important decisions"]).
descriptionTest('iArea1038',["Main Bridge - Lower Half","You find yourself on the lower half of the main bridge of the USS Enterprise","An enormous view screen covers almost the entire northern wall, and is currently displaying a view of the stars rushing by","Three large chairs in the northern part of the room, in front of the railing, face the screen","This is where the Captain, Commander Riker, and Counselor Troi sit","Two computer consoles with built in chairs rest about ten feet in front of the chairs, also facing the view screen","This is where the ship's pilot and information officer sit"]).
descriptionTest('iArea1037',["Conference Room","You're in the conference room of the Enterprise","A large glass rectangular table sits in the middle of the room, surrounded by about a dozen comfortable looking office chairs","The entire eastern wall is covered with windows, looking out into space","This is where the senior officers of the Enterprise meet and discuss important issues"]).
descriptionTest('iArea1040',["Outer Space","You're floating in outer space right above the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold"]).
descriptionTest('iArea1041',["Outer Space","You're floating in outer space right above the USS Enterprise","You can see stars in every direction, which provide the only light here","You feel very cold"]).


% =================================================================
% english2Kif
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% =================================================================
/*

clientEvent(Channel,Agent,english(phrase([learn|Input],Codes), _)):-!,
	    AS = exec_lf(and(equals('?TargetAgent','Self'),equals('?Speaker',Agent),['or'|Ors])),
	    findall(Kif,english2Kif(Input,Kif),Ors),fmt(AS).

clientEvent(Channel,Agent,english(phrase(Input,Codes), _)):-
	    AS = exec_lf(and(equals('?TargetAgent','Self'),equals('?Speaker',Agent),['or'|Ors])),
	    findall(Kif,english2Kif(Input,Kif),Ors),
	    sendEvent(Channel,Agent,(AS)).
*/


e2c(English):- english2Kif(English).
e2c(English,CycLOut):- english2Kif(English,CycLOut),!.

english2Kif(Sentence):- t_l:noreorder, english2Kif(Sentence,Kif),fmt(Kif).

english2Obj(Sentence):-english2Obj(Sentence,Kif),portray_clause(Kif).

english2Kif(Sentence,Kif):-
  locally(thglobal:use_cyc_database,
      (notrace(convertToWordage(Sentence,Words)),
        wordageToKif(Words,Kif))).

english2Obj(Sentence,noun_phrase(A,C)):-
  locally(thglobal:use_cyc_database,
      (notrace(convertToWordage(Sentence,Words)),
         phrase(noun_phrase(A, _ ,C),Words))).

english2Kif:-english2Kif('i am happy').


% ===================================================================
   

convertToWordage(Var,Var):-var(Var),!.
convertToWordage([],['True']):-!.
convertToWordage(Atom,C):- not(is_list(Atom)),!,
   must_det(into_lexical_segs(Atom,List)),
   must_det(convertToWordage(List,C)),!.

convertToWordage(Words,C):- 
      removeBlanks(Words,WordsO),
  Words \=@= WordsO,!,
   must_det(convertToWordage(WordsO,C)).

convertToWordage(Words,C):- fail,
      must_det(idioms(e2c,Words,WordsO)), % was chat
   Words \=@= WordsO,!,
   must_det(convertToWordage(WordsO,C)).

  
convertToWordage(Words,C):-  fail,
      removeRepeats(Words,WordsO),
   Words \=@= WordsO,!,
   must_det(convertToWordage(WordsO,C)).

convertToWordage(Words,Next):-
      fdelete(Words,['Hm','hm','ah','uh','Uh','Um','um'],WordsNoIT),!,
      vsubst(WordsNoIT,i,'I',Next),!.

convertToWordage(O,O).

removeRepeats(WordsNoIT,WordsNoITO):-
      removeRepeats1(WordsNoIT,M),
      removeRepeats2(M,WordsNoITO),!.
removeRepeats(WordsNoIT,WordsNoIT).

removeBlanks([],[]):-!.
removeBlanks([H|T],TT):- is_blankWord(H),!,removeBlanks(T,TT),!.
removeBlanks([H|T],[H|TT]):- removeBlanks(T,TT),!.


non_blankWord(Var):-var(Var),!,fail.
non_blankWord(B):-is_blankWord(B),!,fail.
non_blankWord(_).

is_blankWord(Var):-var(Var),!,fail.
is_blankWord('').
is_blankWord('\n').
is_blankWord([]).
is_blankWord([A]):-!,is_blankWord(A).

% ================================
% local helpers
% ================================
optionalText(X) --> { length(X,L),L > 0, L < 33 } , theText(X).
optionalText(_) --> [].

theVariable(Atom)--> theText([Longer]),{atom_concat(':',Atom,Longer)}.
theVariable(Atom)--> theText([Atom]).

isShortcut(X)--> theText(X).
% =================================================================
% wordageToKif
% =================================================================
      
wordageToKif(Words,ResultO):- reverse(Words,[Symbol|Rev]),reverse(Rev,Quest),!,
	 wordageToKif(Symbol,Words,Quest,ResultO). %,simplifyLF(Result,ResultO).

% TODO move this to to the last clause
wordageToKif(_Symbol,Words,_Quest,(words(Words):-POSList)):-get_wordage_list(Words,POSList).
wordageToKif(('?'),_Words,Quest,query(Kif)) :- phrase(questionmark_sent(Kif),Quest).
wordageToKif(('.'),_Words,Quest,assert(Kif)) :- phrase(period_sent(Kif),Quest).
wordageToKif(('!'),_Words,Quest,assert(Kif)) :- phrase(period_sent(Kif),Quest).
wordageToKif(Symbol,Words,_Quest,Kif) :- not(memberchk(Symbol,[('.'),('?'),('!')])),phrase(sentence(Kif),Words).
wordageToKif(_Symbol,Words,_Quest,words(Words)).
wordageToKif(_Symbol,Words,_Quest,posList(POSList,Words)):-get_pos_list(Words,POSList).

% =================================================================
% wordageToKif
% =================================================================

:-export(notPrefixOrSuffix/1).
notPrefixOrSuffix(CycWord):- not(cyckb_t_e2c(isa, CycWord, 'LexicalPrefix')),not(cyckb_t_e2c(isa, CycWord, 'LexicalSuffix')).
is_cycWord_chk(CycWord):- cyckb_t_e2c(isa,CycWord,'EnglishWord'),!.

usefull_wordage(Var):-var(Var),!,fail.
usefull_wordage(wordage(_,Props)):-!,usefull_wordage(Props).
usefull_wordage([txt([_,_|_])]):-!,fail. 
usefull_wordage(X):-member(form(_,_,_),X).


get_wordage_list(Words,O):- dictionary(slang,B,A),stringToWords(B,Didnt),stringToWords(A,NewList),append_ci(Didnt,Rest,Words),append(NewList,Rest,Ws),Words\=@=Ws,!,get_wordage_list(Ws,O).


get_wordage_list([],[]):-!.
% get_wordage_list(Words,O):- dictionary(contractions,Didnt,NewList), append_ci(Didnt,Rest,Words),append(NewList,Rest,Ws),Words\=@=Ws,!,get_wordage_list(Ws,O).
get_wordage_list([W,'\'','nt'|Rest],O):- equals_icase(W,'do'), get_wordage_list([do,not|Rest],O).
get_wordage_list([W,'\'','ve'|Rest],O):-get_wordage_list([W,have|Rest],O).
get_wordage_list([W,'\'','re'|Rest],O):-get_wordage_list([W,are|Rest],O).
get_wordage_list([W,'\'','nt'|Rest],O):-get_wordage_list([W,not|Rest],O).
get_wordage_list([W,'\'','s'|Rest],O):-get_wordage_list([W,'appostrophyS'|Rest],O).
% O'Brien
get_wordage_list(['O','\'',Dell|Rest],[DCG|POSList]):-atom_concat('O\'',Dell,Pre),!,must_det((get_wordage([Pre],DCG),get_wordage_list(Rest,POSList))).
get_wordage_list([Word,'s'|Rest],[DCG|POSList]):-atomic_list_concat([Word,'s'],'',Pre),get_wordage([Pre],DCG),usefull_wordage(DCG),!,get_wordage_list(Rest,POSList).
get_wordage_list([Word,'\'',Contr|Rest],[DCG|POSList]):-atomic_list_concat([Word,'\'',Contr],'',Pre),get_wordage([Pre],DCG),usefull_wordage(DCG),!,get_wordage_list(Rest,POSList).
get_wordage_list([W|Words],[DCG|POSList]):- parse_singularly(W),get_wordage([W],DCG),usefull_wordage(DCG),!,get_wordage_list(Words,POSList).
get_wordage_list([W|Words],[DCG|POSList]):- not(parse_singularly(W)),member(X,[2,1,3]),length(Pre,X),append(Pre,Rest,[W|Words]),get_wordage(Pre,DCG),usefull_wordage(DCG),!,get_wordage_list(Rest,POSList).
get_wordage_list([W|Words],[w(W)|POSList]):-get_wordage_list(Words,POSList).

parse_singularly(W):-string(W),string_to_atom(W,A),!,parse_singularly(A).
parse_singularly(W):-not(atom(W)),!.
parse_singularly(W):-atom(W),atom_length(W,L),L<3.

has_wordage(String):-is_wordage_cache(String,_).
is_wordage_prop(String,Prop):-is_wordage_cache(String, wordage(_,Props)),!,member(Prop,Props).
is_wordage_prop(String,Prop):-is_wordage_cache(String,Props),!,member(Prop,Props).

:-dynamic(is_wordage_cache/2).
:-export(is_wordage_cache/2).
get_wordage([of, the],_Props):-!,fail.

get_wordage(A,Props):-atom(A),!,get_wordage([A],Props).
get_wordage(Pre,Props):-is_wordage_cache(Pre,Props),!.
get_wordage(Pre,Props):-do_get_wordage(Pre,Props),!,ignore((usefull_wordage(Props),call(asserta,is_wordage_cache(Pre,Props)))).
do_get_wordage(Pre,wordage(Pre,More)):- 
  must_det(( wo_tl(t_l:omitCycWordForms,
     wo_tl(t_l:allowTT,
        locally(t_l:useOnlyExternalDBs,(findall(Prop,string_props(1,Pre,Prop),UProps),get_more_props(Pre,UProps,More))))))).
   


get_more_props(_,Props,Props):- memberchk(form(_,_,_),Props),memberchk(pos(_,_,_),Props),!.
get_more_props(Pre,Props,More):-
 wo_tl(t_l:omitCycWordForms,
   locally(t_l:allowTT,
     locally(t_l:useOnlyExternalDBs,((findall(Prop,string_props(2,Pre,Prop),UProps),
      flatten([UProps,Props],UMore),list_to_set(UMore,More)))))).

:-export(list_wordage/0).

list_wordage:- listing(is_wordage_cache),retractall(is_wordage_cache(_,_)).

% :-list_wordage.
% string_props(Pass,String,posMeans(POS,Form,CycL)):-posMeans(String,POS,Form,CycL).
string_props(Pass,String,tt(Pass,CycWord,Form)):- 
 locally(t_l:omitCycWordForms, locally(t_l:allowTT,(meetsForm(String,CycWord,Form),atom(Form),atom_concat(infl,_,Form),notPrefixOrSuffix(CycWord)))).
string_props(1,[Num],number(Num)):-number(Num).
string_props(1,[Atom],number(Num)):-atom_number(Atom,Num).
string_props(1,Text,txt(Text)).
string_props(Pass,[Num],Props):-number(Num),atom_number(Atom,Num),!,string_props(Pass,[Atom],Props).
string_props(2,String,base(CycWord)):- stringToCycWord(String,CycWord).
string_props(2,[String],Props):-toLowercase(String,Lower),String\=@=Lower,string_props(3,[Lower],Props).
string_props(Pass,String,form(Pass,CycWord,Form)):- Pass\=@=2, meetsForm(String,CycWord,Form),notPrefixOrSuffix(CycWord).
string_props(Pass,String,pos(3,CycWord,POS)):- Pass\=@=2, meetsPos(String,CycWord,POS),notPrefixOrSuffix(CycWord).


%string_props(Pass,[String],PredProps):-!,  is_speechPartPred_tt(Pred),cyckb_t_e2c(Pred,CycWord,String),pred_props(Pred,CycWord,PredProps).
%string_props(Pass,String,PredProps):- is_speechPartPred_tt(Pred),cyckb_t_e2c(Pred,CycWord,String),pred_props(Pred,CycWord,PredProps).

posName(POS):-
  member(POS,['Preposition','Verb','Conjunction','Determiner','Pronoun','Adjective',
   'Noun','Adverb','Number','Punctuation','Quantifier','QuantifyingIndexical']).
posAttrib(POS):-posName(POS).
posAttrib(POS):-member(POS,['Untensed','Tensed','Aux','Be','Do']).
posAttrib(POS):-posAttrib_lc(POS).

posAttrib_lc(POS):-member(POS,['NonSingular','NonPlural','Expletive','ThirdPerson','NonThird','SecondPerson','FirstPerson','Mass','Agentive','Gerundive',
                              'Nongradable','Gradable',
                       'Singular','Present','Indicative','Participle','Universal','Infinitive','Generic','Superlative','Plural',
                        'Indefinite','Past','Particle','WH','Denominal',
                       'Simple','Count','Proper','Active','Passive','Unchecked','Object','Possessive','Feminine','Masculine','Neuter',
                       'Reciprocal','Reflexive','SubjectOrObject','Subject','PasseSimple','Future']).

posSubProp(SUB,POS):-member(SUB-POS,['firstPerson'-'Pronoun','Sg'-'Singular','Pl'-'Plural','gerund'-'Gerundive','3rd'-'ThirdPerson','1st'-'FirstPerson','2nd'-'SecondPerson', 'pn'-'Proper']).
posSubProp(SUB,POS):-posAttrib_lc(POS),string_lower(POS,Low),string_to_atom(Low,SUB).

pred_props(Compound,_,_):-compound(Compound),!,fail.
pred_props(NI,_,_):- atom_contains(NI,'NameInitial'),!,fail.
pred_props(_,Compound,_):-compound(Compound),!,fail.
pred_props(_,'He-TheWord',_):-!,fail.
pred_props(Pred,CycWord,form(2,CycWord,Pred)).
pred_props(Pred,CycWord,pos(2,CycWord,POS)):-
 posAttrib(POS),
 atom_contains(Pred,POS).
pred_props(Pred,CycWord,pos(3,CycWord,POS)):- posSubProp(SUB,POS),atom_contains(Pred,SUB),not(atom_contains(Pred,POS)).


% string_props(Pass,String,cycl(Form)):-  meetsForm(String,CycWord,Form),notPrefixOrSuffix(CycWord).

:-dynamic(word_no_pos/1).

get_pos_list([],[]):-!.
get_pos_list(Words,[DCG|POSList]):- between(1,3,X),length(Pre,X),append(Pre,Rest,Words),get_dcg(DCG,Pre),get_pos_list(Rest,POSList).
get_pos_list([W|Ords],[w(W)|POSList]):-assert_if_new(word_no_pos(W)),get_pos_list(Ords,POSList).
   
get_dcg(DCG,Pre):- dcgPredicate(baseKB,F,_,P),once((P=..[F|ARGS],append(LDCG,[S,E],[F|ARGS]),S=Pre,E=[])),call(P),DCG=..LDCG.

dcgPredicate(M,F,A,P):- module_property(M, exports(List)),member(F/A,List),is_dcg_pred(M,F,A,P).
dcgPredicate(M,F,A,P):- module_property(M, exports(List)),
 findall(F/A,(module_predicate(M,F,A),
   not(member(F/A,List))), Private),
   member(F/A,Private),is_dcg_pred(M,F,A,P).

:- style_check(-singleton).
is_dcg_pred(M,F,A,P):-A >= 2, functor(P,F,A),M:predicate_property(P,number_of_rules(N)),N>0,!, M:clause(P,B),compound(B),arg(A,P,LA),var(LA),\+ \+ is_dcg_pred_pass2(M,F,A,P,B),!.
%is_dcg_pred_pass2(M,F,A,P,B):- pred_contains_term(vcall((compound(B),functor(B,phrase,3))),B,_Match).
is_dcg_pred_pass2(M,F,A,P,B):- pred_contains_term('==',B,phrase).
is_dcg_pred_pass2(M,F,A,P,B):- ((P=..[F|ARGS],append(_LDCG,[S,E],[F|ARGS]), S =@= [_|_], pred_contains_term('=@=',B, _=_ ))).
:- style_check(+singleton).



% makes pred_contains_term(vcall(Call), A, B)  work!
vcall(Call,A,B):- vsubst(Call,A,B,VCall),!,VCall.
pred_contains_term(Pred, A, B) :- call(Pred, A, B).
pred_contains_term(Pred, A, B) :- compound(A), (functor(A,C,_);arg(_, A, C)), pred_contains_term(Pred,C, B), !.

%:-disable_body_textstr.
% % :- enable_body_textstr.


:-discontiguous(simplifyLF/2).
% =======================================================
% sentence(CycL, [every,man,that,paints,likes,monet],[]) 
% =======================================================
%sentence(S) --> conjunct(_),!,syntact(S).
%sentence(S) --> interjections(_),!,syntact(S).
questionmark_sent(true(CycL)) --> assertion_nl(CycL).
questionmark_sent(interogative(CycL)) --> interogative(CycL).
questionmark_sent(areDoing(CycL)) --> imparative(CycL).

simplifyLF(true(X),X).
simplifyLF(yn(X),X).

period_sent(CycL) --> assertion_nl(CycL).
period_sent(command(Act)) --> imparative(Act).
period_sent(interogative(CycL)) --> interogative(CycL).

simplifyLF(interogative(X),X).
simplifyLF(command(X),X).

sentence(command(Act)) --> imparative(Act).
sentence(assert(CycL)) --> assertion_nl(CycL).
sentence(query(CycL)) --> interogative(CycL).
    
simplifyLF(interogative(X),X).
simplifyLF(assert(X),assert(X)).

literal([E|F], C, B):-!,append([E|F],B, C).
literal(E, [E|C], C).
literal([], C, C).

% =================================================================
% interjections  TODO
% =================================================================
interjections(interject(W)) --> interjection(W).
interjections(interjects(H,T)) --> interjection(H),interjections(T).

interjection(C) --> isPOS('Interjection-SpeechPart',C).



% =================================================================
% imparative  TODO
% =================================================================

% tell me
imparative(CycL) --> verb_phrase(TargetAgent,ImparitiveEvent,CycL),
	 {varnameIdea('?TargetAgent',TargetAgent),varnameIdea('ImparitiveEvent',ImparitiveEvent)}.

% =================================================================
% interogative  TODO
% =================================================================
% How are you
% What have you
% What do you have?
% What do you think?
% How will you
% Could the dog
% could he think of it? 
% are you happy
% * ?
interogative(CycL) --> verb_phrase(TargetAgent,ImparitiveEvent,CycL),
	 {varnameIdea('?TargetAgent',TargetAgent),varnameIdea('QuestionEvent',ImparitiveEvent)}.

% =================================================================
% assertion_nl
% =================================================================
% Now lets say that the input values for the memory NN uses the pattern from the other nodes output
% our naming specialist, Linda Bergstedt
% it is good
% the fubar licks the bowl
% It should be a mix.

% the dog licks the bowl
assertion_nl(CycL) --> noun_phrase(Subj,CycL1,CycL),verb_phrase_after_nouns(Subj,_Event,CycL1).


% gen assertion 1
assertion_nl(gen_assert(Call,Result)) --> [S],
	    { 'genFormat'(Predicate,[S|Template],ArgsI),atom(Predicate),
	    (compound(ArgsI) -> trasfromArgs(ArgsI,Args) ; Args=[1,2]),
	    length(Args,Size),functor(Call,Predicate,Size),
	    placeVars(Blanks,Args,Call)},
	    do_dcg(Template,Blanks,Result).

assertion_nl(gen_assert(Predicate)) --> [S],
	    { 'genFormat'(Predicate,S, _) }.

% =================================================================
% WHORDS
% =================================================================

% which do
what_do(W,V) --> query_starter(W),isPOS('DoAux',V).

% where 
%query_starter(W)  --> isPOS('WHAdverb',W).
% could / which
query_starter(W)  --> isPOS('Modal',W);isPOS('WHWord',W).

% =======================================================
% Rel Clauses
% =======================================================

% Linda Bergstedt
human_name(([First,Last])) --> capitolWord(First),capitolWord(Last).
% Linda
human_name(Name) --> capitolWord(Name). 

capitolWord(A) --> [A],{atom(A),atom_codes(A,[C|_]),char_type(C,upper)}.

% =======================================================
% Nouns Phrases
% =======================================================
% =======================================================
% TODO
%'properNounSemTrans'('Egyptian-TheWord', 0, 'RegularNounFrame', 'citizens'('Egypt', ':NOUN'), 'GeneralEnglishMt', v(v('Egypt', 'Egyptian-TheWord', 'RegularNounFrame', 'citizens', ':NOUN'), A)).
% =======================================================


% TODO
% =======================================================
%'nlPhraseTypeForTemplateCategory'('PhraseFn-Bar1'('Verb'), 'PerfectiveVBarTemplate', 'AuxVerbTemplateMt', v(v('PerfectiveVBarTemplate', 'PhraseFn-Bar1', 'Verb'), A)).


% TODO
% =======================================================
%'nounSemTrans'('Aspect-TheWord', 0, nartR('PPCompFrameFn','TransitivePPCompFrame', 'Of-TheWord'), 'hasAttributes'(':OBLIQUE-OBJECT', A), 'GeneralEnglishMt', v(v('Aspect-TheWord', 'Of-TheWord', 'PPCompFrameFn', 'TransitivePPCompFrame', 'hasAttributes', ':OBLIQUE-OBJECT'), ['?ATTR'=A|B])).

% TODO
% a man hapilly maried
% a man who knows
% a man of his word that walks
% a man of his word
% the cost of what the product is


%noun_phrase(Subj,In,also(A,LL)) --> [A|LL],{cont ([A|LL])}.

noun_phrase(_ ,_ , _) --> dcgStartsWith1(isPOS(DET)),{ cantStart(noun_phrase,DET), !,fail}. 

noun_phrase(List, In, Out, [M,N,O|More], F):- 
 (nth1(Loc,[M,N,O],(','));nth1(Loc,[M,N,O],'and')),
   noun_phrase_list(Loc,List, In, Out, [M,N,O|More], F).

% a man that walks
noun_phrase(S,A,B)-->subject5(S,A,B). 

noun_phrase_list(_  ,[H],In,Out) --> subject5(H,In,Out).
noun_phrase_list(Loc,[H|T],In,Out) --> ([and];[(',')];[]),
      subject5(H,In,Mid),([and];[(',')];[]),
      noun_phrase_list(Loc,T,Mid,Out),{!}. 


%rel_clause(Subj,HowDoes) -->isPOS('Complementizer',Modal,String),verb_phrase(Subj,Event,HowDoes),{varnameIdea(String,Event)}.
noun_phrase_rel_clause(_Loc,Subj,In,rel_clause(In,Out)) -->  % {stack_depth(SD), SD<600},
	 subject5(Subj,HowDoes,Out), 
	 (isPOS('Complementizer',_ModalWord,_String);[]),
	 verb_phrase(Subj,_Event,HowDoes).

% =======================================================
subject_isa(_SubjectIsa,Subj,Template,TemplateO) --> subject5(Subj,Template,TemplateO).


% =======================================================

%subject5(_ , _ , _ ) --> isPOS('Verb', _),{!,fail}, _.

% a man who|that walks
subject5(List, In, Out, [M,N,O|More], F) :-
      nonvar(More),
      (nth1(Loc,[M,N,O],'who');nth1(Loc,[M,N,O],'that')),
      noun_phrase_rel_clause(Loc,List, In, Out, [M,N,O|More], F).


% =======================================================
% 'Determiner-Indefinite' , 'Determiner-Definite'
%'determinerAgreement'('A-Half-Dozen-MWW', 'plural-Generic', ..)

% all dogs
subject5(Subj,In,'forAll'(Subj,AttribIsa)) --> 
    ([every];[all];[forall];[each];[for,all]),
    det_object(Subj,In,AttribIsa).


% the happy dog
%subject5(X,In,referant(X,isa(X,Subj),AttribIsa)) --> [the],      det_object(Subj,In,AttribIsa),{varnameIdea('Thing',X),!}.
subject5(Subj,In,AttribIsa) --> [the],det_object(Subj,In,AttribIsa).

% a dog
subject5(Subj,In,'thereExists'(Subj,AttribIsa)) --> 
    ([a];[an];[some];[there,is];[there,are];[there,exists]),
    det_object(Subj,In,AttribIsa).

% your rainbow
subject5(X,A,and(ownedBy(X,Agent),isa(X,Thing),B)) --> possessive(Agent),noun_phrase(Thing,A,B),{varnameIdea('Thing',X),!}.

% he
subject5(PN,CycL,CycL) --> pronoun(PN),{!}.

% Joe blow
subject5(named(Name),CycL,CycL) --> human_name(Name),{!}.

% a man

% dog
subject5(Subj,In,AttribIsa) --> det_object(Subj,In,AttribIsa).

:- style_check(-singleton).

:- discontiguous(det_object//3).

det_object(_,_,_) --> isPOS(DET),{ cantStart(noun_phrase,DET), !,fail}. 

% =======================================================
%'multiWordSemTrans'([equilateral], 'Shaped-TheWord', 'Adjective', 'RegularAdjFrame', 'shapeOfObject'(':NOUN', 'EquilateralShaped'), 'EnglishMt', v(v('Adjective', 'EquilateralShaped', 'RegularAdjFrame', 'Shaped-TheWord', 'shapeOfObject', ':NOUN', equilateral), A)).
det_object(Subj,In,in_and(In,Extras,Out)) --> [S],
  {'multiWordSemTrans'([S|String],CycWord, 'Adjective', NextFrame,Template)},
     String, isCycWord(CycWord), frame_template(NextFrame,Subj,Result,Extras),
    {apply_frame(Template,Subj,_Event,_Obj,Result,Out)}.

det_object(Subj,In,Out) --> 
	 isPOS('Adjective',CycAdj),
	 det_object_adj(CycAdj,Subj,In,Out).

% =======================================================
%'adjSemTrans-Restricted'('Wooden-TheWord', 0, 'RegularAdjFrame', 'Artifact', 'isa'(':NOUN', 'Wood'), 'GeneralEnglishMt', v(v('Artifact', 'RegularAdjFrame', 'Wood', 'Wooden-TheWord', 'isa', ':NOUN'), A)).
det_object_adj(CycAdj,Subj,In,in_and(In,nop(frameType(FrameType)),Out)) --> 
	 {'adjSemTrans-Restricted'(CycAdj, _ , FrameType, NounIsa, Template)},
	subject_isa(NounIsa,Subj,Template,TemplateO),
      {apply_frame(TemplateO,Subj,Event,Obj,Result,Out)}.

% =======================================================
%'adjSemTrans'('Tame-TheWord', 0, 'RegularAdjFrame', 'isa'(':NOUN', 'TameAnimal'), 'GeneralEnglishMt', v(v('RegularAdjFrame', 'Tame-TheWord', 'TameAnimal', 'isa', ':NOUN'), A)).
%'adjSemTransTemplate'('ColorTingeAttribute', 'RegularAdjFrame', 'objectHasColorTinge'(':NOUN', ':DENOT'), 'GeneralEnglishMt', v(v('ColorTingeAttribute', 'RegularAdjFrame', 'objectHasColorTinge', ':DENOT', ':NOUN'), A)).
det_object_adj(CycAdj,Subj,In,and(Extras,Out)) --> 
	 {'adjSemTrans'(CycAdj, _ , FrameType, Template);
	 ('adjSemTransTemplate'(AdjIsa, FrameType, Template),cycQueryIsa(CycAdj,AdjIsa))},
	frame_template(NextFrame,Subj,Result,Extras),
      {apply_frame(Template,Subj,Event,Obj,Result,Out)}.


det_object_adj(CycAdj,Subj,In,and(Isa,hasTrait(Subj,CycL))) -->
       det_object(Subj,In,Isa),{cvtWordPosCycL(CycAdj,'Adjective',CycL),!}.

% =======================================================
det_object(Subj,In,Isa) --> object5(Subj,In,Isa).

% =======================================================
det_object(PN,CycL,CycL) --> proper_object(PN).

%'multiWordSemTrans'([intended, recipient, of], 'Communicate-TheWord', 'SimpleNoun', 'RegularNounFrame', 'communicationTarget'(A, ':NOUN'), 'EnglishMt', v(v('Communicate-TheWord', 'RegularNounFrame', 'SimpleNoun', 'communicationTarget', ':NOUN', intended, of, recipient), ['?X'=A|B])).
object5(Subj,In,'and'(In,Out)) --> [S],
   {'multiWordSemTrans'([S|String],CycWord,POS, NextFrame,Template),POS \=@= 'Adjective'},
     String,isCycWord(CycWord), frame_template(NextFrame,Subj,Result,Extras),
    {apply_frame(Template,Subj,Event,Obj,Result,Out)}.

% =======================================================
%'nounPrep'('Address-TheWord', 'Of-TheWord', ['pointOfContactInfo', ':OBLIQUE-OBJECT', 'ContactLocation', 'addressText', ':NOUN']).
%'nounPrep'('Retail-TheWord', 'Of-TheWord', ['sellsProductType', ':NOUN', ':OBLIQUE-OBJECT']).
%'nounPrep'('Market-TheWord', 'Of-TheWord', ['sellsProductType', ':NOUN', ':OBLIQUE-OBJECT']).
%'nounPrep'('Start-TheWord', 'Of-TheWord', ['startingPoint', ':OBLIQUE-OBJECT', ':NOUN']).
%'nounPrep'('City-TheWord', 'Of-TheWord', 'equals'(':OBLIQUE-OBJECT', ':NOUN'), 'EnglishMt', v(v('City-TheWord', 'Of-TheWord', 'equals', ':NOUN', ':OBLIQUE-OBJECT'), A)).
%'nounPrep'('Victim-TheWord', 'Of-TheWord', 'victim'(':OBLIQUE-OBJECT', ':NOUN'), 'EnglishMt', v(v('Of-TheWord', 'Victim-TheWord', 'victim', ':NOUN', ':OBLIQUE-OBJECT'), A)).
object5(Subject,In,and(CycL,Out)) --> isPOS('Noun',CycWord), 
      {'nounPrep'(CycWord,CycWordPrep, Template)},
      isCycWord(CycWordPrep),subject5(Result,In,CycL),
    {apply_frame(Template,Subject,Event,Object,Result,Out)}.

% the happy dog
object5(Subj,CycL,and(CycL,Isa)) --> colection(Subj,Isa).

%% of what the product is
%'team-mate'
% happy dog
% kickoff call
colection(Subj,isaMember(Subj,W)) --> [W],{atom(W),atom_concat('', _ ,W),!}.
colection(Subj,isaMember(Subj,CycL)) --> isPOS('Noun',CycWord,String),
	    {cvtWordPosCycL(CycWord,'Noun',CycL),varnameIdea(String,Subj),!}.

wordPosCycL(CycWord,POS,CycL):-
      'denotation'(CycWord,POS, _ , CycL);'denotationRelatedTo'(CycWord,POS, _ , CycL).
wordPosCycL(CycWord,POS,CycL):- 'genls'(Child,POS),wordPosCycL(CycWord,Child,CycL).
wordPosCycL(CycWord, _ ,CycL):-
      'denotation'(CycWord,POS, _ , CycL);'denotationRelatedTo'(CycWord,POS, _ , CycL).
      

cvtWordPosCycL(CycWord,POS,CycL):-wordPosCycL(CycWord,POS,CycL),!.
cvtWordPosCycL(CycWord,POS,CycL):-CycL=..[POS,CycWord],!.
% ==========================================================
% String to CYC -POS
% ==========================================================
:- discontiguous(proper_object//1).

proper_object(CycL) --> dcgStartsWith1(isPOS(DET)),{ cantStart(noun_phrase,DET), !,fail}. 
proper_object(CycL) --> theText([S,S2]),{poStr(CycL,[S,S2|String])},theText(String).
proper_object(CycL) --> theText([String]),{poStr(CycL,[String])}.
proper_object(multFn(Multiply,Collection)) --> [String],
	 {'unitOfMeasurePrefixString'(Multiply, Affix),
	 words_concat(Affix,Rest,String),!,phrase(collection3(Collection),[Rest])}.
proper_object(CycL) --> pos_cycl(Noun,CycL), { goodStart(noun_phrase,Noun) } .


poStr(CycL,String):- stringArg(String, poStr0(CycL,String)).
poStr0(CycL,String):- strings_match,
      'genlPreds'(FirstName,'nameString'),
       cyckb_t_e2c(FirstName,CycL,String).

poStr0(CycL,String):- strings_match,
      'initialismString'(CycL,String);
      'abbreviationString-PN'(CycL,String);
      'preferredNameString'(CycL,String);
      'countryName-LongForm'(CycL,String);
      'countryName-ShortForm'(CycL,String);
      'acronymString'(CycL,String);
      'scientificName'(CycL,String);
      'termStrings'(CycL,String);
      'termStrings-GuessedFromName'(CycL,String);
      'prettyName'(CycL,String);
      'nameString'(CycL,String);
      'nicknames'(CycL,String);
      'preferredTermStrings'(CycL,String).


%possessive(Agent)-->pronoun(Agent),isCycWord('Have-Contracted'),{!}.
possessive(Agent)-->pronoun(Agent),isCycWord('Be-Contracted').
possessive(Agent)-->isPOS('PossessivePronoun-Pre',Agent).
possessive(Agent)-->isPOS('PossessivePronoun-Post',Agent).
possessive(Inters)-->human_name(Inters),['\'',s].
%possessive(Agent)-->pronoun(Agent).

pronoun('?Speaker') --> ['i'];['I'];isCycWord('I-TheWord');isCycWord('Me-TheWord').
pronoun('?He') --> isCycWord('He-TheWord').
pronoun('?TargetAgent') --> isCycWord('You-TheWord').
pronoun('?Where') --> isCycWord('Where-TheWord').
pronoun('?How') --> isCycWord('How-TheWord').
pronoun('?IT') --> isCycWord('It-TheWord').
pronoun('?She') --> isCycWord('She-TheWord').

pronoun(X) --> wh_pronoun(X).
pronoun(ref(CycWord)) --> isPOS('Pronoun', CycWord).

wh_pronoun('?Agent') --> [who].
wh_pronoun('?What') --> [what].

% =======================================================
% Qualified Noun
% =======================================================
collection_noun_isa(Subj,'isa'(Subj,CycLCollection)) --> collection_noun(Subj,CycLCollection).

collection_noun(Subj,CycLCollection) --> [A,B,C,D],{phraseNoun([A,B,C,D],Form,Subj,CycLCollection)}.
collection_noun(Subj,CycLCollection) --> [A,B,C],{phraseNoun([A,B,C],Form,Subj,CycLCollection)}.
collection_noun(Subj,CycLCollection) --> [A,B],{phraseNoun([A,B],Form,Subj,CycLCollection)}.
collection_noun(Subj,CycLCollection) --> [A],{phraseNoun([A],Form,Subj,CycLCollection)}.
collection_noun(Subj,'AdultMalePerson') --> [man].

collection3(M)--> collection_noun('?Subj',CycLCollection).

phraseNoun(Eng,Form,Subj,CycLCollection):-
      phraseNoun_each(Eng,Form,CycLCollction),
      eng_subj(Eng,Subj).

eng_subj(Eng,Subj):-var(Subj),getVarAtomName(Subj,Atom),concat_atom([?|Eng],'',T),atom_concat(T,Atom,Subj).
eng_subj(Eng,Subj):-!.

getVarAtomName(Value,Name):-var(Value),!,term_to_atom(Value,Vname),atom_codes(Vname,[95, _|CODES]),atom_codes(Name,CODES),!.
getVarAtomName('$VAR'(VNUM),Name):-concat_atom([VNUM],Name),!.

% ['h','e','l','l','o'] = "hello" = [104, 101, 108, 108, 111]

phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'SimpleNoun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'MassNoun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'AgentiveNoun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'Noun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'QuantifyingIndexical', _ ,CycL).
							 

% =======================================================
% Conjunctions
% =======================================================
% [that];[who]

conjunct --> conjunct(X).
conjunct(C) --> isPOS('CoordinatingConjunction',C).
conjunct(and_also)--> [and];[also].

disj_word --> [or];[not];[but].


modal_phrase(CycAuxVerb,Subj,Event,Out)-->aux_phrase(CycAuxVerb,Subj,Event,Out).
% =======================================================
% Aux Phrases
% =======================================================

% is good, was meaningfull ,  are greatfull
aux_phrase('Be-TheWord',Subj,Event,and(Frame,hasTrait(Subj,AdjFrame))) -->
         isPOS('Adjective',NomicAdj),
      aux_phrase('Be-TheWord',Subj,Event,Frame),
      {cvtWordPosCycL(NomicAdj,'Adjective',AdjFrame)}.


% can
aux_phrase('Can-TheModal',Subj,Event,'can'(Subj,Frame)) --> 
	  verb_phrase(Subj,Event,Frame).

% do/is/be/does
aux_phrase(CycAuxVerb,Subj,Event,aux_isa_for(Subj,Event,Action)) --> [],
	 {cvtWordPosCycL(CycAuxVerb,'Verb',Action)}.

% do <X>
aux_phrase('Do-TheWord',Subj,Event,(CycL)) --> 
	 verb_phrase(Subj,Event,CycL) .

% =======================================================
%'verbPrep-Passive'('Make-TheWord', 'Of-TheWord', 'mainConstituent'(':OBJECT', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Make-TheWord', 'Of-TheWord', 'mainConstituent', ':OBJECT', ':OBLIQUE-OBJECT'), A)).
aux_phrase(CycWord,Subj,Event,CycLO) --> 
      {'verbPrep-Passive'(CycWord, CycWord2, Template)},
       isCycWord(CycWord2),subject5(Result,Out,CycLO),
      {apply_frame(Template,Subj,Event,Obj,Result,Out)}.

% =======================================================
%'prepSemTrans'('Above-TheWord', 0, 'Post-NounPhraseModifyingFrame', 'above-Generally'(':NOUN', ':OBJECT'), 'GeneralEnglishMt', v(v('Above-TheWord', 'Post-NounPhraseModifyingFrame', 'above-Generally', ':NOUN', ':OBJECT'), A)).
aux_phrase(CycAuxWord,Subj,Event,Out) -->
      {'prepSemTrans'(CycWordPrep, _ , NextFrame, Template)},
      isCycWord(CycWordPrep),subject5(Obj,Out,CycLO),
    {apply_frame(Template,Subj,Event,Obj,Result,Out)}.

% =======================================================
% preopistional_phrase
% =======================================================
preopistional_phrase(Oblique,CycWordPrep,CycL) -->
	 isPOS('Preposition',CycWordPrep), 
	 noun_phrase(Oblique,Prep,CycL),{varnameIdea('Prep',Prep)}.
      
% =======================================================
% verb_phrase
% =======================================================

:- style_check(-discontiguous).

% no verb
verb_phrase(_ , _ , _ ) --> dcgStartsWith1(isPOS(DET)),{ cantStart(verb_phrase,DET), !,fail}.

verb_phrase_after_nouns(Subj,Event,exists(Subj)) --> [].

verb_phrase_after_nouns(Subj,Event,exists(Subj,'verb_phrase')) --> isShortcut('verb_phrase').

verb_phrase_after_nouns(Subj,Event,CycL) --> verb_phrase(Subj,Event,CycL).

% One verb
% verb_phrase(Subj,Event,do(Subj,Verb)) --> [Verb].

% known phrase
verb_phrase(Subj,Event,known_phrase(CycL)) --> 
	    isPOS('Verb',CycVerb),
	    verb_phrase_known(CycVerb,Subj,Event,CycL).

% gen phrase 2
verb_phrase(Subj,Event,gen_phrase2(Call,Result)) --> [S,N],
	    { 'genFormat'(Predicate,['~a',S,N|Template],ArgsI),atom(Predicate),
	    (compound(ArgsI) -> trasfromArgs(ArgsI,Args) ; Args=[1,2]),
	    length(Args,Size),functor(Call,Predicate,Size),
	    placeVars([Subj|Blanks],Args,Call)},
	    do_dcg(Template,Blanks,Result).
	    
% modal phrase
verb_phrase(Subj,Event,modal(CycL)) --> 
	    isPOS('Modal',CycWord,String),
	    modal_phrase(CycWord,Subj,Event,CycL),{varnameIdea(String,Event)}.

% aux phrase
verb_phrase(Subj,Event,(CycL)) --> 
	    isPOS('AuxVerb',CycWord,String),
	    aux_phrase(CycWord,Subj,Event,CycL),{varnameIdea(String,Event)}.

% adverbal phrase
verb_phrase(Subj,Event,'and_adverbal'(Event,AdvCycL,CycL))  --> 
	    isPOS('Adverb',CycWord),
	    verb_phrase(Subj,Event,CycL),
	    {cvtWordPosCycL(CycWord,'Adverb',AdvCycL)}.

% unknown phrase has arity CycL + object5 %TODO rename subject5/3 to noun_phrase/3
verb_phrase(Subj,Event,and_concat(CycL)) --> [Verb],
	 {atom(Verb),((words_concat('',Verb,Predicate),cyckb_t_e2c('arity',Predicate,N));(cyckb_t_e2c('arity',Verb,N),Predicate=Verb)),!},
	 verb_phrase_arity(N,Predicate,Subj,Event,CycL).

% :-index(verb_phrase_arity(0,0,0,0,0,0,0)).
%TODO rename subject5/3 to noun_phrase/3
verb_phrase_arity(2,Predicate,Subj,Event,CycL) --> 
	       best_subject(Obj,ACT,Mid),
	       colect_noun_list(List,Mid,CycL),
	       {apply_act(Predicate,Subj,[Obj|List],ACT)}.
%and
verb_phrase_arity(3,Predicate,Subj,Event,CycL) --> 
	 best_subject(Obj,Event,Mid),
	 best_subject_constituant(RES,Event,Mid,CycL),
	{ACT=..[Predicate,Subj,Obj,RES]}.

colect_noun_list([],In,In) --> [].
colect_noun_list([H|T],In,Out) --> ([(',')];[and];[]),
      best_subject(H,In,Mid),
      colect_noun_list(T,Mid,Out).

verb_phrase(Subj,Event,(CycL)) --> 
	 isPOS('Verb',CycVerb,String),
%	 best_subject(Obj,true,CycL),
 %        best_subject_constituant(Target,Event,CycL,CycLO),
	 {cvtWordPosCycL(CycVerb,'Verb',Verb),
	 (atom(Verb),(atom_concat('',Verb,Predicate),cyckb_t_e2c('arity',Predicate,N));(cyckb_t_e2c('arity',Verb,N),Predicate=Verb)),!},
	  verb_phrase_arity(N,Predicate,Subj,Event,CycL),{varnameIdea(String,Event)}.
	 
% gen phrase 1
verb_phrase(Subj,Event,gen_phrase1(Call,Result)) --> [S],
	    {S\=@=is, 'genFormat'(Predicate,['~a',S|Template],ArgsI),
	    (compound(ArgsI) -> trasfromArgs(ArgsI,Args) ; Args=[1,2]),
	    length(Args,Size),functor(Call,Predicate,Size),atom(Predicate),
	    placeVars([Subj|Blanks],Args,Call)},
	    do_dcg(Template,Blanks,Result).



% unkown phrase	+ object5 %TODO rename subject5/3 to noun_phrase/3
verb_phrase(Subj,Event,and(isaAction(Event,Action),'doneBy'(Event,Subj),'constituentInSituation'(Event,Obj),CycLO)) --> 
	 isPOS('Verb',CycVerb,String),
	 best_subject(Obj,true,CycL),
	 best_subject_constituant(Target,Event,CycL,CycLO),
	 {cvtWordPosCycL(CycVerb,'Verb',Action),varnameIdea(String,Event)}.
														  
:- style_check(+discontiguous).

% unkown phrase + text
best_subject(Obj,Event,CycL) --> isPOS('Preposition', _),{!},best_subject(Obj,Event,CycL).
best_subject(Obj,Event,CycL) --> noun_phrase(Obj,Event,CycL),{!}.
best_subject(Obj,CycL,CycL) --> rest_of(Obj).

best_subject_constituant(RES,Event,CycL,CycL) --> [].
best_subject_constituant(Target,Event,CycL,and(CycL,CycLO,'eventOccursAt'(Event,Target))) --> 
	 best_subject(Target,Event,CycLO).
   
%rest_of(txt([A|C])) --> [A|C].
rest_of(thingFor(Rest), [A|Rest], []):-notrace(meetsPos(A,CycWord,'Determiner')),!.
rest_of(thingFor(Rest), Rest, []):-Rest=[_|_].


apply_act(Predicate,Subj,Obj,ACT) :- \+ ';'(is_list(Subj),is_list(Obj)),!,ACT=..[Predicate,Subj,Obj].

apply_act(Predicate,Subj,[Obj],ACT):-!,ACT=..[Predicate,Subj,Obj].
apply_act(Predicate,Subj,[Obj|List],each(ACT,MORE)):-
      ACT=..[Predicate,Subj,Obj],apply_act(Predicate,Subj,List,MORE),!.

apply_act(Predicate,[Obj],Subj,ACT):-!,ACT=..[Predicate,Obj,Subj].
apply_act(Predicate,[Obj|List],Subj,each(ACT,MORE)):-
      ACT=..[Predicate,Obj,Subj],apply_act(Predicate,List,Subj,MORE),!.

% =======================================================
% GENFORMAT Verbs TODO
% =======================================================

do_dcg([], _ ,nil_true) --> {!},[].
do_dcg(['~a'|Template],[Subj|Blanks],(Result)) -->{!},
	    noun_phrase(Subj,More,Result),
      do_dcg(Template,Blanks,More).
do_dcg(Template,Blanks,end_true) --> Template,{!}.
do_dcg([Word|Template],[Subj|Blanks],(Result)) --> [Word],
      {append(Find,['~a'|More],Template),!},
      Find,noun_phrase(Subj,CycL,Result),
      do_dcg(More,Blanks,CycL).

/*
genFormatVerb2(Term,String,More,Subj,CycLO,noun_phrase(Object,[Predicate,Subj,Object],CycLO)):-
      append(String,['~a'],More),!.
genFormatVerb2([2,1],Predicate,String,More,Subj,CycLO,noun_phrase(Object,[Predicate,Object,Subj],CycLO)):-
      append(String,['~a'],More),!.

genFormatVerb2(Call,[Subj|Blanks],String,More,CycLO,ToDO):-!.
*/

%trasfromArgs(Args,List).
trasfromArgs('NIL',[1,2,3,4,5,6]):-!.
trasfromArgs([H],[HH]):-trasfromArg(H,HH),!.
trasfromArgs([H|T],[HH|TT]):-trasfromArg(H,HH),trasfromArgs(T,TT),!.

trasfromArg([[]|_], _).
trasfromArg([H|_],H).
trasfromArg(H,H).

placeVars([Subj],[N],Call):-integer(N),arg(N,Call,Subj),!.
placeVars([Subj|Blanks],[N|More],Call):-integer(N),arg(N,Call,Subj),placeVars(Blanks,More,Call),!.
      

      	 

%'genTemplate'('many-GenQuantRelnToType', 'TermParaphraseFn'([':ARG1', 'BestDetNbarFn'('TermParaphraseFn'('Many-NLAttr'), 'TermParaphraseFn-Constrained'('plural-Generic', ':ARG2')), 'ConditionalPhraseFn'('equals'(':ARG3', 'Thing'), 'BestNLPhraseOfStringFn'(something), 'BestDetNbarFn'('TermParaphraseFn'('BareForm-NLAttr'), 'TermParaphraseFn-Constrained'('nonSingular-Generic', ':ARG3')))]), 'EnglishParaphraseMt', v(v('BareForm-NLAttr', 'BestDetNbarFn', 'BestNLPhraseOfStringFn', 'ConditionalPhraseFn', 'Many-NLAttr', 'TermParaphraseFn', 'TermParaphraseFn-Constrained', 'Thing', 'equals', 'many-GenQuantRelnToType', 'nonSingular-Generic', 'plural-Generic', ':ARG1', ':ARG2', ':ARG3', something), A)).
%'genTemplate'('many-GenQuant', 'TermParaphraseFn'('elementOf'('BestDetNbarFn'('TermParaphraseFn'('Many-NLAttr'), 'TermParaphraseFn-Constrained'('nonSingular-Generic', ':ARG1')), ':ARG2')), 'EnglishParaphraseMt', v(v('BestDetNbarFn', 'Many-NLAttr', 'TermParaphraseFn', 'TermParaphraseFn-Constrained', 'elementOf', 'many-GenQuant', 'nonSingular-Generic', ':ARG1', ':ARG2'), A)).
%'genTemplate'('markCreated', 'ConcatenatePhrasesFn'('TermParaphraseFn-NP'(':ARG2'), 'BestHeadVerbForInitialSubjectFn'('Be-TheWord'), 'BestNLPhraseOfStringFn'([the, mark, created, by]), 'TermParaphraseFn-NP'(':ARG2')))

% =======================================================
% Intrans phrase                                                                    
verb_phrase_known(CycWord,Subj,Event,CycLO) --> 
	 [],{cvtWordPosCycL(CycWord,'Verb',CycL),
	 (('argIsa'(CycL,2,Type),Rel=..[CycL,Subj,Obj],
	 CycLO = and_iv(isa(Obj,Type),Rel) );
	 CycLO=and_iv('bodilyDoer'(Subj,Event),event_isa(Event,CycL))),varnameIdea('Intrans',Event),varnameIdea('Thing',Obj)}.

% TODO
%'agentiveNounSemTrans'('Assist-TheWord', 0, 'RegularNounFrame', 'assistingAgent'(A, ':NOUN'), 'GeneralEnglishMt', v(v('Assist-TheWord', 'RegularNounFrame', 'assistingAgent', ':NOUN'), ['?X'=A|B])).
%'agentiveNounSemTrans'('Emit-TheWord', 0, 'RegularNounFrame', ['emitter', '?X', ':NOUN']).	    
% =======================================================
%'lightVerb-TransitiveSemTrans'('Take-TheWord', 'DrugProduct', 'and'('isa'(':ACTION', 'Ingesting'), 'performedBy'(':ACTION', ':SUBJECT'), 'primaryObjectMoving'(':ACTION', ':OBJECT')), 'EnglishMt', v(v('DrugProduct', 'Ingesting', 'Take-TheWord', 'and', 'isa', 'performedBy', 'primaryObjectMoving', ':ACTION', ':OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,'lightVerb-TransitiveSemTrans'(Out)) -->
	{'lightVerb-TransitiveSemTrans'(CycWord,ObjectIsa, Template)},
	subject_isa(ObjectIsa,Object,Template,TemplateO),
     {apply_frame(TemplateO,Subj,Event,Object,Result,Out)}.

% =======================================================
%'prepReln-Action'('LosingUserRights', 'Agent', 'From-TheWord', 'fromPossessor'(':ACTION', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Agent', 'From-TheWord', 'LosingUserRights', 'fromPossessor', ':ACTION', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Action'('MovementEvent', 'PartiallyTangible', 'From-TheWord', 'fromLocation'(':ACTION', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('From-TheWord', 'MovementEvent', 'PartiallyTangible', 'fromLocation', ':ACTION', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Action'('Movement-TranslationEvent', 'SomethingExisting', 'On-TheWord', 'toLocation'(':ACTION', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Movement-TranslationEvent', 'On-TheWord', 'SomethingExisting', 'toLocation', ':ACTION', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Action'('Stealing-Generic', 'Agent', 'From-TheWord', 'victim'(':ACTION', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Agent', 'From-TheWord', 'Stealing-Generic', 'victim', ':ACTION', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Action'('TransportationEvent', 'Conveyance', 'By-TheWord', 'transporter'(':ACTION', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('By-TheWord', 'Conveyance', 'TransportationEvent', 'transporter', ':ACTION', ':OBLIQUE-OBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,'prepReln-Action'(CycLO,Out)) -->
      {'prepReln-Action'(EventIsa, SubjIsa, CycWordPrep, Template),cycQueryIsa(Subj,SubjIsa)},
	verb_phrase_event_isa(CycWord,EventIsa,Subj,Object,Event,EventMid),
      isCycWord(CycWordPrep),subject5(Result,EventMid,CycLO),
    {apply_frame(Template,Subject,Event,Object,Result,Out)}.

% =======================================================
%'prepReln-Object'('Action', 'PartiallyTangible', 'Of-TheWord', 'objectActedOn'(':NOUN', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Action', 'Of-TheWord', 'PartiallyTangible', 'objectActedOn', ':NOUN', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Object'('AnimalBodyPartType', 'Animal', 'Of-TheWord', 'anatomicalParts'(':OBLIQUE-OBJECT', ':NOUN'), 'EnglishMt', v(v('Animal', 'AnimalBodyPartType', 'Of-TheWord', 'anatomicalParts', ':NOUN', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Object'('Area', 'PartiallyTangible', 'Of-TheWord', 'areaOfObject'(':OBLIQUE-OBJECT', ':NOUN'), 'EnglishMt', v(v('Area', 'Of-TheWord', 'PartiallyTangible', 'areaOfObject', ':NOUN', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Object'('CapitalCityOfRegion', 'IndependentCountry', 'Of-TheWord', 'capitalCity'(':OBLIQUE-OBJECT', ':SUBJECT'), 'EnglishMt', v(v('CapitalCityOfRegion', 'IndependentCountry', 'Of-TheWord', 'capitalCity', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'prepReln-Object'('Communicating', 'Agent', 'By-TheWord', 'senderOfInfo'(':NOUN', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Agent', 'By-TheWord', 'Communicating', 'senderOfInfo', ':NOUN', ':OBLIQUE-OBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,'prepReln-Object'(Out)) -->
      {'prepReln-Object'(SubjIsa, ObjectIsa, CycWordPrep, Template),cycQueryIsa(Subj,SubjIsa)},
	subject_isa(ObjectIsa,Object,Template,TemplateO),
      isCycWord(CycWordPrep),subject5(Result,TemplateO,CycLO),
    {apply_frame(CycLO,Subject,Event,Object,Result,Out)}.

% =======================================================
%'verbSemTrans'('Depart-TheWord', 0, nartR('PPCompFrameFn','TransitivePPCompFrame', 'From-TheWord'), 'and'('isa'(':ACTION', 'LeavingAPlace'), 'fromLocation'(':ACTION', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'GeneralEnglishMt', v(v('Depart-TheWord', 'From-TheWord', 'LeavingAPlace', 'PPCompFrameFn', 'TransitivePPCompFrame', 'and', 'doneBy', 'fromLocation', 'isa', ':ACTION', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,verbSemTrans(Out,Extras)) -->
	 {'verbSemTrans'(CycWord, _ , NextFrame, Template)},
	    frame_template(NextFrame,Object,Result,Extras),
	 {apply_frame(Template,Subj,Event,Object,Result,Out)}.

% =======================================================
%'verbPrep-Transitive'('Ablate-TheWord', 'From-TheWord', 'and'('isa'(':ACTION', 'Ablation'), 'objectOfStateChange'(':ACTION', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT'), 'objectRemoved'(':ACTION', ':OBJECT')), 'EnglishMt', v(v('Ablate-TheWord', 'Ablation', 'From-TheWord', 'and', 'doneBy', 'isa', 'objectOfStateChange', 'objectRemoved', ':ACTION', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,'verbPrep-Transitive'(Out,Extras)) -->
   {'verbPrep-Transitive'(CycWord, CycWord2, Template)},
	 isCycWord(CycWord2),{!},subject5(Result,Out,CycLO),
   {apply_frame(Template,Subj,Event,Obj,Result,Out)}.
   
% =======================================================
%'compoundVerbSemTrans'('Give-TheWord', [off], 'TransitiveNPCompFrame', 'and'('isa'(':ACTION', 'EmittingAnObject'), 'emitter'(':ACTION', ':SUBJECT'), 'objectEmitted'(':ACTION', ':OBJECT')), 'EnglishMt', v(v('EmittingAnObject', 'Give-TheWord', 'TransitiveNPCompFrame', 'and', 'emitter', 'isa', 'objectEmitted', ':ACTION', ':OBJECT', ':SUBJECT', off), A)).
verb_phrase_known(CycWord,Subj,Event,compoundVerbSemTrans(Out,Extras)) -->
   [S],{'compoundVerbSemTrans'(CycWord, [S|String],NextFrame, Template)},
   String,frame_template(NextFrame,Object,Result,Extras),
   {apply_frame(Template,Subj,Event,Object,Result,Out)}.

% =======================================================
%'compoundSemTrans'('End-TheWord', [during], 'Verb', 'TransitiveNPCompFrame', 'endsDuring'(':SUBJECT', ':OBJECT'), 'EnglishMt', v(v('End-TheWord', 'TransitiveNPCompFrame', 'Verb', 'endsDuring', ':OBJECT', ':SUBJECT', during), A)).
verb_phrase_known(CycWord,Subj,Event,compoundSemTrans(Out,Extras)) -->
    [S],{'compoundSemTrans'(CycWord, [S|String], 'Verb', NextFrame, Template)},
   String,frame_template(NextFrame,Obj,Result,Extras),
   {apply_frame(Template,Subj,Event,Obj,Result,Out)}.

% =======================================================
%'nonCompositionalVerbSemTrans'('Separate-TheWord', 'Mixture', 'and'('isa'(':ACTION', 'SeparatingAMixture'), 'doneBy'(':ACTION', ':SUBJECT'), 'objectOfStateChange'(':ACTION', ':OBJECT')), 'EnglishMt', v(v('Mixture', 'Separate-TheWord', 'SeparatingAMixture', 'and', 'doneBy', 'isa', 'objectOfStateChange', ':ACTION', ':OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,nonCompositionalVerbSemTrans(Out)) -->
	{'nonCompositionalVerbSemTrans'(CycWord,ObjectIsa, Template)},
	subject_isa(ObjectIsa,Object,Template,TemplateO),
     {apply_frame(TemplateO,Subj,Event,Object,Result,Out)}.


% =======================================================
%'verbPrep-TransitiveTemplate'('Constructing', 'Out-Of-MWW', 'and'('isa'(':ACTION', ':DENOT'), 'inputs'(':ACTION', ':OBLIQUE-OBJECT'), 'products'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'EnglishMt', v(v('Constructing', 'Out-Of-MWW', 'and', 'doneBy', 'inputs', 'isa', 'products', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbPrep-TransitiveTemplate'('DistributionEvent', 'To-TheWord', 'and'('isa'(':ACTION', ':DENOT'), 'toLocation'(':ACTION', ':OBLIQUE-OBJECT'), 'objectMoving'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'EnglishMt', v(v('DistributionEvent', 'To-TheWord', 'and', 'doneBy', 'isa', 'objectMoving', 'toLocation', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbPrep-TransitiveTemplate'('Evaluating', 'For-TheWord', 'and'('isa'(':ACTION', ':DENOT'), 'performedBy'(':ACTION', ':SUBJECT'), 'evaluee-Direct'(':ACTION', ':OBJECT'), 'purposeInEvent'(':SUBJECT', ':ACTION', 'knowsAbout'(':SUBJECT', ':OBLIQUE-OBJECT'))), 'EnglishMt', v(v('Evaluating', 'For-TheWord', 'and', 'evaluee-Direct', 'isa', 'knowsAbout', 'performedBy', 'purposeInEvent', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbPrep-TransitiveTemplate'('FusionEvent', 'With-TheWord', 'and'('isa'(':ACTION', ':DENOT'), 'inputs'(':ACTION', ':OBJECT'), 'inputs'(':ACTION', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'EnglishMt', v(v('FusionEvent', 'With-TheWord', 'and', 'doneBy', 'inputs', 'isa', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbPrep-TransitiveTemplate'('HoldingAnObject', 'By-TheWord', 'and'('isa'(':ACTION', ':DENOT'), 'objectActedOn'(':ACTION', ':OBLIQUE-OBJECT'), 'physicalParts'(':OBJECT', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'EnglishMt', v(v('By-TheWord', 'HoldingAnObject', 'and', 'doneBy', 'isa', 'objectActedOn', 'physicalParts', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbPrep-TransitiveTemplate'('InformationRemoving', 'From-TheWord', 'and'('isa'(':ACTION', ':DENOT'), 'informationOrigin'(':ACTION', ':OBLIQUE-OBJECT'), 'infoRemoved'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'EnglishMt', v(v('From-TheWord', 'InformationRemoving', 'and', 'doneBy', 'infoRemoved', 'informationOrigin', 'isa', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,'verbPrep-TransitiveTemplate'(Out,EventMidO)) -->
	 {'verbPrep-TransitiveTemplate'(EventIsa, CycWordPrep, Template)},
      verb_phrase_event_isa(CycWord,EventIsa,Subj,Object,Event,EventMid),
      isCycWord(CycWordPrep),subject5(Result,EventMid,EventMidO),
   {apply_frame(Template,Subj,Event,Object,Result,OutD),vsubst(OutD,':DENOT',EventIsa,Out)}.
  
% =======================================================
%'verbSemTransTemplate'('InformationRemoving', nartR('PPCompFrameFn','DitransitivePPCompFrame', 'From-TheWord'), 'and'('isa'(':ACTION', ':DENOT'), 'informationOrigin'(':ACTION', ':OBLIQUE-OBJECT'), 'infoRemoved'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'GeneralEnglishMt', v(v('DitransitivePPCompFrame', 'From-TheWord', 'InformationRemoving', 'PPCompFrameFn', 'and', 'doneBy', 'infoRemoved', 'informationOrigin', 'isa', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbSemTransTemplate'('InformationRemoving', 'TransitiveNPCompFrame', 'and'('isa'(':ACTION', ':DENOT'), 'infoRemoved'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'GeneralEnglishMt', v(v('InformationRemoving', 'TransitiveNPCompFrame', 'and', 'doneBy', 'infoRemoved', 'isa', ':ACTION', ':DENOT', ':OBJECT', ':SUBJECT'), A)).
%'verbSemTransTemplate'('Killing-Biological', 'TransitiveNPCompFrame', 'and'('isa'(':ACTION', ':DENOT'), 'inputsDestroyed'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'GeneralEnglishMt', v(v('Killing-Biological', 'TransitiveNPCompFrame', 'and', 'doneBy', 'inputsDestroyed', 'isa', ':ACTION', ':DENOT', ':OBJECT', ':SUBJECT'), A)).
%'verbSemTransTemplate'('LeavingAPlace', nartR('PPCompFrameFn','TransitivePPCompFrame', 'From-TheWord'), 'and'('isa'(':ACTION', ':DENOT'), 'fromLocation'(':ACTION', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'GeneralEnglishMt', v(v('From-TheWord', 'LeavingAPlace', 'PPCompFrameFn', 'TransitivePPCompFrame', 'and', 'doneBy', 'fromLocation', 'isa', ':ACTION', ':DENOT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbSemTransTemplate'('Bartering', nartR('PPCompFrameFn','DitransitivePPCompFrame', 'For-TheWord'), 'thereExists'(A, 'thereExists'(B, 'and'('isa'(':ACTION', ':DENOT'), 'exchangers'(':ACTION', ':SUBJECT'), 'subEvents'(':ACTION', A), 'subEvents'(':ACTION', B), 'toPossessor'(B, ':SUBJECT'), 'objectOfPossessionTransfer'(A, ':OBLIQUE-OBJECT'), 'objectOfPossessionTransfer'(B, ':OBJECT'), 'fromPossessor'(A, ':SUBJECT'), 'reciprocalTransfers'(A, B)))), 'GeneralEnglishMt', v(v('Bartering', 'DitransitivePPCompFrame', 'For-TheWord', 'PPCompFrameFn', 'and', 'exchangers', 'fromPossessor', 'isa', 'objectOfPossessionTransfer', 'reciprocalTransfers', 'subEvents', 'thereExists', 'toPossessor', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), ['?T1'=A, '?T2'=B|C])).
%'verbSemTransTemplate'('CarryingWhileLocomoting', nartR('PPCompFrameFn','DitransitivePPCompFrame', 'By-TheWord'), 'and'('isa'(':ACTION', ':DENOT'), 'transportees'(':ACTION', ':OBJECT'), 'physicalParts'(':OBJECT', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT'), 'objectsInContact'(':ACTION', ':OBLIQUE-OBJECT', ':SUBJECT')), 'GeneralEnglishMt', v(v('By-TheWord', 'CarryingWhileLocomoting', 'DitransitivePPCompFrame', 'PPCompFrameFn', 'and', 'doneBy', 'isa', 'objectsInContact', 'physicalParts', 'transportees', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,verbSemTransTemplate(Out,EventMidO)) -->
      {'verbSemTransTemplate'(EventIsa,NextFrame, Template)},
	 verb_phrase_event_isa(CycWord,EventIsa,Subj,Object,Event,EventMid),
      frame_template(NextFrame,Obj,Result,Extras),
   {apply_frame(Template,Subj,Event,Object,Result,OutD),vsubst(OutD,':DENOT',EventIsa,Out)}.

verb_phrase_known(CycWord,Subj,Event,auxV(Out)) --> isPOS('AuxVerb', _),
      verb_phrase_known(CycWord,Subj,Event,Out).


% =======================================================
% verb_phrase_event_isa
% =======================================================
verb_phrase_event_isa(CycWord,EventIsa,Subj,Object,Event,Out) -->
      {cycWordForISA(CycWord,EventIsa)},verb_phrase_known(CycWord,Subj,Event,Out).


% =======================================================
% frame_template
% =======================================================



frame_template('TransitiveNPCompFrame',Obj,Result,Extras) --> noun_phrase(Obj,true,Extras). 
%$DitransitivePPCompFrame','TransitivePPCompFrame'
frame_template(nartR('PPCompFrameFn',_ ,CycPrep),Obj,Result,Extras) --> isCycWord(CycPrep),{!},best_subject(Result,true,Extras).
frame_template(nartR('PPCompFrameFn',_ ,CycPrep),Obj,Result,Extras) --> noun_phrase(Obj,true,Mid),isCycWord(CycPrep),{!},best_subject(Result,Mid,Extras).
frame_template('RegularAdjFrame',Subj,Result,Extras) -->noun_phrase(Subj,true,Extras).


% ==========================================================
% String to String
% ==========================================================

:-disable_body_textstr.
% % :- enable_body_textstr.


removeRepeats1([],[]):-!.
removeRepeats1([H|T],[HH|TT]):- stringToString(H,HH),!,removeRepeats1(T,TT).
removeRepeats1([H,H1|Rest],Out):-once(toLowercase(H,HL)),H1=HL,!,removeRepeats1([H|Rest],Out).
removeRepeats1([H|Rest],[H|Out]):-removeRepeats1(Rest,Out).
removeRepeats1(X,X).

removeRepeats2(X,X):-!.

removeRepeats2(X,O):-append(L,R,X),
	    append([S,O|Me],LL,L),
	    append([S,O|Me],RR,R),!,
	    flatten([[S,O|Me],LL,RR],O).

stringToString(Before,After):-'abbreviationForString'(After, Before).

simplifyLF(Result,Result):-!.

cycQueryIsa(X,Y):-fail,writeq(cycQueryIsa(X,Y)),nl.


/*

% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================

% String Based
'genTemplate'('abnormal', 'ConcatenatePhrasesFn'('TermParaphraseFn'(':ARG1'), 'BestNLPhraseOfStringFn'([-, with, the, exception, of]), 'TermParaphraseFn'(':ARG2')), 'EnglishParaphraseMt', v(v('BestNLPhraseOfStringFn', 'ConcatenatePhrasesFn', 'TermParaphraseFn', 'abnormal', -, ':ARG1', ':ARG2', exception, of, the, with), A)).
'genTemplate-Constrained'('arg1Isa', 'isa'(':ARG1', 'BinaryPredicate'), 'TermParaphraseFn'('implies'('thereExists'(A, [':ARG1', B, A]), 'ConcatenatePhrasesFn'('TermParaphraseFn-NP'(B), 'BestNLPhraseOfStringFn'([must, be]), 'BestDetNbarFn-Indefinite'('TermParaphraseFn-Constrained'('nonPlural-Generic', ':ARG2'))))), 'EnglishParaphraseMt', v(v('BestDetNbarFn-Indefinite', 'BestNLPhraseOfStringFn', 'BinaryPredicate', 'ConcatenatePhrasesFn', 'TermParaphraseFn', 'TermParaphraseFn-Constrained', 'TermParaphraseFn-NP', 'arg1Isa', 'implies', 'isa', 'nonPlural-Generic', 'thereExists', ':ARG1', ':ARG2', be, must), ['?Y'=A, '?X'=B|C])).
'genQuestion'('genls', 1, [what, kinds, of, '~a', are, there], [[2, ':NON-SINGULAR-GENERIC']], 'EnglishParaphraseMt', v(v('genls', ':NON-SINGULAR-GENERIC', are, kinds, of, there, what, '~a'), A)).
'genQuestion'('hasAttributes', 2, ['What', attributes, does, '~a', 'have?'], [1], 'EnglishParaphraseMt', v(v('hasAttributes', 'What', attributes, does, 'have?', '~a'), A)).
%'assertTemplate-Reln'(TemplateType,Predicate, Match, Template).
	    %InfinitivalVPTemplate'
	    % 'aspectPerfect'
	    %[have, not, 'PerfectiveVPTemplate'(':VP-CONT')]
	    %'NLNegFn'('NotVP-NLAttr', 'aspectPerfect'(':VP-CONT'))


% meetsParam(transverb,String,CycWord):-genFormatAccess(String,CycWord).

%genFormat(Verb,X,'NIL'):-'genFormat'(Verb,X, _).


'genFormat'('abbreviationString-PN', abbreviation, 'NIL', 'EnglishParaphraseMt', v(v('abbreviationString-PN', 'NIL', abbreviation), A)).
'genFormat'('abbreviationString-PN', ['~a', is, the, abbreviated, form, of, the, name, for, '~a'], [[2, ':QUOTE'], 1], 'EnglishParaphraseMt', v(v('abbreviationString-PN', ':QUOTE', abbreviated, for, form, is, name, of, the, '~a'), A)).
'genFormat'('adjSemTrans', ['~a', is, the, semantic, translation, of, word, sense, number, '~a', of, '~a', with, the, subcategorization, frame, '~a'], [[4, ':QUOTE'], 2, [1, ':QUOTE'], [3, ':QUOTE']], 'EnglishParaphraseMt', v(v('adjSemTrans', ':QUOTE', frame, is, number, of, semantic, sense, subcategorization, the, translation, with, word, '~a'), A)).
'genFormat'('adjSemTrans-Restricted', [the, semantic, translation, of, word, sense, number, '~a', of, '~a', (','), when, modifying, '~a', with, the, subcategorization, frame, '~a', (','), is, '~a'], [2, [1, ':QUOTE'], [4, ':A'], [3, ':QUOTE'], 5], 'EnglishParaphraseMt', v(v('adjSemTrans-Restricted', (','), ':A', ':QUOTE', frame, is, modifying, number, of, semantic, sense, subcategorization, the, translation, when, with, word, '~a'), A)).
'genFormat'('Area1023', ['Troi', '\'', s, 'Quarters'], 'NIL', 'EnglishParaphraseMt', v(v('Area1023', '\'', 'NIL', 'Quarters', 'Troi', s), A)).
'genFormat-ArgFixed'('SubcollectionOfWithRelationFromTypeFn', 2, 'surfaceParts', ['~A', on, '~A'], [[1, ':MASS-NUMBER', ':PLURAL', ':GERUND'], [3, ':PLURAL', ':MASS-NUMBER']], 'EnglishParaphraseMt', v(v('SubcollectionOfWithRelationFromTypeFn', 'surfaceParts', ':GERUND', ':MASS-NUMBER', ':PLURAL', on, '~A'), A)).
'genFormat-ArgFixed'('SubcollectionOfWithRelationToFn', 2, 'containsInformationAbout', ['~A', about, '~A'], [[1, ':MASS-NUMBER', ':PLURAL', ':GERUND'], 3], 'EnglishParaphraseMt', v(v('SubcollectionOfWithRelationToFn', 'containsInformationAbout', ':GERUND', ':MASS-NUMBER', ':PLURAL', about, '~A'), A)).
'genFormat-ArgFixed'('SubcollectionOfWithRelationToFn', 2, 'eventOccursAt', ['~A', in, '~A'], [[1, ':MASS-NUMBER', ':PLURAL', ':GERUND'], 3], 'EnglishParaphraseMt', v(v('SubcollectionOfWithRelationToFn', 'eventOccursAt', ':GERUND', ':MASS-NUMBER', ':PLURAL', in, '~A'), A)).
'genFormat-Precise'('synonymousExternalConcept', [the, 'Cyc', concept, '~s', is, synonymous, with, the, concept, named, by, '~s', in, the, external, data, source, '~a'], [1, 3, 2], 'EnglishParaphraseMt', v(v('synonymousExternalConcept', 'Cyc', by, concept, data, external, in, is, named, source, synonymous, the, with, '~a', '~s'), A)).
'genFormat-Precise'('tastes', [the, agent, '~a', can, taste, '~a'], 'NIL', 'EnglishParaphraseMt', v(v('tastes', 'NIL', agent, can, taste, the, '~a'), A)).
'genFormat-Precise'('temporalBoundsIntersect', [the, temporal, interval, of, '~a', intersects, the, temporal, interval, of, '~a'], 'NIL', 'EnglishParaphraseMt', v(v('temporalBoundsIntersect', 'NIL', intersects, interval, of, temporal, the, '~a'), A)).



'formalityOfWS'('Aussie-TheWord', 'ProperCountNoun', 0, 'InformalSpeech', 'GeneralEnglishMt', v(v('Aussie-TheWord', 'InformalSpeech', 'ProperCountNoun'), A)).
'formalityOfWS'('Babe-TheWord', 'SimpleNoun', 1, 'InformalSpeech', 'GeneralEnglishMt', v(v('Babe-TheWord', 'InformalSpeech', 'SimpleNoun'), A)).
'politenessOfWS'('Cock-TheWord', 'SimpleNoun', 1, 'VulgarSpeech', 'GeneralEnglishMt', v(v('Cock-TheWord', 'SimpleNoun', 'VulgarSpeech'), A)).

% ==========================================================
% WordsList Heuristics
% ==========================================================
'determinerAgreement'('A-Dozen-MWW', 'plural-Generic', 'EnglishMt', v(v('A-Dozen-MWW', 'plural-Generic'), A)).

'denotesArgInReln'('Acquaint-TheWord', 'CountNoun', 'acquaintedWith', 2, 'GeneralEnglishMt', v(v('Acquaint-TheWord', 'CountNoun', 'acquaintedWith'), A)).
'generateArgWithOutsideScope'('several-GenQuantRelnToType', 2, 'ParaphraseMt', v(v('several-GenQuantRelnToType'), A)).
'generateQuantOverArg'('few-GenQuantRelnFrom', 'Few-NLAttr', 3, 'ParaphraseMt', v(v('Few-NLAttr', 'few-GenQuantRelnFrom'), A)).
'genNatTerm-ArgLast'('PureFn', [pure], 'Noun', 'EnglishMt', v(v('Noun', 'PureFn', pure), A)).
'genNatTerm-compoundString'('AttemptingFn', 'Try-TheWord', [to], 'Verb', 'infinitive', 'EnglishMt', v(v('AttemptingFn', 'Try-TheWord', 'Verb', 'infinitive', to), A)).
'genNatTerm-multiWordString'('TreatmentFn', 'NIL', 'Treatment-TheWord', 'MassNoun', 'nonPlural-Generic', 'EnglishMt', v(v('MassNoun', 'Treatment-TheWord', 'TreatmentFn', 'nonPlural-Generic', 'NIL'), A)).
%'headsPhraseOfType'('Pronoun', 'Noun', 'GeneralLexiconMt', v(v('Noun', 'Pronoun'), A)).
'ncRuleConstraint'('AttackingDogs-NCR', 'NCGenlsConstraintFn'('TheNCModifier', 'Event'), 'GeneralLexiconMt', v(v('AttackingDogs-NCR', 'Event', 'NCGenlsConstraintFn', 'TheNCModifier'), A)).
'ncRuleLabel'('WaterSolution-NCR', [water, solution], 'GeneralLexiconMt', v(v('WaterSolution-NCR', solution, water), A)).
'ncRuleTemplate'('AnimalPopulations-NCR', 'SubcollectionOfWithRelationToTypeFn'('TheNCHead', 'groupMembers', 'TheNCModifier'), 'GeneralLexiconMt', v(v('AnimalPopulations-NCR', 'SubcollectionOfWithRelationToTypeFn', 'TheNCHead', 'TheNCModifier', 'groupMembers'), A)).
'posForTemplateCategory'('Verb', 'ProgressiveVPTemplate', 'EnglishTemplateMt', v(v('ProgressiveVPTemplate', 'Verb'), A)).
'posOfPhraseType'('NounPhrase', 'Noun', 'GeneralLexiconMt', v(v('Noun', 'NounPhrase'), A)).
'posOfPhraseType'('PhraseFn'(A), A, 'GeneralLexiconMt', v(v('PhraseFn', '$VAR'), ['VAR1'=A|B])).
'posPredForTemplateCategory'('presentParticiple', 'ProgressiveVPTemplate', 'EnglishTemplateMt', v(v('ProgressiveVPTemplate', 'presentParticiple'), A)).
%'prepCollocation'('Beset-TheWord', 'Adjective', 'By-TheWord').      
'prepCollocation'('Wrangle-TheWord', 'Verb', 'With-TheWord', 'EnglishMt', v(v('Verb', 'With-TheWord', 'Wrangle-TheWord'), A)).
'relationIndicators'('ailmentConditionAffects', 'Infect-TheWord', 'SimpleNoun', 'EnglishMt', v(v('Infect-TheWord', 'SimpleNoun', 'ailmentConditionAffects'), A)).
'requiredActorSlots'('MonetaryExchangeOfUserRights', 'buyer', 'HumanActivitiesMt', v(v('MonetaryExchangeOfUserRights', 'buyer'), A)).
%'semTransArg'('adjSemTrans', 4, 'GeneralLexiconMt', v(v('adjSemTrans'), A)).
%'semTransArg'('adjSemTrans-Restricted', 5, 'GeneralLexiconMt', v(v('adjSemTrans-Restricted'), A)).
'subcatFrame'('Argue-TheWord', 'Verb', 0, 'TransitiveNPCompFrame', 'GeneralEnglishMt', v(v('Argue-TheWord', 'TransitiveNPCompFrame', 'Verb'), A)).
'subcatFrameArity'('Post-NounPhraseModifyingFrame', 1, 'GeneralLexiconMt', v(v('Post-NounPhraseModifyingFrame'), A)).
'subcatFrameDependentConstraint'('TransitiveNPCompFrame', 1, 'PhraseFn'('Noun'), 'GeneralLexiconMt', v(v('Noun', 'PhraseFn', 'TransitiveNPCompFrame'), A)).
'subcatFrameDependentKeyword'('Post-NounPhraseModifyingFrame', 1, ':OBJECT', 'GeneralLexiconMt', v(v('Post-NounPhraseModifyingFrame', ':OBJECT'), A)).
'subcatFrameKeywords'('MiddleVoiceFrame', ':ACTION', 'InferencePSC', v(v('MiddleVoiceFrame', ':ACTION'), A)).

'psRuleArity'('PSRule-AdjPFromAdj', 1, 'EnglishLexiconMt', v(v('PSRule-AdjPFromAdj'), A)).
'psRuleArity'('PSRule-AdvP-AdvPAdvP', 2, 'EnglishLexiconMt', v(v('PSRule-AdvP-AdvPAdvP'), A)).
'psRuleCategory'('PSRule-V-VAdvP', 'Verb', 'EnglishLexiconMt', v(v('PSRule-V-VAdvP', 'Verb'), A)).
'psRuleConstraint'('PSRule-AdjPFromAdj', 'ConstituentTypeConstraintFn'(1, 'Adjective'), 'EnglishLexiconMt', v(v('Adjective', 'ConstituentTypeConstraintFn', 'PSRule-AdjPFromAdj'), A)).
'psRuleExample'('PSRule-VbarVComps', [likes, emus], 'EnglishLexiconMt', v(v('PSRule-VbarVComps', emus, likes), A)).
'psRuleSemanticsFromDtr'('PSRule-DbarFromDet', 1, 'EnglishLexiconMt', v(v('PSRule-DbarFromDet'), A)).
'psRuleSemanticsHandler'('PSRule-NP-DetNbar', 'PSP-SEMX-FOR-DET-NBAR', 'EnglishLexiconMt', v(v('PSRule-NP-DetNbar', 'PSP-SEMX-FOR-DET-NBAR'), A)).
'psRuleSyntacticHeadDtr'('PSRule-AdjPFromAdj', 1, 'EnglishLexiconMt', v(v('PSRule-AdjPFromAdj'), A)).
'psRuleTemplateBindings'('PSRule-V-VAdvP', 'PSBindingFn'(1, ':ACTION'), 'EnglishLexiconMt', v(v('PSBindingFn', 'PSRule-V-VAdvP', ':ACTION'), A)).
'psRuleTemplateDtr'('PSRule-AdjPFromAdj', 1, 'EnglishLexiconMt', v(v('PSRule-AdjPFromAdj'), A)).

*/
/*===================================================================
Convert S-Expression originating from user to a Prolog Clause representing the surface level

Recursively creates a Prolog term based on the S-Expression to be done after compiler
                                                 
Examples:

| ?- sterm_to_pterm([a,b],Pterm).
Pterm = a(b)

| ?- sterm_to_pterm([a,[b]],Pterm).    %Note:  This is a special Case
Pterm = a(b)

| ?- sterm_to_pterm([holds,X,Y,Z],Pterm).    %This allows Hilog terms to be Converted
Pterm = _h76(_h90, _h104)                    

| ?- sterm_to_pterm([X,Y,Z],Pterm).   %But still works in normal places
Pterm = _h76(_h90, _h104)                    

| ?- sterm_to_pterm(['AssignmentFn',X,[Y,Z]],Pterm).                                
'Pterm = 'AssignmentFn'(_h84,[_h102, _h116])'

*/

sterm_to_pterm(VAR,VAR):-isSlot(VAR),!.
sterm_to_pterm([VAR],VAR):-isSlot(VAR),!.
sterm_to_pterm([X],Y):-!,nonvar(X),sterm_to_pterm(X,Y).

sterm_to_pterm([S|TERM],PTERM):-isSlot(S),
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[holds,S|PLIST].

sterm_to_pterm([S|TERM],PTERM):-number(S),!,
            sterm_to_pterm_list([S|TERM],PTERM).            
	    
sterm_to_pterm([S|TERM],PTERM):-nonvar(S),atomic(S),!,
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[S|PLIST].

sterm_to_pterm([S|TERM],PTERM):-!,  atomic(S),
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[holds,S|PLIST].

sterm_to_pterm(VAR,VAR):-!.

sterm_to_pterm_list(VAR,VAR):-isSlot(VAR),!.
sterm_to_pterm_list([],[]):-!.
sterm_to_pterm_list([S|STERM],[P|PTERM]):-!,
              sterm_to_pterm(S,P),
              sterm_to_pterm_list(STERM,PTERM).
sterm_to_pterm_list(VAR,[VAR]).


atom_junct(Atom,Words):-!,into_lexical_segs(Atom,Words),!.

atom_junct(Atom,Words):-
   concat_atom(Words1,' ',Atom),
   atom_junct2(Words1,Words),!.

atom_junct2([],[]).
atom_junct2([W|S],[A,Mark|Words]):- member(Mark,['.',',','?']),atom_concat(A,Mark,W),not(A=''),!,atom_junct2(S,Words).
atom_junct2([W|S],[Mark,A|Words]):- member(Mark,['.',',','?']),atom_concat(Mark,A,W),not(A=''),!,atom_junct2(S,Words).
atom_junct2([W|S],[W|Words]):-atom_junct2(S,Words).

% :- include(logicmoo(vworld/moo_footer)).

%:- module_predicates_are_exported(parser_e2c).
%:- module_meta_predicates_are_transparent(parser_e2c).

% :-module(parser_e2c).

%:- debug, make:list_undefined. 
 % speechPartPreds_transitive(X,Y).
%:- parser_e2c:prolog.

%:- user:prolog.

%:- dm1.






skip_ops:-  system:((
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-'))).


:- set_prolog_flag(do_renames,false).

hard_words(X):-member(X,[
tricorder,
teleport,
recieved,
picard,
located,
incurred,
ensigns,
dilithium,
betazoid,
alexander,
'READY-',
'INVIS',
'Enterprise\'s',
worf,
wimpy,
vulcan,
turbolift,
synthehol,
starfleet,
somwhere,
rozhenko,
romulan,
riker,
phasors,
phasers,
phaser,
mudMaxHitPoints,
mudLevelOf,
mudBareHandDamage,
marketpace,
managed,
holodeck,
geordi,
ferengi,
embedded,
easils,
damageSizeDice,
damageNumberDice,
chargeRemaining,
chargeCapacity,
bustling,
burgandy,
\,
+,
***,
********,
********************,
*********************,
'|',
'you\'ve',
'worf\'s',
'Worf',
'WIMPY',
'WeaponBlasting',
'USS',
'Turbolift',
'Troi\'s',
'Tricorder',
'there\'s',
'Synthehol',
'Starfleet\'s',
'ship\'s',
'she\'s',
'says:\n\n***************************************************\n',
'riker\'s',
'picard\'s',
'Phaser',
'people\'s',
'NPC',
'NOTRACK',
'NOSUMMON',
'NOSLEEP',
'NOCHARM',
'NOBLIND',
'NOBASH',
'NOBACKSTAB',
'NCC',
'Logged',
'Lemonade\'Prune',
'it\'s',
'holodeck\'s',
'Holodeck',
'he\'s',
'_______',
'____/___',
'___',
'_/',
'_',
'\n***************************************************\n',
'\n',
'9d9',
'8d8',
'800',
'5000',
'4000',
'3400',
'2600',
'20d20',
'18d18',
'1701',
'1600',
'1400',
'12d12',
'10d10',
'-ENTER',
'+mudToHitArmorClass0',
'*\n**************************************************',
'*\n***********************************************',
'*\n',
'***(_',
'$PunchingSomething',
'$LightingDevice',
'"']).

:-kb_shared(properNameStrings/2).
properNames(['Geordi',
'Guinan',
'LaForge',
'Troi',
'Picard',
'Rozhenko',
'Riker',
'O\'Brien',
'Crusher',
'Data',
'CycLBot',
'CycBot1',
'CycBot',
'Romulan',
'Starfleet',
'Klingon',
'Ferengi']).




end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
=========================================
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
=========================================
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
=========================================
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
=========================================
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
=========================================

/*
   
cycPred('subcatFrameKeywords').
cycPred('nounPrep').
cycPred('verbPrep-Transitive').
cycPred('prepReln-Action').
cycPred('prepReln-Obj').
cycPred('preferredGenUnit').
cycPred('prepSemTrans').
cycPred('subcatFrame').
cycPred('verbSemTrans').
cycPred('genTemplate').
cycPred('multiWordSemTrans').
cycPred('denotationPlaceholder').
cycPred('denotesArgInReln').
cycPred('nounSemTrans').
cycPred('multiWordStringDenotesArgInReln').
cycPred('headMedialString').
cycPred('verbPrep-TransitiveTemplate').
cycPred('agentiveNounSemTrans').
cycPred('nonCompositionalVerbSemTrans').
cycPred('abbreviationForMultiWordString').
cycPred('abbreviationForLexicalWord').
cycPred('formalityOfWS').
cycPred('relationIndicators-Strong').
cycPred('verbSemTransPartial').
cycPred('implies').
cycPred('verbPrep-Passive').
cycPred('compoundStringDenotesArgInReln').
cycPred('genNatTerm-multiWordString').
cycPred('hyphenString').
cycPred('adjSemTrans').
cycPred('properNounSemTrans').
cycPred('morphologicallyDerivedFrom').
cycPred('politenessOfWS').
cycPred('genNatTerm-compoundString').
cycPred('genPhrase').
cycPred('abbreviationForCompoundString').
cycPred('posForms').
cycPred('plural').
cycPred('synonymousExternalConcept').
cycPred('lightVerb-TransitiveSemTrans').
cycPred('genTemplate-Constrained').
cycPred('expansion').
cycPred('compoundSemTrans').
cycPred('genStringAssertion-Old').
cycPred('compoundVerbSemTrans').
cycPred('adjSemTrans-Restricted').
cycPred('massNounSemTrans').
cycPred('morphologicalComposition').
cycPred('determinerAgreement').
cycPred('TT-lexMap').
cycPred('TT-lex').
cycPred('TTPred-denotation').
cycPred('TTPred-thetaRoleFeat-Frequent').
cycPred('TTPred-thetaRoleFeat-Trademark').
cycPred('TTPred-thetaRoleFeat-Informal').
cycPred('TTPred-thetaRoleFeat-LiteraryTechnical').
cycPred('TTPred-thetaRoleFeat-DefiniteArticle').
cycPred('TTPred-thetaRole').
cycPred('TTPred-thetaRoleFeat-ZeroArticle').
cycPred('TTPred-thetaRoleFeat-Slang').
cycPred('TTPred-thetaRoleFeat-Dated').
cycPred('TTPred-thetaRoleFeat-MassNoun').
cycPred('TTPred-thetaRoleFeat-Clause2Only').
cycPred('TTPred-thetaRoleFeat-Coordinator').
cycPred('TTPred-thetaRoleFeat-Abstract').
cycPred('TTPred-thetaRoleFeat-Plural').
cycPred('TTPred-thetaRoleFeat-CommonInflection').
cycPred('TTPred-thetaRoleFeat-AmericanEnglish').
cycPred('TTPred-thetaRoleFeat-BritishEnglish').
cycPred('TTPred-thetaRoleFeat-Infrequent').
cycPred('TTPred-thetaRoleSubcat-Indicative').
cycPred('TTPred-thetaRoleFeat-OtherRegionalDialect').
cycPred('TTPred-thetaRoleFeat-Feminine').
cycPred('TTPred-thetaRoleSubcat-Infinitive').
cycPred('TTPred-thetaRoleSubcat-PresentParticiple').
cycPred('TTPred-thetaRoleFeat-Humorous').
cycPred('TTPred-thetaRoleFeat-Masculine').
cycPred('TTPred-thetaRoleFeat-DoNotReorder').
cycPred('TTPred-thetaRoleFeat-ProgressiveNontaker').
cycPred('TTPred-thetaRoleFeat-BEEBlock').
cycPred('TTPred-thetaRoleFeat-ZBZBlock').
cycPred('TTPred-thetaRoleFeat-BWWBlock').
cycPred('TTPred-thetaRoleFeat-WBWBlock').
cycPred('TTPred-thetaRoleFeat-BZZBlock').
cycPred('TTPred-thetaRoleFeat-TranslationOnly').
cycPred('TTPred-thetaRoleFeat-PreposedAdjective').
cycPred('TTPred-thetaRoleFeat-Singular').
cycPred('TTPred-thetaRoleFeat-Derogatory').
cycPred('TTPred-thetaRoleFeat-Attributive').
cycPred('TTPred-thetaRoleSubcat-Subjunctive').
cycPred('TTPred-thetaRoleFeat-Predicative').
cycPred('TTPred-thetaRoleFeat-Canadian').
cycPred('TTPred-thetaRoleFeat-Tutoiement').
cycPred('TTPred-thetaRoleFeat-Etre').
cycPred(speechPartPreds_transitive).
cycPred('genlPreds').
cycPred('afterRemoving').
cycPred('isa').
cycPred('quotedArgument').
cycPred('notAssertible').
cycPred('genKeyword').
cycPred('basicSpeechPartPred').
cycPred('genFormat').
cycPred('arg2Format').
cycPred('argFormat').
cycPred('comparativeDegree').
cycPred('regularSuffix').
cycPred('paraphraseCoercionAllowedFrom').
cycPred('arg1Format').
cycPred('posPredForTemplateCategory').
cycPred('superlativeDegree').
cycPred('relationAllInstance').
cycPred('backchainForbidden').
cycPred('typedGenlPreds').
cycPred('transitiveViaArg').
cycPred('functionalInArgs').
cycPred('ncRuleLabel').
cycPred('keRequirementPreds').
cycPred('ncRuleTemplate').
cycPred('ncRuleConstraint').
cycPred('defaultCorrespondingRoles').
cycPred('headsPhraseOfType').
cycPred('posOfPhraseType').
cycPred('argGenl').
cycPred('barLevelOfPhraseType').
cycPred('transitiveViaArgInverse').
cycPred('nlPhraseTypeForTemplateCategory').
cycPred('denotatumArg').
cycPred('placeName-ShortForm').
cycPred('abnormal').
cycPred('scientificName').
cycPred('keWeakSuggestionPreds').
cycPred('sharedNotes').
cycPred('nameString').
cycPred('termStrings').
cycPred('interArgReln1-3').
cycPred('termStrings-GuessedFromName').
cycPred('acronymString').
cycPred('genFormat-Precise').
cycPred('completeExtentKnown').
cycPred('preferredTermStrings').
cycPred('initialismString').
cycPred('abbreviationString-PN').
cycPred('formerName').
cycPred('preferredNameString').
cycPred('countryName-LongForm').
cycPred('countryName-ShortForm').
cycPred('keStrongSuggestionPreds').
cycPred('arg5Isa').
cycPred('semTransArg').
cycPred('assertTemplate-Reln').
cycPred('interArgIsa3-4').
cycPred('interArgIsa4-5').
cycPred('arg4Format').
cycPred('termPOS-Strings').
cycPred('adjSemTransTemplate').
cycPred('salientAssertions').
cycPred('verbSemTransTemplate').
cycPred('semTransPredForPOS').
cycPred('arg5Format').
cycPred('not').
cycPred('phraseTemplateArg').
cycPred('ist').
cycPred('requiredArg1Pred').
cycPred('genPreferredKeyword').
cycPred('genQuestion').
cycPred('genExpansion').
cycPred('genStringAssertion').
cycPred('genFormat-ArgFixed').
cycPred('genStringAssertion-Precise').
cycPred('genFormat-NP').
cycPred('genNatTerm-ArgLast').
cycPred('genCodeSupport').
cycPred('relationAll').
cycPred('unitOfMeasurePrefixString').
cycPred('generateQuantOverArg').
cycPred('interArgIsa1-3').
cycPred('generateArgWithOutsideScope').
cycPred('interArgIsa1-2').
cycPred('formalityOfWS-New').
cycPred('languageOfLexicon').
cycPred('arg6Isa').
cycPred('abbreviationForString').
cycPred('instancesDontNeedLexification').
cycPred('lexicalWordTypeForLanguage').
cycPred('psRuleTemplateBindings').
cycPred('genlFuncs').
cycPred('resultIsaArgIsa').
cycPred('reformulatorEquiv').
cycPred('reformulatorEquals').
cycPred('reformulationPrecondition').
cycPred('subcatFrameDependentConstraint').
cycPred('subcatFrameArity').
cycPred('subcatFrameDependentKeyword').
cycPred('subcatFrameExample').
cycPred('prefixString').
cycPred('derivedUsingPrefix').
cycPred('relationAllExists').
cycPred('baseForm').
cycPred('derivedUsingSuffix').
cycPred('negationInverse').
cycPred('relationExistsAll').
cycPred('posBaseForms').
cycPred('suffixString').
cycPred('phoneticVariantOfPrefix').
cycPred('phoneticVariantOfSuffix').
cycPred('relationAllExistsMin').
cycPred('derivationalAffixBasePOS').
cycPred('affixRuleArity').
cycPred('affixRuleCategorialConstraint').
cycPred('derivationalAffixResultPOS').
cycPred('affixRuleTypeMorphemePosition').
cycPred('etymologicalVariantOfSuffix').
cycPred('variantOfSuffix').
cycPred('relationInstanceAll').
cycPred('genls').
cycPred('disjointWith').
cycPred('coExtensional').
cycPred('partitionedInto').
cycPred('notAssertibleCollection').
cycPred('typeGenls').
cycPred('resultIsaArg').
cycPred('resultGenlArg').
cycPred('arityMin').
cycPred('arityMax').
cycPred('argAndRestIsa').
cycPred('functionCorrespondingPredicate-Canonical').
cycPred('argsIsa').
cycPred('evaluationDefn').
cycPred('psRuleArity').
cycPred('psRuleSyntacticHeadDtr').
cycPred('psRuleTemplateDtr').
cycPred('psRuleConstraint').
cycPred('psRuleCategory').
cycPred('psRuleExample').
cycPred('psRuleSemanticsHandler').
cycPred('keConsiderationPreds').
cycPred('genlAttributes').
cycPred('negationAttribute').
cycPred('TTPred-thoughtTreasureToCyc').
cycPred('rewriteOf').
cycPred('TTPred-cycToThoughtTreasure').
cycPred('psRuleSemanticsFromDtr').
cycPred('posForTemplateCategory').
cycPred('backchainRequired').
cycPred('defnIff').
cycPred('completeCollectionExtent').
cycPred('quotedCollection').
cycPred('keClarifyingCollection').
cycPred('relationAllExistsCount').
cycPred('rolesForEventType').
cycPred('defaultReformulationDirectionInModeForPred').
cycPred('keStrongConsiderationPreds').
cycPred('keStrongSuggestion').
cycPred('requiredActorSlots').
cycPred('collectionUnion').
cycPred('keCommonQueryPredForInstances').
cycPred('subjectRoles').
cycPred('trueRule').
cycPred('TTPred-processor-of').
cycPred('TTPred-create').
cycPred('TTPred-eng-aux-verb-of').
cycPred('TTPred-cpart-of').
cycPred('TTPred-capital-of').
cycPred('TTPred-polity-of').
cycPred('TTPred-first-surname-of').
cycPred('TTPred-performed-in').
cycPred('TTPred-part-of').
cycPred('TTPred-processors-of').
cycPred('TTPred-owner-of').
cycPred('TTPred-cost-of').
cycPred('TTPred-first-name-of').
cycPred('TTPred-population-of').
cycPred('TTPred-antonym-of').
cycPred('TTPred-time-range-of').
cycPred('TTPred-event02-of').
cycPred('TTPred-fr-infl-tense-of').
cycPred('TTPred-role07-of').
cycPred('TTPred-stereo').
cycPred('TTPred-NTSC').
cycPred('TTPred-phone-prefix-of').
cycPred('TTPred-variant-of').
cycPred('TTPred-r1').
cycPred('TTPred-time-off').
cycPred('TTPred-diminutive-of').
cycPred('TTPred-value-of').
cycPred('TTPred-result-of').
cycPred('TTPred-eng-aux-tense-of').
cycPred('TTPred-fr-subjunctive-of').
cycPred('TTPred-street-of').
cycPred('TTPred-feed-of').
cycPred('TTPred-role03-script-of').
cycPred('TTPred-atomic-weight-of').
cycPred('TTPred-political-affiliation-of').
cycPred('TTPred-dark-brown').
cycPred('TTPred-violet').
cycPred('TTPred-SECAM').
cycPred('TTPred-fr').
cycPred('TTPred-product-of').
cycPred('TTPred-computer-bus-of').
cycPred('TTPred-event03-of').
cycPred('TTPred-attend-twelfth-grade').
cycPred('TTPred-inside').
cycPred('TTPred-street-number-of').
cycPred('TTPred-period-of').
cycPred('TTPred-role02-of').
cycPred('TTPred-MIPS-of').
cycPred('TTPred-attr-occultist').
cycPred('TTPred-canonical-factor-of').
cycPred('TTPred-diploma-of').
cycPred('TTPred-event04-of').
cycPred('TTPred-duration-of').
cycPred('TTPred-specialty-of').
cycPred('TTPred-attr-rel-value').
cycPred('TTPred-eng-infl-mood-of').
cycPred('TTPred-fr-size-of').
cycPred('TTPred-us').
cycPred('TTPred-producer-of').
cycPred('TTPred-second-name-of').
cycPred('TTPred-emotion-of').
cycPred('TTPred-event01-of').
cycPred('TTPred-silk').
cycPred('TTPred-max-value-of').
cycPred('TTPred-preppy').
cycPred('TTPred-frequency-of').
cycPred('TTPred-attend-kindergarten').
cycPred('TTPred-politically-ultraconservative').
cycPred('TTPred-other-language-of').
cycPred('TTPred-SPECintRate92-of').
cycPred('TTPred-role02-script-of').
cycPred('TTPred-event05-of').
cycPred('TTPred-slots-of').
cycPred('TTPred-attr-South-Korean').
cycPred('TTPred-coordinates-of').
cycPred('TTPred-goal-of').
cycPred('TTPred-incorporated-in').
cycPred('TTPred-green').
cycPred('TTPred-male').
cycPred('TTPred-clothing-middle').
cycPred('TTPred-female').
cycPred('TTPred-href').
cycPred('TTPred-red').
cycPred('TTPred-r8').
cycPred('TTPred-many-to-one').
cycPred('TTPred-clothing-bottom').
cycPred('TTPred-role05-of').
cycPred('TTPred-dark-gray').
cycPred('TTPred-nationality-of').
cycPred('TTPred-activated-emotion-of').
cycPred('TTPred-brown').
cycPred('TTPred-headquarters-of').
cycPred('TTPred-attr-Liverpudlian').
cycPred('TTPred-attr-polytheist').
cycPred('TTPred-event09-of').
cycPred('TTPred-actor-of').
cycPred('TTPred-antiparticle-of').
cycPred('TTPred-baryon-number-of').
cycPred('TTPred-role01-of').
cycPred('TTPred-thin-stripes').
cycPred('TTPred-fr-infl-mood-of').
cycPred('TTPred-gray').
cycPred('TTPred-clothing-top').
cycPred('TTPred-max-value1-of').
cycPred('TTPred-first-OS-of').
cycPred('TTPred-row-distance-of').
cycPred('TTPred-event11-of').
cycPred('TTPred-religion-of').
cycPred('TTPred-SPECint92-of').
cycPred('TTPred-Dacron').
cycPred('TTPred-r3').
cycPred('TTPred-postal-code-of').
cycPred('TTPred-event22-of').
cycPred('TTPred-r2').
cycPred('TTPred-role04-of').
cycPred('TTPred-symmetric').
cycPred('TTPred-official-language-of').
cycPred('TTPred-leadto1').
cycPred('TTPred-height-of').
cycPred('TTPred-sphere').
cycPred('TTPred-white').
cycPred('TTPred-luminance-of').
cycPred('TTPred-acrylic').
cycPred('TTPred-clothing-ankle').
cycPred('TTPred-fr-aux-tense-of').
cycPred('TTPred-event06-of').
cycPred('TTPred-text-ref').
cycPred('TTPred-us-size-of').
cycPred('TTPred-min-value-of').
cycPred('TTPred-length-of').
cycPred('TTPred-atomic-number-of').
cycPred('TTPred-affiliation-of').
cycPred('TTPred-humanmade').
cycPred('TTPred-made-in').
cycPred('TTPred-attr-ENA').
cycPred('TTPred-virtual-memory-of').
cycPred('TTPred-nonencrypted').
cycPred('TTPred-MFLOPS-of').
cycPred('TTPred-contralto').
cycPred('TTPred-SPECmark89-of').
cycPred('TTPred-has-ceiling').
cycPred('TTPred-travel-max-speed-of').
cycPred('TTPred-event12-of').
cycPred('TTPred-fr-tense-0-of').
cycPred('TTPred-triangles').
cycPred('TTPred-travel-cargo-capacity-of').
cycPred('TTPred-rhs-assertion-of').
cycPred('TTPred-eng-main-tense-of').
cycPred('TTPred-role10-of').
cycPred('TTPred-phone-number-of').
cycPred('TTPred-attend-day-care').
cycPred('TTPred-event08-of').
cycPred('TTPred-event17-of').
cycPred('TTPred-attr-rel-range').
cycPred('TTPred-topic-of').
cycPred('TTPred-inverse-of').
cycPred('TTPred-occupation-of').
cycPred('TTPred-isospin-of').
cycPred('TTPred-fr-main-tense-of').
cycPred('TTPred-canonical-of').
cycPred('TTPred-original-run').
cycPred('TTPred-event15-of').
cycPred('TTPred-first-author-of').
cycPred('TTPred-weight-of').
cycPred('TTPred-clock-frequency-of').
cycPred('TTPred-residence-of').
cycPred('TTPred-intelligent').
cycPred('TTPred-post-title-of').
cycPred('TTPred-shirtlayer').
cycPred('TTPred-RAM-of').
cycPred('TTPred-blue').
cycPred('TTPred-topological-genus-of').
cycPred('TTPred-saturation-of').
cycPred('TTPred-arch-of').
cycPred('TTPred-electric-charge-of').
cycPred('TTPred-r4').
cycPred('TTPred-vitamin-B2').
cycPred('TTPred-cotton').
cycPred('TTPred-blue-green').
cycPred('TTPred-wool').
cycPred('TTPred-attr-Anglican').
cycPred('TTPred-diameter-of').
cycPred('TTPred-eng-infl-adverb-of').
cycPred('TTPred-studio-of').
cycPred('TTPred-strangeness-of').
cycPred('TTPred-spin-of').
cycPred('TTPred-lhs-class-of').
cycPred('TTPred-role03-of').
cycPred('TTPred-famous').
cycPred('TTPred-underlying-of').
cycPred('TTPred-case-of').
cycPred('TTPred-counter-tenor').
cycPred('TTPred-gen-max-of').
cycPred('TTPred-unwalkable').
cycPred('TTPred-entry-condition-of').
cycPred('TTPred-politically-radical-socialiste').
cycPred('TTPred-event07-of').
cycPred('TTPred-FPU-of').
cycPred('TTPred-magenta').
cycPred('TTPred-avoid').
cycPred('TTPred-orientation-of').
cycPred('TTPred-third-name-of').
cycPred('TTPred-baritone').
cycPred('TTPred-fine-weave').
cycPred('TTPred-light-gray').
cycPred('TTPred-rhs-feat-of').
cycPred('TTPred-clothing-foot').
cycPred('TTPred-affiliate-of').
cycPred('TTPred-schizophrenia').
cycPred('TTPred-col-distance-of').
cycPred('TTPred-attr-Baptist').
cycPred('TTPred-frying-of').
cycPred('TTPred-comment').
cycPred('TTPred-stripes').
cycPred('TTPred-mother-of').
cycPred('TTPred-role13-of').
cycPred('TTPred-host-of').
cycPred('TTPred-adult').
cycPred('TTPred-min-value2-of').
cycPred('TTPred-tt-ticker-of').
cycPred('TTPred-vertical-polarization').
cycPred('TTPred-skeptical').
cycPred('TTPred-ca').
cycPred('TTPred-seat-of').
cycPred('TTPred-preposition-of').
cycPred('TTPred-eng-infl-tense-of').
cycPred('TTPred-fr-tense-neg-4-of').
cycPred('TTPred-lhs-pos-of').
cycPred('TTPred-fr-literary-subjunctive-of').
cycPred('TTPred-litigious').
cycPred('TTPred-role06-of').
cycPred('TTPred-cardioid').
cycPred('TTPred-omnidirectional').
cycPred('TTPred-fr-tense-pos-2-of').
cycPred('TTPred-black').
cycPred('TTPred-clothing-hand').
cycPred('TTPred-optimistic').
cycPred('TTPred-ap').
cycPred('TTPred-event19-of').
cycPred('TTPred-walkable').
cycPred('TTPred-SPECfpRate92-of').
cycPred('TTPred-level-of').
cycPred('TTPred-eng-tense-0-of').
cycPred('TTPred-rhs-pos-of').
cycPred('TTPred-figure-8').
cycPred('TTPred-serve-meal').
cycPred('TTPred-bigoted').
cycPred('TTPred-middle-aged-adult').
cycPred('TTPred-role18-of').
cycPred('TTPred-anchor-of').
cycPred('TTPred-pdg-of').
cycPred('TTPred-lighter-than-air').
cycPred('TTPred-attr-Canadian').
cycPred('TTPred-fr-aux-verb-of').
cycPred('TTPred-manufacturer-of').
cycPred('TTPred-eng-tense-neg-5-of').
cycPred('TTPred-believe').
cycPred('TTPred-sister-of').
cycPred('TTPred-superior-of').
cycPred('TTPred-attr-Irish').
cycPred('TTPred-exchange-ticker-of').
cycPred('TTPred-amber').
cycPred('TTPred-role08-of').
cycPred('TTPred-ideal-sleep-of').
cycPred('TTPred-Caucasian').
cycPred('TTPred-politically-subversive').
cycPred('TTPred-attr-Chinese').
cycPred('TTPred-eat').
cycPred('TTPred-tweed').
cycPred('TTPred-one-to-one').
cycPred('TTPred-pink').
cycPred('TTPred-waveform-of').
cycPred('TTPred-light-blue').
cycPred('TTPred-fanciful').
cycPred('TTPred-success-emotion-of').
cycPred('TTPred-aunt-of').
cycPred('TTPred-unique-author-of').
cycPred('TTPred-event27-of').
cycPred('TTPred-barrier-isa').
cycPred('TTPred-politically-Leninist').
cycPred('TTPred-event14-of').
cycPred('TTPred-free-object').
cycPred('TTPred-gen-min-of').
cycPred('TTPred-travel-max-distance-of').
cycPred('TTPred-currency-of').
cycPred('TTPred-tie-dye').
cycPred('TTPred-attr-Kashmiri').
cycPred('TTPred-isbn-of').
cycPred('TTPred-dots').
cycPred('TTPred-unit-of').
cycPred('TTPred-overlayer').
cycPred('TTPred-event10-of').
cycPred('TTPred-taped').
cycPred('TTPred-event16-of').
cycPred('TTPred-cycle-time-of').
cycPred('TTPred-stored-in').
cycPred('TTPred-r9').
cycPred('TTPred-sold-at').
cycPred('TTPred-event29-of').
cycPred('TTPred-event13-of').
cycPred('TTPred-attr-Swiss').
cycPred('TTPred-live').
cycPred('TTPred-surface-area-of').
cycPred('TTPred-eng-tense-pos-3-of').
cycPred('TTPred-fr-translation-of').
cycPred('TTPred-attr-African').
cycPred('TTPred-similar').
cycPred('TTPred-attr-Menton').
cycPred('TTPred-related-concept-of').
cycPred('TTPred-language-of').
cycPred('TTPred-closed-captioned').
cycPred('TTPred-eng-tense-neg-4-of').
cycPred('TTPred-used-for').
cycPred('TTPred-infant').
cycPred('TTPred-psychosis').
cycPred('TTPred-film-converted-to-NTSC').
cycPred('TTPred-attend-eighth-grade').
cycPred('TTPred-light-brown').
cycPred('TTPred-stitch-strings').
cycPred('TTPred-politically-Owenite').
cycPred('TTPred-model-number-of').
cycPred('TTPred-professional-product').
cycPred('TTPred-used-at').
cycPred('TTPred-attr-Neuilly').
cycPred('TTPred-polyester').
cycPred('TTPred-event18-of').
cycPred('TTPred-role11-of').
cycPred('TTPred-role09-of').
cycPred('TTPred-politically-Republican').
cycPred('TTPred-response-of').
cycPred('TTPred-leadto12').
cycPred('TTPred-r7').
cycPred('TTPred-min-value1-of').
cycPred('TTPred-power-of').
cycPred('TTPred-attr-North-Korean').
cycPred('TTPred-rhs-class-of').
cycPred('TTPred-yellow').
cycPred('TTPred-fr-tense-literary-neg-4-of').
cycPred('TTPred-event25-of').
cycPred('TTPred-fr-tense-pos-1-of').
cycPred('TTPred-SPECfp92-of').
cycPred('TTPred-politically-Social-Democratic').
cycPred('TTPred-mezzo-soprano').
cycPred('TTPred-computer-chassis-of').
cycPred('TTPred-eng-translation-of').
cycPred('TTPred-politically-phalansterian').
cycPred('TTPred-attend-graduate-school').
cycPred('TTPred-time-on').
cycPred('TTPred-attend-seventh-grade').
cycPred('TTPred-uk').
cycPred('TTPred-print').
cycPred('TTPred-role06-script-of').
cycPred('TTPred-underlayer').
cycPred('TTPred-skill-of-play').
cycPred('TTPred-attr-Scottish').
cycPred('TTPred-eng-progressive-of').
cycPred('TTPred-eng-tense-pos-2-of').
cycPred('TTPred-politically-nazi').
cycPred('TTPred-UK-eng-subjunctive-of').
cycPred('TTPred-attr-libertine').
cycPred('TTPred-travel-crew-of').
cycPred('TTPred-fr-tense-neg-3-of').
cycPred('TTPred-attr-Czech').
cycPred('TTPred-diagonal-length-of').
cycPred('TTPred-crosses').
cycPred('TTPred-cyan').
cycPred('TTPred-Ceefax').
cycPred('TTPred-unit2-of').
cycPred('TTPred-fruit-of').
cycPred('TTPred-last-OS-of').
cycPred('TTPred-attend-ninth-grade').
cycPred('TTPred-very-old-adult').
cycPred('TTPred-annoying').
cycPred('TTPred-champagne').
cycPred('TTPred-gestation-period-of').
cycPred('TTPred-groggy').
cycPred('TTPred-steel').
cycPred('TTPred-thin-checker').
cycPred('TTPred-cylinder').
cycPred('TTPred-create-paint').
cycPred('TTPred-vitamin-A').
cycPred('TTPred-clothing-calf').
cycPred('TTPred-minitel-number-of').
cycPred('TTPred-vestlayer').
cycPred('TTPred-charm-of').
cycPred('TTPred-inexperienced').
cycPred('TTPred-illegal').
cycPred('TTPred-leadto2').
cycPred('TTPred-coatlayer').
cycPred('TTPred-attr-Episcopalian').
cycPred('TTPred-child').
cycPred('TTPred-creator-of').
cycPred('TTPred-gold').
cycPred('TTPred-attr-Zen-Buddhist').
cycPred('TTPred-r6').
cycPred('TTPred-max-value2-of').
cycPred('TTPred-attr-British').
cycPred('TTPred-event32-of').
cycPred('TTPred-role16-of').
cycPred('TTPred-attr-Greek-Orthodox').
cycPred('TTPred-fr-tense-neg-7-of').
cycPred('TTPred-unit1-of').
cycPred('TTPred-snobby').
cycPred('TTPred-attr-Burgundian').
cycPred('TTPred-attend-school').
cycPred('TTPred-width-of').
cycPred('TTPred-video-channel-of').
cycPred('TTPred-soprano').
cycPred('TTPred-attr-Protestant').
cycPred('TTPred-next-state-of').
cycPred('TTPred-cowardly').
cycPred('TTPred-OS-of').
cycPred('TTPred-attr-doer').
cycPred('TTPred-apolitical').
cycPred('TTPred-event20-of').
cycPred('TTPred-wife-of').
cycPred('TTPred-composer-of').
cycPred('TTPred-fixed-object5').
cycPred('TTPred-white-wine').
cycPred('TTPred-neurosis').
cycPred('TTPred-small-squares').
cycPred('TTPred-radio-station-of').
cycPred('TTPred-Shetland-wool').
cycPred('TTPred-politically-Maoist').
cycPred('TTPred-prejudiced').
cycPred('TTPred-attr-Sorbonne').
cycPred('TTPred-issuer-of').
cycPred('TTPred-r5').
cycPred('TTPred-computer-monitor-of').
cycPred('TTPred-role04-script-of').
cycPred('TTPred-politically-progressive').
cycPred('TTPred-calfskin').
cycPred('TTPred-bass-baritone').
cycPred('TTPred-clothing-thigh').
cycPred('TTPred-example').
cycPred('TTPred-attr-Lutheran').
cycPred('TTPred-calfskin-velvet').
cycPred('TTPred-politically-fascist').
cycPred('TTPred-worsted').
cycPred('TTPred-crosshatch').
cycPred('TTPred-translation-of').
cycPred('TTPred-attr-East-Ender').
cycPred('TTPred-event23-of').
cycPred('TTPred-publish').
cycPred('TTPred-clothing-wrist').
cycPred('TTPred-event21-of').
cycPred('TTPred-supercardioid').
cycPred('TTPred-lazy').
cycPred('TTPred-attr-Vedaic').
cycPred('TTPred-glove-leather').
cycPred('TTPred-fine-stitch').
cycPred('TTPred-vitamin-B5').
cycPred('TTPred-politically-anticapitalist').
cycPred('TTPred-attr-shamanist').
cycPred('TTPred-corduroy').
cycPred('TTPred-olive-green').
cycPred('TTPred-eng-tense-pos-1-of').
cycPred('TTPred-first-editor-of').
cycPred('TTPred-humorous').
cycPred('TTPred-mono').
cycPred('TTPred-attend-eleventh-grade').
cycPred('TTPred-RIC-of').
cycPred('TTPred-khaki-color').
cycPred('TTPred-politically-liberal').
cycPred('TTPred-role12-of').
cycPred('TTPred-roommate-of').
cycPred('TTPred-fr-tense-neg-6-of').
cycPred('TTPred-old-adult').
cycPred('TTPred-role17-of').
cycPred('TTPred-attr-Hongkong').
cycPred('TTPred-attr-anti-religious').
cycPred('TTPred-pin-stripe').
cycPred('TTPred-rayon').
cycPred('TTPred-leather').
cycPred('TTPred-encrypted').
cycPred('TTPred-wide-angle-cardioid').
cycPred('TTPred-politically-collectivist').
cycPred('TTPred-official-residence-of').
cycPred('TTPred-politically-right-wing').
cycPred('TTPred-attend-technical-school').
cycPred('TTPred-attr-Jaina').
cycPred('TTPred-drivable').
cycPred('TTPred-politically-conservative').
cycPred('TTPred-attend-junior-college').
cycPred('TTPred-attr-Shintoist').
cycPred('TTPred-trade-arbitrage').
cycPred('TTPred-politically-Tory').
cycPred('TTPred-acetate').
cycPred('TTPred-second-author-of').
cycPred('TTPred-thirty-something').
cycPred('TTPred-architect-of').
cycPred('TTPred-attr-rel-proportional').
cycPred('TTPred-nonborn').
cycPred('TTPred-delayed').
cycPred('TTPred-fabric-linen').
cycPred('TTPred-politically-Marxist-Leninist').
cycPred('TTPred-entertaining').
cycPred('TTPred-unsuccessful').
cycPred('TTPred-attr-Danish').
cycPred('TTPred-cone').
cycPred('TTPred-abstinent').
cycPred('TTPred-young-adult').
cycPred('TTPred-nonreligious').
cycPred('TTPred-dark-blue').
cycPred('TTPred-eng-tense-literary-neg-4-of').
cycPred('TTPred-create-write-music').
cycPred('TTPred-attr-Taoist').
cycPred('TTPred-attr-Mormon').
cycPred('TTPred-politically-radical').
cycPred('TTPred-cusip-of').
cycPred('TTPred-politically-royalist').
cycPred('TTPred-overlayerpost').
cycPred('TTPred-attr-Finnish').
cycPred('TTPred-create-draw').
cycPred('TTPred-eng-tense-neg-7-of').
cycPred('TTPred-vitamin-B9').
cycPred('TTPred-attr-New-York').
cycPred('TTPred-hypercardioid').
cycPred('TTPred-liquid').
cycPred('TTPred-employer-of').
cycPred('TTPred-attr-agnostic').
cycPred('TTPred-super-100-CycL').
cycPred('TTPred-fr-tense-neg-5-of').
cycPred('TTPred-attend-first-grade').
cycPred('TTPred-attr-Orthodox-Eastern-Church').
cycPred('TTPred-travel-passengers-of').
cycPred('TTPred-secretary-of').
cycPred('TTPred-key-of').
cycPred('TTPred-eng-tense-neg-3-of').
cycPred('TTPred-sick').
cycPred('TTPred-rare').
cycPred('TTPred-Hispanic').
cycPred('TTPred-bottomness-of').
cycPred('TTPred-suburban').
cycPred('TTPred-US-eng-subjunctive-of').
cycPred('TTPred-coupon-of').
cycPred('TTPred-floral').
cycPred('TTPred-attr-kharidjite').
cycPred('TTPred-attr-Christian').
cycPred('TTPred-bits-of').
cycPred('TTPred-fr-tense-neg-2-of').
cycPred('TTPred-nylon').
cycPred('TTPred-attr-monotheist').
cycPred('TTPred-chine-cotton').
cycPred('TTPred-attend-junior-high-school').
cycPred('TTPred-tenor').
cycPred('TTPred-market-of').
cycPred('TTPred-attr-Korean').
cycPred('TTPred-underlayerpre').
cycPred('TTPred-politically-yippie').
cycPred('TTPred-researcher-of').
cycPred('TTPred-politically-individualistic').
cycPred('TTPred-natural-parent-of').
cycPred('TTPred-vitamin-B12').
cycPred('TTPred-politically-anarcho-syndicalist').
cycPred('TTPred-blue-gray').
cycPred('TTPred-checker').
cycPred('TTPred-attr-Episcopal').
cycPred('TTPred-attr-Asian').
cycPred('TTPred-event31-of').
cycPred('TTPred-ivory-colored').
cycPred('TTPred-circumference-of').
cycPred('TTPred-politically-left-wing-radical').
cycPred('TTPred-attr-X').
cycPred('TTPred-second-editor-of').
cycPred('TTPred-eng-tense-neg-6-of').
cycPred('TTPred-market-manipulation').
cycPred('TTPred-insider-trading').
cycPred('TTPred-role01-script-of').
cycPred('TTPred-spinoff-of').
cycPred('TTPred-embryonic').
cycPred('TTPred-attr-Calvinist').
cycPred('TTPred-copper').
cycPred('TTPred-politically-Stalinist').
cycPred('TTPred-attr-Catholic').
cycPred('TTPred-Panda1').
cycPred('TTPred-circadian-rhythm-of').
cycPred('TTPred-iodine').
cycPred('TTPred-nerdy').
cycPred('TTPred-PAL').
cycPred('TTPred-small-dots').
cycPred('TTPred-attend-third-grade').
cycPred('TTPred-attr-Jewish').
cycPred('TTPred-poor').
cycPred('TTPred-bars').
cycPred('TTPred-event28-of').
cycPred('TTPred-role14-of').
cycPred('TTPred-distillation-of').
cycPred('TTPred-attr-Dutch').
cycPred('TTPred-attr-Swedish').
cycPred('TTPred-politically-Democratic').
cycPred('TTPred-attr-Afghan').
cycPred('TTPred-attr-Mennonite').
cycPred('TTPred-create-write-literature').
cycPred('TTPred-viscose').
cycPred('TTPred-addiction-of').
cycPred('TTPred-squares').
cycPred('TTPred-rubber').
cycPred('TTPred-attr-Muslem').
cycPred('TTPred-attr-wrongdoing').
cycPred('TTPred-politically-Saint-Simonian').
cycPred('TTPred-bullying').
cycPred('TTPred-attend-secondary-school').
cycPred('TTPred-attr-Indian').
cycPred('TTPred-carrier-of').
cycPred('TTPred-attr-Russian-Orthodox').
cycPred('TTPred-dead').
cycPred('TTPred-talkative').
cycPred('TTPred-writer-of').
cycPred('TTPred-attr-Confucianist').
cycPred('TTPred-create-arrange').
cycPred('TTPred-noble').
cycPred('TTPred-attend-medical-school').
cycPred('TTPred-unkind').
cycPred('TTPred-Sanforized').
cycPred('TTPred-fly').
cycPred('TTPred-second-surname-of').
cycPred('TTPred-eng-tense-literary-0-of').
cycPred('TTPred-skin-material-resin').
cycPred('TTPred-can-lift').
cycPred('TTPred-attend-doctoral-program').
cycPred('TTPred-ellipsoid').
cycPred('TTPred-introverted').
cycPred('TTPred-attr-Tokyoite').
cycPred('TTPred-ceo-of').
cycPred('TTPred-father-of').
cycPred('TTPred-polka-dot').
cycPred('TTPred-politically-reactionary').
cycPred('TTPred-event24-of').
cycPred('TTPred-herringbone').
cycPred('TTPred-orange').
cycPred('TTPred-attr-German').
cycPred('TTPred-vitamin-E').
cycPred('TTPred-clothing-forearm').
cycPred('TTPred-attr-Buddhist').
cycPred('TTPred-male-chauvinist').
cycPred('TTPred-politically-capitalist').
cycPred('TTPred-politically-socialist').
cycPred('TTPred-attr-Polish').
cycPred('TTPred-eng-tense-neg-2-of').
cycPred('TTPred-politically-moderate').
cycPred('TTPred-dg-adjoint-of').
cycPred('TTPred-attr-chafiite').
cycPred('TTPred-smooth').
cycPred('TTPred-splotches').
cycPred('TTPred-silent').
cycPred('TTPred-VRAM-of').
cycPred('TTPred-director-of').
cycPred('TTPred-attr-hanafite').
cycPred('TTPred-combed-cotton').
cycPred('TTPred-clothing-knee').
cycPred('TTPred-BW').
cycPred('TTPred-fr-tense-neg-1-of').
cycPred('TTPred-black-leather').
cycPred('TTPred-attr-Slovakian').
cycPred('TTPred-attr-pantheist').
cycPred('TTPred-nubuck').
cycPred('TTPred-attr-Spanish').
cycPred('TTPred-bass').
cycPred('TTPred-political').
cycPred('TTPred-attr-Londoner').
cycPred('TTPred-amplitude-modulation-of').
cycPred('TTPred-construction-membrane').
cycPred('TTPred-attr-ENS').
cycPred('TTPred-orange-red').
cycPred('TTPred-heavier-than-air').
cycPred('TTPred-attr-American').
cycPred('TTPred-narrator-of').
cycPred('TTPred-small-chevrons').
cycPred('TTPred-extroverted').
cycPred('TTPred-vitamin-B3').
cycPred('TTPred-politically-Fabian').
cycPred('TTPred-attr-Biarritz').
cycPred('TTPred-Black').
cycPred('TTPred-failure-emotion-of').
cycPred('TTPred-brother-of').
cycPred('TTPred-attr-Hindu').
cycPred('TTPred-event26-of').
cycPred('TTPred-politically-state-socialist').
cycPred('TTPred-jacketlayer').
cycPred('TTPred-Nagravision').
cycPred('TTPred-politically-Bolshevist').
cycPred('TTPred-attend-sixth-grade').
cycPred('TTPred-attr-Italian').
cycPred('TTPred-attr-Shiite').
cycPred('TTPred-attr-materialist').
cycPred('TTPred-rural').
cycPred('TTPred-atom-nickel').
cycPred('TTPred-politically-Marxist').
cycPred('TTPred-politically-progressiste').
cycPred('TTPred-fr-tense-pos-3-of').
cycPred('TTPred-light-violet').
cycPred('TTPred-iron').
cycPred('TTPred-blackcurrant-liqueur').
cycPred('TTPred-attend-nursery-school').
cycPred('TTPred-politically-worker').
cycPred('TTPred-trade-speculate').
cycPred('TTPred-attr-Taiwanese').
cycPred('TTPred-levels-of').
cycPred('TTPred-attr-Methodist').
cycPred('TTPred-event30-of').
cycPred('TTPred-gas').
cycPred('TTPred-attend-elementary-school').
cycPred('TTPred-bronze').
cycPred('TTPred-attr-New-Jersey').
cycPred('TTPred-fluent-language-of').
cycPred('TTPred-politically-left-wing').
cycPred('TTPred-fr-tense-literary-neg-7-of').
cycPred('TTPred-department-head-of').
cycPred('TTPred-attr-Northern-Irish').
cycPred('TTPred-role15-of').
cycPred('TTPred-urban').
cycPred('TTPred-attr-Roman-Catholic').
cycPred('TTPred-ptrans-swim').
cycPred('TTPred-politically-nationalist').
cycPred('TTPred-underlayerpost').
cycPred('TTPred-politically-syndicalist').
cycPred('TTPred-circles').
cycPred('TTPred-teenager').
cycPred('TTPred-politically-nihilist').
cycPred('TTPred-attend-law-school').
cycPred('TTPred-unintelligent').
cycPred('TTPred-attr-Czechoslovakian').
cycPred('TTPred-gullible').
cycPred('TTPred-ovals').
cycPred('TTPred-J17').
cycPred('TTPred-bouncy').
cycPred('TTPred-tail-length-of').
cycPred('TTPred-vitamin-B1').
cycPred('TTPred-burgundy').
cycPred('TTPred-denim').
cycPred('TTPred-attr-Anglo-Catholic').
cycPred('TTPred-attr-European').
cycPred('TTPred-attr-Welsh').
cycPred('TTPred-good').
cycPred('TTPred-eng-tense-neg-1-of').
cycPred('TTPred-radius-of').
cycPred('TTPred-weave').
cycPred('TTPred-attr-Zoroastrian').
cycPred('TTPred-product-release').
cycPred('TTPred-attend-fifth-grade').
cycPred('TTPred-lucky').
cycPred('TTPred-teach').
cycPred('TTPred-pacifist').
cycPred('TTPred-clumsy').
cycPred('TTPred-attr-Californian').
cycPred('TTPred-edition-of').
cycPred('TTPred-attend-second-grade').
cycPred('TTPred-attr-atheist').
cycPred('TTPred-do-postdoctoral-work').
cycPred('TTPred-fr-progressive-of').
cycPred('TTPred-rich').
cycPred('TTPred-attr-good-Samaritan').
cycPred('TTPred-politically-Titoist').
cycPred('TTPred-politically-neonazi').
cycPred('TTPred-attr-Japanese').
cycPred('TTPred-technical').
cycPred('TTPred-politically-Trotskyite').
cycPred('TTPred-attend-college').
cycPred('TTPred-attr-Martinique').
cycPred('TTPred-executive-producer-of').
cycPred('TTPred-fetal').
cycPred('TTPred-racist').
cycPred('TTPred-attr-English').
cycPred('TTPred-sky-blue').
cycPred('TTPred-husband-of').
cycPred('TTPred-vitamin-B6').
cycPred('TTPred-rollable').
cycPred('TTPred-attr-witch').
cycPred('TTPred-attend-preschool').
cycPred('TTPred-attr-animist').
cycPred('TTPred-not').
cycPred('TTPred-attend-MBA-program').
cycPred('TTPred-attend-fourth-grade').
cycPred('TTPred-attr-Ile-de-France').
cycPred('TTPred-politically-revolutionary').
cycPred('TTPred-in-color').
cycPred('TTPred-politically-communist').
cycPred('TTPred-attr-Seventh-Day-Adventist').
cycPred('TTPred-yellow-green').
cycPred('TTPred-attend-tenth-grade').
cycPred('TTPred-attr-malekite').
cycPred('TTPred-politically-Castroite').
cycPred('TTPred-large-stripes').
cycPred('TTPred-politically-Marxist-revisionist').
cycPred('TTPred-politically-extremist').
cycPred('TTPred-attr-Douarnenez').
cycPred('TTPred-attr-Sunnite').
cycPred('TTPred-tielayer').
cycPred('TTPred-attr-Adventist').
cycPred('TTPred-content-of').
cycPred('TTPred-Asian').
cycPred('TTPred-can-hold').
cycPred('TTPred-politically-anarchist').
cycPred('TTPred-fan-of').
cycPred('TTPred-unique-translator-of').
cycPred('TTPred-large-crosshatch').
cycPred('TTPred-attr-Thai').
cycPred('TTPred-attr-French').
cycPred('TTPred-unique-editor-of').
cycPred('TTPred-vitamin-C').
cycPred('TTPred-sing').
cycPred('TTPred-sentient').
cycPred('TTPred-role09-script-of').
cycPred('TTPred-alto').
cycPred('TTPred-attr-Parisian').
cycPred('TTPred-event33-of').
cycPred('TTPred-spaced-out').
cycPred('TTPred-attr-Quaker').
cycPred('TTPred-attend-high-school').
cycPred('TTPred-attr-Christian-Scientist').
cycPred('TTPred-attr-Sikh').
cycPred('genlMt').

*/
% ==========================================================
% BEGIN OLD CODE
% ==========================================================
/*
% =======================================================
% sentence(CycL, [every,man,that,paints,likes,monet],[]) 
% =======================================================
:-export((sentence//1)).

sentence(CycL) --> theVariable(sent).
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
      optionalText([the]),trans2_verb(Subj,Event,Obj,VProp),possible_prep,
      noun_phrase(Obj,VProp,CycL1),[is],noun_phrase(Subj,CycL1,CycL).


possible_prep--> optionalText([of]).


inquiry('thereExists'(Actor,CycL)) --> wh_pronoun(Actor), verb_phrase(Actor,'?QuestionEvent',CycL),[?].
inquiry(CycL) --> inv_sentence(CycL),optionalText([(?)]).
inquiry(CycL) --> declaritive_sentence(CycL),([(?)]).

wh_pronoun('?Who') --> [who].
wh_pronoun('?What') --> [what].


inv_sentence(CycL) --> aux, declaritive_sentence(CycL).
aux --> [does].


% =======================================================
% Quantification (DET Phrases)
% =======================================================

quant_phrase(Subj,Prop,CycL1,'thereExists'(Subj,'and'(Prop , CycL1))) --> existential_words.
quant_phrase(Subj,Prop,CycL1,'forAll'(Subj,'implies'(Prop , CycL1))) --> universal_words.

existential_words --> existential_word,existential_word.
existential_words --> existential_word.
existential_word --> [a];[an];[the];[some];[there,is];[there,are];[there,exists].

universal_words --> universal_word,universal_word.
universal_words --> universal_word.
universal_word --> [every];[all];[forall];[each];[for,all].

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
noun_expression(Subj,CycLVerb,('thereExists'(Subj,'and'(AttribIsa,CycLVerb)))) -->  
   adjectives_phrase(Subj,AttribIsa1,AttribIsa),
   collection_noun_isa(Subj,Isa),
   adjectives_phrase(Subj,Isa,AttribIsa1).

%noun_expression(Subj,CycL1,CycL) -->  quant_phrase(Subj,Prop12,CycL1,CycL),collection_noun_isa(Subj,Prop1),rel_clause(Subj,Prop1,Prop12).
%noun_expression(Subj,CycL1,CycL) -->  quant_phrase(Subj,Prop1,CycL1,CycL),collection_noun_isa(Subj,Prop1).


% =======================================================
% Adjective Phrases
% =======================================================

adjectives_phrase(Subj,IsaDoes,'and'(IsaDoes, AttribProp)) --> adj_phrase(Subj,AttribProp).
adjectives_phrase(_ ,IsaDoes,IsaDoes) --> [].

adj_phrase(Subj,Formula) -->  [A,B,C],{phrase_meaning_adj([A,B,C],Subj,Formula)}.
adj_phrase(Subj,Formula) -->  [A,B],{phrase_meaning_adj([A,B],Subj,Formula)}.
adj_phrase(Subj,Formula) -->  [A],{phrase_meaning_adj([A],Subj,Formula)}.


%'adjSemTrans'('Cloud-TheWord', 0, 'RegularAdjFrame', ['weather', ':NOUN', 'Cloudy']).
phrase_meaning_adj(String,Subj,CycL):-
	 pos(String,CycWord,_Form,'Adjective'),      
	 'adjSemTrans'(CycWord, _ , _ , Formula),
	 Repl='CollectionOfFn'(Subj),
	 %posMeans(String,'Verb',POS,WordMeaning),
	 vsubst(Formula,':SUBJECT',Subj,Formula21),
	 vsubst(Formula21,':REPLACE',Repl,Formula2),
	 vsubst(Formula2,':NOUN',Subj,Formula3),
	 vsubst(Formula3,':ACTION','?ACTION',Formula4),
	 vsubst(Formula4,':OBJECT','?OBJECT',Formula6),
	 list_to_term(Formula6,CycL).

phrase_meaning_adj(String,Subj,'hasAttributeOrCollection'(Subj,CycL)):-
      posMeans(String,'Adjective',Form,CycL),not(lowerCasePred(CycL)).

% =======================================================
% Conjunctions
% =======================================================

conj_word --> [X],{conj_word(X)}.
conj_word --> [X],{connective_word(X)}.
disj_word --> [X],{disj_word(X)}.

conj_word(and). conj_word(also). disj_word(or).  
% and	but	or	yet	for	nor	so


common_Subordinating_Conjunctions([
after,although,as,[as,if],[as,long,as],[as,though],because,before,[even,if],[even,though],if,
[if,only],[in,order,that],[now,that],once,[rather,than],since,[so,that],than,that,though,till,unless,until,when,whenever,where,whereas,wherever,while]).

% =======================================================
% Rel Clauses
% =======================================================

connective_word(that). connective_word(who). connective_word(which). 

rel_clause(Subj,Isa,'and'(Isa, HowDoes)) --> [X],{connective_word(X)},adverbs_phrase(Event,Does,HowDoes),verb_phrase(Subj,Event,Does).
rel_clause(_ ,Isa,Isa) --> [].


% =======================================================
% Pronoun Phrases
% =======================================================
%'nounPrep'('Address-TheWord', 'Of-TheWord', ['pointOfContactInfo', ':OBLIQUE-OBJECT', 'ContactLocation', 'addressText', ':NOUN']).


pronoun('?Speaker') --> ['I'];[i];[me].
pronoun('?TargetAgent') --> ['you'];['You'].
pronoun(CycLTerm) --> [A,B,C],{lex_pronoun([A,B,C],CycLTerm)}.
pronoun(CycLTerm) --> [A,B],{lex_pronoun([A,B],CycLTerm)}.
pronoun(CycLTerm) --> [A],{lex_pronoun([A],CycLTerm)}.

lex_pronoun(W,Fixed):-
      lex_pronoun2(W,T),
      fix_pronoun(W,T,Fixed).

fix_pronoun(W,[null],Fixed):-!,concat_atom(['?'|W],Fixed).
fix_pronoun(W,Fixed,Fixed).

lex_pronoun2(Words,CycLTerm):-posMeans(Words,'WHPronoun-Subj', _ ,CycLTerm).
lex_pronoun2(Words,CycLTerm):-posMeans(Words,'Pronoun', _ ,CycLTerm).
lex_pronoun2(Words,CycLTerm):-posMeans(Words,'ObjectPronoun', _ ,CycLTerm).

% =======================================================
% Proper Noun Phrases
% =======================================================

proper_noun_phrase(CycLTerm) --> [the],proper_noun(CycLTerm).
proper_noun_phrase(CycLTerm) --> proper_noun(CycLTerm).

proper_noun(CycLTerm) --> [A,B,C],{lex_proper_noun_cached( [A,B,C],CycLTerm)}.
proper_noun(CycLTerm) --> [A,B],{lex_proper_noun_cached( [A,B],CycLTerm)}.
proper_noun(CycLTerm) --> [A],{lex_proper_noun_cached( [A],CycLTerm)}.

lex_proper_noun_cached(Words,CycLTerm):-posMeans(Words,'ProperNoun', _ ,CycLTerm).

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
      posMeans(VerbPhrase,'Verb',Form,CycLPred),
     nonvar(CycLPred),
      ignore(Event='?ACTION'),
      lex_trans3_verb2(VerbPhrase,CycLPred,Subj,Event,Obj,Prep,Target,VProp).
      
lex_trans3_verb2(VerbPhrase,CycLPred,Subj,Event,Obj,Prep,Target,VProp):-
      lowerCasePred(CycLPred) -> 
      VProp=..[CycLPred,Subj,Obj,Target] ; 
      VProp = 'and'('isa'(Event,CycLPred),'doneBy'(Event,Subj),'eventOccursAt'(Event,Obj),'constituentInSituation'(Event,Target)).

   
      

      
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

adverbs_phrase(Event,IsaDoes,'and'(IsaDoes, AttribProp)) --> adv_phrase(Event,AttribProp).
adverbs_phrase(_ ,IsaDoes,IsaDoes) --> [].

adv_phrase(Event,Formula) -->  [A,B,C],{lex_adverb([A,B,C],Event,Formula)}.
adv_phrase(Event,Formula) -->  [A,B],{lex_adverb([A,B],Event,Formula)}.
adv_phrase(Event,Formula) -->  [A],{lex_adverb([A],Event,Formula)}.

lex_adverb(String,Event,'hasAttributeOrCollection'(Event,Trait)):-
      posMeans(String,'Adverb',Form,Trait).

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

intrans_verb(Subj,Event,'and'('bodilyDoer'(Subj,Event),'isa'(Event,actOf(paint)))) --> [paints].

% ============================================================================
% Verb CycL Tense
% ============================================================================

%   'verbSemTrans'('Fancy-TheWord', 0, 'TransitiveNPCompFrame', ['likesObject', ':SUBJECT', ':OBJECT']).
lex_verb_meaning(String,MeaningTerm,Subj,Event,Obj):-
   tensed_lex_verb_meaning(String,MeaningTerm,Subj,Event,Obj,Tense).

lex_verb_meaning(String,MeaningTerm,Subj,Event,Obj):-
   lex_trans2_verb2(String,MeaningTerm,Subj,Event,Obj).

% rewrites
tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,now):-
   atom(Words),atom_concat(String,'s',Words),
   lex_trans2_verb2([String],MeaningTerm,Subj,Event,Obj).

tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,past):-
   atom(Words),atom_concat(String,'d',Words),
   lex_trans2_verb2([String],MeaningTerm,Subj,Event,Obj).

tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,past):-
   atom(Words),atom_concat(String,'ed',Words),
   lex_trans2_verb2([String],MeaningTerm,Subj,Event,Obj).

tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,nowing):-
   atom(Words),atom_concat(String,'ing',Words),
   lex_trans2_verb2([String],MeaningTerm,Subj,Event,Obj).


% ============================================================================
% lex_trans2_verb2 Templates
% ============================================================================

lex_trans2_verb2(VerbPhrase,CycL,Subj,Event,Obj):-
      verb_frame(VerbPhrase,CycWord,2,CycPred,Formula),
      apply_frame(Formula,Subj,Event,Obj,'?OBLIQUE-OBJECT',CycL).

verb_frame([is,the,subclass,of],CycWord,Arity,CycPred,['genls',':SUBJECT',':OBJECT']).
verb_frame([is,a,subclass,of],CycWord,Arity,CycPred,['genls',':SUBJECT',':OBJECT']).
verb_frame([is,a],CycWord,Arity,CycPred,['isa',':SUBJECT',':OBJECT']).

verb_frame(VerbPhrase,CycWord,Arity,CycPred,Formula):-
      pos(VerbPhrase,CycWord,_Form,'Verb'),      
      'verbSemTrans'(CycWord, _ , 'TransitiveNPCompFrame', Formula),
      (contains_obliqe(Formula) -> Arity=3;Arity=2).

verb_frame([is,the,Verb,String],CycWord1,2,CycPred,Formula2):-!,
      stringToCycWord([Verb],CycWord1),      
      stringToCycWord([String],CycWord2),
      'nounPrep'(CycWord1,CycWord2, Formula),
      vsubst(Formula,':NOUN',':SUBJECT',Formula1),
      vsubst(Formula1,':OBLIQUE-OBJECT',':OBJECT',Formula2).

verb_frame([is,Verb,String],CycWord1,2,CycPred,Formula2):-!,
      stringToCycWord([Verb], CycWord1),      
      stringToCycWord([String],CycWord2),
      'nounPrep'(CycWord1,CycWord2, Formula),
      vsubst(Formula,':NOUN',':SUBJECT',Formula1),
      vsubst(Formula1,':OBLIQUE-OBJECT',':OBJECT',Formula2).


/*
the start of Obleec is Noun

Obleec is start of noun

'nounPrep'('Address-TheWord', 'Of-TheWord', ['pointOfContactInfo', ':OBLIQUE-OBJECT', 'ContactLocation', 'addressText', ':NOUN']).
'nounPrep'('Retail-TheWord', 'Of-TheWord', ['sellsProductType', ':NOUN', ':OBLIQUE-OBJECT']).
'nounPrep'('Market-TheWord', 'Of-TheWord', ['sellsProductType', ':NOUN', ':OBLIQUE-OBJECT']).
'nounPrep'('Start-TheWord', 'Of-TheWord', ['startingPoint', ':OBLIQUE-OBJECT', ':NOUN']).
*/

apply_frame(Formula,Subj,Event,Obj,Target,CycL):-
      ignore(Event='?ACTION'),
      vsubst(Formula,':SUBJECT',Subj,Formula2),
      vsubst(Formula2,':ACTION',Event,Formula3),
      vsubst(Formula3,':OBJECT',Obj,Formula4),
      vsubst(Formula4,':EVENT',Event,Formula5),
      vsubst(Formula5,':OBLIQUE-OBJECT',Target,Formula6),
      vsubst(Formula6,':ARG1',Subj,Formula7),
      vsubst(Formula7,':ACTION',Event,Formula8),
      vsubst(Formula8,':ARG2',Obj,Formula9),
      vsubst(Formula9,':EVENT',Event,Formula10),
      vsubst(Formula10,':ARG3',Target,Formula11),
      list_to_term(Formula11,CycL).

contains_obliqe(Formula):-flatten(Formula,Flat),member(':OBLIQUE-OBJECT',Flat).


lex_trans2_verb2(VerbPhrase,CycL,Subj,Event,Obj):-
      posMeans(VerbPhrase,'Verb',Form,CycLPred),
      ignore(Event='?ACTION'),
      atom(CycLPred),
      (lowerCasePred(CycLPred) -> 
	 CycL =..[CycLPred,Subj,Obj] ;
	 CycL = 'and'('isa'(Event,CycLPred),'doneBy'(Event,Subj),'eventOccursAt'(Event,Obj))).


% uses genFormat
lex_trans2_verb2(String,MeaningTerm,Subj,Event,Obj):-
   convert_to_1cycString(String,CycString),
   append(CycString,["~a"],Rest),!,
   not(memberchk("~a",Rest)),
   'genFormat'(Pred,["~a"|Rest],Format),
   do_genformat(Format,Pred,Subj,Obj,MeaningTerm).

do_genformat(['NIL'],Pred,Subj,Obj,MeaningTerm):-MeaningTerm =..[Pred,Subj,Obj].
do_genformat([P1,P2],Pred,Subj,Obj,MeaningTerm):-fp(P1,1),fp(P2,2),!, MeaningTerm =..[Pred,Subj,Obj].
do_genformat([P2,P1],Pred,Subj,Obj,MeaningTerm):-fp(P1,1),fp(P2,2),!, MeaningTerm =..[Pred,Obj,Subj].

fp(N,N).
fp([N|_],N).
*/


% ==========================================================
% END OLD CODE
% ==========================================================



end_of_file.


(ssz/2),
(idioms/3),
(dictionary/3),
(form80/2),
(wnS/6)
% (tag_pos/2),

        common_logic_kb_hooks:t/1),
        common_logic_sexpr:is_quantifier/1),
        common_logic_snark:~/1),
        cyckb_t/4),
        do_dcg/5 is declared as meta_predicate do_dcg(?,?,?,?,?), but has no clauses
        e2c_in_file/2),
        isPOS/5 is declared as meta_predicate isPOS(?,?,?,?,?), but has no clauses
        logicmoo_util_shared_dynamic:decl_as/1 is declared as meta_predicate decl_as(+), but has no clauses
        mpred_agenda:arity/2),
        baseKB:call_OnEachLoad/1),
        mpred_at_box:genlMt/2),
        mpred_expansion:t/2),
        mpred_hooks:arity/2),
        mpred_hooks:mpred_f/5),
        mpred_hooks:mpred_f/6),
        mpred_hooks:mpred_f/7),
        mpred_hooks:support_hilog/2),
        mpred_hooks:t/1),
        mpred_hooks:t/2),
        mpred_hooks:rtNotForUnboundPredicates/1),
        mpred_kb_ops:arity/2),
        mpred_kb_ops:support_hilog/2),
        mpred_pfc:arity/2),
        mpred_pfc:maybe_prepend_mt/3),
        mpred_props:ttPredType/1),
        mpred_storage:arity/2),
        mpred_storage:is_known_trew/1),
        mpred_storage:meta_argtypes/1),
        mpred_storage:t/1),
        mpred_storage:tCol/1),
        mpred_storage:ttExpressionType/1),
        mpred_stubs_file_module:constrain_args_pttp/2),
        mpred_stubs_file_module:t/1),
        mpred_type_args:genls/2),
        mpred_type_args:resultIsa/2),
        mpred_type_args:tCol/1),
        mpred_type_args:tRelation/1),
        mpred_type_constraints:genls/2),
        mpred_type_constraints:lambda/5),
        mpred_type_constraints:t/2),
        tCol/1),
        genls/2),
        mpred_type_isa:isa/2),
        mpred_type_isa:tCol/1),
        mudKeyword/2),

        reorderClause/2 is declared as meta_predicate reorderClause(?,?), but has no clauses
Warning: /opt/PrologMUD/pack/prologmud/runtime/init_mud_server.pl:380:
