%<?
% ===================================================================
% File 'e2c.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog  
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2005/06/06 15:43:15 $
% from Bratko chapter 17 page 455. This comes from Pereira and Warren paper, AI journal, 1980 
/*

THIS IS NOT LOADED DIRRECTLY - INSTEAD USE logicmoo_nl_sem.pl

*/
% ===================================================================


:- style_check(-singleton).
:- style_check(-discontiguous).
%:- style_check(-atom).

:- use_module(library(logicmoo_plarkc)).

:- set_prolog_flag(double_quotes, string).
:- install_constant_renamer_until_eof.
:- set_prolog_flag(do_renames_sumo,never).

% =======================================================
% Mine out Cyc idiomatic DCGs
% =======================================================

holdsWithQuery(A,B,C,D,E):- ac(A,B,C,D,E).
holdsWithQuery(A,B,C,D):- ac(A,B,C,D).
holdsWithQuery(A,B,C):- ac(A,B,C).

cyc_decyclify(DCG,UDCG):- if_defined(cyc:decyclify(DCG,UDCG)),!.
cyc_decyclify(DCG,DCG):- !.

debugOnFailure(G):- must_or_rtrace(G).

:-if(\+ current_predicate(isSlot/1)).
isSlot(Var):-var(Var),!.
isSlot('$VAR'(Var)):-number(Var).
:-endif.

:-if(\+ current_predicate(pterm_to_sterm/2)).

pterm_to_sterm(X,Y):-local_pterm_to_sterm(X,Y).

tellall(X,W):- X, numbervars(X,0,_),(format('~q.~n',[W])),fail.
tellall(_X,_W):-flush_output.

:-endif.


:-dynamic(cache_dcgRedressStart/4).

list_dcgRedressStart :- cache_dcgRedressStart(Reln,DCGPre,DCG,Vars),
      numbervars(cache_dcgRedressStart(Reln,DCGPre,DCG,Vars),0,_),
      noteFmt('?- ~q. ~n ',[dcgRedressStart(Reln,DCGPre,DCG,Vars)]),fail.
list_dcgRedressStart :- retractall(cache_dcgRedressStart(_,_,_,_)).

dcgRedressStart(Reln,DCGPre,DCG,VarsO):-
      nonvar(DCGPre),
      asserta(cache_dcgRedressStart(Reln,DCGPre,DCG,Vars)),
      catch((dcgRedress(DCGPre,DCG,Vars),sort(Vars,VarsO)), E,
               (printAndThrow('Error: ~q in ~q/~q ~q ~n',[E,DCG,Vars,DCGPre]),list_dcgRedressStart,trace)),
      retractall(cache_dcgRedressStart(_,_,_,_)).

% todo fix ac('genTemplate-Constrained', subsetOf, [and, [isa, svar(_G276, ':ARG1'), 'Collection'], [isa, svar(_G291, ':ARG2'), 'Collection']], ['NPIsNP-NLSentenceFn', ['PhraseFormFn', singular, ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G276, ':ARG1')]]], ['IndefiniteNounPPFn', 'Subset-TheWord', string([of]), ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G291, ':ARG2')]]]]).
dcgt :- dcgRedress( ['NPIsNP-NLSentenceFn', ['PhraseFormFn', singular, ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G276, ':ARG1')]]], ['IndefiniteNounPPFn', 'Subset-TheWord', string([of]), ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G291, ':ARG2')]]]],DCG,Vars).
dcgt(DCG,Vars) :- dcgRedress( ['NPIsNP-NLSentenceFn', ['PhraseFormFn', singular, ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G276, ':ARG1')]]], ['IndefiniteNounPPFn', 'Subset-TheWord', string([of]), ['ConcatenatePhrasesFn', ['BestNLPhraseOfStringFn', string([the, set, of, all])], ['TermParaphraseFn-Constrained', 'nonSingular-Generic', svar(_G291, ':ARG2')]]]],DCG,Vars).

mustNonvar(Vars,Call):-call(Call),once(nonvar(Vars);(noteFmt('mustNonvar ~q~n',[Call]),flush, trace)).

appliedTemplate_mine('GenerationPhrase',Reln,DCG,CycL9):-
         genFormatU(DCGPre,[Reln|Args]),
         dcgRedressStart([Reln|Args],DCGPre,DCG,VarsS),
         once(cyclRedress([Reln|Args],CycL)),substEach(CycL,VarsS,CycL9).

appliedTemplate_mine(TempType,Reln,DCG,CycL9):-
         cmember(Reln,['assertTemplate','rewriteTemplate','termTemplate','queryTemplate','commandTemplate']),
         holdsWithQuery(Reln,TempType,DCGPre,CycLPre),
         dcgRedressStart(CycLPre,DCGPre,DCG,VarsS),
         once(cyclRedress(CycLPre,CycL)),substEach(CycL,VarsS,CycL9).

appliedTemplate_mine(TempType,Reln:Name,DCG,CycL9):-
         cmember(Reln,['assertTemplate-Reln','metaStatementTemplate-Reln','termTemplate-Reln','queryTemplate-Reln','commandTemplate-Reln']),
         holdsWithQuery(Reln,TempType,Name,DCGPre,CycLPre),
         dcgRedressStart(Name-CycLPre,DCGPre,DCG,VarsS),
         once(cyclRedress(CycLPre,CycL)),substEach(CycL,VarsS,CycL9).

appliedTemplate_mine(TempType,Reln:Name,DCG,CycL9):-
         cmember(Reln,['assertTemplate-Test','termTemplate-Test','queryTemplate-Test']),
         holdsWithQuery(Reln,TempType,Name,DCGPre,CycLPre,Test),
         dcgRedressStart(implies(Test,CycLPre),DCGPre,DCG,VarsS),
         once(cyclRedress(implies(Test,CycLPre),CycL)),substEach(CycL,VarsS,CycL9).

appliedTemplate_mine('GenerationTemplateConstrained',Reln,DCG,CycL9):-
         holdsWithQuery('genTemplate-Constrained',Reln,Constr,DCGPre),nonvar(DCGPre),
         once((getArity(Reln,Arity),number(Arity),makeArgNameLen(Arity,Args))),
         dcgRedressStart([Reln|Args],DCGPre,DCG,VarsS),
         once(cyclRedress(implies(Constr,[Reln|Args]),CycL)),substEach(CycL,VarsS,CycL9).

appliedTemplate_mine('GenerationTemplate',Reln,DCG,CycL9):-
         genTemplateU(Reln,DCGPre,[Reln|Args]),nonvar(DCGPre),CycLPre=[Reln|Args],
         dcgRedressStart([Reln|Args],DCGPre,DCG,VarsS),
         once(cyclRedress(CycLPre,CycL)),substEach(CycL,VarsS,CycL9).


tl:-told,
  tell('dcgTermTemplate2.pl'),
  tellall(appliedTemplate_mine(TempType,Reln,DCG,CycL9),appliedTemplate(TempType,Reln,DCG,CycL9)),told.

:- if(exists_source(dcgTermTemplate)).
:- ensure_loaded(dcgTermTemplate).
:- endif.
:- if(exists_source(dcgTermTemplate2)).
:- ensure_loaded(dcgTermTemplate2).
:-endif.

isCycKeyword(KW):-atom(KW),atom_concat(':',_,KW).
getKeyVariable(KW,KW):-isCycKeyword(KW),!.
getKeyVariable(KW,KW):-atom(KW),!,atom_concat('?',_,KW),!.
getKeyVariable(SVAR,KW):-nonvar(SVAR),SVAR = svar(_,KW),!.
getKeyVariable(SVAR,KW):-nonvar(SVAR),!,SVAR = [svar,_,KW].
getKeyVariable(SVAR,KW):-sformat(S,':VAR~w',[SVAR]),string_to_atom(S,KW),!,noteFmt(';; ~q.~n',[getKeyVariable(SVAR,KW)]),list_dcgRedressStart. %,ignore(KW=SVAR).

cyclRedress(V,V):-var(V),!.
cyclRedress(svar(_,VarName),VarName):-!.
cyclRedress([Cyc|L],[CycO|LO]):-cyclRedress(Cyc,CycO),cyclRedress(L,LO),!.
cyclRedress(CycL,CycL9):-compound(CycL),!,CycL=..[Cyc|L],cyclRedress([Cyc|L],CycLL),CycL9=..CycLL,!.
cyclRedress(CycL,CycL):-!.
      

juntifyList(Pred,List,Res):-!,delete(List,theGText(['']),List1),delete(List1,theGText(''),List2),juntifyList0(Pred,List2,Res),!.

juntifyList0(Pred,[],Pred):-!.
juntifyList0(dcgSeq,[List],List):-!.
%juntifyList0(Pred,[List],Res):-!,Res=..[Pred,List],!.
juntifyList0(Pred,[H|List],Res):-length([H|List],Len),Res=..[Pred,Len,[H|List]],!.
juntifyList0(Pred,[H|List],Res):-juntifyList0(Pred,List,PRES),Res=..[Pred,H,PRES],!.


dcgRedressJunctify(DcgSeq,List,DCG,Vars):- throwOnFailure((dcgRedressL(List,RList,Vars),juntifyList(DcgSeq,RList,DCG))),
      throwOnFailure(nonvar(DCG),dcgRedressJunctify(DcgSeq,List,DCG,Vars)),!.

dcgRedressL(I,O,V):-debugOnFailure(dcgRedressL0(I,O,V)),throwOnFailure(nonvar(O),dcgRedressJunctify(DcgSeq,List,DCG,Vars)),!.
dcgRedressL0([],[],[]):-!.
dcgRedressL0([H|T],HHTT,Vars):-dcgRedressSafe(H,HH,Var1),!,dcgRedressL0(T,TT,Var2),append(Var1,Var2,Vars),!,HHTT=[HH|TT].

dcgRedressSafe(H,HH,Var1):-throwOnFailure((dcgRedress(H,HH,Var1))),throwOnFailure((nonvar(HH),nonvar(Var1)),dcgRedressSafe(H,HH,Var1)).

% dcgRedress(A,B,C):-trace,throw(dcgRedress(A,B,C)).


dcgRedress(V,theVar(KW,V),[]):-var(V),!,getKeyVariable(V,KW),!,list_dcgRedressStart.
dcgRedress(Var,A,B):-var(Var),trace,!,printAndThrow('dcgRedress got var in arg1 ~q',[dcgRedress(Var,A,B)]).
dcgRedress(A,NonVar,B):-nonvar(NonVar),trace,!,printAndThrow('dcgRedress got nonvar in arg2 ~q ',[dcgRedress(A,NonVar,B)]).
dcgRedress(A,B,NonVar):-nonvar(NonVar),trace,!,printAndThrow('dcgRedress got nonvar in arg3 ~q ',[dcgRedress(A,B,NonVar)]).
dcgRedress(V,varError(V),[]):-var(V),!,trace,!. %,throw(error(dcgRedress/3,'Arguments are not sufficiently instantiated'))


dcgRedress(X,S,Vars):-string(X),string_to_atom(X,A),dcgRedress_string(A,S,Vars).
dcgRedress(DCG,theGText(DCG),[]):-number(DCG),!.

dcgRedress([Var|Rest],varError([Var|Rest]),[]):-
         var(Var),
         noteFmt('Var in head of list: ~w~n',[[Var|Rest]]),
         list_dcgRedressStart,trace.

dcgRedress(string(S),A,Vars):-!,dcgRedress_string(S,A,Vars).
dcgRedress([string,S],A,Vars):-!,dcgRedress_string(S,A,Vars).

   dcgRedress_string((string(String)),DCG,Var):-!,dcgRedress_string(String,DCG,Var).
   dcgRedress_string([string,String],DCG,Var):-!,dcgRedress_string(String,DCG,Var).
   dcgRedress_string([string|String],DCG,Var):-!,dcgRedress_string(String,DCG,Var).
   dcgRedress_string(X,S,Vars):-string(X),string_to_atom(X,A),dcgRedress_string(A,S,Vars).
   dcgRedress_string(([DCG]),theGText(DCG),[]):-!.
   dcgRedress_string((""),dcgNone,[]):-!.
   dcgRedress_string((''),dcgNone,[]):-!.
   dcgRedress_string((String),DCG,Vars):-atom(String),atom_concat('"',Right,String),atom_concat(New,'"',Right),dcgRedress_string(New,DCG,Vars),!.
   dcgRedress_string((String),DCG,Vars):-atom(String),concat_atom([H,T|List],' ',String),!,dcgRedress_string([H,T|List],DCG,Vars),!.
   dcgRedress_string((String),theGText(A),[]):-atom(String),cleanEachAtomList(String,A),!,(atom(A)->throwOnFailure(atom_codes(A,[_|_]));throwOnFailure(A=[_|_])).
   dcgRedress_string(([D|CG]),theText(DCG),[]):-throwOnFailure(cleanEachAtomList([D|CG],DCG)),noteFmt('~q.~n',theText(DCG)),!,list_dcgRedressStart.

dcgRedress(D,DCG,Vars):-string(D),!,dcgRedress(string(D),DCG,Vars),!.
dcgRedress(String,DCG,Var):-atom(String),atom_concat('"',_,String),!,dcgRedress(string(String),DCG,Var),!.
dcgRedress(TemplateMarker,DCG,Vars):-templateMarkerRepresentation(TemplateMarker,Char),!,dcgRedress(string(Char),DCG,Vars),!.
dcgRedress([TemplateMarker],DCG,Vars):-templateMarkerRepresentation(TemplateMarker,Char),!,dcgRedress(string(Char),DCG,Vars),!.


cleanEachAtomList(B,AA):-cleanEachAtom(B,A),(is_list(A)->delete(A,'',AA);AA=A).

cleanEachAtom([],[]):-!.
cleanEachAtom([svar|S],[svar|S]):-!,trace.
cleanEachAtom(['\'',OneTwo|CG],['\''|CG2]):-atom(OneTwo),atom_length(OneTwo,N),N>0,N<3,!,
   cleanEachAtom([OneTwo|CG],CG2),!.
cleanEachAtom([D|CG],CG2):- cleanEachAtom(D,D2),D2='\'',!,cleanEachAtomList(CG,CG2),!.
cleanEachAtom([D|CG],[D2|CG2]):-!,
   cleanEachAtom(D,D2),!,
   cleanEachAtomList(CG,CG2),!.
cleanEachAtom(D,A):-string(D),string_to_atom(D,M),!,cleanEachAtom(M,A),!.
cleanEachAtom(D,A):-not(atom(D)),!,(D=A->true;trace).
cleanEachAtom(D,D):-atom_codes(D,[_]),!.
cleanEachAtom(D,A):-atom_concat('\\"',M,D),M\='',cleanEachAtom(M,A),!.
cleanEachAtom(D,A):-atom_concat(M,'\\"',D),M\='',cleanEachAtom(M,A),!.
cleanEachAtom(D,A):-atom_concat(M,'\\',D),cleanEachAtom(M,A),!.
cleanEachAtom(D,A):-atom_concat('\\',M,D),cleanEachAtom(M,A),!.
cleanEachAtom(D,A):-atom_concat('\'',M,D),cleanEachAtom(M,A),!.
cleanEachAtom(D,A):-atom_concat(M,'\'',D),cleanEachAtom(M,A),!.
cleanEachAtom(D,A):-atom_concat('"',M,D),cleanEachAtom(M,A),!.
cleanEachAtom(D,A):-atom_concat(M,'"',D),cleanEachAtom(M,A),!.
cleanEachAtom(A,A).

dcgRedress(['theGText'([Word])],DCG,Vars):-dcgRedress_string(Word,DCG,Vars),!.
dcgRedress(['theGText'(Word)],DCG,Vars):-dcgRedress_string(Word,DCG,Vars),!.
dcgRedress(['theGText'|Word],DCG,Vars):-dcgRedress_string(Word,DCG,Vars),!.
dcgRedress(['theGen',VarName,[]],dcgTemplate('AnyTemplate',VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.
dcgRedress(['theGen',VarName,Details],dcgTemplateKW(Details,VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.

dcgRedress(svar(Subj,VarName),theVar(VarName,Subj),[VarName-Subj]):-!,noteFmt('~q~n',[theVar(VarName,Subj)]),list_dcgRedressStart.

dcgRedress(['OptionalSome'|List],dcgOptionalSome(RList),Vars):-dcgRedressL(List,RList,Vars),!.
dcgRedress(['OPTIONALSOME'|List],dcgOptionalSome(RList),Vars):-dcgRedressL(List,RList,Vars),!.
dcgRedress(['OptionalOne'|List],dcgOptionalOne(RList),Vars):-dcgRedressL(List,RList,Vars),!.
dcgRedress(['NLPatternList'|List],DCG,Vars):-!,dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.
dcgRedress(['NLPattern-Exact'|List],DCG,Vars):-!,dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.
dcgRedress(['RequireOne'|List],DCG,Vars):-dcgRedressJunctify(dcgOr,List,DCG,Vars),!.
dcgRedress(['RequireSome'|List],DCG,Vars):-dcgRedressJunctify(dcgRequireSome,List,DCG,Vars),!.
dcgRedress(['WordSequence'],dcgNone,[]):-!.
dcgRedress('WordSequence',dcgNone,[]):-!.
dcgRedress(['WordSequence'|List],DCG,Vars):-dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.
dcgRedress(['NLPattern-Word',Word,Pos],thePOS(Word,Pos),[]):-!.
dcgRedress(['NLPattern-Term',Term,Pos],dcgAnd(thePOS(Pos),theTerm(Term)),[]):-!.
dcgRedress(['TermPOSPair',Term,Pos],dcgAnd(thePOS(Pos),theTerm(Term)),[]):-!.
dcgRedress(['NLPattern-TermPred',Term,Pos],theTermPred(Term,Pos),[]):-!.
dcgRedress(['NLPattern-Template',TempType, VarName],dcgTemplate(TempType,VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.
dcgRedress(['NLPattern-POS',VarName,Pos],dcgPosVar(Pos,VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.
dcgRedress(['NLPattern-Agr',VarName,Pos],dcgPosVar(Pos,VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.

dcgRedress(['WordWithSuffixFn',Word,Suffix],theWord(['WordWithSuffixFn',Word,Suffix]),[]):-!. 

dcgRedress(['BestNLPhraseOfStringFn',VarName],dcgTemplate('AnyTemplate',VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.
dcgRedress(['BestNLPhraseOfStringFn',VarName],dcgTemplate('AnyTemplate',VarName,Subj),[VarName-Subj]):-var(VarName),trace,!.
dcgRedress(['BestNLPhraseOfStringFn',string(String)],DCG,Var):-!,dcgRedress(string(String),DCG,Var),!.
dcgRedress(['BestNLPhraseOfStringFn',String],DCG,Var):-!,dcgRedress(string(String),DCG,Var),!.
dcgRedress(['BestNLWordFormOfLexemeFn',Word],theWord(Word),[]):-!.
dcgRedress(['BestHeadVerbForInitialSubjectFn',Word],theWord(Word),[]):-!.
dcgRedress(['HeadWordOfPhraseFn',Word],Out,Vars):-!,dcgRedress(Word,Out,Vars),!.
dcgRedress(['BestSymbolPhraseFn',Word],dcgOr(theWord(Word),theTermStrings(Word)),[]):-!.

dcgRedress(['ConcatenatePhrasesFn-NoSpaces'|List],dcgNoSpaces(DCG),Vars):-dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.

dcgRedress(['ConcatenatePhrasesFn'|List],DCG,Vars):-dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.
dcgRedress(['TermParaphraseFn-NP', VarName],dcgTemplate('NPTemplate',VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.

% this makes a new arg
dcgRedress(['TermParaphraseFn-NP', Constr],dcgAnd(dcgTemplate('NPTemplate',VarNameO,Subj),isPOS(Constr)),[VarNameO-VarName]):-getKeyVariable(VarName,VarNameO),!.

dcgRedress(['StringMentionFn', VarName],dcgTemplate('StringTemplate',VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.
dcgRedress(['PercentParaphraseFn', VarName],dcgTemplate('PercentTemplate',VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.
dcgRedress(['TermParaphraseFn-Possessive', VarName],dcgTemplate('PossessiveTemplate',VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.

dcgRedress(['QuotedParaphraseFn'|List],dcgMaybeQuoted(DCG),Vars):-dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.


%% TODO these need more work
dcgRedress(['TermParaphraseFn', VarName],dcgTemplate('AnyTemplate',VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.
dcgRedress(['TermParaphraseFn', [KW|List]],DCG,Vars):-getKeyVariable(KW,_VarNameO), dcgRedressJunctify(dcgSeq,[['TermParaphraseFn', _VarNameO]|List],DCG,Vars),!.
dcgRedress(['TermParaphraseFn'],varError('TermParaphraseFn'),[]):- trace,!.
dcgRedress(['TermParaphraseFn' |List],DCG,Vars):- dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.
%dcgRedress(['TermParaphraseFn', VarName],dcgInverse(VarName),[]):-!.

dcgRedress(['BestCycLPhraseFn', VarNameO],dcgCycL(Subj,VarNameO),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.

dcgRedress(['TermParaphraseFn-Constrained', Pos, VarName],dcgPosVar(Pos,VarNameO,Subj),[VarNameO-Subj]):-atom( Pos),getKeyVariable(VarName,VarNameO),!.
dcgRedress(['TermParaphraseFn-Constrained', Constr,List],dcgConstraintBindings(DCG,Constr),Vars):-dcgRedress(List,DCG,Vars),!.
dcgRedress(['BestNLWordFormOfLexemeFn-Constrained', Pos, Word],thePOS(Word,Pos),[]):-atom( Pos),atom(Word),!.
dcgRedress(['ConditionalPhraseFn',Constr|List],dcgConstraint(DCG,Constr),Vars):-!,dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.

% dcgSeqReinterp
dcgRedress(['BestBindingsPhraseFn',Constr,List],dcgConstraintBindings(DCG,Constr),Vars):-nonvar(List),dcgRedress(List,DCG,Vars),!.

dcgRedress(['BestBindingsPhraseFn'|ConstrList],Foo,Bar):-true,!,ignore(Foo=dcgConstraintBindings(ConstrList)),ignore(Bar=[]).


dcgRedress(['RepeatForSubsequentArgsFn', Num,List],dcgRepeatForSubsequentArgsFn(Num,DCG),Vars):-dcgRedress(List,DCG,Vars),!.

dcgRedress(['BestChemicalFormulaFn', List,Counts],dcgBestChemicalFormulaFn(DCG,Counts),Vars):-dcgRedress(List,DCG,Vars),!.




dcgRedress(VarName,dcgTemplate('AnyTemplate',VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.



dcgRedress(['BestDetNbarFn'|List],DCG,Vars):-!,dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.

dcgRedress(['BestDetNbarFn-Indefinite'|List],dcgPhraseType('BestDetNbarFn-Indefinite',DCG),Vars):-!,dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.
dcgRedress(['BestDetNbarFn-Definite'|List],dcgPhraseType('BestDetNbarFn-Definite',DCG),Vars):-!,dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.


dcgRedress(['TypeClarifyingPhraseFn',NP,COMP],dcgSeq(dcgOptional(dcgOptional(theWord('The-TheWord')),DCG1,dcgOptional(theWord('Of-TheWord'))),DCG2),Vars):-!,
   dcgRedress(NP,DCG1,Vars1),dcgRedress(COMP,DCG2,Vars2),append(Vars1,Vars2,Vars),!.



dcgRedress(['BestPPFn',A,B],dcgSeq(theWord(A,'Preposition'),DCG2),Vars):-!,dcgRedress(B,DCG2,Vars),!.

dcgRedress(['BestPPFn'|List],DCG,Vars):-!,dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.


dcgRedress(['NPIsXP-NLSentenceFn',NP,COMP],dcgSeq(DCG1,theWord('Be-TheWord'),DCG2),Vars):-!,
   dcgRedress(NP,DCG1,Vars1),dcgRedress(COMP,DCG2,Vars2),append(Vars1,Vars2,Vars),!.

% ['DefiniteNounPPFn', 'Government-TheWord', '"of"', ['TermParaphraseFn-NP', ':ARG1']]
dcgRedress(['DefiniteNounPPFn'|List],dcgPhraseType('DefiniteNounPPFn',DCG),Vars):-!,dcgRedressJunctify(dcgSeq,List,DCG,Vars),!.

dcgRedress(['PhraseFormFn',Pos,Template],dcgAnd(thePOS(Pos),DCG),Vars):-!,dcgRedress(Template,DCG,Vars),!.
dcgRedress(['BestVerbFormForSubjectFn', Word, NOTE],dcgAnd(thePOS(Word,'Verb'),dcgNote(NOTE)),[]):-!.

dcgRedress(['GenTemplateRecipeOmitsArgFn', Arg, Template], dcgAnd(DCG,dcgNote('GenTemplateRecipeOmitsArgFn'(Arg))),Vars):-dcgRedress(Template,DCG,Vars),!.
      

dcgRedress(['NPTemplate', VarName],dcgTemplate('NPTemplate',VarName,Subj),[VarName-Subj]):-!.
dcgRedress(['PossessiveTemplate', VarName],dcgTemplate('PossessiveTemplate',VarName,Subj),[VarName-Subj]):-!.
dcgRedress(['NBarTemplate', VarName],dcgTemplate('NBarTemplate',VarName,Subj),[VarName-Subj]):-!.
dcgRedress([TemplateType, VarName],dcgTemplate(TemplateType,VarName,Subj),[VarName-Subj]):-
   atom(TemplateType),debugOnError(cycQuery(genls(TemplateType,'ParsingTemplateCategory'))),!.


dcgRedress(['NLConjunctionFn'|List],DCG,Vars):-!, throwOnFailure((dcgRedressL(List,RList,Vars),juntifyList(dcgConjWithWord,['And-TheWord',RList],DCG))),!.

dcgRedress(['NLDisjunctionFn'|List],DCG,Vars):-!, throwOnFailure((dcgRedressL(List,RList,Vars),juntifyList(dcgConjWithWord,['Or-TheWord',RList],DCG))),!.



dcgRedress(THEWORD,theWord(THEWORD),[]):-atom(THEWORD),atom_concat(_,'-TheWord',THEWORD),!.


isNLFunction(NL):-not(atom(NL)),!,fail.
isNLFunction(NL):-concat_atom([_,'NL',_],'',NL),!.
isNLFunction('NPIsNP-NLSentenceFn'):-!.
isNLFunction(NL):-cycQueryIsaCache(NL,'NLFunction').

:-dynamic(notIsaIC/2).
cycQueryIsaCache(I,C):-notIsaIC(I,C),!,fail.
cycQueryIsaCache(I,C):-catch(holdsWithQuery(isa,I,C),_,fail),!.
cycQueryIsaCache(I,C):-asserta(notIsaIC(I,C)),!,fail.

% Term to List
dcgRedress(DCG,DOIT,Vars):-not(DCG=[_|_]),compound(DCG),pterm_to_sterm(DCG,DCGL),(nonvar(DCGL);trace),!,dcgRedress(DCGL,DOIT,Vars),!.
%dcgRedress(DCG,Out,Vars):-notrace(once(cyc:decyclify(DCG,UDCG))),(DCG\== UDCG),!,throwOnFailure(dcgRedress(UDCG,Out,Vars)),!.


isSomeFn(X):-nonvar(X),cmember(X,['SomeFn','AnyFn','GovernmentFn','EveryFn','ManyFn','CollectionSubsetFn','FormulaArgFn','SpecsFn']).
isSomeFn(X):-atom(X),atom_concat('SubcollectionOf',_,X).
isSomeFn(X):-atom(X),concat_atom(['Collection',_,'Fn'],'',X).


dcgRedress([SomeFn,Var|Rest],dcgAnd(dcgReverse([SomeFn,VarNameO|Rest]),theVar(Var,Subj)),[VarNameO-Subj]):-isSomeFn(SomeFn),getKeyVariable(Var,VarNameO),!.
dcgRedress([SomeFn|Rest],dcgReverse([SomeFn|Rest]),[]):-isSomeFn(SomeFn),!.

dcgRedress(['PhraseCycLFn',Var,Rest],dcgAnd(DCG,theVar(VarNameO,Subj)),[VarNameO-Subj|Vars]):-getKeyVariable(Var,VarNameO),dcgRedress(Rest,DCG,Vars),!.

isLexicalWord(THEWORD):-not(atom(THEWORD)),!,fail.
isLexicalWord([]):-!,fail.
isLexicalWord(THEWORD):-atom_length(THEWORD,N),N<3,!,fail.
isLexicalWord(THEWORD):-atom_concat(_,'-TheWord',THEWORD),!.
isLexicalWord(THEWORD):-atom_concat(_,'-MWS',THEWORD).
isLexicalWord(THEWORD):-atom_concat(_,'-MWW',THEWORD).
isLexicalWord(THEWORD):-cycQueryIsaCache(THEWORD,'LexicalWord').

dcgRedress([THEWORD,POS],theWord(THEWORD,POS),[]):-isLexicalWord(THEWORD),!.
dcgRedress(['svar',P,OS],DCG,Vars):-dcgRedress('svar'(P,OS),DCG,Vars),!.
dcgRedress([THEWORD|POS],dcgReverse([THEWORD|POS]),[]):-isLowercaseAtom(THEWORD),noteFmt('dcgReverse: ~q.~n',[[THEWORD|POS]]),list_dcgRedressStart.

isLowercaseAtom(THEWORD):-not(atom(THEWORD)),!,fail.
isLowercaseAtom([]):-!,fail.
isLowercaseAtom(THEWORD):-atom_codes(THEWORD,[C|_]),!,char_type(C,lower).

dcgRedress(DCG,Out,Vars):-notrace(once(cyc_decyclify(DCG,UDCG))),(DCG\== UDCG),!,throwOnFailure(dcgRedress(UDCG,Out,Vars)),!.

dcgRedress([TerseParaphraseFn,Phrase],dcgAnd(DCG,dcgNote(TerseParaphraseFn)),Vars):-
      cmember(TerseParaphraseFn,['TerseParaphraseFn','PreciseParaphraseFn']),
      dcgRedress(Phrase,DCG,Vars),!.



dcgRedress([D|CG],dcgDressed(DCG),Vars):-compound(D),throwOnFailure((dcgRedressL([D|CG],RList,Vars),juntifyList(dcgSeq,RList,DCG))),!.
dcgRedress([D|CG],dcgDressedString(DCG),Vars):-atom(D),atom_concat('"',_,D),throwOnFailure((dcgRedressL([D|CG],RList,Vars),juntifyList(dcgSeq,RList,DCG))),!.


isNLFunctionNonExpandedArgs('NDecimalPlaceParaphraseFn').
dcgRedress([REDRESS,KW|ARGS],dcgNonExpandedVar(REDRESS,VarNameO,ARGS,Subj),[VarNameO-Subj]):-isNLFunctionNonExpandedArgs(REDRESS),getKeyVariable(KW,VarNameO),!.
dcgRedress([REDRESS|ARGS],dcgNonExpanded(REDRESS,ARGS),[]):-isNLFunctionNonExpandedArgs(REDRESS),!.

isNLFunctionNonExpanded(A):-not(atom(A)),!,fail.
isNLFunctionNonExpanded([]):-!,fail.
isNLFunctionNonExpanded(NP,'NounPhrase'):-holdsWithQuery(resultIsa,NP,'NounPhrase'),!.
isNLFunctionNonExpanded('NPIsNP-NLSentenceFn','NLSentence'):-!.
isNLFunctionNonExpanded('NPIsXP-NLSentenceFn','NLSentence'):-!.
isNLFunctionNonExpanded(NP,RealType):-holdsWithQuery(resultIsa,NP,'NLPhrase'),catch(cycQuery(and(resultIsa(NP,RealType),genls(RealType,'NLPhrase'))),_,RealType='NLPhrase'),!,
   noteFmt('nl: ~q.~n',[isNLFunctionNonExpanded(NP,RealType)]),!,list_dcgRedressStart.

dcgRedress([REDRESS|ARGS],dcgNonExpandedFrom(RI,REDRESS,DCG),Vars):- nonvar(REDRESS), isNLFunctionNonExpanded(REDRESS,RI),!,
   % noteFmt('isNLFunctionNonExpanded: ~q.~n',[[REDRESS|ARGS]]),!,list_dcgRedressStart,
   % trace,
   dcgRedressL(ARGS,DCG,Vars),!.



%use expansion
dcgRedress([REDRESS|ARGS],Out,Vars):-fail,
   isNLFunction(REDRESS),
   holdsWithQuery(expansion,REDRESS,NEW),
   cycUseExpansion([REDRESS|ARGS],NEW2),
   dcgRedress(NEW2,Out,Vars),!,
   noteFmt('~q -> ~q -> ~q ~n',[[REDRESS|ARGS],NEW2,Out]),
   list_dcgRedressStart.

cycUseExpansion([REDRESS|ARGS],NEW2):-holdsWithQuery(expansion,REDRESS,NEW),
   V = varHolder(NEW),
   ignore((
      nth0(N,[REDRESS|ARGS],ARG),N>0,
      arg(1,V,Value),
      atom_concat(':ARG',N,ARGNAME),
      subst(Value,ARGNAME,ARG,NewValue),
      nb_setarg(1,V,NewValue),
      fail)),
   arg(1,V,NEW2),!.
   
   

   /*

%use expansion
dcgRedress([REDRESS,ARG1],Out,Vars):-isNLFunction(REDRESS),holdsWithQuery(expansion,REDRESS,NEW),
   substEach(NEW,[':ARG1'-ARG1],NEW2),dcgRedress(NEW2,Out,Vars),!,list_dcgRedressStart.
dcgRedress([REDRESS,ARG1,ARG2],Out,Vars):- isNLFunction(REDRESS),holdsWithQuery(expansion,REDRESS,NEW),
   substEach(NEW,[':ARG1'-ARG1,':ARG2'-ARG2],NEW2),dcgRedress(NEW2,Out,Vars),!,list_dcgRedressStart.
dcgRedress([REDRESS,ARG1,ARG2,ARG3],Out,Vars):- isNLFunction(REDRESS),holdsWithQuery(expansion,REDRESS,NEW),
   substEach(NEW,[':ARG1'-ARG1,':ARG2'-ARG2,':ARG3'-ARG3],NEW2),dcgRedress(NEW2,Out,Vars),!,list_dcgRedressStart.
dcgRedress([REDRESS,ARG1,ARG2,ARG3,ARG4],Out,Vars):- isNLFunction(REDRESS),holdsWithQuery(expansion,REDRESS,NEW),
   substEach(NEW,[':ARG1'-ARG1,':ARG2'-ARG2,':ARG3'-ARG3,':ARG4'-ARG4],NEW2),dcgRedress(NEW2,Out,Vars),!,list_dcgRedressStart.
dcgRedress([REDRESS,ARG1,ARG2,ARG3,ARG4,ARG5],Out,Vars):- isNLFunction(REDRESS),holdsWithQuery(expansion,REDRESS,NEW),
   substEach(NEW,[':ARG1'-ARG1,':ARG2'-ARG2,':ARG3'-ARG3,':ARG4'-ARG4,':ARG5'-ARG5],NEW2),dcgRedress(NEW2,Out,Vars),!,list_dcgRedressStart.
*/
dcgRedress([REDRESS|ARGS],DCG,Vars):-isNLFunction(REDRESS),
   noteFmt('isNLFunction: ~q.~n',[[REDRESS|ARGS]]),!,list_dcgRedressStart,
   %trace,
   dcgRedressL(ARGS,DCG,Vars),!.


%TODO comment out unless for file testing
dcgRedress([D|CG],dcgSeqReinterp([D|CG]),[]):-noteFmt('dcgSeqReinterp: ~q.~n',[[D|CG]]),list_dcgRedressStart,!.

dcgRedress([D|CG],dcgSeqReinterp(DCG),Vars):-noteFmt('dcgSeqReinterp: ~q.~n',[[D|CG]]),list_dcgRedressStart,
      throwOnFailure((dcgRedressL([D|CG],RList,Vars),juntifyList(dcgSeq,RList,DCG))),!.


dcgRedress('\\',dcgNone,[]):-!.
dcgRedress(THEWORD,theWord(THEWORD),[]):-isLexicalWord(THEWORD),!.
dcgRedress(VarName,dcgTemplate('AnyTemplate',VarNameO,Subj),[VarNameO-Subj]):-getKeyVariable(VarName,VarNameO),!.


dcgRedress(DCG,dcgReinterp(DCG),[]):-noteFmt('dcgReinterp: ~q.~n',[DCG]),!,list_dcgRedressStart.


%holdsWithQuery(genTemplate, geographicalSubRegions, ['ConcatenatePhrasesFn', ['TermParaphraseFn-DO', svar(A, ':ARG2')], ['BestHeadVerbForInitialSubjectFn', 'Be-TheWord'], ['BestNLPhraseOfStringFn', string([a])], ['BestNLPhraseOfStringFn', string([geographical])], ['BestNLPhraseOfStringFn', string([subregion])], ['BestPPFn', 'Of-TheWord', ['TermParaphraseFn-DO', svar(B, ':ARG1')]]]).



% =======================================================
% Mining genFormat
% =======================================================

/*


 S = '(#$headMedialString "the head-medial string containing ~a, ~a, and ~a is ~a and denotes ~a" (#$TheList 1 (#$TheList 2 :|equals|) 3 (#$TheList 4 :A-THE-WORD) (#$TheList 5 :|equals|)))',
 getWordTokens(S,W).




 atom_codes("(#$headMedialString \"the head-medial string containing ~a, ~a, and ~a is ~a and denotes ~a\" (#$TheList 1 (#$TheList 2 :|equals|) 3 (#$TheList 4 :A-THE-WORD) (#$TheList 5 :|equals|)))",C),getSurfaceFromChars(C,F,A).

 atom_codes("(#$headMedialString \"th ~a\" )",C),tokenize3(C,Ts).


 OLD 

genFormatH(W1,W2,Reln,[W1,'~a',W2|REst],ArgListL):-cmember(GenFormat,['genFormat-Precise','genFormat']),holdsWithQuery(GenFormat,Reln,String,ArgListL),stringUnify(String,[W1,'~a',W2|REst]).
genFormatH(W1,W2,Reln,['~a',W1,W2|REst],ArgListL):-cmember(GenFormat,['genFormat-Precise','genFormat']),holdsWithQuery(GenFormat,Reln,String,ArgListL),stringUnify(String,['~a',W1,W2|REst]).
genFormatH(W1,W2,Reln,[W1,W2|REst],ArgListL):-cmember(GenFormat,['genFormat-Precise','genFormat']),holdsWithQuery(GenFormat,Reln,String,ArgListL),stringUnify(String,[W1,W2|REst]).

genFormatU([W1,W2],DCG,[Reln|Args]):-
        genFormatH(W1,W2,Reln,DCGPre,ArgListLC),notrace(cyc:decyclify(ArgListLC,ArgListL)),!,
         reformList(ArgListL,ArgList),
        % noteFmt('~nArgList: ~q -> ~q:~q ~n',[Reln,DCGPre,ArgList]),
         once(dcgRedressGenFormat(0,DCGPre,ArgList,DCG,Vars)),Vars=Vars,
         %catch((once(dcgRedressGenFormat(1,DCGPre,ArgList,DCG,Vars)),sort(Vars,VarsS)),E,noteFmt('Error: ~q ~n',[DCGPre])),
         once(((getArity(Reln,Arity);length(ArgList,Arity)),number(Arity),makeArgNameLen(Arity,Args))).
*/

genFormatH(Reln,[S|Plit],ArgListL):-cmember(GenFormat,['genFormat-Precise','genFormat']),
     holdsWithQuery(GenFormat,Reln,String,ArgListL),
      nonvar(String), %%writeq(holdsWithQuery(GenFormat,Reln,String,ArgListL)),nl,
     throwOnFailure(nonvar(String)),
     once(notrace(stringUnify(String,[S|Plit]))),ignore((cmember('~A',[S|Plit]),trace,writeq(String),stringUnify(String,[_S|_Plit]))).

genFormatU(DCG,[Reln|Args]):-
        genFormatH(Reln,DCGPre,ArgListL),       
         once(reformList(ArgListL,ArgList)),
        % noteFmt('~nArgList: ~q -> ~q:~q ~n',[Reln,DCGPre,ArgList]),
         once(dcgRedressGenFormat(0,DCGPre,ArgList,DCG,Vars)),Vars=Vars,
         %catch((once(dcgRedressGenFormat(1,DCGPre,ArgList,DCG,Vars)),sort(Vars,VarsS)),E,noteFmt('Error: ~q ~n',[DCGPre])),
         once(((getArity(Reln,Arity);length(ArgList,Arity)),number(Arity),makeArgNameLen(Arity,Args))).



%dcgRedressGenFormat([DC|GPre],ArgList,DCG,Vars).
replGenFormat(N,varError(N)):-not(ground(N)),!,trace.
replGenFormat([string(['[',']']),string(['s'])],dcgTrue).
replGenFormat([string(['is']),string(['are'])],dcgTrue).
replGenFormat([string(N)|How],New):-replGenFormat([N|How],New).
replGenFormat([N|How],theGen(Name,How)):-argName(N,Name). %,unSvar(SHow,How).
replGenFormat(N,theGen(Name,[])):-argName(N,Name). %,unSvar(SHow,How).
replGenFormat(N,N):-trace.

% utils

reformList(X,X):-var(X),!.
reformList('TheEmptyList',[]):-!.
reformList(X,X):-not(compound(X)),!.
reformList([TheList|ArgList],X):- TheList=='TheList',!,reformList(ArgList,X),!.
reformList([H|T],[HH|TT]):-!,reformList(H,HH),!,reformList(T,TT),!.
reformList(svar(_,B),B):-!.
reformList(B,A):-functor(B,'TheList',_), B=..BL,reformList(BL,A),!.
reformList(B,A):-B=..BL,reformList(BL,AL),A=..AL,!.


cleanForA(S,[],S):-!.
cleanForA(S,[B-A],SA):-concat_atom(List,B,S),concat_atom(List,A,SA),!.
cleanForA(S,[B-A|More],SA1):-
  cleanForA(S,[B-A],SA),cleanForA(SA,More,SA1),!.

stringUnify(S,S):-var(S),trace,!,fail.
stringUnify(S,SU):-var(SU),S=SU,!.
stringUnify([S|H],U):-concat_atom([S|H],' ',A),!,stringUnify(A,U),!.
stringUnify(string(S),U):-!,stringUnify(S,U),!.
stringUnify(S,U):-atom(S),atom_concat('"',Right,S),atom_concat(New,'"',Right),!,stringUnify(New,U),!.
stringUnify(S,U):-string(S),!,string_to_atom(S,A),!,stringUnify(A,U),!.
stringUnify(S,U):-not(atom(S)),!,trace,fail.
stringUnify(S,U):-cleanForA(S,[
   '<br>'-' ',
      '\n'-' ',
      '\t'-' ',
      '~A'-'~a',
      '~a'-' ~a ',
      (',')-' , ',
      ('.')-' . ',
      (':')-' : ',
      ('!')-' ! ',
                            '('-' ( ',
                            ')'-' ) ',
                            '['-' [ ',
                            ']'-' ] ',
                            '{'-' { ',
                            ('}')-(' } '),
                            ('\`')-(' \` '),
                            ('$')-' $ ',
                            '%'-' % ',
                            '"'-' " ',
                            '?'-' ? ',
                            '\''-' \' ',
                            % this one breaks it? bug in concat_atom '\\'-' \\ ',
   '  '-' ',
   '  '-' ',
   '# $ '-'#$'

   ],SA),concat_atom(U,' ',SA),!.
stringUnify(S,U):-concat_atom(U,' ',S),!.


dcgRedressGenFormat(N,A2,ArgList,DCG,varError(N)):-not(ground(ArgList:A2)),!,trace.
dcgRedressGenFormat(N,['~a'|DCGPre],[How|ArgList],[D|DCG],Vars):-replGenFormat(How,D),N2 is N+1,dcgRedressGenFormat(N2,DCGPre,ArgList,DCG,Vars).
dcgRedressGenFormat(N,['~a'|DCGPre],ArgList,[theGen(SVAR,[])|DCG],Vars):-N2 is N+1,argName(N2,SVAR),dcgRedressGenFormat(N2,DCGPre,ArgList,DCG,Vars).
%dcgRedressGenFormat(N,['~a'|DCGPre],[['TheList',Num,Type]|ArgList],[repl(SVAR,Type)|DCG],Vars):-argName(Num,SVAR),dcgRedressGenFormat(N,DCGPre,ArgList,DCG,Vars).
dcgRedressGenFormat(N,[W|DCGPre],ArgList,[theGText([W])|DCG],Vars):-dcgRedressGenFormat(N,DCGPre,ArgList,DCG,Vars).
dcgRedressGenFormat(N,DCG,ArgList,DCG,Vars).

%writeq(ac(comment, 'ScientificNumberFn', string(['(', '#$ScientificNumberFn', 'SIGNIFICAND', 'EXPONENT', ')', denotes, a, number, in, scientific, notation, which, has, 'SIGNIFICAND', with, an, implicit, decimal, point, after, its, first, digit, as, its, significand, and, 'EXPONENT', as, its, exponent, '.', 'So', (','), e, '.', g, '.', (','), '(', '#$ScientificNumberFn', 314159, 0, ')', denotes, '3.14159.', 'Likewise', (','), '(', '#$ScientificNumberFn', 23648762469238472354, 32, ')', denotes, 2.36487624692e+032, or, 2.36487624692, *, 10, ^, 32, '.']))).

genTemplateU(Reln,DCGPre,[Reln|Args]):-
         holdsWithQuery(genTemplate,Reln,DCGPre),
         once((getArity(Reln,Arity),number(Arity),makeArgNameLen(Arity,Args))).

/*
%todo
getGenFormat(PredType,_Template,ARGS):-
   holdsWithQuery(genFormat,Pred,string(Str),How),%getPredType(Pred,PredType,Arity),
   genArgList(1,PredType,Arity,How,Str,StrFmt,ARGS).

%genArgList(N,How,Str,StrFmt,ARGS):-
*/


      /*
        ,cycAssert(wordSemTrans(WORD,33,'Post-NounPhraseModifyingFrame',CYCL,'Preposition','prepReln-Object',and(isa(':NOUN',NOUN),isa(':OBLIQUE-OBJECT',OBJECT))),'#$EnglishMt'),fail.

        (implies (and (compoundSemTrans ?WORD  (TheList ?S1)  ?POS TransitiveInfinitivePhraseFrame   ?CYCL)
        (wordStrings ?WORDW ?S1) )
        (wordSemTrans ?WORD 30 (PPCompFrameFn TransitiveParticleNPFrameType ?WORDW) ?CYCL ?POS compoundSemTrans True))                                               
          */

        %wordSemTrans(Word,SenseNum,Frame,CycL,Pos
        %(resultIsa FrameRestrictionFn SubcategorizationFrame)
        %  ?SubcategorizationFrame 


         /*

        (cyc-assert ' #$UniversalVocabularyMt)

        (isa wordSemTrans NLSemanticPredicate)               
        (arity wordSemTrans 7)

        (argIsa wordSemTrans 1 LexicalWord)
        (argIsa wordSemTrans 2 Thing)
        (argIsa wordSemTrans 3 Thing)
        (argIsa wordSemTrans 4 Thing)
        (argIsa wordSemTrans 5 LinguisticObjectType)
        (argIsa wordSemTrans 6 Predicate)
        (argIsa wordSemTrans 7 Thing)
        (#$comment   #$wordSemTrans "(#$wordSemTrans #$LexicalWord #$Integer #$SubcategorizationFrame #$NLTemplateExpression #$LinguisticObjectType #$Predicate #$NLTemplateExpression)")


        (and



        (implies (clauseSemTrans ?WORD ?NUM ?FRAME  ?CYCL) (wordSemTrans  ?WORD ?NUM ?FRAME ?CYCL Conjunction clauseSemTrans True))
        (implies (nounSemTrans ?WORD ?NUM ?FRAME  ?CYCL) (wordSemTrans  ?WORD ?NUM ?FRAME ?CYCL Noun nounSemTrans True))

        (implies 
               (and (denotation ?WORD ?POS ?NUM ?COL)(isa ?COL Collection)(genls ?POS Noun))
                       (wordSemTrans ?WORD ?NUM RegularNounFrame (isa :NOUN ?COL) ?POS denotation True))
        (implies 
               (and (denotation ?WORD ?POS ?NUM ?COL)(isa ?COL Event)(genls ?POS Verb))
                       (wordSemTrans ?WORD ?NUM RegularNounFrame (and (situationConstituents :ACTION :SUBJECT)(isa :ACTION ?COL)) ?POS denotation True))

        (implies (and (compoundSemTrans ?WORD  (TheList ?S1)  ?POS IntransitiveVerbFrame ?CYCL)
        (partOfSpeech ?WORDW ?POSW ?S1) )
        (wordSemTrans ?WORD 30 (PPCompFrameFn TransitiveParticleNPFrameType ?WORDW) ?CYCL ?POS compoundSemTrans True))                                               

        (implies (and (compoundSemTrans ?WORD  (TheList ?S1)  ?POS TransitiveNPFrame  ?CYCL)
        (partOfSpeech ?WORDW ?POSW ?S1) )
        (wordSemTrans ?WORD 30 (PPCompFrameFn TransitiveParticleNPFrameType ?WORDW) ?CYCL ?POS compoundSemTrans True))      


        (implies (and (compoundSemTrans ?WORD  (TheList ?S1)  ?POS TransitiveInfinitivePhraseFrame   ?CYCL)
        (wordStrings ?WORDW ?S1) )
        (wordSemTrans ?WORD 30 (PPCompFrameFn TransitiveParticleNPFrameType ?WORDW) ?CYCL ?POS compoundSemTrans True))                                               

        (implies 
               (prepSemTrans-New ?WORD ?POS ?FRAME ?CYCL) 
               (wordSemTrans ?WORD 30 ?FRAME ?CYCL ?POS prepSemTrans-New True))

        (implies 
               (and 
                   (nounPrep ?WORD ?PREP ?CYCL) 
                   (termOfUnit ?PPCOMPFRAMEFN 
                       (PPCompFrameFn TransitivePPFrameType ?PREP))) 
               (wordSemTrans ?WORD 30 ?PPCOMPFRAMEFN ?CYCL Noun nounPrep True))

        (implies 
               (prepReln-Action ?ACTION ?OBJECT ?WORD ?CYCL) 
               (wordSemTrans ?WORD 30 VerbPhraseModifyingFrame ?CYCL Preposition prepReln-Action 
                   (and 
                       (isa :ACTION ?ACTION) 
                       (isa :OBLIQUE-OBJECT ?OBJECT))))
        (implies 
               (prepReln-Object ?NOUN ?OBJECT ?WORD ?CYCL) 
               (wordSemTrans ?WORD 30 Post-NounPhraseModifyingFrame ?CYCL Preposition prepReln-Object 
                   (and 
                       (isa :NOUN ?NOUN) 
                       (isa :OBLIQUE-OBJECT ?OBJECT))))
        (implies 
               (and 
                   (semTransPredForPOS ?POS ?Pred) 
                   (?Pred ?WORD ?NUM ?FRAME ?CYCL)) 
                     (wordSemTrans ?WORD ?NUM ?FRAME ?CYCL ?POS ?Pred True))

        (implies 
                     (adjSemTrans-Restricted ?WORD ?NUM ?FRAME ?COL ?CYCL)
                     (wordSemTrans ?WORD ?NUM ?FRAME ?CYCL Adjective adjSemTrans-Restricted (isa :NOUN ?COL)))

        (implies 
                     (nonCompositionalVerbSemTrans ?WORD ?COL ?CYCL)
                     (wordSemTrans ?WORD 666 VerbPhraseModifyingFrame ?CYCL Verb nonCompositionalVerbSemTrans (isa :OBJECT ?COL)))
        (implies 
                     (lightVerb-TransitiveSemTrans ?WORD ?COL ?CYCL)
                     (wordSemTrans ?WORD 667 VerbPhraseModifyingFrame ?CYCL Verb lightVerb-TransitiveSemTrans (isa :OBJECT ?COL)))

        (implies 
                     (verbSemTransPartial  ?WORD ?NUM ?FRAME ?CYCL)
                     (wordSemTrans ?WORD ?NUM ?FRAME ?CYCL Verb verbSemTransPartial True))





        (implies 
               (and 
                   (isa ?OBJ ?COL) 
                   (adjSemTransTemplate ?COL ?FRAME ?CYCL) 
                   (denotation ?WORD Adjective ?NUM ?OBJ) 
                   (evaluate ?TRANS 
                       (SubstituteFormulaFn ?OBJ :DENOT ?CYCL))) 
               (wordSemTrans  ?WORD ?NUM ?FRAME ?TRANS Adjective adjSemTrans True ))

        (implies 
                     (and 
                         (genls ?SPEC ?COL) 
                         (verbSemTransTemplate ?COL ?FRAME ?CYCL) 
                         (denotation ?WORD Verb ?NUM ?SPEC) 
                         (evaluate ?TRANS 
                             (SubstituteFormulaFn ?SPEC :DENOT ?CYCL))) 
               (wordSemTrans  ?WORD ?NUM ?FRAME ?TRANS Verb verbSemTrans True ))
             )  
             
             
(implies 
    (and 
        (relationInstanceAll performsInstancesAsPartOfJob ?REFINISHING ?REFINISHER) 
        (subcatFrame ?RENOVATE-THEWORD Verb ?NUM ?TRANSITIVENPFRAME) 
        (denotation ?RENOVATE-THEWORD AgentiveNoun ?NUM2 ?REFINISHER)) 
    (wordSemTrans ?RENOVATE-THEWORD Verb ?TRANSITIVENPFRAME 
        (thereExists :ACTION 
            (and 
                (bodilyDoer :SUBJECT :ACTION) 
                (isa :ACTION ?REFINISHING) 
                (possible 
                    (isa :SUBJECT ?REFINISHER)))) performsInstancesAsPartOfJob))

(implies 
    (and 
        (different ?WORD1 ?WORD2) 
        (semTransPredForPOS ?POS ?PRED) 
        (denotesOpposite ?WORD1 ?POS ?NUM1 ?CONCEPT) 
        (denotation ?WORD2 ?POS ?NUM2 ?CONCEPT) 
        (?PRED ?WORD2 ?NUM2 ?FRAME ?FORMULA)) 
    (?PRED ?WORD1 ?NUM1 ?FRAME 
        (not ?FORMULA)))
        
        
(implies 
    (and 
        (isa ?BELIEFS BinaryPredicate) 
        (denotesOpposite ?DISBELIEVE-THEWORD Verb ?NUM ?BELIEFS)) 
    (wordSemTrans ?DISBELIEVE-THEWORD Verb TransitiveNPFrame 
        (thereExists :ACTION 
            (not 
                (holdsIn :ACTION 
                    (?BELIEFS :SUBJECT :OBJECT)))) denotesOpposite))       
                    
(implies 
    (and 
        (isa ?BELIEFS BinaryPredicate) 
        (denotation ?BELIEVE-THEWORD Verb ?NUM ?BELIEFS)) 
    (wordSemTrans ?BELIEVE-THEWORD Verb TransitiveNPFrame 
        (thereExists :ACTION 
                (holdsIn :ACTION 
                    (?BELIEFS :SUBJECT :OBJECT))) denotation))                                              
(implies 
    (and 
        (isa ?BELIEFS BinaryPredicate) 
        (denotationRelatedTo ?BELIEVE-THEWORD Verb ?NUM ?BELIEFS)) 
    (wordSemTrans ?BELIEVE-THEWORD Verb TransitiveNPFrame 
        (thereExists :ACTION 
                (holdsIn :ACTION 
                    (?BELIEFS :SUBJECT :OBJECT))) denotationRelatedTo))                                              
                                                                                                            


         */

%>

