% ===================================================================
% File 'logicmoo_module_aiml_graphmaster.pl'
% Purpose: An Implementation in SWI-Prolog of AIML
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_module_aiml_graphmaster.pl' 1.0.0
% Revision:  $Revision: 1.7 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================

:-ensure_loaded(library('programk/logicmoo_module_aiml_gmidx.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_convertor.pl')).
:-ensure_loaded(library('programk/logicmoo_module_aiml_cxt_path.pl')).

% ===============================================================================================
% Callable input
% ===============================================================================================
callableInput(_Ctx,String,_Input,_Output):-traceIf(var(String)),fail.
callableInput(Ctx,[S|Tring],Input,Output):-joinAtoms([S|Tring],' ',String),!,callableInput0(Ctx,String,Input,Output).
callableInput(Ctx,String,Input,Output):-string(String),string_to_atom(String,Atom),!,callableInput0(Ctx,Atom,Input,Output).
callableInput(Ctx,Atom,Input,Output):-callableInput0(Ctx,Atom,Input,Output).

callableInput0(Ctx,[String],Input,Output):-!,callableInput(Ctx,String,Input,Output).
callableInput0(_Ctx,NonAtom,_Input,_Output):- not(atom(NonAtom)),!,fail.
callableInput0(_Ctx,Atom,_Input,result(Term,Vars)):- notrace(catch(atom_to_term(Atom,Term,Vars),_,fail)),
  callable(Term),catch(callInteractive0(Term,Vars /*,Results */),_,fail).
callableInput0(Ctx,Atom,_Input,VotesO-Output):-atom_prefix(Atom,'@'),
  % re-direct to input
  withAttributes(Ctx,[],prolog_must(computeAnswer(Ctx,1,element(system,[],Atom),Output,VotesO))),!.


% ===============================================================================================
% Eval a SRAI
% ===============================================================================================
computeSRAIElement(Ctx,Votes,ATTRIBS,Input0,Output,VotesO):-
 withAttributes(Ctx,ATTRIBS, ((
   computeSRAIElement0(Ctx,Votes,ATTRIBS,Input0,OutputM,VotesOM),
   computeTemplateOutput(Ctx,VotesOM,OutputM,Output,VotesO)))),!.

computeSRAIElement0(Ctx,Votes,ATTRIBS,Input0,Output,VotesO):-
  prolog_must(ground(Input0)),!,
  flatten([Input0],Input),
  thread_local_flag(sraiDepth,SraiDepth,SraiDepth+1),
  computeSRAIElement1(Ctx,Votes,SraiDepth,ATTRIBS,Input,Output,VotesO),
  thread_local_flag(sraiDepth,_,SraiDepth),!.

computeSRAIElement1(Ctx,Votes,SraiDepth,ATTRIBS,Input,Output,VotesO):-SraiDepth>1,!,evalSRAI(Ctx,Votes,SraiDepth,ATTRIBS,Input,Output,VotesO),!.
computeSRAIElement1(Ctx,Votes,SraiDepth,ATTRIBS,Input,Output,VotesO):-catch(evalSRAI(Ctx,Votes,SraiDepth,ATTRIBS,Input,Output,VotesO),aiml_goto(Output,VotesO),thread_local_flag(sraiDepth,_,0)),!.


evalSRAI(Ctx,Votes,SraiDepth,ATTRIBS,_Input,_Unusued,_VotesO):- SraiDepth>80,
  getAliceMem(Ctx,bot,'infinite-loop-input',Output),!,VotesO is Votes * 0.8,
  throw_aiml_goto(element(srai,ATTRIBS,Output),VotesO).
  %%throw_aiml_goto(proof(element(template,ATTRIBS,[element(srai,ATTRIBS,Output)]),loop(sraiDepth,SraiDepth,80,ATTRIBS,Input)),VotesO).

/*
evalSRAI(Ctx,Votes,_SraiDepth,ATTRIBS,Input,_Unusued,_VotesO):-
 frame_depth(Depth),Depth>3000,getAliceMem(Ctx,bot,'infinite-loop-input',Output),!,VotesO is Votes * 0.8,
 throw_aiml_goto(proof(element(template,ATTRIBS,[element(srai,ATTRIBS,Output)]),loop(frameDepth,Depth,3000,ATTRIBS,Input)),VotesO).
*/


evalSRAI(Ctx,Votes,_SraiDepth,ATTRIBS,[I|Input0],Output,VotesO):-atom(I),atom_prefix(I,'@'),!,
  % re-direct to input
  withAttributes(Ctx,ATTRIBS,prolog_must(computeAnswer(Ctx,Votes,element(system,ATTRIBS,[I|Input0]),Output,VotesO))),!.

evalSRAI(Ctx,Votes,_SraiDepth,ATTRIBS,Input,Output,VotesO):-
 prolog_must(var(SYM)),
 prolog_must(peekNameValue(Ctx,ATTRIBS,['evalsrai','userdict','scope'],SYMPREV,'$value'(user))),
 ifThen(var(SYM),evalsrai(SYM)),
 nop(ignore(var(Proof))), 
   withAttributes(Ctx,['evalsrai'=SYM,proof=Proof],
  ((
    setup_call_cleanup(addInherit(SYM,SYMPREV),
    ((
    debugOnError(computeSRAI(Ctx,Votes,SYM,Input,MidIn,VotesM,Proof)),

    computeSRAIStars(Ctx,ATTRIBS,Input,MidIn,VotesM,SYM,Proof,Output,VotesO))),
    remInherit(SYM,SYMPREV)),
    ifThen(nonvar(SYM),retractallSrais(SYM))))).

    
computeSRAIStars(Ctx,ATTRIBS,Input,MidIn,VotesM,SYM,Proof,Output,VotesO):- fail,
    prolog_must((nonvar(MidIn),
                 nonvar(SYM),
                 singletons([Ctx,ATTRIBS]),
                 nonvar(Proof))),
      %% Proof = Output,       
      MidIn = Output, 
      VotesM = VotesO,
      nop(debugFmt(computeSRAIStars(SYM,Input,Output))),
      prolog_must((ground(Output),number(VotesO))),!.

computeSRAIStars(Ctx,ATTRIBS,Input,MidIn,VotesM,SYM,Proof,Output,VotesO):-
    prolog_must((nonvar(MidIn),
                 nonvar(SYM),
                 nonvar(Proof))),
      setCtxValue(Ctx,'evalsrai',SYM),
      %%MidProof = Proof, 
      computeElementMust(Ctx,VotesM,template,ATTRIBS,MidIn,MidIn9,VotesI9),
      prolog_must(answerOutput(MidIn9,Mid9)),
      debugFmt(evalSRAI(SYM,Input,MidIn,MidIn9,Mid9)),
      prolog_must(computeAnswer(Ctx,VotesI9,Mid9,Output,VotesO)),
      prolog_must((ground(Output),number(VotesO))),!.

 evalsrai(SYM):-gensym('evalsrai',SYM).

% ===============================================================================================
% Apply Input Match
% ===============================================================================================

computeSRAI(_Ctx,_Votes,_SYM,[],_,_,_Proof):- !,atrace,fail.

computeSRAI(Ctx,Votes,SYM,Input,Result,VotesO,Proof):-
   getAliceMem(Ctx,'bot','me',Robot),
   getAliceMem(Ctx,'bot','you',User),
   ifThen(var(SYM),evalsrai(SYM)),
   getConversationThread(Ctx,User,Robot,ConvThread),
   prolog_must(computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof)).

getConversationThread(Ctx,User,Robot,ConvThread):-
   ConvThread = fromTo(User,Robot),
   setCtxValue(Ctx,'convthread',ConvThread),!.

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):-   
   computeInnerTemplate(Ctx,Votes,Input,NewIn,VotesM),NewIn \== Input,!,
   computeSRAI0(Ctx,VotesM,ConvThread,SYM,NewIn,Result,VotesO,Proof),!.

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):- not(is_list(Input)),compound(Input),
   answerOutput(Input,InputO),Input\==InputO,!,atrace,
   computeSRAI0(Ctx,Votes,ConvThread,SYM,InputO,Result,VotesO,Proof).

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):- not(is_list(Input)),compound(Input),
   computeAnswer(Ctx,Votes,Input,InputO,VotesM),Input\==InputO,!,atrace,
   computeSRAI0(Ctx,VotesM,ConvThread,SYM,InputO,Result,VotesO,Proof).

computeSRAI0(Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):-
  Each = (OutputLevel - e(VotesM,Result,Proof)), %% VotesO make it sort/2-able
  Each2Test = (_ - e(_,Result2Test,_)), 
  Call = computeSRAI2(Ctx,Votes,ConvThread,SYM,Input,Result,VotesM,Proof,OutputLevel),
  copy_term(Each:Call,EachFound:CallFound),  
  findall(EachFound:CallFound, CallFound, FOUND),
  FOUND=[_|_],
  memberchk(Each2Test:_,FOUND),
  nonvar(Result2Test),
  sort(FOUND,ORDER),!,
 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   member(Each:Call,ORDER),
   prolog_must(nonvar(Result)),
   debugFmt(computeSRAI(Input,SYM,Each)),
   VotesO is VotesM * 1.1.

computeSRAI0(_Ctx,Votes,ConvThread,SYM,Input,Result,VotesO,Proof):- !, VotesO is Votes * 0.7,
     Result = ['I',heard,you,think,SYM,and,'say:'|Input],
      Proof = result(Result,failed_computeSRAI2(Votes,Input,ConvThread)),
      debugFmt(Proof),!.

% now atrace is ok

% this next line is what it does on fallback
computeSRAI0(Ctx,Votes,ConvThread,SYM,[B|Flat],[B|Result],VotesO,Proof):- fail,
   computeSRAI2(Ctx,Votes,ConvThread,SYM,Flat,Result,VotesO,Proof,_PostOutputLevel3),prolog_must(nonvar(Result)).

checkSym(_SYM).

subclassMakeUserDict(Ctx,UserDict,SYM):-debugFmt(subclassMakeUserDict(Ctx,UserDict,SYM)),addInherit(UserDict,SYM).

convThreadDict(_Ctx,ConvThreadHint,ConvThread):-answerOutput(ConvThreadHint,First),unlistify(First,ConvThread),!.

computeSRAI222(CtxIn,Votes,ConvThreadHint,SYM,Pattern,Compute,VotesO,ProofOut,OutputLevel):-    
 prolog_mustEach((
   %%convertToMatchable(Pattern,InputPattern),
   prolog_must(current_value(CtxIn,'evalsrai',SYM2)),
   ifThen(var(SYM),SYM=SYM2),
   ifThen(SYM\==SYM2,debugFmt(syms(SYM\==SYM2))),
      convThreadDict(Ctx,ConvThreadHint,ConvThread),
         getCategoryArg(Ctx,'template',Out, _Out_ ,CateSig),!,
         getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'topic',Topic,['Nothing']),
         getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'userdict',UserDict,'user'), 
         %%getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'convthread',ConvThread,SYM,ConvThreadHint), 
         subclassMakeUserDict(CtxIn,UserDict,SYM),
         getAliceMemOrSetDefault(CtxIn,ConvThread,SYM,'that',That,['Nothing']),
  
   PreTopic = ignore(CtxIn=Ctx),PreTopic,
   TTP = topicThatPattern(Topic,That,Pattern),
   debugFmt(TTP),!,
   traceIf(not(ground(TTP))),
   must_be_openCate(CateSig),!,
   /*
   prolog_must(topicThatPattern(Ctx,Topic,That,Pattern,PreTopic,Out,CateSig,OutputLevel,StarSets_All,ClauseNumber,CommitTemplate)),
   */
   
   UNIF= topicThatPattern(Ctx,Topic,That,Pattern,PreTopic,Out,CateSig,OutputLevel,StarSets_All,ClauseNumber,CommitTemplate),
   singletons([ClauseNumber]),
   findall(UNIF,UNIF,UNIFS),!,
   traceIf(UNIFS=[]),
   %%%%% iterate from here %%%%%
   member(UNIF,UNIFS), 
         once(/* prolog_mustEach */((
            retractallSrais(SYM),
            prolog_must(CommitTemplate),
            prolog_must(nonvar(Out)),
            cateStrength(CateSig,Mult),
           %% not(contextUsedClaused(Ctx,CateSig,ClauseNumber)),
            VotesO is Votes * Mult,
            makeWithAttributes(StarSets_All,Out,Compute),       
            %%MoreProof = [cn(ClauseNumber),CateSig],
            MoreProof = [topicThatPatternOutput(Topic,That,Pattern,Out)],
            ProofOut=..[proof,Compute|MoreProof]))))).


clauseRef(_CateSig,0):-!.
clauseRef(CateSig,Pattern:Template):-arg(6,CateSig,Pattern),arg(11,CateSig,Template),!.
clauseRef(CateSig,ClauseNumber):-trace,clause(CateSig,true,ClauseNumber).
clauseRef(_CateSig,-1):-!.

savedParts(Save,PreTopic,CommitTemplate,OutputLevel,StarSets_All,Out,ClauseNumber,CateSig):-
      Save = OutputLevel - StarSets_All - Out - ClauseNumber - CateSig - CommitTemplate - PreTopic.

starSetsAll(Ctx,Topic,That,Pattern,Save,PreTopic):-
   savedParts(Save,PreTopic,_CAfterPattern,OutputLevel,StarSets_All,Out,ClauseNumber,CateSig),
   getCategoryArg(Ctx,'template',Out, _Out_ ,CateSig),
   functor(CateSig,CateSigFunctor,_Args),
   OutputLevel = OutputLevel1 - OutputLevel2 - OutputLevel3,!,
   
   %%%%% Iterate here %%%%
   cate_match(Ctx,CateSigFunctor,'pattern',Pattern,CateSig,_MatchPattern,StarSets_Pattern,OutputLevel3),   
   call(CateSig),
   clauseRef(CateSig,ClauseNumber), %%%%%
   once(( cate_match(Ctx,CateSigFunctor,'topic',Topic,CateSig,_MatchTopic,StarSets_Topic,OutputLevel1),
   cate_match(Ctx,CateSigFunctor,'that',That,CateSig,_MatchThat,StarSets_That,OutputLevel2),
   combineStarSets(StarSets_Topic,StarSets_That,StarSets_Pattern,StarSets_All) )).

combineStarSets(StarSets_Topic,StarSets_That,StarSets_Pattern,StarSets_All):-
   append(StarSets_Topic,StarSets_That,StarSets_TopicThat),
   append(StarSets_Pattern,StarSets_TopicThat,StarSets_All),!.

cate_match(Ctx,CateSigFunctor,StarName,TextPattern,CateSig,MatchPattern,StarSets,OutputLevel):-
    getCategoryArg1(Ctx,StarName,MatchPattern,_StarNumber,CateSig),!,
    argNFoundGenerate(CateSigFunctor,StarName,MatchPattern,_IndexPattern,TextPattern),
    make_star_binders(Ctx,StarName,1,TextPattern,MatchPattern,OutputLevelInv,StarSets),OutputLevel is 1/OutputLevelInv.

ctrace2:-atrace.

%%checkStarSets(StarSets):-member(Bad=[],StarSets),!,ctrace2,warnIf(member(Bad=[],StarSets)).
checkStarSets(_StarSets). %%ctrace2.


argNFoundGenerate(CateSigFunctor,StarName,MatchPattern,IndexPattern,Nothing):-meansNothing(Nothing,_),!,argNFound(CateSigFunctor,StarName,MatchPattern,IndexPattern).
argNFoundGenerate(CateSigFunctor,StarName,MatchPattern,IndexPattern,TextPattern):-textPatternToMatchPatternTest(TextPattern,IndexPattern),argNFound(CateSigFunctor,StarName,MatchPattern,IndexPattern),nop(traceIf(not(textPatternToMatchPattern(TextPattern,IndexPattern)))).
%%%argNFoundGenerate(CateSigFunctor,StarName,MatchPattern,IndexPattern,TextPattern):-trace,argNFound(CateSigFunctor,StarName,MatchPattern,IndexPattern).

argNFoundGenerate(_CateSigFunctor,pattern,MatchPattern,IndexPattern,TextPattern):-textPatternToMatchPattern(TextPattern,IndexPattern),cid_pattern(_,IndexPattern),fromIndexableSArg0(IndexPattern,MatchPattern).
argNFoundGenerate(_CateSigFunctor,that,MatchPattern,IndexPattern,TextPattern):-textPatternToMatchPattern(TextPattern,IndexPattern),cid_that(_,IndexPattern),fromIndexableSArg0(IndexPattern,MatchPattern).
argNFoundGenerate(_CateSigFunctor,topic,MatchPattern,IndexPattern,TextPattern):-textPatternToMatchPattern(TextPattern,IndexPattern),cid_topic(_,IndexPattern),fromIndexableSArg0(IndexPattern,MatchPattern).


toTAtom(Text,Text):-atom(Text),!.
toTAtom(Text,Atom):-number(Text),atom_number(Atom,Text),!.
toTAtom(Text,Atom):-trace,atom_to_number(Atom,Text).

textPred(element(A,B,C),element(A,B,C)):-!.
textPred(Text,Pred):-toTAtom(Text,Atom),toLowercase(Atom,Pred).


textPatternToMatchPattern([Text],MatchPattern):-textPred(Text,Pred),!,member(MatchPattern,[Pred,*,'_']).
textPatternToMatchPattern([Text,_P|_Attern],MatchPattern):-textPred(Text,Pred),functor(MatchPattern,Pred,1).
textPatternToMatchPattern([_Text|Pattern],MatchPattern):-member(T,Pattern),textPred(T,Pred),functor(MatchPattern,Pred,2).
textPatternToMatchPattern(_TextPattern,'_').
textPatternToMatchPattern(_TextPattern,'*').

%%textPatternToMatchPatternTest(I,O):-textPatternToMatchPattern(I,O).
textPatternToMatchPatternTest(_TextPattern,_).

%%argNFound(aimlCate,pattern,['ARE', *, 'REAL'],are(real(idx_endswith, *, real))).
%%argNFound(aimlCate, pattern, ['_', 'OFF'], off(idx_endswith, '_', off)).
%%argNFound(aimlCate,pattern,['HOW', 'MANY', 'YEARS', *, 'IN', 'SAN', 'FRANCISCO'],how(many(years(in(idx_startswith(*), san(francisco)))))).

%% simpler but slower.. maybe comment (fail) this one out for the faster next one
%% DOES NOT USE INDEXES 
topicThatPattern(Ctx,Topic,That,Pattern,PreTopic,Out,CateSig,OutputLevel,StarSets_All,ClauseNumber,CommitTemplate):- useNewCateSigSearch_broken_now,
  prolog_mustEach((
   debugFmt(debugWarn(useNewCateSigSearch_broken_now)), 
   CommitTemplate = (nop(CateSig),prolog_must(PreTopic)),
   savedParts(Save,PreTopic,CommitTemplate,OutputLevel,StarSets_All,Out,ClauseNumber,CateSig),
   findall(Save,starSetsAll(Ctx,Topic,That,Pattern,Save,PreTopic),AllCateSig),
   prolog_must(AllCateSig=[_|_]),
   sort(AllCateSig,SetOfAllCateSig),!,
   %%%%% Iterate here %%%%
   member(Save,SetOfAllCateSig))).

%% WILL GET HERE ONLY IF NEW ROUTINES ARE NOT AS FAST (WHICH IS THE CASE)
topicThatPattern(Ctx,Topic,That,Pattern,PreTopic,Out,CateSig,OutputLevel,StarSets_All,ClauseNumber,CommitTemplate):-
 prolog_mustEach((
   traceIf(useNewCateSigSearch_broken_now),
   OutputLevel = OutputLevel1 - OutputLevel2 - OutputLevel3,!,
   CPreTopic = true,
   make_preconds_for_match(Ctx,'topic',Topic,CateSig,PreTopic,AfterTopic,CPreTopic,CAfterTopic,Out,MinedCates,EachMatchSig_Topic,StarSets_Topic,OutputLevel1),
   make_preconds_for_match(Ctx,'that',That,CateSig,AfterTopic,AfterThat,CAfterTopic,CAfterThat,Out,MinedCates,EachMatchSig_That,StarSets_That,OutputLevel2),
   make_preconds_for_match(Ctx,'pattern',Pattern,CateSig,AfterThat,FindPatternGoal,CAfterThat,CommitTemplate,Out,MinedCates,EachMatchSig_Pattern,StarSets_Pattern,OutputLevel3),
   prolog_must((var(Out),var(OutputLevel1),var(OutputLevel2),var(OutputLevel3))),
   must_be_openCate(CateSig))),
  prolog_mustEach((
   %%%%% Iterate here %%%%   
   atLeastOne(call((FindPatternGoal,
       /*
       combineStarSets(StarSets_Topic,StarSets_That,StarSets_Pattern,StarSets_All),
       copy_term(CateSig,CateSig2),ignore(CateSig2),
       traceIf((That=['How',_|_],nop(CateSig=CateSig2))),
       */
       CateSig))),   
   clauseRef(CateSig,ClauseNumber),
   singletons([EachMatchSig_Topic,EachMatchSig_That,EachMatchSig_Pattern]),
   combineStarSets(StarSets_Topic,StarSets_That,StarSets_Pattern,StarSets_All))).

savedSetPatterns(LSP,OutputLevel,StarSets,MatchPattern):- LSP = lsp(OutputLevel,StarSets,MatchPattern).

make_preconds_for_match(Ctx,StarName,TextPattern,CateSig,PrecondsSearch,PostcondsSearch,PrecondsCommit,PostcondsCommit,Out,MinedCates,EachMatchSig,StarSets,
 OutputLevel):-   
   make_prepost_conds(Ctx,StarName,TextPattern,CateSig,FindPatternGoal,CommitTemplate,Out,MinedCates,EachMatchSig,StarSets,OutputLevel),
   combineConjCall(PrecondsSearch,FindPatternGoal,PostcondsSearch),
   combineConjCall(PrecondsCommit,CommitTemplate,PostcondsCommit).

%%TODO: MAKE THIS ONE WORK ! (CURRENTLY WORKS)
make_prepost_conds(Ctx,StarName,TextPattern,CateSig,FindPatternGoal,CommitTemplate,Out,MinedCates,EachMatchSig,StarSets,OutputLevel):-  
 prolog_mustEach((
  CommitTemplate = true,
  generateMatchPatterns(Ctx,StarName,Out,TextPattern,CateSig,MinedCates,EachMatchSig),
  savedSetPatterns(LSP,OutputLevel,StarSets,MatchPattern),
  getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSig),
  FindPatternGoal = ( member(LSP,EachMatchSig)/*,CateSig */) )),!.

contextUsedClaused(Ctx,CateSig,ClauseNumber):- fail, contains_term(Ctx,CateSig)->not(contains_term(Ctx,ClauseNumber));not(contains_term(Ctx,ClauseNumber)).

makeWithAttributes([],Proof,Proof):-!.
makeWithAttributes(StarSets_All,Proof,withAttributes(StarSets_All,Proof)).

retractallSrais(SYM):-prolog_must(nonvar(SYM)),ifThen(nonvar(SYM),(retractall(dict(SYM,_,_)))),fail.
retractallSrais(_SYM):-!.

cateStrength(_CateSig,1.1):-!.

computeSRAI2(Ctx,Votes,ConvThread,_SYM1,Pattern,Out,VotesO,ProofOut,OutputLevel):- !, %% avoid next one    
    computeSRAI222(Ctx,Votes,ConvThread,_SYM2,Pattern,Out,VotesO,ProofOut,OutputLevel).

getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSig):-
   prolog_must(getCategoryArg0(Ctx,StarName,MatchPattern,Out,CateSig)),!.

getCategoryArg0(Ctx,StarName,MatchPattern,_Out,CateSig):-atomic(StarName),!,
  getCategoryArg1(Ctx,StarName,MatchPattern,_StarNumber,CateSig),!.
  
getCategoryArg0(Ctx,FAB,OutAOutB,Out,CateSig):- FAB=..[F,A,B],
      getCategoryArg(Ctx,A,OutA,Out,CateSig),!,
      getCategoryArg(Ctx,B,OutB,Out,CateSig),!,
      OutAOutB=..[F,OutA,OutB].

getCategoryArg1(_Ctx,StarName,MatchPattern,StarNumber,CateSig):-
   prolog_must(aimlCateSig(CateSig)),
   aimlCateOrder(Order),
   nth1(StarNumber,Order,StarName),
   prolog_must(arg(StarNumber,CateSig,MatchPattern)),!.


meansNothing(Var,_Nothing):-var(Var),!,aiml_error(meansNothing(var(Var))),!.
meansNothing([Atomic],Nothing):-nonvar(Atomic),!,meansNothing(Atomic,Nothing),!.
meansNothing(N,['Nothing']):-member(N,[[],'Nothing']),!.
meansNothing(Atom,['Nothing']):-atom(Atom),!,fail.
meansNothing(InputNothing,InputPattern):-prolog_must((ground(InputNothing),var(InputPattern))),meansNothing0(InputNothing,InputPattern),!.

meansNothing0([Atom],Out):-!,meansNothing0(Atom,Out).
meansNothing0('_',['Nothing']).
meansNothing0('*',['Nothing']).

combineConjCall(A,B,C):-A==true,!,C=B.
combineConjCall(A,B,C):-B==true,!,C=A.
combineConjCall(A,B,C):- C = (A,B).

addToMinedCates(_MinedCates,_CateSig):-!.
addToMinedCates(MinedCates,CateSig):-prolog_must(ground(CateSig)),append(_,[CateSig|_],MinedCates),!.
addToMinedCates(MinedCates,CateSig):-atrace,var(MinedCates),!,MinedCates=[CateSig|_].

notSingletons(_Singleton_List):-!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% generateMatchPatterns -  finds the candidate indexers for some textInput
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generateMatchPatterns(_Ctx,_StarName,_Out,_InputNothing,_CateSig,_NC_MinedCates,EachMatchSig):-nonvar(EachMatchSig),trace,!. % already done

generateMatchPatterns(Ctx,StarName,Out,InputNothing,CateSig,NC_MinedCates,EachMatchSig):- fail,
  hotrace(meansNothing(InputNothing,InputPattern)),
  InputNothing\==InputPattern,!,
  generateMatchPatterns(Ctx,StarName,Out,InputPattern,CateSig,NC_MinedCates,EachMatchSig).

generateMatchPatterns(Ctx,StarName,Out,InputNothing,CateSig,_NC_MinedCates,EachMatchSig):- fail,
  hotrace(meansNothing(InputNothing,_InputPattern)),!,
  traceIf(InputNothing\==['Nothing']),
  must_be_openCate(CateSig),
  getCategoryArg(Ctx,StarName,'*',Out,CateSig),
   prolog_must(EachMatchSig=[_|_]),
  must_be_openCate(CateSig),!.

%% The NEWEST match patterns NOW using Indexing!
generateMatchPatterns(Ctx,StarName,Out,InputPattern,CateSigIn,MinedCates,SetOfEachMatchSig):-  useIndexPatternsForCateSearch,
 prolog_mustEach((
  copy_term(CateSigIn,CateSig),
  CateSigIn=CateSig,
 functor(CateSig,CateSigFunctor,_Args),
  must_be_openCate(CateSig),
  getCategoryArg(Ctx,StarName,IndexPattern,Out,CateSig),
  savedSetPatterns(LSP,OutputLevel,StarSets,IndexPattern),   
  findall(LSP,
             (argNFoundGenerate(CateSigFunctor,StarName,MatchPattern,IndexPattern,InputPattern),
              %%argNFound(CateSigFunctor,StarName,MatchPattern,IndexPattern),
              canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets)),
      EachMatchSig),
  prolog_must(EachMatchSig=[_|_]),
  sort(EachMatchSig,SetOfEachMatchSig),  
  prolog_must(debugFmtList([
        starName = StarName,
        %%eachMatchSig(EachMatchSig),
        setOfEachMatchSig=SetOfEachMatchSig,
        eachMatchSig=EachMatchSig,
        matchPattern=MatchPattern,
        minedCates=(MinedCates),
        cateSig=CateSig
        ])))),!.


%% The OLD match patterns NOT using Indexing
generateMatchPatterns(Ctx,StarName,Out,InputPattern,CateSig,_MinedCates,EachMatchSig):- ifThen(useIndexPatternsForCateSearch,atrace),
 %% convertToMatchable(TextPattern,InputPattern),
  must_be_openCate(CateSig),
  copy_term(CateSig,CateSigC),!,
  getCategoryArg(Ctx,StarName,MatchPattern,Out,CateSigC),
  findall(MatchPattern,CateSigC,AllMatchSig),!,sort(AllMatchSig,SetOfEachMatchSig),!,
  savedSetPatterns(LSP,OutputLevel,StarSets,MatchPattern),
  findall(LSP,
             (member(MatchPattern,SetOfEachMatchSig), 
              canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets)),
      EachMatchSig),
 %%traceIf((StarName==pattern,InputPattern=[_,_|_])),
   prolog_must(EachMatchSig=[_|_]),
  prolog_must(debugFmtList([
        starName = StarName,
        eachMatchSig(EachMatchSig),
        setOfEachMatchSig=SetOfEachMatchSig,
        eachMatchSig=EachMatchSig,
        matchPattern=MatchPattern,
        cateSig=CateSig
        ])).

% ========================================================================================
%  canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets)
% ========================================================================================

canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets):-
    make_star_binders(Ctx,StarName,1,InputPattern,MatchPattern,OutputLevelInv,StarSets),checkStarSets(StarSets),!,OutputLevel is 1/OutputLevelInv ,
    nop(debugFmt(pass_canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets))),!.

canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern,_OutputLevel,_StarSets):-
    nop(debugFmt(fail_canMatchAtAll_debug(Ctx,StarName,InputPattern,MatchPattern))),!,fail.

% skip over skipable words
consumeSkippables([],[]).
consumeSkippables([Skipable|B],BB):- isIgnoreableWord(Skipable),!,consumeSkippables(B,BB).
consumeSkippables(A,A).

removeSkippables(A,A):-atomic(A),!.
removeSkippables([Skipable|B],BB):- isIgnoreableWord(Skipable),!,removeSkippables(B,BB).
removeSkippables([Skipable|B],[Skipable|BB]):- removeSkippables(B,BB).
removeSkippables(A,A).

% ======================================================================================== 
% make_star_binders(Ctx, StarName, Text , Pattern, 1/OutputLevel, StarSetsNameValues).
%
% pattern_match(Text , Pattern)..  would be simply   make_star_binders(_Ctx, starName, 1, Text , Pattern, _OutputLevel, _StarSetsNameValues).
% ========================================================================================
make_star_binders(_Ctx,StarName,_N,InputPattern,MatchPattern,OutputLevel,StarSets):- 
   prolog_must(var(StarSets)),prolog_must(var(OutputLevel)),prolog_must(ground(StarName:InputPattern:MatchPattern)),fail.  

make_star_binders(Ctx,StarName,N,InputText,Indexical,WildValue,Pred):-
   %%% side with most stars should have to be shorter or same length?
   %%% cant preguess dual sided stars: ((is_list(InputText),is_list(Indexical))-> (length(InputText,IL),IL2 is IL+2,length(Indexical,PL),PL=<IL2) ; true),
   removeSkippables(Indexical,IndexicalChanged),
   %% IF Pattern contains no skippables
     (Indexical==IndexicalChanged-> 
      %% THEN remove Skippables
       (removeSkippables(InputText,Text),!,make_star_binders0(Ctx,StarName,N,Text,Indexical,WildValue,Pred));
      %% ELSE use skippables
       make_star_binders0(Ctx,StarName,N,InputText,Indexical,WildValue,Pred)),!.

/*
SO.. The IF/THEN/ELSE pattern is for when the pattern contains non-text

46 ?- starMatch([('"'),a,*,*],[('"'),a,be,c,('"')],StarSets).
StarSets = [tstar1=be, tstar2=[c, '"']].

47 ?- starMatch([a,*,*],[('"'),a,be,c,('"')],StarSets).
StarSets = [tstar1=be, tstar2=[c]].

starMatch(Pattern,Text,StarSets)
*/

starMatch(Pattern,Text,StarSets):-make_star_binders(_Ctx,'t',1,Text,Pattern,_OutputLevelInv,StarSets).

:-setLogLevel(make_star_binders0,none).

%end check
make_star_binders0(_Ctx,_StarName,_N,L,R,1,[]):-R==[],consumeSkippables(L,LL),LL==[],!.
make_star_binders0(_Ctx,_StarName,_N,L,R,1,[]):-L==[],consumeSkippables(R,RR),RR==[],!.

% left hand star/wild  (cannot really happen (i hope))
%make_star_binders0(_Ctx,StarName,N,Star,_Match,_OutputLevel,_StarSets):- fail, not([StarName]=Star),isStarOrWild(StarName,N,Star,_WildValue,_WMatch,_Pred),!,atrace,fail. 


% simplify
make_star_binders0(Ctx,StarName,N,[Word1|B],[Word2|BB],CountO,StarSets):-
     sameWords(Word1,Word2),!,make_star_binders0(Ctx,StarName,N,B,BB,Count,StarSets),CountO is Count + 1.

/*
% simplify (from last)

 41 ?- starMatch([('"'),a,*,c,*,('"')],[('"'),a,be,c,('"')],StarSets).
 StarSets = [tstar1=[be], tstar2='"']

   might be good to uncomment the next two lines to prevent this!
*/
make_star_binders0(Ctx,StarName,N,Word1B,Word2BB,CountO,StarSets):-append(B,[Word1],Word1B),append(BB,[Word2],Word2BB),
     sameWords(Word1,Word2),!,make_star_binders0(Ctx,StarName,N,B,BB,Count,StarSets),CountO is Count + 1.

% wildcard and match on opposite sides!  makes tests fail though TODO .. make this possible w/o loops
%%make_star_binders0(Ctx,StarName,N,[WildCard|BB],[MW|MatchB],ValueO,PredO):- isStarOrWild(StarName,N,WildCard,_WildValue,_Match,_Pred),not(isStarOrWild(StarName,N,MW,_,_,_)),
%%     make_star_binders0(Ctx,StarName,N,[MW|MatchB],[WildCard|BB],ValueO,PredO).

% tail (all now in) star/wildcard
make_star_binders0(_Ctx,StarName,N,InputText,WildCard,WildValue,[Pred]):-isStarOrWild(StarName,N,WildCard,WildValue,InputText,Pred),!,checkStarSets([Pred]).

% once in star.. walk past star
make_star_binders0(Ctx,StarName,N,InputText,[WildCard,M0|More],ValueO,[Pred|StarSets]):-isStarOrWild(StarName,N,WildCard,WildValue,SkipedSTAR,Pred),
         (WildCard=='^'->SkipedSTAR=_;SkipedSTAR=[_|_]),append(SkipedSTAR,[M1|LeftMore],InputText),sameWords(M0,M1),N2 is N+1,
         make_star_binders0(Ctx,StarName,N2,LeftMore,More,Value,StarSets),!,ValueO is WildValue + Value,checkStarSets([Pred|StarSets]).

% is mid-right hand wildcard (this should be the last test)
make_star_binders0(Ctx,StarName,N,[Match|B],[WildCard|BB],ValueO,[Pred|StarSets]):- isStarOrWild(StarName,N,WildCard,WildValue,Match, Pred),!,
     N2 is N+1,
     make_star_binders0(Ctx,StarName,N2,B,BB,Value,StarSets),!,ValueO is WildValue + Value,checkStarSets([Pred|StarSets]).

% tail is an atom (indexical unifier)
make_star_binders0(Ctx,StarName,N,InputText,Indexical,WildValue,Pred):-
      atom(Indexical),!,
      make_star_binders0(Ctx,StarName,N,InputText,[Indexical],WildValue,Pred),checkStarSets([Pred]).

% tail is a compound (indexical unifier)
make_star_binders0(Ctx,StarName,N,InputText,Indexical,WildValue,Pred):-
      not(is_list(Indexical)),/*compound(Indexical),*/fromIndexableSArg0(Indexical,LIST),LIST=[_|_],
      make_star_binders0(Ctx,StarName,N,InputText,LIST,WildValue,Pred),checkStarSets([Pred]).



% skip over skippable words
make_star_binders0(Ctx,StarName,N,Skipable,BB,CountO,StarSets):- 
  skipablePhrase(Skipable,B),!,make_star_binders0(Ctx,StarName,N,B,BB,Count,StarSets),CountO is Count + 1.
  %%warnIf((isIgnoreableWord(Skipable),!,make_star_binders0(Ctx,StarName,N,B,BB,Count,StarSets),CountO is Count + 1)),number(CountO).


skipablePhrase([Skipable|B],B):-isIgnoreableWord(Skipable),!.
skipablePhrase([Skip,'\b',Ble|B],[Skipable|B]):-joinAtoms([Skip,'\b',Ble],' ',Skipable),!.

isIgnoreableWord(Skipable):-member(Skipable,['-','(',')',',','?','.','','\'',('"')]).
isIgnoreableWord(Skipable):-isWhiteWord(Skipable).

isWhiteWord(Skipable):-member(Skipable,[' ','\b','\n','']).

%
% re-write section
%
%
/*
make_star_binders(Ctx,StarName,InputNothing,MatchPattern,OutputLevel,StarSets):- 
   hotrace((InputNothing \== '*',(InputPattern==StarName ; meansNothing(InputNothing,InputPattern)))),!, atrace,
   make_star_binders(Ctx,StarName,['Nothing'],MatchPattern,OutputLevel,StarSets).


% must come before search failures
make_star_binders(Ctx,StarName,TextPattern,MatchPattern,OutputLevel,StarSets):- fail,
  hotrace(((convertToMatchable(TextPattern,InputPattern),TextPattern \== InputPattern))),!,
  make_star_binders(Ctx,StarName,InputPattern,MatchPattern,OutputLevel,StarSets),!,atrace.

% fast veto
make_star_binders(_Ctx,StarName,[I0|Pattern],[Match|MPattern],_OutputLevel,_Commit):-
   member(M,[Match|MPattern]),requireableWord(StarName,M),not(member(M,[I0|Pattern])),!,fail.

% fast veto
make_star_binders(_Ctx,_StarName,[_],[_,_|_],_NoNum,_NoCommit):-!,fail.


make_star_binders(_Ctx,StarName,[E|More],Match,Value,[tryLater([E|More],Match)]):-compound(E),atrace,isWildCard(StarName,E,Value),!.

% weird atom
%%make_star_binders(_Ctx,StarName,I,Atom,12,[Atom=I]):-atom(Atom),atrace,!,loggerFmt(make_star_binders,canMatchAtAll_atom(StarName,I,Atom)),!.

*/
starNameTransform(Star,StarStar):-starName(Star,StarStar),!.
starNameTransform(StarName,StarName):-atom_concat(_,'star',StarName),!.
starNameTransform(StarName,StarNameStar):-atom_concat(StarName,'star',StarNameStar),!.

isStarOrWild(StarName,N,[StarNameText],WildValue,InputText,Pred):-nonvar(StarNameText),!,isStarOrWild(StarName,N,StarNameText,WildValue,InputText,Pred),!.

isStarOrWild(StarName,_N,WildCardText,WildValue,InputText,Pred):- isWildCard(StarName,WildCardText,WildValue,InputText,Pred),!.
isStarOrWild(StarName,N,StarNameText,WildValue,InputText,StarNameStarN=InputText):-
   isStar(StarName,StarNameText,WildValue),!,starNameTransform(StarName,StarNameStar),atom_concat(StarNameStar,N,StarNameStarN),!,traceIf(isStarValue(InputText)).

isWildCard(StarName,Wild,1,InputText,call(sameWCBinding(StarName,Wild,InputText))):- not(is_list(Wild)),compound(Wild),!. %%Wild=..LWild,!. %%not(not(member(StarName,LWild))),!.

requireableWord(StarName,M):-not(isOptionalOrStar(StarName,M)).

isOptionalOrStar(_StarName,M):-not(atom(M)),!,atrace.
isOptionalOrStar(StarName,M):-isStar2(StarName,M),!.

/*
isStar(StarName,'topic'):-!. %%,atrace.
isStar(StarName,'that'):-!.
isStar(StarName,'input'):-!.
*/
isStar2(StarName,StarNameText):-isStar(StarName,StarNameText,_Order),!.
isStar(StarName,StarNameText,WildValue):-not(ground(StarNameText)),atrace,debugFmt(isStar(StarName,StarNameText,WildValue)),!,fail.
isStar(StarName,[StarNameText],WildValue):-isStar(StarName,StarNameText,WildValue),!.
isStar(StarName,element(StarName,_,_),0.8).
isStar(StarName,star(StarName,_,_),0.8).
%%isStar(_StarName,element(_,_,_),0.7).
isStar(_StarName,'*',0.3).
isStar(_StarName,'^',0.8).
isStar(_StarName,'_',0.8).
%%WAS VERY BAD IDEA:  isStar(StarName,StarNameText,6):-atom(StarName),!,StarNameText==StarName,writeq(qqqq-qq),atrace.


must_be_openCate(_CateSig):-!.
must_be_openCate(CateSig):- prolog_must(hotrace((((nonvar(CateSig),not(ground(CateSig)),must_be_openCate0(CateSig)))))),!.
must_be_openCate0(CateSig):- arg(_,CateSig,Arg),must_be_openCateArgs(Arg,CateSig),fail.
must_be_openCate0(_CateSig):-!.

must_be_openCateArgs(Arg,_CateSig):-var(Arg),!.
must_be_openCateArgs('*',_CateSig):-!.
must_be_openCateArgs(List,CateSig):-atrace, throw(List:CateSig),!.

starSets(Ctx,List):-prolog_must((mapsome_openlist(starMust0,List),mapsome_openlist(starMust1(Ctx),List),mapsome_openlist(starMust2,List))),!.

star_flag(Flag,Out,In):- starNameTransform(Flag,StarFlag), flag(StarFlag,Out,In),!. %%,prolog_must(atom_concat(_,'star',Flag)),!.

endOfList(EndOfList):-(var(EndOfList);atomic(EndOfList)),!.

starMust0(StarName=_):-star_flag(StarName,_,1).
starMust1(Ctx,StarName=Value):-starSet(Ctx,StarName,Value).
starMust2(call(Call)):-!,prolog_must(Call).
starMust2(_Skip).

starSet(Ctx,StarNameI,Pattern):- 
   starName(StarNameI,StarName),
   ignore((nop(var(N)),star_flag(StarName,N,N))),
   traceIf(isStarValue(Pattern)),
   getDictFromAttributes(Ctx,'evalsrai',[],Dict),
   prolog_must(Dict\==user),
   atom_concat(StarName,N,StarNameN),
   prolog_must(not((getAliceMemComplete(Ctx,Dict,StarNameN,Old),debugFmt(getAliceMemComplete(Ctx,Dict,StarNameN,Old))))),
   setAliceMem(Ctx,Dict,StarNameN,Pattern),!,star_flag(StarName,NN,NN+1).

%%REAL-UNUSED  set_matchit1(StarName,Pattern,Matcher,OnBind):- length(Pattern,MaxLen0), MaxLen is MaxLen0 + 2,
%%REAL-UNUSED    set_matchit2(StarName,Pattern,Matcher,MaxLen,OnBind).

%%isStar0(Word1):- member(Word1,[*,'_']).
isStar0(X):-var(X),!,aiml_error(isStar0(X)).
isStar0('*').
isStar0('_').

sameWords(Word1,Word2):-atom(Word1),atom(Word2),atoms_match0(Word1,Word2).
 atoms_match0(Word1,Word2):- (isStar0(Word1);isStar0(Word2)),!,fail.
 atoms_match0(Word1,Word1):-!.
 atoms_match0(Word1,Word2):-literal_atom(Word1,WordO),literal_atom(Word2,WordO),!.


