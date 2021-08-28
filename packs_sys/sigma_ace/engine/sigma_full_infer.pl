% ==========================================
% INFERENCE
% ==========================================

:-include('sigma_header.pl').

:-dynamic(complete_goal/1).

:-multifile(expireOptimizationsInKB/3).

:-was_indexed(prove_goal(0,0,0,1,0,0,0)).

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

:-ensure_loaded(library(occurs)).

% ======================================================
% QUERY PROC POSITIVE/NEGATIVE
% ======================================================
tkq1:-agentQuery("(isa Joe ?Class)").
tkq2:-agentQuery("(isa on Relation)").

agentQueryF(KIFCharsIn):-
 forall(agentQuery(KIFCharsIn,UResultsSoFar,Result,Proof),
   wdmsg(UResultsSoFar->Result-->Proof)).


agentQueryF(KIFCharsIn,UResultsSoFar,Result,Proof):-
  agentQueryF(KIFCharsIn,'ToplevelContext','Merge','TheAgent',UResultsSoFar,Result,Proof).

agentQueryF(KIFCharsIn,Ctx,KB,User,UResultsSoFar,Result,Proof):-
	agentQueryF(KIFCharsIn,Ctx,KB,User,UResultsSoFar,Result,Proof,Status),
	(Status = done(_) -> ! ; true).
	
:- with_no_output(use_module(library(wamcl_runtime))).

ask_parse_queryF(TermIn,FmlInOpen,Vars):- compound(TermIn), \+ is_list(TermIn), FmlInOpen = TermIn, get_clause_vars(FmlInOpen,Vars).
ask_parse_queryF(KIFCharsIn,FmlInOpen,Vars):- isCharCodelist(KIFCharsIn), string_clean(KIFCharsIn,KIFChars),
	logOnFailure(ask_parse_chars(KIFChars,FmlInOpen,Vars)),!.
ask_parse_queryF(KIFCharsIn,FmlInOpen,Vars):-
	logOnFailure(input_to_forms(KIFCharsIn,FmlInOpen,Vars)).
		
agentQueryF(KIFIn,Ctx,KB,User,UResultsSoFar,Result,Proof,Status):-	
	logOnFailure(ask_parse_query(KIFIn,FmlInOpen,Vars)),!,
	agentQueryFull(KIFIn,FmlInOpen,Vars,Ctx,KB,User,UResultsSoFar,Result,Proof,Status).
			

agentQueryFull(KIFCharsIn,FmlInOpen,Vars,Ctx,KB,User,UResultsSoFar,Result,ProofOut,Status):-
	TN = User, % Tracks query based on 'User'
	destroyTN(KB,TN,_Ctx),	% Removes previous Query
	getOpenVariablesWFF(FmlInOpen,Vars,ChaseVars),
	getPrologVars(ChaseVars,QVars,_,_),
	QueryPrototype=..[query|QVars],
	getQueryClauses(KB,Ctx,'<=>'(FmlInOpen,QueryPrototype),ConjAssertsClauses,Vars,Flags),	
	createQueryAssertions(true,ConjAssertsClauses,CAN),!,
	assert(sigmaDatabase('<=>'(FmlInOpen,QueryPrototype),CAN,Flags,Vars,KB,Ctx,TN,User,on)),
	once(recanonicalizeTN(KB,TN)),    % Compile Inference
	assert(tq_attempted_query), 
        unnumbervars((FmlInOpen,QueryPrototype,ChaseVars),(UFmlInOpen,UQuery,UChaseVars)),
	writeDebug(blue,'Stage 1 - Compilation ':CAN:Flags),!,
	agentQueryEach(UFmlInOpen,UQuery,UChaseVars,Ctx,KB,User,UResultsSoFar,Result,Proof,Status),
	commitCleanProof(Proof,ProofOut).

% Check For Theorem
agentQueryEach(FmlInOpen,UQuery,UVars,Ctx,KB,User,1,['Result' = 'True'],formula(instance(FormulaIN,'Theorem')),done(true:thereom)):-
	resetTableFlags,
	writeDebug(purple,'Stage 2 - Theorem Check ':FmlInOpen),
	isTheorem(FmlInOpen,_),!,
	retain_yes,sendNote('(theorem)'),!.
  
agentQueryEach(FmlInOpen,UQuery,UVars,Ctx,KB,User,UA, UVars,Proof,Result):-
	agentInference(FmlInOpen,UQuery,UVars,Ctx,KB,User,UA, UVars, Proof,Result).

% Query Failed
agentQueryEach(FmlInOpen,UQuery,UVars,Ctx,KB,User,0,['Result'='none'|_],'Unproven',done(possible:searchfailed)):-
	flag('UAnswers',UA,UA),UA<1,!.

% Query Succeeded
agentQueryEach(FmlInOpen,UQuery,UVars,Ctx,KB,User,UA,['Result'='true'|_],found(UA),done(true:UA)):-!,
	flag('UAnswers',UA,UA).      


getQueryDefaults(UQuery,OAnswers,BackchainsMax,Deductions):-
        (getSigmaOption(opt_backchains_max=BackchainsMax)),
        (getSigmaOption(opt_deductions_max=Deductions)),!,
        ignore((ground(UQuery) -> Answers=1 ; Answers=PAnswers)),
	(getSigmaOption(opt_answers_max=Answers)),!,
	ignore(BackchainsMax=30),ignore(Answers=60),OAnswers is Answers,!.

set_quit_time(Num):-
	(getSigmaOption(opt_timeout=QuitTime)),!,ignore(QuitTime=5),!,
	retractAllProlog(cpuend(_)),    
	getCputime(Now),
	Then is Now + QuitTime*Num,
	asserta(cpuend(Then)),!.
	
:-dynamic(noexistencials/0).



%edify_vars(X,X):-!.
edify_vars(Var,Var):-var(Var),!.
edify_vars([],[]):-!.
edify_vars([N=V|T],[N=RV|NT]):-
            eval_lr(V,RV),!,retain_answer(RV),
            edify_vars(T,NT),!.
edify_vars([H|T],[H|NT]):-
            edify_vars(T,NT),!.




/*

agentInference(FmlInOpen,Literal,VarsRequested,Ctx,KB,User,UA, UVars,[All|Proof],done(true:UA)):- 
	writeDebug(green,'Stage 3 - Positive ':FmlInOpen ),
	inferGoal(findAllUsingGafsAndOrBackResolution(Literal,VarsRequested,10),VarsRequested,Ctx,KB, [All|Proof]),
	length([All|Proof],UA),!.

agentInference(FmlInOpen,NLiteral,VarsRequested,Ctx,KB,User,UA, UVars,[All|Proof],done(false:UA)):- 
	NLiteral=..[F|Args],
	(atom_concat('~',FN,F);atom_concat('~',F,FN)),
	Literal=..[FN|Args],!,
	writeDebug(red,'Stage 4 -Negative ':FmlInOpen ),
	inferGoal(findSingleUsingGafsAndOrBackResolution(Literal,VarsRequested,10),VarsRequested,Ctx,KB, [All|Proof]),
	length([All|Proof],UA),!.
*/

/*

sigmaCache('~subclass'(v('Abstract', A, ['Class'|B]), v('Abstract', 'Entity', ['Class'|C])), guard([A, B, C], [D, E, F], '~instance'(v('Abstract', D, ['Class'|E]), v('Abstract', 'Class', ['Class', 'Abstract', 'Class'|G])), 4, ['CLASS'=D], [D], [], [], [], [D], '~subclass'(v('Abstract', D, ['Class'|E]), v('Abstract', 'Entity', ['Class'|F]))), not(subclass), 'Merge', 'BASE ONTOLOGY', 637).
sigmaCache(instance(v('Abstract', A, ['Class'|B]), v('Abstract', 'Class', ['Class', 'Abstract', 'Class'|C])), guard([A, B, C], [D, E, F], subclass(v('Abstract', D, ['Class'|E]), v('Abstract', 'Entity', ['Class'|G])), 3, ['CLASS'=D], [D], [], [], [], [D], instance(v('Abstract', D, ['Class'|E]), v('Abstract', 'Class', ['Class', 'Abstract', 'Class'|F]))), instance, 'Merge', 'BASE ONTOLOGY', 637).
sigmaCache('~instance'(v('Abstract', A, ['Class'|B]), v('Abstract', 'Class', ['Class', 'Abstract', 'Class'|C])), guard([A, B, C], [D, E, F], '~subclass'(v('Abstract', D, ['Class'|E]), v('Abstract', 'Entity', ['Class'|G])), 2, ['CLASS'=D], [D], [], [], [], [D], '~instance'(v('Abstract', D, ['Class'|E]), v('Abstract', 'Class', ['Class', 'Abstract', 'Class'|F]))), not(instance), 'Merge', 'BASE ONTOLOGY', 637).
sigmaCache('~subclass'(v('Abstract', A, ['Class'|B]), v('Abstract', 'Entity', ['Class'|C])), guard([A, B, C], [D, E, F], '~instance'(v('Abstract', D, ['Class'|E]), v('Abstract', 'Class', ['Class', 'Abstract', 'Class'|G])), 4, ['CLASS'=D], [D], [], [], [], [D], '~subclass'(v('Abstract', D, ['Class'|E]), v('Abstract', 'Entity', ['Class'|F]))), not(subclass), 'Merge', 'BASE ONTOLOGY', 637).
sigmaCache(instance(v('Abstract', A, ['Class'|B]), v('Abstract', 'Class', ['Class', 'Abstract', 'Class'|C])), guard([A, B, C], [D, E, F], subclass(v('Abstract', D, ['Class'|E]), v('Abstract', 'Entity', ['Class'|G])), 3, ['CLASS'=D], [D], [], [], [], [D], instance(v('Abstract', D, ['Class'|E]), v('Abstract', 'Class', ['Class', 'Abstract', 'Class'|F]))), instance, 'Merge', 'BASE ONTOLOGY', 637).
sigmaCache('~instance'(v('Abstract', A, ['Class'|B]), v('Abstract', 'Class', ['Class', 'Abstract', 'Class'|C])), guard([A, B, C], [D, E, F], '~subclass'(v('Abstract', D, ['Class'|E]), v('Abstract', 'Entity', ['Class'|G])), 2, ['CLASS'=D], [D], [], [], [], [D], '~instance'(v('Abstract', D, ['Class'|E]), v('Abstract', 'Class', ['Class', 'Abstract', 'Class'|F]))), not(instance), 'Merge', 'BASE ONTOLOGY', 637).

	[sigmaCache(RealHead,guard(
		RFVH,FVH,
			Body,
			CLID,KRVars,RuleVars,
			UnivHead,
			BodyUniv,
			BodySelfConnected,Shared,Head),Key,KB,Ctx,TN)]):-!, 


     %   recorda(Functor,RealHead,FRef),
	%stepQualifier(Ref,Functor,RealHead:Head,RFVH:FVH,Body,TN,CID,KRVars,RuleVars,UnivHead,BodyUniv,BodySelfConnected,RealShared,Qualifier),
	%findall(Body,call_with_depth_limit(block(Ref,callBody(Body,true),Result),47,_),Sols),!,	
	%findall(RFVH,(inferGoal(Ref,Body)),Sols)
	%findall(Body,Body,Sols),!,
	%sort(Sols,SolsS)

*/

%ensureKey(Literal,Proto,HashKey,Depth,HashKey):-nonvar(HashKey),!.
%ensureKey(Literal,Proto,HashKey,Depth,HashKey):-nonvar(Proto),!,hash_term(Proto,HashKey).
ensureKey(Literal,Proto,HashKey,Depth,HashKey):-
	copy_term(Literal,Proto),sigma_numbervars(Proto,'$',0,Depth),hash_term(Proto,HashKey),!.

% inferGoal(Todo,VarsIn,Ctx,KB, AccumulatedOut).
inferGoal(Todo,VarsIn,Ctx,KB, AccumulatedOut):-
	writeDebug((Todo-VarsIn-Ctx)),fail.
	
      
% Find Single (UsingGafs/UsingBackResolution) Gaf Part
inferGoal(findSingleUsingGafsAndOrBackResolution(Literal,UVars,Depth),VarsRequested,Ctx,KB, Result):-
	inferGoal(findSingleUsingGafs(Literal,UVars,Depth),VarsRequested,Ctx,KB, Result),!.

% Find Single (UsingGafs/UsingBackResolution) BackResolution Part
inferGoal(findSingleUsingGafsAndOrBackResolution(Literal,UVars,Depth),VarsRequested,Ctx,KB, Result):-
	inferGoal(findSingleUsingBackResolution(Literal,UVars,Depth),VarsRequested,Ctx,KB, Result),!.
	
% Find Single (UsingGafs) sigmaDatabase Part
inferGoal(findSingleUsingGafs(Literal,UVars,Depth),VarsRequested,Ctx,KB, [Accum]):-
	sigmaDatabase(Literal, _, KB, PCtx, Proof),
	putIntoZ(Literal,Key,HashKey,proved(Literal,Proof),Accum),!.

sigmaDatabase(Literal, How, KB, Ctx, Proof):-
	sigmaCache(Literal, How, KB, Ctx, Proof).
sigmaDatabase(Literal, ReQlist, How, KB, Ctx, Proof):-
	sigmaCache(Literal, How,ReQlist, KB, Ctx, Proof).


% Find Single (UsingBackResolution) sigmaDatabase Part
inferGoal(findSingleUsingBackResolution(Literal,UVars,Depth),VarsRequested,Ctx,KB,AccumulatedOut):- !,
	shouldCanBackChainOn(Literal,UVars,Depth),
	not(not(sigmaDatabase(Literal,Reqs,_,KB,_Ctx,Proof))),
	ensureKey(Literal,Key,HashKey,_,Pointer),!,
	findall(Sum-Info,(
			sigmaDatabase(Literal, ReqList,Type,KB,__Ctx,Proof),
			examineRuleForInfo(Literal,Key,VCount,Pointer,VarsRequested,UVars,Depth,Proof,ReqList,Info,Sum)
		),[All|Infomation]),
	keysort([All|Infomation],RAllInfomation),
	inferGoal(nowProveOneWithInfoFromDisj([All|Infomation]),VarsRequested,Ctx,KB,AccumulatedOut).



% Find Single (UsingBackResolution) With Lits
inferGoal(nowProveOneWithInfoFromDisj(Infomation),VarsRequested,Ctx,KB,AccumulatedOut):-
	member(First,Infomation),
	inferGoal(nowProveWithInfoConj(First),VarsRequested,Ctx,KB,AccumulatedOut),!.
	
% Find All (UsingGafs/UsingBackResolution) 
inferGoal(findAllUsingGafsAndOrBackResolution(Literal,UVars,Depth),VarsRequested,Ctx,KB, Result):-
	inferGoal(findAllUsingGafs(Literal,UVars,Depth),VarsRequested,Ctx,KB, GafResults),!,
	inferGoal(findAllUsingBackResolution(Literal,UVars,Depth),VarsRequested,Ctx,KB, BKResults),
	append(GafResults,BKResults,Result).

% Find All UsingGafs
inferGoal(findAllUsingGafs(Literal,UVars,Depth),VarsRequested,Ctx,KB, All):-
	ensureKey(Literal,Key,HashKey,VCount,Pointer),
	findall(Accum,(sigmaDatabase(Literal, _, KB, _Ctx, Proof),putIntoZ(Literal,Key,HashKey,proved(Literal,Proof)),Accum),All),!.

% Find All UsingBackResolution
inferGoal(findAllUsingBackResolution(Literal,UVars,Depth),VarsRequested,Ctx,KB, AccumulatedOut):-
	%trace,
	ensureKey(Literal,Key,HashKey,VCount,Pointer),!,
	%VCount < Depth,
	not(recorded(findAllUsingBackResolution,Literal)),
	recorda(findAllUsingBackResolution,Literal),
	%shouldCanBackChainOn(Literal,UVars,Depth),!,
	findall(Sum - Info,(
			sigmaDatabase(Literal, ReqList,Type,KB,__Ctx,Proof),
			examineRuleForInfo(Literal,Key,VCount,Pointer,VarsRequested,UVars,Depth,Proof,ReqList,Info,Sum)
		),[All|Infomation]),
	keysort([All|Infomation],RAllInfomation),
	reverse(RAllInfomation,AllInfomation),!,
	inferGoal(proveAllWithInfoDisj(AllInfomation),VarsRequested,Ctx,KB,AccumulatedOut).
			

recordaLogged(A,B):-recordaLogged(A,B,C).
recordaLogged(A,B,C):-
	recorda(A,B,C),writeDebug(recorda(A,B)).

recordzLogged(A,B):-recordzLogged(A,B,C).
recordzLogged(A,B,C):-
	recordz(A,B,C),writeDebug(recordz(A,B)).

recordedLogged(A,B):-recordedLogged(A,B,C).
recordedLogged(A,B,C):-recorded(A,B,C),!,writeDebug(recorded(A,B,C)).
	
%recordedLogged(A,B,C):- writeDebug(recordedChk(A,B)),recorded(A,B,C),writeDebug(recorded(C)).

% recordAwakensReq(ReqPointer,Req,ReqKey,ReqKeys).

:-dynamic(u/2).

count_unique_keys(X):-
	retractall(u(A,B)),
	sigmaDatabase(Literal, ReqList,Type,KB,__Ctx,Proof),
	ensureKey(Literal,ReqKey,Pointer,VCount,_Pointer),
	not(u(ReqKey,ReqKey)),
	assert(u(ReqKey,ReqKey)),
	write(ReqKey=Literal),nl,
	fail.
	
count_unique_keys(X):-predicate_property(u(_,_),X).
count_unique_keys(X):-predicate_property(sigmaDatabase(Literal, ReqList,Type,KB,__Ctx,Proof),X).
	
	



recordAwakensReq(ReqPointer,Req,ReqKey,[ReqKey]).
recordAwakensReq(ReqPointer,Req,ReqKey,Original):-
	delete(Original,ReqKey,Neibs),
	recordaLogged(ReqPointer,awakens(Req,Neibs),RefGives),!.

testReqListViability([Req],[lit(Req,ReqKey,VCount,Pointer)],[ReqKey]):-!,
	ensureKey(Req,ReqKey,Pointer,VCount,_Pointer),!.
	%findall(GeneralFalses,(recordedLogged(Pointer,mostGeneralFalses(GeneralFalses)),Req=GeneralFalses),[]),!.
			
testReqListViability([Req|List],[lit(Req,ReqKey,VCount,Pointer)|Pointers],[ReqKey|ReqKeyS]):-
	ensureKey(Req,ReqKey,Pointer,VCount,_Pointer),!,
       %findall(GeneralFalses,(recordedLogged(Pointer,mostGeneralFalses(GeneralFalses)),Req=GeneralFalses),[]),
	testReqListViability(List,Pointers,ReqKeyS),!.

examineRuleForInfo(Literal,Key,VCount,Pointer,VarsRequested,UVars,Depth,Proof,ReqList,Info,Sum):-
	not(recorded(Pointer,Proof)),
	recorda(Pointer,Proof),
	testReqListViability(ReqList,RecPointers,ReqKeys),
	%trace,
	%write(ReqList),nl,nl,
	examineRuleForProving(Literal,Key,VCount,Pointer,VarsRequested,UVars,Depth,RecPointers,ReqKeys,Proof,KeyInfo),
	keysort(KeyInfo,RKeyInfo),
	reverse(RKeyInfo,Info),
	sumkeys(Info,Sum),!.
		
examineRuleForProving(Literal,Key,VCount,Pointer,VarsRequested,UVars,Depth,[Item],ReqKeys,Proof,[Info]):-    
	examineRuleForProving(Literal,Key,VCount,Pointer,VarsRequested,UVars,Depth,Item,ReqKeys,Proof,Info),!.
examineRuleForProving(Literal,Key,VCount,Pointer,VarsRequested,UVars,Depth,[Item|More],ReqKeys,Proof,[Info|InfoR]):-!,
	examineRuleForProving(Literal,Key,VCount,Pointer,VarsRequested,UVars,Depth,Item,ReqKeys,Proof,Info),
	examineRuleForProving(Literal,Key,VCount,Pointer,VarsRequested,UVars,Depth,More,ReqKeys,Proof,InfoR),!.

examineRuleForProving(Literal,Key,VCount,Pointer,VarsRequested,UVars,Depth,lit(Req,ReqKey,RVCount,ReqPointer),ReqKeys,Proof,Info):-
	recorded(ReqPointer,'-'(Len,OtherLitKeys),Ref),
	trace,nonvar(Len),!,
	examineRuleForProving2(Literal,Key,Len,OtherLitKeys,Ref,VCount,Pointer,VarsRequested,UVars,Depth,lit(Req,ReqKey,RVCount,ReqPointer),ReqKeys,Proof,Info),!.

examineRuleForProving2(Literal,Key,Len,OtherLitKeys,Ref,VCount,Pointer,VarsRequested,UVars,Depth,lit(Req,ReqKey,RVCount,ReqPointer),ReqKeys,Proof,
	nonvar(Ref),
	NLen - delayed(Req,ReqPointer,UVars,[Key|OtherLitKeys])):-
	not(not(memberchk(Key,OtherLitKeys))),erase(Ref),
        trace,
	NLen is Len + 1,recordaLogged(ReqPointer,(NLen-[Key|OtherLitKeys])),!.

examineRuleForProving2(Literal,Key,Len,OtherLitKeys,Ref,VCount,Pointer,VarsRequested,UVars,Depth,lit(Req,ReqKey,RVCount,ReqPointer),ReqKeys,Proof,
	Len - checkTable(Req,ReqPointer,UVars,OtherLitKeys)):-
	recordAwakensReq(ReqPointer,Req,ReqKey,ReqKeys),!.
		
examineRuleForProving(Literal,Key,VCount,Pointer,VarsRequested,UVars,Depth,lit(Req,ReqKey,RVCount,ReqPointer),ReqKeys,Proof,
	(RVCount - req(Req,ReqPointer,UVars))):-!.

% Prove One or More
inferGoal(proveAllWithInfoDisj(Infomation),VarsRequested,Ctx,KB,AccumulatedOut):-
	findall([Accu|MulatedOut],( 
			member(Cost-First,Infomation),
			findall(VarsRequested,inferGoal(nowProveWithInfoConj(Cost,First),VarsRequested,Ctx,KB,AccumulatedMid),[Accu|MulatedOut])
		),AccumulatedOut).
	

% Prove Single Item in Conj Then do the rest
inferGoal(nowProveWithInfoConj(_,[]),VarsRequested,Ctx,KB,[]):-!.
inferGoal(nowProveWithInfoConj(Cost,[First|Rest]),VarsRequested,Ctx,KB,Accumulated):-
	inferGoal((First),VarsRequested,Ctx,KB,Results),
	inferGoal(nowProveWithInfoConj(Cost,FirsRest),VarsRequested,Ctx,KB,AccumulatedMid),
	append(Results,AccumulatedMid,Accumulated).

% ==================================================	
% Literal Resoltuon
% ==================================================	

% Prove For Self Only  Means no goals where blocked on this generalization
inferGoal((Priorty-req(Req,ReqPointer,UVars)),VarsRequested,Ctx,KB,[Re|Sults]):-!,
	(varintersection(VarsRequested,UVars) ->
		inferGoal(findAllUsingGafsAndOrBackResolution(Req,UVars,Depth),VarsRequested,Ctx,KB, [Re|Sults]) 
		;inferGoal(findSingleUsingGafsAndOrBackResolution(Req,UVars,Depth),VarsRequested,Ctx,KB, [Re|Sults])).
	
	
% Prove For Self First Time (And/Or Others) goals where blocked on this generalization
inferGoal((Priorty-delayed(Req,ReqPointer,UVars,OtherLitKeys)),VarsRequested,Ctx,KB,[Re|Sults]):-!,
	(varintersection(VarsRequested,UVars) ->
		inferGoal(findAllUsingGafsAndOrBackResolution(Req,UVars,Depth),VarsRequested,Ctx,KB, [Re|Sults]) 
		;inferGoal(findSingleUsingGafsAndOrBackResolution(Req,UVars,Depth),VarsRequested,Ctx,KB, [Re|Sults])).

% Prove For Self Second Time (And/Or Others) goals where blocked on this generalization and on this path
inferGoal((Priorty-checkTable(Req,ReqPointer,UVars,OtherLitKeys)),VarsRequested,Ctx,KB,[Re|Sults]):-!,
	(varintersection(VarsRequested,UVars) ->
		inferGoal(findAllUsingGafsAndOrBackResolution(Req,UVars,Depth),VarsRequested,Ctx,KB, [Re|Sults]) 
		;inferGoal(findSingleUsingGafsAndOrBackResolution(Req,UVars,Depth),VarsRequested,Ctx,KB, [Re|Sults])).

varintersection(_,_).
	

bindFrom(Literal,Key,HashKey,Data):-
	ensureKey(Literal,Key,HashKey,_,Pointer),
	recordedLogged(Pointer,Data).

bindFromChk(Literal,Key,HashKey,Data):-
	ensureKey(Literal,Key,HashKey,_,Pointer),
	recordedLogged(Pointer,Data),!.

deleteFrom(Literal,Key,HashKey,Data):-
	ensureKey(Literal,Key,HashKey,_,Pointer),
	ignore((recordedLogged(Pointer,Data,Ref),erase(Ref),fail)).

pullFrom(Literal,Key,HashKey,Data):-
	ensureKey(Literal,Key,HashKey,_,Pointer),
	recordedLogged(Pointer,Data,Ref),erase(Ref).


pushInto(Literal,Key,HashKey,Data):-
	ensureKey(Literal,Key,HashKey,_,Pointer),
	recordaLogged(Pointer,Data),!.

putIntoZ(Literal,Key,HashKey,Data):-
	ensureKey(Literal,Key,HashKey,_,Pointer),
	recordzLogged(Pointer,Data),!.

putIntoZ(Literal,Key,HashKey,Data,Ref):-
	ensureKey(Literal,Key,HashKey,_,Pointer),
	recordzLogged(Pointer,Data,Ref),!.


putInto(Literal,Key,HashKey,Data):-
	putInto(Literal,Key,HashKey,_,Ref).

putIntochk(Literal,Key,HashKey,Data,Ref):-
	ensureKey(Literal,Key,HashKey,_,Pointer),
	((recordedLogged(Pointer,Data),Ref=old);recordaLogged(Pointer,Data,Ref)).
	
putInto(Literal,Key,HashKey,Data,Ref):-
	ensureKey(Literal,Key,HashKey,_,Pointer),
	(recordedLogged(Pointer,Data,Ref);recordaLogged(Pointer,Data,Ref)).




% Precall Hooks
%pre_infer_true(KB,Ctx):-!.

pre_infer_true(KB,Ctx):-
		retractall(is_tabled_true(_,_,_,_)),
		retractall(is_tabled_incomplete(_,_,_)).

pre_infer_false(KB,Ctx):-
		listing(is_tabled_true),
		listing(is_tabled_incomplete).


shouldCanBackChainOn(Literal,UVars,_):-not(not(sigmaDatabase(Literal,Reqs,_,KB,Ctx,Proof))).


sumkeys([],0).
sumkeys([K-_],K).
sumkeys([K1-_,K2-_],N):-
	N is K1 + K2,!.
sumkeys([K1-_,K2-_,K3-_],N):-
	N is K1 + K2 + K3,!.

sumkeys([K-_|More],N):-
	sumkeys(More,Keys),
	N is K + Keys,!.
	


/*

% Positive Query       
agentQueryEach(FmlInOpen,UQuery,UVars,Ctx,KB,User,UA, UVars,bullet(FmlInOpen) * Proof,Result):-
	set_quit_time(1.1),
	writeDebug(green,'Stage 3 - Positive ':FmlInOpen ),
	pre_infer_true(KB,Ctx),
	getQueryDefaults(UQuery,Answers,BackchainsMax,Deductions),
	free_variables(UQuery,FUVars),length(FUVars,H),
	writeDebug(infer_Stage_3(inferGoal(kbForm(TodoOrs,DoneTrue-DoneFail),H:FUVars,Depth,[],UQuery,Ctx,KB,Proof))),
	inferGoal(DB,H:FUVars,VO,Depth /* BackchainsMax */,[],UQuery,Ctx,KB,Proof),
	format('~n<hr> ~w: ~q~n<hr>~n',[UQuery,Proof]),
	flag('Returned Answers',RA,RA+1),
	is_unique(UQuery,UA),
	should_cut(RA,UA,Deductions,Answers,Result). %UA>0.
	
is_unique(UQuery,UA):-copy_term(UQuery,Copy),sigma_numbervars(Copy,0,_),!,is_unique2(Copy,UA),!.
is_unique2(Copy,-1):-t_answer_found(Copy),writeDebug(used(Copy)),!.
is_unique2(Copy,UA):-asserta(t_answer_found(Copy)),flag('UAnswers',UA,UA+1),!.

should_cut(RA,UA,Deductions,Answers,done(true:UA)):-
	(UA >= Answers; RA >= Deductions),!,retractall(cpuend(Then)),asserta(cpuend(-1)).
should_cut(RA,UA,Deductions,Answers,more(true:UA)):-!.


% Negative Query       
agentQueryEach(FmlInOpen,UQuery,UVars,Ctx,KB,User,UA2,['Result'='false'],Proof,done(false:FmlInOpen:Proof)):-%trace,
	flag('UAnswers',UA,UA),UA<1, % only calls in no answers found previous    
	UQuery=..[F|Args],
	NegUQuery=..['~query'|Args],
	set_quit_time(1.1),
	writeDebug(red,'Stage 4 - Negation ':not(FmlInOpen)),
	pre_infer_false(KB,Ctx),
        (getSigmaOption(opt_backchains_max_neg=BackchainsMax)),
	ignore(BackchainsMax=10),!,
	free_variables(Args,FUVars),
	length(FUVars,H),
	writeDebug(infer_Stage_3(inferGoal(DB,H:FUVars,BackchainsMax,[],NegUQuery,Ctx,KB,Proof))),
	inferGoal(DB,H:FUVars,VO,BackchainsMax,[],NegUQuery,Ctx,KB,Proof),
	flag('Returned Answers',RA,RA+1),!.
*/



% ===============================================================
% QUERY COMPILER
% Converts a KIF Surface Query to Query Macro in Heuristic Language
% ===============================================================

query_compile(not queryyn(PLOG),(PROLOG),KB,Ctx,VarsIn,VarsOut,Given):- !,                        
                        query_compile(not(PLOG),PROLOG,KB,Ctx,VarsIn,VarsOut,Given),!.                       
query_compile(queryyn(PLOG),(PROLOG),KB,Ctx,VarsIn,VarsOut,Given):- !,                        
                        query_compile(PLOG,PROLOG,KB,Ctx,VarsIn,VarsOut,Given),!.                       
query_compile(not(not(PLOG)),(PROLOG),KB,Ctx,VarsIn,VarsOut,Given):- !,                       
                        query_compile(PLOG,PROLOG,KB,Ctx,VarsIn,VarsOut,Given),!.                       

query_compile(This,This,KB,Ctx,VarsIn,VarsOut,true).







	

/*


			
			Info=  ,
			% Awakens Rules:  [Key|OtherLitKeys]
			% Awakens Reqs: ReqKeys 
*/
        		
% No Others waiting Return its 'complexity rating'


% Hack in foreign calls
inferGoal(DB,VarsIn,VarsOut,Predicate,Logic,Depth,Table,(NewQuery),Agent,KB,Proof):-
	inferGoal(DB,VarsIn,VarsOut,Depth,Table,(NewQuery),Agent,KB,Proof).

bad_fact(holds(foo,_,_)).
bad_fact(holds(_,X,_)):-X==instance.
%bad_fact(holds(instance,_,X)):-X=='SymmetricRelation';X=='EquivalenceRelation';X=='ReflexiveRelation'.
bad_fact(holds(foo,_,_,_)).
bad_fact(holds(foo,_,_,_,_)).
bad_fact(holds(foo,_,_,_,_,_)).
bad_fact(holds(_,X,_)):-X==element.

/*
bad_fact(holds(Foo,_,Foo2)):- % This also stops getPrologVars in Foo position
	(disabled(Foo);(rd(Foo3),Foo3==Foo2)),!.
	
	
disabled(subsumesContentInstance).
disabled(equivalentContentInstance).
disabled(subsumesContentClass).
disabled(equivalentContentClass).
disabled(equal).
disabled(element).
disabled(piece).

rd('TrichotomizingRelation').
rd('TotalOrderingRelation').
rd('PartialOrderingRelation').

*/

	
% Datastructure Of inferGoal

%inferGoal(DB,PG,NG,VG,PGD,NGD,PRD,NRD,VD,PGC,NGC,PRC,NRC,VC,Depth)
%PG PosGoals
%NG NegGoals
%VG Vars Goals
%PGD PosGoalsDelayed List of Most Specific Versions that are delayed
%NGD NegGoalsDelayed
%PRD PosRulesDelayed
%NRD NegRulesDelayed
%VD Vars Delayed
%PGC PosGoalsComplete  List of Most General Versions that are completed
%NGC NegGoalsComplete
%PRC PosRulesComplete
%NRC NegRulesComplete
%VC VarsComplete
%Depth

inferGoal(DB,~ NG):-!,
	inferGoal(DB,[],NG,VG,[],[],[],[],[],[],[],[],[],[],Depth).

inferGoal(DB,PG):-	!,
	inferGoal(DB,PG,[],VG,[],[],[],[],[],[],[],[],[],[],Depth).



putDBZ(DB,M,TypeGoalsComplete):-
	ToMatch=..[TypeGoalsComplete,M],!,
	recordzLogged(M,ToMatch).

isMoreGeneral(DB,Lit,TypeGoalsComplete,Match):-
	ToMatch=..[TypeGoalsComplete,Cmp],!,
	mrecordedLogged(Lit,ToMatch),
	not(not(Lit=Cmp)),
	once(compareVariant(Lit,Cmp,M,C1,C2)),nonvar(M),
	C1<C2.
	
mrecordedLogged(X,Y):-recordedLogged(X,Y).
mrecordedLogged(X,Y):-
	Y=..[posGoalsComplete,Literal],
	functor(X,F,A),
	functor(Literal,F,A),
	sigmaCache(Literal,_,KB,Ctx,Proof).
	


isLessGeneral(DB,Lit,TypeGoalsComplete,Match):-
	ToMatch=..[TypeGoalsComplete,Cmp],!,
	mrecordedLogged(Lit,ToMatch),
	not(not(Lit=Cmp)),
	once(compareVariant(Lit,Cmp,M,C1,C2)),nonvar(M),
	C1>=C2.

getDelayer(DB,posGoalsDelayed,M,Undelay):-
	posGoalsDelayed(M,Undelay).

getDelayer(DB,negGoalsDelayed,M,Undelay):-
	negGoalsDelayed(M,Undelay).
	
putDelayer(DB,negGoalsDelayed,NG,delayedOn(M)):-
	asserta(negGoalsDelayed(NG,M)).

putDelayer(DB,posGoalsDelayed,NG,delayedOn(M)):-
	asserta(posGoalsDelayed(NG,M)).
	

		
findWaiting(DB, ~NG,M):-!,negGoalsDelayed(NG,M).      
findWaiting(DB, NG,M):-posGoalsDelayed(NG,M).      
 
putSupport(DB,M,support(POS)).

putSolution(DB,M,solution(UPOS)).

inferGoal(DB,[G],Depth):-!,
	inferGoal(DB,G,Depth).

inferGoal(DB,[G|Rest],Depth):-!,
	inferGoal(DB,G,Depth),
	inferGoal(DB,Rest,Depth).

inferGoal(DB,~ NG,Depth):-inferGoalNeg(DB,NG,Depth).

inferGoal(DB,Pos,Depth):-not(Pos = ~ _),inferGoalPos(DB,Pos,Depth).

% Negative Stack

% Found less general Refution completed
inferGoalNeg(DB,NG,Depth):-
	isLessGeneral(DB,NG,posGoalsComplete,M),
	putDBZ(DB,~M,negGoalsDead),!,fail. % Get Next Job

% Found more general Refution completed
inferGoalNeg(DB,NG,Depth):-
	isMoreGeneral(DB,NG,posGoalsComplete,M),
	putDBZ(DB,~NG,negGoalsDead),!,fail. % Get Next Job

% Found more general Refution delayed
inferGoalNeg(DB,NG,Depth):-
	isLessGeneral(DB,NG,posGoalsDelayed,M),
	getDelayer(DB,posGoalsDelayed,M,Undelay),
	putDelayer(DB,negGoalsDelayed,NG,delayedOn(M)),!,
	inferGoal(DB,M,Depth). %New Job

% Found less general Refution delayed
inferGoalNeg(DB,NG,Depth):-
	isMoreGeneral(DB,NG,posGoalsDelayed,M),
	getDelayer(DB,posGoalsDelayed,M,Undelay),
	putDelayer(DB,negGoalsDelayed,NG,delayedOn(M)),!,
	inferGoal(DB,Undelay,Depth). %New Job

% Found less general goal delayed
inferGoalNeg(DB,NG,Depth):-
	isMoreGeneral(DB,NG,negGoalsDelayed,M),
	getDelayer(DB,negGoalsDelayed,M,Undelay),
	putDelayer(DB,negGoalsDelayed,NG,delayedOn(~M)),!,
	inferGoal(DB,Undelay,Depth).

% Found more general goal delayed (informational) Add support link
inferGoalNeg(DB,NG,Depth):-
	isLessGeneral(DB,NG,negGoalsDelayed,M),
	putSupport(DB,~M,support(~NG)),fail.

% Found more general goal complete (informational) Add solution link
inferGoalNeg(DB,NG,Depth):-
	isMoreGeneral(DB,NG,negGoalsComplete,M),
	putSolution(DB,~M,solution(~NG)),fail.	
	
% Found more general goal completed (find those waiting on it and call it)
inferGoalNeg(DB,NG,Depth):-
	isLessGeneral(DB,NG,negGoalsComplete,M),
	putDBZ(DB, NG, negGoalsComplete), %TODO guard
	((findWaiting(DB, ~NG,NWaiter),inferGoal(DB,NWaiter,Depth));
	(findWaiting(DB, ~NG,PWaiter),inferGoal(DB,PWaiter,Depth)));
	true.


% Positive Stack

% Found less general Refution completed
inferGoalPos(DB,POS,Depth):-
	isLessGeneral(DB,POS,negGoalsComplete,M),
	putDBZ(DB,M,posGoalsDead),!,fail. % Get Next Job

% Found more general Refution completed
inferGoalPos(DB,POS,Depth):-
	isMoreGeneral(DB,POS,negGoalsComplete,M),
	putDBZ(DB,POS,posGoalsDead),!,fail. % Get Next Job

% Found more general Refution delayed
inferGoalPos(DB,POS,Depth):-
	isLessGeneral(DB,POS,negGoalsDelayed,M),
	getDelayer(DB,negGoalsDelayed,M,Undelay),
	putDelayer(DB,posGoalsDelayed,POS,delayedOn(~M)),!,
	inferGoal(DB,M,Depth). %New Job

% Found less general Refution delayed
inferGoalPos(DB,POS,Depth):-
	isMoreGeneral(DB,POS,negGoalsDelayed,M),
	getDelayer(DB,negGoalsDelayed,M,Undelay),
	putDelayer(DB,posGoalsDelayed,POS,delayedOn(~M)),!,
	inferGoal(DB,Undelay,Depth). %New Job

% Found less general goal delayed
inferGoalPos(DB,POS,Depth):-
	isMoreGeneral(DB,POS,posGoalsDelayed,M),
	getDelayer(DB,posGoalsDelayed,M,Undelay),
	putDelayer(DB,posGoalsDelayed,POS,delayedOn(M)),!,
	inferGoal(DB,Undelay,Depth).

% Found more general goal delayed (informational) Add support link
inferGoalPos(DB,POS,Depth):-
	isLessGeneral(DB,POS,posGoalsDelayed,M),
	putSupport(DB,M,support(POS)),fail.

% Found more general goal complete (informational) Add solution link
inferGoalPos(DB,POS,Depth):-
	isMoreGeneral(DB,POS,posGoalsComplete,M),
	putSolution(DB,M,solution(POS)),fail.	
	
% Found more general goal completed (find those waiting on it and call it)
inferGoalPos(DB,POS,Depth):-
	isLessGeneral(DB,POS,posGoalsComplete,M),
	putDBZ(DB, POS, posGoalsComplete), %TODO guard
	((findWaiting(DB, POS, posGoalsDelayed,NWaiter),inferGoal(DB,NWaiter,Depth));
	(findWaiting(DB, POS, negGoalsDelayed,PWaiter),inferGoal(DB,PWaiter,Depth)));
	true.


% Look for Rules
inferGoal(DB,Goal,Depth):- 
	 isLessGeneralRule(DB,Goal,Head,Body,UHead,UBody),
	 taskRule(DB,Head,Body,UHead,UBody,Body,UBody,TODOLIST),
	 inferGoal(DB,TODOLIST,Depth).

taskRule(DB,Head,Body,UHead,UBody,[H],[UH],TODOLIST):-!,
	taskItem(DB,Head,Body,UHead,UBody,H,UH,TODOLIST),!.	

taskRule(DB,Head,Body,UHead,UBody,[H|T],[UH|UT],TODOLIST):-
	taskItem(DB,Head,Body,UHead,UBody,H,UH,TH),!,
	taskRule(DB,Head,Body,UHead,UBody,T,UT,TT),
	append(TH,TT,TODOLIST),!.
	
taskItem(DB,Head,Body,UHead,UBody,~ NG, ~UNG,TH):-!,
	getSingleVars(NG,UNG,Head,Body,UHead,UBody,Singletons),
	taskItemNeg(DB,Head,Body,UHead,UBody,NG,UNG,TH).
taskItem(DB,Head,Body,UHead,UBody,POS,UPOS,TH):-
	getSingleVars(POS,UPOS,Head,Body,UHead,UBody,Singletons),
	taskItemPos(Singletons,DB,Head,Body,UHead,UBody,POS,UPOS,TH).
	
getSingleVars(POS,UPOS,Head,Body,UHead,UBody,Singletons).

% Found more general Refution completed
taskItemPos(Singletons,DB,Head,Body,UHead,UBody,POS,UPOS,[POS]):-
	isMoreGeneral(DB,UPOS,negGoalsComplete,M),!,fail. % Rule Dead

% Found less general Refution completed
taskItemPos(Singletons,DB,Head,Body,UHead,UBody,POS,UPOS,[POS]):-
	isLessGeneral(DB,UPOS,negGoalsComplete,M),!,fail. % Rule too expensive for this item

% Found more general Refution delayed
taskItemPos(Singletons,DB,Head,Body,UHead,UBody,POS,UPOS,[M]):-
	isLessGeneral(DB,UPOS,negGoalsDelayed,M),
	getDelayer(DB,negGoalsDelayed,M,Undelay),
	putDelayer(DB,posGoalsDelayed,UPOS,delayedOn(M)),!.
	
% Found less general Refution delayed
taskItemPos(Singletons,DB,Head,Body,UHead,UBody,POS,UPOS,[M]):-
	isMoreGeneral(DB,UPOS,negGoalsDelayed,M),
	getDelayer(DB,negGoalsDelayed,M,Undelay),
	putDelayer(DB,posGoalsDelayed,UPOS,delayedOn(M)),!.
	
% Found less general goal delayed
taskItemPos(Singletons,DB,Head,Body,UHead,UBody,POS,UPOS,[M]):-
	isMoreGeneral(DB,UPOS,posGoalsDelayed,M),
	getDelayer(DB,posGoalsDelayed,M,Undelay),
	putDelayer(DB,posGoalsDelayed,UPOS,delayedOn(M)),!.
	
% Found more general goal delayed (informational) Add support link
taskItemPos(Singletons,DB,Head,Body,UHead,UBody,POS,UPOS,[UPOS]):-
	isLessGeneral(DB,UPOS,posGoalsDelayed,M),
	putSupport(DB,M,support(UPOS)),fail.

% Found more general goal complete (informational) Add solution link
taskItemPos(Singletons,DB,Head,Body,UHead,UBody,POS,UPOS,[]):-
	isMoreGeneral(DB,UPOS,posGoalsComplete,M),
	putSolution(DB,M,solution(UPOS)),fail.	
	
% Found more general goal completed (find those waiting on it and call it)
taskItemPos(Singletons,DB,Head,Body,UHead,UBody,POS,UPOS,[NWaiter,PWaiter]):-
	isLessGeneral(DB,UPOS,posGoalsComplete,M),
	putDBZ(DB, UPOS, posGoalsComplete), %TODO guard
	((findWaiting(DB, UPOS, posGoalsDelayed,NWaiter));
	(findWaiting(DB, UPOS, negGoalsDelayed,PWaiter))).
	

% Found more general Refution completed
taskItemNeg(Singletons,DB,Head,Body,UHead,UBody,NEG,UNEG,[NEG]):-
	isMoreGeneral(DB,UNEG,posGoalsComplete,M),!,fail. % Rule Dead

% Found less general Refution completed
taskItemNeg(Singletons,DB,Head,Body,UHead,UBody,NEG,UNEG,[NEG]):-
	isLessGeneral(DB,UNEG,posGoalsComplete,M),!,fail. % Rule too expensive for this item

% Found more general Refution delayed
taskItemNeg(Singletons,DB,Head,Body,UHead,UBody,NEG,UNEG,[M]):-
	isLessGeneral(DB,UNEG,posGoalsDelayed,M),
	getDelayer(DB,posGoalsDelayed,M,Undelay),
	putDelayer(DB,negGoalsDelayed,UNEG,delayedOn(~M)),!.
	
% Found less general Refution delayed
taskItemNeg(Singletons,DB,Head,Body,UHead,UBody,NEG,UNEG,[M]):-
	isMoreGeneral(DB,UNEG,posGoalsDelayed,M),
	getDelayer(DB,posGoalsDelayed,M,Undelay),
	putDelayer(DB,negGoalsDelayed,UNEG,delayedOn(~M)),!.
	
% Found less general goal delayed
taskItemNeg(Singletons,DB,Head,Body,UHead,UBody,NEG,UNEG,[M]):-
	isMoreGeneral(DB,UNEG,negGoalsDelayed,M),
	getDelayer(DB,negGoalsDelayed,M,Undelay),
	putDelayer(DB,negGoalsDelayed,UNEG,delayedOn(~M)),!.
	
% Found more general goal delayed (informational) Add support link
taskItemNeg(Singletons,DB,Head,Body,UHead,UBody,NEG,UNEG,[UNEG]):-
	isLessGeneral(DB,UNEG,negGoalsDelayed,M),
	putSupport(DB,~M,support(~UNEG)),fail.

% Found more general goal complete (informational) Add solution link
taskItemNeg(Singletons,DB,Head,Body,UHead,UBody,NEG,UNEG,[]):-
	isMoreGeneral(DB,UNEG,negGoalsComplete,M),
	putSolution(DB,~M,solution(~UNEG)),fail.	
	
% Found more general goal completed (find those waiting on it and call it)
taskItemNeg(Singletons,DB,Head,Body,UHead,UBody,NEG,UNEG,[NWaiter,PWaiter]):-
	isLessGeneral(DB,UNEG,negGoalsComplete,M),
	putDBZ(DB, UNEG, negGoalsComplete), %TODO guard
	((findWaiting(DB, UNEG, negGoalsDelayed,NWaiter));
	(findWaiting(DB, UNEG, posGoalsDelayed,PWaiter))).
	     
	
	 

	/*
	
% divideReqlist(ReqList,PGD,NGD,PGC,NGC,RPGD,RNGD,RPGC,RNGC).
divideReqlist([H],PGD,NGD,PGC,NGC,HPGD,HNGD,HPGC,HNGC,NewGoals):-
	divideReq(H,PGD,NGD,PGC,NGC,HPGD,HNGD,HPGC,HNGC,NewGoals),!.
divideReqlist([H|T],PGD,NGD,PGC,NGC,RPGD,RNGD,RPGC,RNGC,NewGoals):-
	divideReq(H,PGD,NGD,PGC,NGC,HPGD,HNGD,HPGC,HNGC,NewGoalsH),!,
	divideReqlist(T,HPGD,HNGD,HPGC,HNGC,RPGD,RNGD,RPGC,RNGC,NewGoalsT),
	append(NewGoalsH,NewGoalsT,NewGoals).
divideReq(~ NH,PGD,NGD,PGC,NGC,HPGD,HNGD,HPGC,HNGC,NewGoals):-
	more_specific_than_listed(NH,NGD,MSNGD), % then delay this as well and make the MSNGD wait on NH
	more_specific_than_listedc(NH,NGC,MSNGC), % then this is already true and add to NGC
	less_specific_than_listed(NH,NGD,LSNGD), % then delay this until LSNGD is complete
	less_specific_than_listedc(NH,NGC,LSNGC), % then this is true (add complete) NewGoals=[]
	
	more_specific_than_listedc(NH,PGC,MSPGC), % then this rule is worthless
	more_specific_than_listed(NH,PGD,MSPGD), % then delay this until MSPGD is complete 
	less_specific_than_listed(NH,PGD,LSPGD), % then delay LSPGD until the delayed NH is complete
	less_specific_than_listedc(NH,PGC,LSPGC), % then this rule is worthless
	
divideReq(PH,PGD,NGD,PGC,NGC,HPGD,HNGD,HPGC,HNGC,NewGoals):-
	more_specific_than_listed(PH,PGD,MSPGD), % then delay this as well and make the MSPGD wait on PH
	more_specific_than_listedc(PH,PGC,MSPGC), % then this is already true and add to PGC
	less_specific_than_listed(PH,PGD,LSPGD), % then delay this until LSPGD is complete
	less_specific_than_listedc(PH,PGC,LSPGC), % then this is true (add complete) NewGoals=[]
	
	more_specific_than_listedc(PH,NGC,MSNGC), % then this rule is worthless
	more_specific_than_listed(PH,NGD,MSNGD), % then delay this until MSNGD is complete 
	less_specific_than_listed(PH,NGD,LSNGD), % then delay LSNGD until the delayed PH is complete
	less_specific_than_listedc(PH,NGC,LSNGC), % then this rule is worthless
	
divideReq(PH,PGD,NGD,PGC,NGC,HPGD,HNGD,HPGC,HNGC,NewGoals):- 
	less_specific_than_listedc(PH,NGC,LSNGC),!,fail.  % then this rule is worthless

divideReq(PH,PGD,NGD,PGC,NGC,HPGD,HNGD,HPGC,HNGC,NewGoals):- 
	less_specific_than_listedc(PH,NGC,LSNGC),!,fail.  % then this rule is worthless
	
	more_specific_than_listedc(PH,NGC,MSNGC),!,fail.
	
	
	

divideReq(PH,PGD,NGD,PGC,NGC,PGD,NGD,[PH|PGC],NGC,[]):-	
	less_specific_than_listedc(PH,PGC,LSPGC),!. % then this is true (add complete) NewGoals=[]

divideReq(PH,PGD,NGD,PGC,NGC,PGD,NGD,[PH|PGC],NGC,[]):-	
	more_specific_than_listedc(PH,PGC,MSPGC), 
	% then this *will be* true but too general and will need to wait until all subvariant delays are completed.
	% this means that all answers to clauses that are subsumed by this will be the final value of this when it completes.
	% how will we know when this gets accomplised?
	% First enummerate all possible specific Possibilities and delay each until someone proves them true
	
	more_specific_than_listed(PH,PGD,MSPGD), % then delay this as well and make the MSPGD wait on PH
	less_specific_than_listed(PH,PGD,LSPGD), % then delay this until LSPGD is complete
	
	more_specific_than_listed(PH,NGD,MSNGD), % then delay this until MSNGD is complete 
	less_specific_than_listed(PH,NGD,LSNGD), % then delay LSNGD until the delayed PH is complete
	*/
	

	
	                    /*
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,or([]),Ctx,KB,ProofOut):-!.
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,[],Ctx,KB,ProofOut):-!.

inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,[Literal],Ctx,KB,ProofOut):-!,
	inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,Literal,Ctx,KB, ProofOut ). % Completeion
	
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,[Literal|LiteralList],Ctx,KB,ProofOut):-
	inferGoal(DB,VarsIn,VarsMid,BackchainsMax,ProofIn, Literal,Ctx,KB, ProofMid ),!, %trace,
	inferGoal(DB,VarsMid,VarsOut,BackchainsMax,ProofMid, LiteralList,Ctx,KB,ProofOut).

inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,tabled_or([LiteralList]),Ctx,KB,ProofOut):-!,
	inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,LiteralList,Ctx,KB, ProofOut). %trace,
	
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,tabled_or([LiteralList|DisjLiteralList]),Ctx,KB,ProofOut):-
	inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn, LiteralList,Ctx,KB, ProofOut)
	;
	inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn, tabled_or(DisjLiteralList),Ctx,KB,ProofOut).

inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,proved_gaf_or([LiteralList]),Ctx,KB,[ProofIn|LiteralList]):-!.
	
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,proved_gaf_or([LiteralList|_]),Ctx,KB,[ProofIn|LiteralList]).
	
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,proved_gaf_or([_,N|More]),Ctx,KB,ProofOut):-!,
	inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,proved_gaf_or([N|More]),Ctx,KB,ProofOut).

inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,or([LiteralList]),Ctx,KB,ProofOut):-!,
	inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,LiteralList,Ctx,KB, ProofOut).  %trace,
	
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,or([LiteralList|DisjLiteralList]),Ctx,KB,ProofOut):-
	inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn, LiteralList,Ctx,KB, ProofOut)
	;
	inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn, or(DisjLiteralList),Ctx,KB,ProofOut).
					*/
% Equals
%inferGoal(DB,VarsOut,VarsOut,BackchainsMax,Table,equal(U,W),Ctx,KB,Proof):-!,
%	prove_ground_goal(equal,Logic,BackchainsMax,Table,equal(U,W),Ctx,KB,Proof).
  /*
        
inferGoal(DB,VarsOut,VarsOut,BackchainsMax,ProofIn, Literal,Ctx,KB, _ ):-	
	mrecordedLogged(failed,Literal),!,fail.
  */
/*
inferGoal(DB,VarsOut,VarsOut,BackchainsMax,ProofIn, Literal,Ctx,KB, _ ):-	
	mrecordedLogged(trying,Literal),!,fail.
*/
	
	
    /*
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,Literal,Ctx,KB, ProofOut):-
	no_rules_about(Literal,Ctx,KB),!,fail.
      */  
:-dynamic(no_rules_about/3).

/*
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,Literal,Ctx,KB, ProofOut):-fail,!,
	ado_cache_access_smallest_first(Literal, ReqList, ThisCost,KB,_Ctx,Proof),
	not(memberchk(Proof,ProofIn)),
	BackchainsMax > ThisCost,
	BackchainsNext is BackchainsMax - ThisCost,
	vars_legal(VarsIn,Literal,GettingThereFaster,MVarsOut),
	makeSubproof(ProofIn,Literal,GettingThereFaster,Proof,ProofMid),
	inferGoal(DB,MVarsOut,VarsOut,BackchainsNext,ProofMid,ReqList,Ctx,KB,ProofOut).

%group_access_legal(N:VarsIn,VarsOut,BackchainsMax,ProofIn,Literal,Ctx,KB):-fail.

group_access_legal(N:VarsIn,VarsOut,BackchainsMax,ProofIn,Literal,Ctx,KB):-!.

*/





	


/*
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,ProofIn,Literal,Ctx,KB, ProofOut):-
	%trace,
      %  not(memberchk(Literal,ProofIn)),
   %     not(mrecordedLogged(trying_rules,Literal)),
	recordaLogged(trying_rules,Literal),
     %   group_access_legal(N:VarsIn,VarsOut,BackchainsMax,ProofIn,Literal,Ctx,KB),!,
	findall(ReqList,((
			ado_cache_access_largest_first(Literal, ReqList, ThisCost,KB,_Ctx,Proof),
			ThisCost < BackchainsMax
			%not(memberchk(Proof,ProofIn))
		       % not(memberchk(Literal,ReqList))
			)),GettingThere),!, %trace,
		list_to_set(GettingThere,[S|ET]),
		writeDebug(cyan,Literal =  [S|ET]),
		length(Set,Cost),  %trace,
		Cost < BackchainsMax,
		make_junts(VarsIn,ProofIn,Literal, [S|ET],GettingThereBetter,ProofJunt,MVarsOut),!,
		writeDebug(cyan,new_goal(GettingThereBetter)),
		%writeObject(new_goal(GettingThereBetter),_),
		BackchainN is BackchainsMax - Cost,
		makeSubproof(ProofIn,Literal,GettingThereFaster,ProofJunt,ProofMid), %trace,
		inferGoal(DB,MVarsOut,VarsOut,BackchainN,ProofMid,GettingThereBetter,_Ctx,KB,ProofOut).
*/		
makeSubproof(ProofIn,Literal,[[GettingThereFaster]],Proof,ProofMid):-!,
	append([Proof|ProofIn],[Literal|GettingThereFaster],ProofMid).
makeSubproof(ProofIn,Literal,GettingThereFaster,Proof,ProofMid):-!,
	append([Proof|ProofIn],[Literal|GettingThereFaster],ProofMid).
makeSubproof(ProofIn,Literal,GettingThereFaster,Proof,[Literal|ProofMid]).


	
mj0:-mj([]).
mj1:-mj([[b(1),c,x,b(Y)]]).
mj2:-mj([[b,c,x],[q,c,y]]).
mj2a:-mj(head(T), ([[b,c,x(T)],[b,c,y(P)]])).

mj2b:-mj([[b,c,x],[b,c,y(_)]]).


mj3:-mj([[b,c,x],[b,c,y],[b,c,z]]).
mj3a:-mj([[b,t,x],[b,c,y],[b,c,z]]).
mj3b:-mj([[b,t,x],[b,t,x],[b,c,z]]).
mj3c:-mj([[a,b,c],[d,e,f],[g,h,i]]).
mj4:-mj([[a(X,T),b(X,T),c(X,T)],[d(X,T),e(Y,T),f(Y,T)],[d(Y,T),d(X,T),b(T,T),f(b)],[g(Q,T),h(Q,T),i(9,T)]]).

mj2b:-mj([[a(X),b(N),c(X,T)],[g(Q,T),h,b(N),a(X)]]).

mj(MJ):-
	mj(head(X),MJ).

mj(Lit,MJ):-
	list_to_set(MJ,MJ1),
	writeq((Lit:-(-(MJ1) : MV))),nl,nl,
	make_junts(6,[],Lit,MJ1,G,PO,MV),
	writeq((Lit:-(+(G) : MV))),nl,nl.
	


/*

cleanGettingThereBetter(or(L),or(LL)):-
	cleanGettingThereBetter(L,LL).

cleanGettingThereBetter([[],H|L],LL):-
	cleanGettingThereBetter([H|L],LL).
	
cleanGettingThereBetter([A|L],[AA|LL]):-
	cleanGettingThereBetter(A,AA),
	cleanGettingThereBetter(L,LL).

cleanGettingThereBetter([A],AA):-
	cleanGettingThereBetter(A,AA).
*/

cleanGettingThereBetter(A,A).

/*
cleanGettingThereBetter([[H]|A],AA):-
	cleanGettingThereBetter([H|A],AA).
cleanGettingThereBetter(or([[H]|A]),AA):-
	cleanGettingThereBetter(or([H|A]),AA).
*/
	

/*
vars_legal(N:VarsIn,Literal,GettingThereFaster,NewN:MVarsOut):-
	N < 10,!,
	free_variables(GettingThereFaster,MVarsOut),length(MVarsOut,NewN).
*/

vars_legal(N:VarsIn,Literal,GettingThereFaster,NewN:MVarsOut):-
	free_variables(GettingThereFaster,MVarsOut),length(MVarsOut,NewN),!. %,(N<12).
	
	



make_junts(N,ProofIn,Literal, [],GettingThere,[GettingThere|ProofIn],NewN:MVarsOut):-!,fail.

make_junts(VarsIn,ProofIn,Literal, [[GettingThere]],GettingThereFaster,[GettingThereFaster|ProofIn],MVarsOut):-!,
	optimize_keys(Literal,GettingThere,GettingThereFaster),
	vars_legal(VarsIn,Literal,GettingThereFaster,MVarsOut).
	

make_junts(N,ProofIn,Literal, GettingThere, GettingThereFaster, ProofIn, NewN:MVarsOut):-
	shorted_paths(Literal,GettingThere,GettingThereFaster),
	vars_legal(VarsIn,Literal,GettingThereFaster,MVarsOut).


shorted_paths(Literal,[],[]).
shorted_paths(Literal,[GettingThere],GettingThereFaster):-
	optimize_keys(Literal,GettingThere,GettingThereFaster),!.
shorted_paths(Literal,[Conj1,Conj2],Best):-
	set_partition(Conj1,Conj2,NewConj1,NewConj2,Intersection12),
	optimize_keys(Literal,Intersection12,Intersection12Faster),
	optimize_keys(Intersection12,NewConj1,NewConj1Faster),
	optimize_keys(Intersection12,NewConj2,NewConj2Faster),
	optimize_keys(Intersection12Faster,[NewConj1Faster,NewConj2Faster],Faster),
	rewrite_leftovers(Intersection12Faster,Faster,Best),!.
shorted_paths(Literal,ConjS,Best):-
	length(ConjS,Depth),
	make_frames(Frame,Depth),
	number_sublits(1,ConjS,Numbered),
	keysort(Numbered,Keyed),
	combine_consts(Keyed,NumKeys),
	optimize_keys(Literal,NumKeys,OptKeys),
	keys_into_targets(Frame,OptKeys,Table,FreeGoals),
	optimize_keys(OptKeys,FreeGoals,FasterFreeGoals),
	rewrite_leftovers(Table,FasterFreeGoals,Best),!.

optimize_keys(Literal,[],[]):-!.
optimize_keys(Literal,[Lit],[Lit]):-!.
optimize_keys(Literal,List1,Opt):-predsort(optimize_keys(Literal),List1,Opt),!.
optimize_keys(Literal,Lit,Lit):-!.
optimize_keys(Literal,Ret,Lit1-L1,Lit2-L2):-length(L1,N1),length(L2,N2),(N1 \= N2,compare(Ret,N1,N2)).
optimize_keys(Literal,Ret,Lit1,Lit2):-free_variables((Lit1:(Literal:Var)),[FV1|_]),free_variables((Lit2:(Literal:Var)),[FV2|_]),!,
			(FV2\==FV1 -> compare(Ret,FV1,FV2) ; compare(Ret,Lit1,Lit2)).



make_frames([[]],1).
make_frames([[],[]],2).
make_frames([[],[],[]],3).
make_frames([[],[],[],[]],4).
make_frames([[],[],[],[],[]],5).
make_frames([[],[],[],[],[],[]],6).
make_frames(Result,Depth):-
	NN is N -6,make_frames(First,NN),
	append(First,[[],[],[],[],[],[]],Result).



keys_into_targets(FreeGoals,[],[],FreeGoals).
keys_into_targets(Frame,[Lit-[N]|Keys],Table,FreeGoals):-
	put_key_in((Lit),Depth,Frame,FrameMid),
	keys_into_targets(FrameMid,Keys,Table,FreeGoals).
keys_into_targets(Frame,[Lit-List|Keys],[Lit|Table],FreeGoals):-
	put_key_in_l(ex(Lit),List,Frame,FrameMid),
	keys_into_targets(FrameMid,Keys,Table,FreeGoals).

put_key_in_l(Key,[Slot],Frame,Framed):-
	put_key_in(Key,Slot,Frame,Framed).	
put_key_in_l(Key,[Slot|Slots],Frame,Framed):-
	put_key_in(Key,Slot,Frame,Framing),
	put_key_in_l(Key,Slots,Framing,Framed).
	

put_key_in(Key,1,[Role|Frame],[[Key|Role]|Frame]).
put_key_in(Key,2,[H1,Role|Frame],[H1,[Key|Role]|Frame]).
put_key_in(Key,3,[H1,H2,Role|Frame],[H1,H2,[Key|Role]|Frame]).
put_key_in(Key,4,[H1,H2,H3,Role|Frame],[H1,H2,H3,[Key|Role]|Frame]).
put_key_in(Key,Depth,[H1,H2,H3|FrameIn],[H1,H2,H3|FrameOut]):-
	NN is N - 4,put_key_in(Key,NN,FrameIn,FrameOut).	




rewrite_leftovers([],FreeGoals,or(FreeGoals)):-!.
rewrite_leftovers(Table,[],or(Table)):-!.
rewrite_leftovers(Table,FreeGoals,[tabled_or(Table),or(FreeGoals)]):-!.

	
	
combine_consts([E],[E]).
combine_consts([(Lit - NL),(Lit2 - [N])|Keyed],NumKeys):-
	Lit == Lit2,
	combine_consts([Lit-[N|NL]|Keyed],NumKeys).
combine_consts([E|Keyed],[E|NumKeys]):-
	combine_consts(Keyed,NumKeys).


			 
			
number_sublits(N,[],[]).
number_sublits(N,[Conj],Numbered):-
	number_each(N,Conj,Numbered).

number_sublits(N,[Conj|More],NumberedAll):-
	number_each(N,Conj,Numbered),
	NN is N + 1,
	number_sublits(NN,More,NumberedMore),
	append(Numbered,NumberedMore,NumberedAll).
	
number_each(N,[Lit],[Lit-[N]]).
number_each(N,[Lit|LitS],[Lit-[N]|NLitS]):-
	number_each(N,LitS,NLitS).

/*

shorted_paths(Literal,[Conj1,Conj2,Conj3],Simpler):-!,
	set_partition(Conj1,Conj2,NewConj12,NewConj21,Intersection12),
	set_partition(Conj1,Conj3,NewConj13,NewConj31,Intersection13),
	set_partition(Conj2,Conj3,NewConj23,NewConj32,Intersection23),
   pick_best(NewConj12,NewConj21,Intersection12,NewConj13,NewConj31,Intersection13,NewConj23,NewConj32,Intersection23,Simpler).

pick_best(NewConj12,NewConj21,[],NewConj13,NewConj31,[],NewConj23,NewConj32,[],or([NewConj12,NewConj21,NewConj32])).
   
*/

	

/*
shorted_paths(Literal,[Conj1,Conj2,Conj3],Simple):-!,
	shorted_paths(Literal,[Conj1,Conj2],Shortest12),
	shorted_paths(Literal,[Conj3,Shortest12],Simple).

shorted_paths(Literal,[Conj1,Conj2,Conj3|ConjS],Simple):-!,
	shorted_paths(Literal,[Conj1,Conj2],Shortest12),
	shorted_paths(Literal,[Conj3|ConjS],Shortest3S),
	shorted_paths(Literal,[Shortest12,Shortest3S],Simple).
*/

shorted_paths(Literal,Conj,Conj):-!.

/*
*/
	
		
	

simpler(or([]),[]):-!.
simpler(or(Conj),or(Simple)):-simpler(Conj,Simple).
simpler([Conj,[]],Simple):-simpler(Conj,Simple).
simpler([[],Conj],Simple):-simpler(Conj,Simple).
simpler([Conj1,Conj2],[SConj1,SConj2]):-
       simpler(Conj1,SConj1),
	simpler(Conj2,SConj2).
	
simpler(Simple,Simple):-!.


       
		

shorted_paths(Literal,ConjS,ConjS):-!.
     

	


ado_cache_access_smallest_first(Literal, ReqList, 1,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 1,KB,Ctx,Proof).
ado_cache_access_smallest_first(Literal, ReqList, 2,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 2,KB,Ctx,Proof).
ado_cache_access_smallest_first(Literal, ReqList, 3,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 3,KB,Ctx,Proof).
ado_cache_access_smallest_first(Literal, ReqList, 4,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 4,KB,Ctx,Proof).
ado_cache_access_smallest_first(Literal, ReqList, 5,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 5,KB,Ctx,Proof).
ado_cache_access_smallest_first(Literal, ReqList, 6,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 6,KB,Ctx,Proof).
ado_cache_access_smallest_first(Literal, ReqList, 7,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 7,KB,Ctx,Proof).
ado_cache_access_smallest_first(Literal, ReqList, 8,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 8,KB,Ctx,Proof).
ado_cache_access_smallest_first(Literal, ReqList, 9,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 9,KB,Ctx,Proof).



ado_cache_access_largest_first(Literal, ReqList, 9,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 9,KB,Ctx,Proof).
ado_cache_access_largest_first(Literal, ReqList, 8,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 8,KB,Ctx,Proof).
ado_cache_access_largest_first(Literal, ReqList, 7,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 7,KB,Ctx,Proof).
ado_cache_access_largest_first(Literal, ReqList, 6,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 6,KB,Ctx,Proof).
ado_cache_access_largest_first(Literal, ReqList, 5,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 5,KB,Ctx,Proof).
ado_cache_access_largest_first(Literal, ReqList, 4,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 4,KB,Ctx,Proof).
ado_cache_access_largest_first(Literal, ReqList, 3,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 3,KB,Ctx,Proof).
ado_cache_access_largest_first(Literal, ReqList, 2,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 2,KB,Ctx,Proof).
ado_cache_access_largest_first(Literal, ReqList, 1,KB, Ctx,Proof):-
	sigmaCache(Literal, ReqList, 1,KB,Ctx,Proof).

	

%sigmaCache(instance('exists '([63, 67, 79, 76, 79, 82, 50], exists([63, 67, 79, 76, 79, 82, 49], exists([63, 80, 65, 82, 84, 50], exists([63, 80, 65, 82, 84, 49], and(superficialPart([63, 80, 65, 82, 84, 49], A), and(superficialPart([63, 80, 65, 82, 84, 50], A), and(attribute([63, 80, 65, 82, 84, 49], [63, 67, 79, 76, 79, 82, 49]), and(attribute([63, 80, 65, 82, 84, 50], [63, 67, 79, 76, 79, 82, 50]), and(instance([63, 67, 79, 76, 79, 82, 49], 'ColorProperty'), and(instance([63, 67, 79, 76, 79, 82, 50], 'ColorProperty'), not(equal([63, 67, 79, 76, 79, 82, 49], [63, 67, 79, 76, 79, 82, 50])))))))))))), 'ColorProperty'), +instance(+, +), [attribute(A, 'Polychromatic'), domainV(A, [:(attribute, 1), :(superficialPart, 2)])], [+attribute(-, +)], 2, 'Merge', 'QUALITIES', surf('Merge', 6917, 12, ['OBJ'=A, 'PART1'=B, 'PART2'=C, 'COLOR1'=D, 'COLOR2'=E|F])).
	
not_in(N,Ele,List):-my_nth_gen(List,Ele,2,O),!,N < O.
my_nth_gen([Elem|_], Elem, Base, Base).
my_nth_gen([_|Tail], Elem, N, Base) :-
        succ(N, M),
        my_nth_gen(Tail, Elem, M, Base).

	

my_nth0(Index, List, Elem) :-
        integer(Index), !,
        Index >= 0,
        my_nth0_det(Index, List, Elem).    %% take my_nth deterministically
my_nth0(Index, List, Elem) :-
        var(Index), !,
        my_nth_gen(List, Elem, 0, Index).  %% match

my_nth0_det(0, [Elem|_], Elem) :- !.
my_nth0_det(1, [_,Elem|_], Elem) :- !.
my_nth0_det(2, [_,_,Elem|_], Elem) :- !.
my_nth0_det(3, [_,_,_,Elem|_], Elem) :- !.
my_nth0_det(4, [_,_,_,_,Elem|_], Elem) :- !.
my_nth0_det(5, [_,_,_,_,_,Elem|_], Elem) :- !.
my_nth0_det(N, [_,_,_,_,_,_   |Tail], Elem) :-
        M is N - 6,
        my_nth0_det(M, Tail, Elem).

/*

% specialization of disjoint true //TODO Specialized
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,Table,holds(disjoint,S,C),Ctx,KB, g_h(holds(disjoint,S,C)) * Proof ):-!,
	inferSurfaceGuarded(disjoint,Logic,holds(disjoint,S,C),Agent,KB,Proof).

% specialization of subrelation true //TODO Specialized
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,Table,holds(subrelation,S,C),Ctx,KB, g_h(holds(subrelation,S,C)) * Proof ):-!,
	inferSurfaceGuarded(subrelation,Logic,holds(subrelation,S,C),Agent,KB,Proof).

% specialization of Sentence ops //TODO Specialized
inferGoal(DB,VarsIn,VarsOut,Depth,Table,Goal,Agent,KB,P):-functor(Goal,F,_),
	hlPredicateAttribute(F,connective),!,
	writeDebug(blue,inferGoal_sentence_op(F,Logic,Depth,Goal,Agent,KB,Table)),
	inferGoal_sentence_op(F,Logic,Depth,Table,Goal,Agent,KB,P).

% ===================================================
% IrreflexiveRelation True
% ===================================================
inferGoal(DB,VarsIn,VarsOut,true,Depth,Table,holds(Predicate,Arg1,Arg2),Agent,KB,_):-
	Predicate\=instance,
	inferInstanceTable(KB,Predicate,'IrreflexiveRelation',_),
	Arg1==Arg2,!,fail.

% ===================================================
% IrreflexiveRelation False
% ===================================================
inferGoal(DB,VarsIn,VarsOut,false,Depth,Table,holds(Predicate,Arg1,Arg2),Agent,KB,P * Proof * bullet(not(holds(Predicate,Arg1,Arg2)))):-
	Predicate\=instance,
	inferInstanceTable(KB,Predicate,'IrreflexiveRelation',_),
	Arg1==Arg2,!,
	Proof=sfindi((instance(Predicate, 'IrreflexiveRelation')=>
		forall(Arg1, not holds(Predicate, Arg1, Arg2)))).

inferGoal(DB,VarsIn,VarsOut,true,Depth,Table,holds(Predicate,Arg1,Arg2),Agent,KB,P * Proof * bullet(not(holds(Predicate,Arg1,Arg2)))):-
	Predicate\=instance,
	Arg1==Arg2,
	inferInstanceTable(KB,Predicate,'SymmetricRelation'),
	Proof=sfindi((instance(Predicate, 'SymmetricRelation')=>
		forall(Arg1, holds(Predicate, Arg1, Arg2)))).


% Ground goal
%inferGoal(DB,VarsIn,VarsOut,BackchainsMax,Table,Fact,Ctx,KB,Proof):-ground(Fact),
%	get_pred(Fact,Predicate,_),!,
%	prove_ground_goal(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB,Proof).

% Instance goal
inferGoal(DB,VarsIn,VarsOut,true,BackchainsMax,Table,holds(instance,E,C),Ctx,KB,Proof):-
	atom(C),!,
	((inferInstanceTable(KB,E,C,Proof))
	%;prove_finite(instance,Logic,BackchainsMax,Table,holds(instance,E,C),Ctx,KB,Proof)
	).

% Instance goal
inferGoal(DB,VarsIn,VarsOut,true,BackchainsMax,Table,holds(instance,E,C),Ctx,KB,Proof):-var(E),var(C),!,
	inferInstanceTable(KB,C,'Class',_),not(C='Entity'),
	prove_finite(instance,true,BackchainsMax,Table,holds(instance,E,C),Ctx,KB,Proof).

% Prove Non Ground Goal
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,Table,Fact,Ctx,KB, Proof):-
	get_pred(Fact,Predicate,[A|Rgs]),
	(
	non_constrained(KB,Predicate,[A|Rgs]);
	mathmatic(Predicate)
	),
	inferSurfaceDomainVector(N,Predicate,VectS,Agent,KB,_),!,
	enforce_type_for_bound(KB,Predicate,VectS,[A|Rgs]),!,
	prove_ground_goal(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB,Proof).

enforce_type_for_bound(KB,Predicate,[],[]).
enforce_type_for_bound(KB,Predicate,[Vect|S],[A|L]):-
	enforce_type_for_bound_arg(KB,Predicate,Vect,A),
	enforce_type_for_bound(KB,Predicate,S,L).

enforce_type_for_bound_arg(KB,Predicate,_,A):-var(A),!.
enforce_type_for_bound_arg(KB,Predicate,Class,A):-
	inferGoal(DB,VarsIn,VarsOut,true,4,g_h([]),holds(instance,Arg,Class),Ctx,KB,Proof).
	


non_constrained(KB,domain,[A|Rgs]).
non_constrained(KB,equal,[A|Rgs]).
non_constrained(KB,documentation,[A|Rgs]).
non_constrained(KB,_,[A|Rgs]).

% Prove Non Ground Goal
inferGoal(DB,VarsIn,VarsOut,BackchainsMax,Table,Fact,Ctx,KB, Proof):-
	get_pred(Fact,Predicate,[A|Rgs]),
%	atom(Predicate),!,
	instance_all_predicate(KB,Predicate,[A|Rgs]),
	writeDebug(green,instanced_all(KB,Predicate,[A|Rgs])),%ground([A|Rgs]),
	%Predicate\=A,
	%safe_args([A|Rgs]),
	prove_ground_goal(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB,Proof).
	*/
	
instance_all_predicate(KB,Predicate,Args):-
	inferSurfaceDomainVector(N,Predicate,VectS,Agent,KB,_),!,
	p_inferPossibleInstancesFromClasslist(KB,VectS,Args).
	
p_inferPossibleInstancesFromClasslist(KB,[],[]).
p_inferPossibleInstancesFromClasslist(KB,[Class|Classes],[Arg|ArgS]):-
	p_inferInstanceTable(KB,Arg,Class),
	p_inferPossibleInstancesFromClasslist(KB,Classes,ArgS).
	
p_inferInstanceTable(KB,Arg,Class):- % trace,
	inferGoal(DB,VarsIn,VarsOut,true,4,g_h([]),holds(instance,Arg,Class),Ctx,KB,Proof),safe_arg(Arg).
	
% every description is eigther too general or too specific in most scenario cases.. 
% the real feat is to adapt to the user's mental pixelation

:-dynamic(is_tabled_true/4).
:-dynamic(is_tabled_incomplete/3).

% Prove Ground Goal
prove_ground_goal(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB,_):-
	is_tabled_incomplete(Logic,Fact,KB),!,fail.

prove_ground_goal(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB,Proof):-
	is_tabled_true(Logic,Fact,KB,Proof),!.


prove_ground_goal(equal,Logic,BackchainsMax,Table,Fact,Ctx,KB,Proof):-!,
	prove_finite(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB,Proof),
	ignore((ground((Fact)),asserta(is_tabled_true(Logic,Fact,KB,Proof)))),!.

prove_ground_goal(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB,Proof):-
	prove_finite(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB,Proof),
	ignore((ground((Fact,Proof)),asserta(is_tabled_true(Logic,Fact,KB,Proof)))).
	
prove_ground_goal(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB,_):-
	ground(Fact),asserta(is_tabled_incomplete(Logic,Fact,KB)),!.
	

nochain(documentation).
%nochain(disjoint).
nochain(finishes).
nochain(part).
nochain(exhaustiveDecomposition).
nochain(domainSubclass).
nochain(subrelation).
nochain(domain).


get_pred(G,Predicate,Args):- G=..[holds,Predicate|Args],!.
get_pred(G,Predicate,Args):- G=..[Predicate|Args].


safe_args([]):-!.
safe_args([H|T]):-
	safe_arg(H),!,
	safe_args(T),!.
	
safe_arg(H):-atomic(H).
safe_arg(H):-var(H).
safe_arg(zzfn(_,S)):-!,not(funcs(S)).
safe_arg(zzskFn(_,S)):-not(funcs(S)).
safe_arg(_).

funcs([H|T]):-
	funct(H);funcs(T).

funct(foo):-!,fail.
funct(zzfn(_,_)).
funct(zzskFn(_,_)).
	
 % Prove Goal
prove_finite(_,Logic,BackchainsMax,Table,G,Ctx,KB,Proof):-
	writeDebug(prove_finite(Logic,BackchainsMax,G,Ctx,KB,Table)),
	fail.
      
prove_finite(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB, g_h(Fact) * Table * Proof ):-
	inferSurface_gaf(Fact,KB,Ctx_,Proof). %,ground(Fact).

prove_finite(equal,true,Depth,Table,equal(U,W),Agent,KB,P):-!,equal(U,W,P),!. %defined in sigma_equal.pl
prove_finite(equal,false,Depth,Table,equal(U,W),Agent,KB,P):-!,not_equal(U,W,P),!. %defined in sigma_equal.pl

prove_finite(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB, g_h(Fact) * Table * Proof ):-
	mathmatic(Predicate),!,	
	inference_math(BackchainsMax,Table,Fact,Agent,KB,Proof).
	
mathmatic(greaterThan).
mathmatic(lessThan).
mathmatic(greaterThanOrEqualTo).
mathmatic(lessThanOrEqualTo).
mathmatic(subset).
mathmatic(element).

	inference_math(Depth,Table,Goal,Agent,KB,_):-
		once(writeDebug(pos(Depth,Table,Goal,Agent,KB))),fail.
	
	%inference_math(Depth,Table,ANY,Agent,KB,_):-!,fail.
	
	inference_math(Depth,Table,'greaterThan'(U,W),Agent,KB,('greaterThan'(U,W))):-'greaterThan'(U,W).
	inference_math(Depth,Table,'lessThan'(U,W),Agent,KB,('greaterThan'(U,W))):-'lessThan'(U,W).
	inference_math(Depth,Table,'greaterThanOrEqualTo'(U,W),Agent,KB,('greaterThanOrEqualTo'(U,W))):-'greaterThanOrEqualTo'(U,W).
	inference_math(Depth,Table,'lessThanOrEqualTo'(U,W),Agent,KB,('lessThanOrEqualTo'(U,W))):-'lessThanOrEqualTo'(U,W).
	
	exact_memberchk(UWFS,Table,exact):-exact_memberchk(UWFS,Table),!.
	exact_memberchk(UWFS,Table,unify):-not(not(memberchk(UWFS,Table))),!.
	exact_memberchk(UWFS,Table,unique):-!.

prove_finite(Predicate,Logic,BackchainsMax,Table,Fact,Ctx,KB, VIA * Proof ):-
	%ground(Fact),
	not(nochain(Predicate)),!,
	writeDebug(green,(Logic:Fact)),
	ado_cache_rule(Predicate,Fact, Conds, RT, Logic, KB, _Agent, (TID * Vars ), Props),
	not(member(on(_),Props)),
	writeDebug(teal,TID:Fact:Logic:Conds),
	check_loop(Table,Conds,Result),
	invoke_bakchain(Result,Fact,Conds,Table,TID,BackchainsMax,Ctx,KB,Proof),
	ground(Fact),
	make_via(Logic,TID,VarsIn,VarsOut,Fact,Conds,VIA).
      
invoke_bakchain(yes,Fact,Conds,Table,TID,BackchainsMax,Ctx,KB,Proof):-
	not(my_member(tid(TID),Table)),
	not(my_member(g_h(Fact),Table)),
        Backchains2 is BackchainsMax-3, %Since a Loop should be explored a little
	inferGoal(DB,VarsIn,VarsOut,true,Backchains2,(tid(TID)  * g_h(Fact) * Table  ),Conds,Ctx,KB, Proof ).

invoke_bakchain(no,Fact,Conds,Table,TID,BackchainsMax,Ctx,KB,Proof):-
	not(my_member(tid(TID),Table)),
	Backchains2 is BackchainsMax-1,
	inferGoal(DB,VarsIn,VarsOut,true,Backchains2,(tid(TID)  * g_h(Fact) * Table  ),Conds,Ctx,KB, Proof ).

make_via(true,TID,VarsIn,VarsOut,Fact,Conds,via(entails(Conds,Fact),Vars) * TID).
make_via(false,TID,VarsIn,VarsOut,Fact,Conds,via(entails(Conds,not(Fact)),Vars) * TID).

check_loop(Table,and(Conds,R),yes):-not(ground(Conds)),!.
check_loop(Table,Conds,yes):-
	not(not(has_loop(Table,Conds))),!.
check_loop(Table,Conds,no).

has_loop(Table,and(A,B)):-!,
	(has_loop(Table,A);has_loop(Table,B)).
has_loop(Table,not(A)):-!,has_loop(Table,A),!.
has_loop(Table,C):-my_member(C,Table),!.
%has_loop(Table,C):-my_member(g_h(C),Table),!.


my_member(X,X *  _):-!.
my_member(X,((_ *  X) * _)):-!.
my_member(X,Y1 *  Y2):-!,my_member(X,Y2),!.
my_member(X,X).



% Computational Intelligence: a logical approach. 
% Prolog Code. 
% A CSP solver using arc consistency (Figure 4.8)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.
                                    
% csp(Domains, Relations) means that each variable has
% an instantiation to one of the values in its Domain 
% such that all the Relations are satisfied.
% Domains represented as list of 
% [dom(V,[c1,...,cn]),...]
% Relations represented as [rel([X,Y],r(X,Y)),...]
%  for some r

csp(Doms,Relns) :-
   ac(Doms,Relns).


% ac(Dom,Relns) is true if the domain constrants
% specified in Dom and the binary relations
% constraints specified in Relns are satisfied.
ac(Doms,Relns) :-
   make_arcs(Relns,A),
   consistent(Doms,[],A,A).

% make_arcs(Relns, Arcs) makes arcs Arcs corresponding to
% relations Relns. There are acrs for each ordering of
% variables in a relations.
make_arcs([],[]).
make_arcs([rel([X,Y],R)|O],
          [rel([X,Y],R),rel([Y,X],R)|OA]) :-
   make_arcs(O,OA).

% consistent(Doms,CA,TDA,A) is true if
% Doms is a set of domains
% CA is a set of consistent arcs,
% TDA is a list of arcs to do
% A is a list of all arcs
consistent(Doms,CA,TDA,A) :-
   consider(Doms,RedDoms,CA,TDA),
   solutions(RedDoms,A).            

% consider(D0,D1,CA,TDA)
% D0 is the set of inital domains
% D1 is the set of reduced domains
% CA = consistent arcs, 
% TDA = to do arcs
consider(D,D,_,[]).
consider(D0,D3,CA,[rel([X,Y],R)|TDA]) :-
   choose(dom(XV,DX),D0,D1),X==XV,
   choose(dom(YV,DY),D1,_),Y==YV, !,
   prune(X,DX,Y,DY,R,NDX),
   ( NDX = DX
   ->
     consider(D0,D3,[rel([X,Y],R)|CA],TDA)
   ; acc_todo(X,Y,CA,CA1,TDA,TDA1),
     consider([dom(X,NDX)|D1],D3,
              [rel([X,Y],R)|CA1],TDA1)).

% prune(X,DX,Y,DY,R,NDX)
% variable X had domain DX
% variable Y has domain DY
% R is a relation on X and Y
% NDX = {X in DX | exists Y such that R(X,Y) is true}
prune(_,[],_,_,_,[]).
prune(X,[V|XD],Y,YD,R,XD1):-
   \+ (X=V,member(Y,YD),R),!,
   prune(X,XD,Y,YD,R,XD1).
prune(X,[V|XD],Y,YD,R,[V|XD1]):-
   prune(X,XD,Y,YD,R,XD1).

% acc_todo(X,Y,CA,CA1,TDA,TDA1)
% given variables X and Y,
% updates consistent arcs from CA to CA1 and
% to do arcs from TDA to TDA1
acc_todo(_,_,[],[],TDA,TDA).        
acc_todo(X,Y,[rel([U,V],R)|CA0],
         [rel([U,V],R)|CA1],TDA0,TDA1) :-
   ( X \== V
   ; X == V,
     Y == U),   
   acc_todo(X,Y,CA0,CA1,TDA0,TDA1).
acc_todo(X,Y,[rel([U,V],R)|CA0],
         CA1,TDA0,[rel([U,V],R)|TDA1]) :-
   X == V,
   Y \== U,
   acc_todo(X,Y,CA0,CA1,TDA0,TDA1).

% solutions(Doms,Arcs) given a reduced set of
% domains, Dome, and arcs Arcs, solves the CSP.
solutions(Doms,_) :-
   solve_singletons(Doms).
solutions(Doms,A) :-
   select(dom(X,[XV1,XV2|XVs]),Doms,ODoms),
   split([XV1,XV2|XVs],DX1,DX2),
   acc_todo(X,_,A,CA,[],TDA),
   ( consistent([dom(X,DX1)|ODoms],CA,TDA,A)
   ; consistent([dom(X,DX2)|ODoms],CA,TDA,A)).

% solve_singletons(Doms) is true if Doms is a
% set of singletom domains, with the variables
% assigned to the unique values in the domain
solve_singletons([]).
solve_singletons([dom(X,[X])|Doms]) :-
   solve_singletons(Doms).

% select(E,L,L1) selects the first element of
% L that matches E, with L1 being the remaining
% elements.
select(D,Doms,ODoms) :-
   remove(D,Doms,ODoms), !.

% choose(E,L,L1) chooses an element of
% L that matches E, with L1 being the remaining
% elements.
choose(D,Doms,ODoms) :-
   remove(D,Doms,ODoms).

% split(L,L1,L2) splits list L into two lists L1 and L2
% with the about same number of elements in each list.
split([],[],[]).
split([A],[A],[]).
split([A,B|R],[A|R1],[B|R2]) :-
   split(R,R1,R2).



% Computational Intelligence: a logical approach. 
% Prolog Code. 
% A CSP solver using generate and test. 
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.
                                    
% csp(Domains, Relations) means that each variable has
% an instantiation to one of the values in its Domain 
% such that all the Relations are satisfied.
% Domains represented as list of 
% [dom(V,[c1,...,cn]),...]
% Relations represented as [rel([X,Y],r(X,Y)),...]
%  for some r
%csp(Doms,Relns) :-
%   generate_and_test(Doms,Relns).

% generate_and_test(Doms,Relns) is true if we can
% find a value for each variable that satisfies
% Relns by generating and testing.
generate_and_test(Doms,Relns) :-
   generate(Doms),
   test(Relns).

generate([]).
generate([dom(X,D)|Ds]) :-
   member(X,D),
   generate(Ds).

test([]).
test([rel(_,R)|Rs]) :-
   call(R),
   test(Rs).

/*
member(A,[A|_]).
member(A,[_|L]) :-
   member(A,L).
*/


% Computational Intelligence: a logical approach. 
% Prolog Code. 
% CSP solver by picking random assignments
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% this assumes that standard.pll and random.pll are also loaded.

% random_csp(Doms,Relns,Maxiter,Asst) chooses at most Maxiter assignments
% at random, until one satisfies Relns.
random_csp(Doms,Relns,_,Asst) :-
   random_assign(Doms,Asst),
   writeln(['Trying Assignment: ',Asst]),
   number_unsat_relns(Relns,Asst,0).
random_csp(Doms,Relns,Maxiter,Asst) :-
   Maxiter>1,
   IterLeft is Maxiter-1,
   random_csp(Doms,Relns,IterLeft,Asst).


   % number_unsat_relns(Relns,Asst,Depth) means N is the number of unsatisfied
% relations of Relns using assignment Asst
number_unsat_relns([],_,0).
number_unsat_relns([rel([X,Y],R)|Rels],Asst,Depth) :-
    number_unsat_relns(Rels,Asst,N0),
    (\+ (val_in_asst(X,Asst), val_in_asst(Y,Asst), R) 
    -> N is N0+1
     ; N=N0).


%%%% INTERFACE TO STANDARD CSP SETUP %%%%

% csp(Domains, Relations) means that each variable has
% an instantiation to one of the values in its Domain 
% such that all the Relations are satisfied.

/*
csp(Doms,Relns) :-
   random_csp(Doms,Relns,Depth,Ans),      % Depth is arbitrary setting
   set_all(Ans).
*/


% set_all(Asst) sets all of the variables in Asst to their values

% Computational Intelligence: a logical approach. 
% Prolog Code. 
% A CSP solver using GSAT. 
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

% this assumes that random.pll and standard.pll are also loaded.
                                    
% DATA STRUCTURES

% Domains represented as list of 
% [dom(X,[c1,...,cn]),...]

% Relations represented as [rel([X,Y],r(X,Y)),...]
%  for some r

% An assignment is represented as a list
% [val(X,V),...] where X is a variable
% and V is a value in the domain of X
% If variable X appears more than once, the first value is meant

% RELATIONS
/*
csp(Doms,Relns) :-
   gsat(Doms,Relns,Depth,Depth,Ans),      % Depth Depth is arbitrary setting
   set_all(Ans).
*/


% gsat(Doms,Relns,Depth,M,Ass) is true if Ass is an
% assignment of a value for each variable that satisfies
% Relns using GSAT. 
% N is the maxumum number of restarts.
% Each restart does M steps of hill climbing.
gsat(Doms,Relns,_,M,SAss) :-
   random_assign(Doms,Asst),
%   writeln(['Given Doms = ',Doms]),
   writeln(['   Random Assignment: ',Asst]),
   it_improve(Asst,Doms,Relns,M,SAss).
gsat(Doms,Relns,Depth,M,SAss) :-
   N>0,
   N1 is N-1,
   gsat(Doms,Relns,N1,M,SAss).

% random_assign(Doms,Asst) is true if Asst is a random assignment
% of a value to each variable, given domains Doms
random_assign([],[]).
random_assign([dom(X,D)|Ds],[val(X,E)|Vs]) :-
   random_elt(E,D),
   random_assign(Ds,Vs).

% it_improve(Asst,Doms,Relns,M,SAss)
% is true if, given
% Asst is a random Assignment,
% Doms is a set of domains
% Relns is a set of Relations
% M is a bound on the number of iterations,
% we can find an satisfying assignment SAss of values to variables that
% satisfies all of the relations.
it_improve(Asst,_,Relns,_,Asst) :-
   number_unsat_relns(Relns,Asst,NSat),
   writeln(['   Value = ',NSat]),
   NSat=0.                     % it randomly chose satisfying asst
it_improve(Asst,Doms,Relns,M,SAss) :-
   getPrologVars(Doms,Vars),
   improve_one(Vars,Doms,Relns,Asst,99999,Asst,BVal,BAss),
   it_improve_new(Asst,Doms,Relns,Vars,M,BVal,BAss,SAss).

% it_improve_new(Asst,Doms,Relns,Vars,M,BVal,BAss,SAss).
% is true if, given
% Asst is a random Assignment,
% Doms is a set of domains
% Relns is a set of Relations
% Vars is the list of all assignments
% M is a bound on the number of iterations,
% BVal is the value of the previous iteration
% BAss is the best assignment of the previous iteration
% we can find an satisfying assignment SAss of values to variables that
% satisfies all of the relations.
% Note that this is seperate from it_improve as we have to do
%  something different in the first iteration.
it_improve_new(_,_,_,_,_,BVal,val(Var,Val),_) :-
   writeln([' Assign ',Val,' to ',Var,'. New value = ',BVal]),
   fail.
it_improve_new(Asst,_,_,_,_,0,BAss,SAss) :-
   update_asst(Asst,BAss,SAss).
it_improve_new(Asst,Doms,Relns,Vars,M,_,val(Var,Val),SAss) :-
   M>0,
   rem_id(Var,Vars,RVars),
   update_asst(Asst,val(Var,Val),NAss),
   writeln(['    New Asst: ',NAss]),
   M1 is M-1,
   improve_one(RVars,Doms,Relns,NAss,99999,NAss,BVal2,BAss2),
   it_improve_new(NAss,Doms,Relns,Vars,M1,BVal2,BAss2,SAss).

% update_asst(Assts,Ass,SAss) given a list of assignments Assts,
% and a new assignment Ass returns the updated assignment list SAss.
update_asst([val(Var1,_)|Vals],val(Var,Val),[val(Var,Val)|Vals]) :-
   Var==Var1,!.
update_asst([val(Var1,Val1)|Vals],val(Var,Val),[val(Var1,Val1)|RVals]) :-
   update_asst(Vals,val(Var,Val),RVals).

% finds the best assignment to improve for this iteration
% improve_one(RemVars,             remaining variables to check
%         Doms,                    domains list
%         Relns,                   relations list
%         CurrentTotalAssignment,
%         CurrentBestValue,        
%         CurrentBestAssign,       current best val(Var,Val) to change
%         FinalBestValue,          final best value
%         FinalBestAssign)         val(Var,Val) for the best one to change
improve_one([],_,_,_,BV,BA,BV,BA).
improve_one(Vars,Doms,Relns,CTA,CBV0,CBA0,FBV,FBA) :-
   random_rem(Var,Vars,Vars2),
   domain_var_check(Var,Dom,Doms),
   lookup(Var,CTA,Val),
   remove(Val,Dom,RDom),
   check_other_vals_for_var(RDom,Var,Relns,CTA,CBV0,CBA0,CBV1,CBA1),
   improve_one(Vars2,Doms,Relns,CTA,CBV1,CBA1,FBV,FBA).

% check_other_vals_for_var(RDom,Var,Relns,CTA,CBV0,CBA0,CBV1,CBA1)
% checks the values RDom for variable Var
% Relns is the list of relations
% CTA is the current total assignment
% CBV0 is the previous best value
% CBA0 is the previous best assignment
% CBV1 is the final best value
% CBA1 is the final best assignment
check_other_vals_for_var([],_,_,_,CBV,CBA,CBV,CBA).
check_other_vals_for_var(Vals,Var,Relns,CTA,CBV0,CBA0,CBV1,CBA1) :-
   random_rem(Val,Vals,RVals),
   number_unsat_relns(Relns,[val(Var,Val)|CTA],Num),
   ( Num < CBV0
   -> check_other_vals_for_var(RVals,Var,Relns,CTA,Num,
                   val(Var,Val),CBV1,CBA1)
   ; check_other_vals_for_var(RVals,Var,Relns,CTA,CBV0,CBA0,CBV1,CBA1)
   ).
   

% domain_var_check(Var,Dom,Doms) is true if Dom is the domain of variable Var in Doms
domain_var_check(Var,Dom,[dom(Var1,Dom)|_]) :- Var==Var1,!.
domain_var_check(Var,Dom,[_|Doms]) :-
   domain_var_check(Var,Dom,Doms).

% val_in_asst(Var,Assignment) unifies Var with its value in Assignment
val_in_asst(Var,[val(Var1,Val1)|_]) :-
   Var==Var1,!,Var=Val1.
val_in_asst(Var,[_|Ass]) :-
   val_in_asst(Var,Ass).

% lookup(Var,Assignment,Val) unifies Var with its value in Assignment
lookup(Var,[val(Var1,Val1)|_],Val) :-
   Var==Var1,!,Val=Val1.
lookup(Var,[_|Ass],Val) :-
   lookup(Var,Ass,Val).

% rem_id(El,Lst,Rem) is true if Rem is the list remaining
% from removing the element of Lst that is identical to El.
rem_id(Var1,[Var|Vars],RVars) :-
   Var==Var1, !, RVars=Vars.
rem_id(Var1,[Var|Vars],[Var|RVars]) :-
    rem_id(Var1,Vars,RVars).

% getPrologVars(Doms,Vars) is true if Vars is the list of variables in Doms
getPrologVars([],[]).
getPrologVars([dom(X,_)|Ds],[X|Vs]) :-
   getPrologVars(Ds,Vs).

%%%% INTERFACE TO STANDARD CSP SETUP %%%%

% csp(Domains, Relations) means that each variable has
% an instantiation to one of the values in its Domain 
% such that all the Relations are satisfied.




% set_all(Asst) sets all of the variables in Asst to their values
set_all([]).
set_all([val(X,X)|R]) :-
   set_all(R).



% Computational Intelligence: a logical approach. 
% Prolog Code. 
% A crossword puzzle example of Figure 4.13.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

crossword(A1,A2,A3,D1,D2,D3) :-     
    cross(A1,A2,A3,D1,D2,D3,
          [[a,d,d], [a,d,o], [a,g,e], [a,g,o], [a,i,d],
           [a,i,l], [a,i,m], [a,i,r], [a,n,d], [a,n,y],
           [a,p,e], [a,p,t], [a,r,c], [a,r,e], [a,r,k],
           [a,r,m], [a,r,t], [a,s,h], [a,s,k], [a,u,k],
           [a,w,e], [a,w,l], [a,y,e], [b,a,d], [b,a,g],
           [b,a,n], [b,a,t], [b,e,e], [b,o,a], [e,a,r],
           [e,e,l], [e,f,t], [f,a,r], [f,a,t], [f,i,t],
           [l,e,e], [o,a,f], [r,a,t], [t,a,r], [t,i,e]
          ]).
           

cross(A1,A2,A3,D1,D2,D3,Words) :-
    csp([dom(A1,Words),
         dom(A2,Words),
         dom(A3,Words),
         dom(D1,Words),
         dom(D2,Words),
         dom(D3,Words)],
        [rel([A1,D1],compatible(A1,1,D1,1)),
         rel([A1,D2],compatible(A1,2,D2,1)),
         rel([A1,D3],compatible(A1,3,D3,1)),
         rel([A2,D1],compatible(A2,1,D1,2)),
         rel([A2,D2],compatible(A2,2,D2,2)),
         rel([A2,D3],compatible(A2,3,D3,2)),
         rel([A3,D1],compatible(A3,1,D1,3)),
         rel([A3,D2],compatible(A3,2,D2,3)),
         rel([A3,D3],compatible(A3,3,D3,3))]).

compatible(Word1,I1,Word2,I2) :- 
    common_letter(Word1,I1,Word2,I2).

common_letter(Word1,I1,Word2,I2) :-
    letter(Word1,I1,Letter),
    letter(Word2,I2,Letter).

letter(Word,I,Letter) :- nth(I,Word,Letter).

% | ?- crossword(A,B,C,D,E,F).

% Computational Intelligence: a logical approach. 
% Prolog Code. 
% Random number generater (Example 2.9 page 485)
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.

:- dynamic seed/1.

seed(447563523).

% rand(R) generates random real number R in the range [0,1)
rand(R) :- R is random(5000) / 5000.

  /*
   retract(seed(S)),
   N is (S * 314159262 + 453816693) mod 2147483647,
   assert(seed(N)),
   R is N / 2147483647.0 .
	*/
  
% ramdom(R,M) generates random integer R in the range 0..M-1
random(R,M) :-
    rand(RR),
    R is integer(M * RR).

% random_list(N,L) creates list L of length N of random numbers in range [0,1)
random_list(0,[]).
random_list(N,[R|L]) :-
    N>0,
    rand(R),
    N1 is N-1,
    random_list(N1,L).

% random_elt(E,L) selects a random element E of list L
random_elt(E,L) :-
   length(L,Depth),
   random(R,Depth),
   N1 is R+1,
   nth(N1,L,E).

% random_rem(E,L,R) selects a random element E of
% list L where list R is contains the other elements.
random_rem(E,L,R) :-
   length(L,Depth),
   random(Rand,Depth),
   N1 is Rand+1,
   nth(N1,L,E,R).


% nth(N,L,E,R) is true if E is the Nth element of
% list L, and R is the remianing elements. We start
% counting positions from 1.
nth(1,[E|R],E,R).
nth(N,[H|T],E,[H|R]) :-
   N>1,
   N1 is N-1,
   nth(N1,T,E,R).

nth(X,Y,Z):-nth1(X,Y,Z).


% :-['sigma_csat.pl'].




% remove(E,L,R) is true if E is an element of L and R is the remaining
% elements after E is removed.
remove(E,[E|R],R).
remove(E,[A|L],[A|R]) :-
   remove(E,L,R).


% ==========================================
% INFERENCE
% ==========================================

:-dynamic(complete_goal/1).

:-multifile(expireOptimizationsInKB/3).

:- style_check(-singleton).
:- style_check(-discontiguous).
:- was_style_check(-atom).
:- was_style_check(-string).

/*

This file impliments proof theory over a compiled derivative of SUO-KIF

For Axiom Key See http://plato.stanford.edu/entries/logic-modal/

*/

% ==========================================
% Hit Variables
% ==========================================
/*inferGoal(Predicate,Logic,Depth,Table,L,Agent,KB,P ):-
	format('~q.\n',[inferGoal(Predicate,Logic,L,Agent,KB,Depth,Table)]),fail.
  */


inferGoal(Predicate,Logic,Depth,Table,subclass(X,Y),Agent,KB,Proof):-X=='Entity',!,Y='Entity'.

inferGoal(Predicate,Logic,Depth,Table,Var,Agent,KB,Proof):-getPrologVars(Var,[_,_,_|_],_,_),!,fail.
inferGoal(_,true,Depth,Table,equal(U,W),Agent,KB,P):-!,equal(U,W,P),!.  %defined in sigma_equal.pl
inferGoal(_,true,Depth,Table,not equal(U,W),Agent,KB,P):-!,not_equal(U,W,P),!. %defined in sigma_equal.pl
inferGoal(holds,Logic,Depth,Table,holds(holds,_,_),Agent,KB,Proof):-!,fail.
inferGoal(Predicate,Logic,Depth,Table,Var,Agent,KB,Proof):-
	once(writeDebug(inferGoal(Predicate,Logic,Var,Depth,Agent,KB,Table))),fail.


inferGoal(holds,Logic,Depth,Table,Var,Agent,KB,Proof):-
	predicate_holds(Var,Predicate),	!,
	inferGoal(Predicate,Logic,Depth,Table,Var,Agent,KB,Proof).
	
predicate_holds(holds(Predicate,_,_),Predicate).
predicate_holds(holds(Predicate,_,_,_),Predicate).
predicate_holds(holds(Predicate,_,_,_,_),Predicate).
predicate_holds(holds(Predicate,_),Predicate).
predicate_holds(G,Predicate):-functor(G,Predicate,_).


inferGoal(Predicate,Logic,Depth,Table,formula,Agent,KB,ProofB ):-!,fail.
%	writeDebug(red,formula),!,fail.

inferGoal(Predicate,var,Depth,Table,_,Agent,KB,B ):-!, true,fail.

inferGoal(_,Logic,Depth,Table,and(A,B),Agent,KB,ProofA * ProofB ):-!,
	inferGoal(holds,Logic,Depth,Table,A,Agent1,KB,ProofA ),
	inferGoal(holds,Logic,Depth,Table,B,Agent2,KB,ProofB ).

inferGoal(_,Logic,Depth,Table,gafs(Logic,A),Agent,KB,ProofA ):-!,
	writeDebug(red,defering_to_gafs(Logic,A)),
	inferGoal(holds,Logic,Depth,[defering_to_gafs(Logic,A)|Table],A,Agent1,KB,ProofA ).

inferGoal(_,Logic,Depth,Table,delay(Logic,A),Agent,KB,ProofA ):-!,
	%(sigmaCache(completed,template(Logic,Template),_)),
	writeDebug(red,delay_to_gafs(Logic,A)),!,
	inferGoal(holds,Logic,Depth,[delay_to_gafs(Logic,A)|Table],A,Agent1,KB,ProofA ).
 
% ==========================================
% Double Negation (Call)
% ==========================================
inferGoal(not,Logic,Depth,Table,not(not(NewQuery)),Agent,KB,ProofB ):-
	writeDebug(not_not),
	functor(NewQuery,Predicate,_), !,
	inferGoal(Predicate,Logic,Depth,Table,Var,Agent,KB,ProofB ).

% ============================================================
% Invert Not
% ============================================================
inferGoal(not,Logic,Depth,Table,not(NewQuery),Agent,KB,Proof):-
	functor(NewQuery,Predicate,_),
	invert_logic(Logic,NewLogic),!,
	inferGoal(Predicate,NewLogic,Depth,Table,NewQuery,Agent,KB,Proof).
	



invert_logic(true,false).
invert_logic(false,true).

	
% =============================================
% Choose between infer_nonground_goal/infer_ground_goal
% =============================================
inferGoal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-  %true,
	ground(Goal),!,
	infer_ground_goal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P),!.

inferGoal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	term_to_atom(Goal,Template),!,
	infer_nonground_goal(Template,Predicate,Logic,Depth,Table,Goal,Agent,KB,P).

	
% =============================================
% Prove a Nonground Goal
% =============================================
infer_nonground_goal(Template,Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	sigmaCache(failed,nonground(Logic,Template,Goal),tried(Agent,KB)),!,
	writeDebug(fail_nonground_goal(Logic,Template,Goal)),!,
	fail.
	

infer_nonground_goal(Template,Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	sigmaCache(completed,template(Logic,Template),tried(Agent,KB)),!,
	writeDebug(using_complete(Template)), !,
	sigmaCache(found,nonground(Logic,Template,Goal),P),
	writeDebug(in_db(Logic,Goal,Agent,KB,P)).

		
infer_nonground_goal(Template,Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	retractAllProlog(sigmaCache(found,nonground(Logic,Template,_),_you )),fail.
	
defered_fact(Logic,Agent,KB,Goal):-
	fact_to_template(Goal,Template),
	defered(Logic,Template,Agent,KB,Goal).

defered(Logic,Template,Agent,KB,Goal):-
	not(not(sigmaCache(tabling,template(Logic,_,Goal),tried(Agent,KB)))).

infer_nonground_goal(Template,Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	once(asserta(sigmaCache(tabling,template(Logic,Template,Goal),tried(Agent,KB)))),fail.

infer_nonground_goal(Template,Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	prove_goal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P),
	record_complete(Logic,Template,Agent,KB,Goal,P).
	
record_complete(Logic,Template,Agent,KB,Goal,P):-sigmaCache(found,nonground(Logic,Template,Goal)),!.
record_complete(Logic,Template,Agent,KB,Goal,P):-asserta(sigmaCache(found,nonground(Logic,Template,Goal),P)).
	
infer_nonground_goal(Template,Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	retractall(sigmaCache(tabling,template(Logic,Template,Goal),tried(Agent,KB))),
	asserta(sigmaCache(completed,template(Logic,Template),tried(Agent,KB))),!.

% =============================================
% Prove a Ground Goal
% =============================================
infer_ground_goal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	sigmaCache(failed,ground(Logic,Goal),tried(Agent,KB)),!,
	writeDebug(fail_ground_goal(Logic,Goal)),!,
	fail.

infer_ground_goal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	sigmaCache(found,ground(Logic,Goal),P),!,
	writeDebug(in_db(Logic,Goal,Agent,KB,P)),!.

infer_ground_goal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	sigmaCache(found,nonground(Logic,Template,Goal),P),!,
	writeDebug(in_db(Logic,Goal,Agent,KB,P)),!.
	
infer_ground_goal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	once(prove_goal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P)),
	asserta(sigmaCache(found,ground(Logic,Goal),P)),!.

% Persist Fail Only Truth
infer_ground_goal(Predicate,true,Depth,Table,Goal,Agent,KB,P):-
	writeDebug(red,[tabled,failed,true,Goal,tried(Agent,KB)]),
	asserta(sigmaCache(failed,ground(true,Goal),tried(Agent,KB))),!,fail.



% ============================================================
% Write the Goal to debugger
% ============================================================
:-was_indexed(inferGoal(1,1,0,0,1,0,0,0)).


prove_goal(Predicate,Logic,Depth,Table,Var,Agent,KB,Proof):-
	once(writeDebug(prove_goal(Predicate,Logic,Var,Depth,Agent,KB,Table))),fail.


prove_goal(Predicate,Logic,Depth,Table,holds(domain, [P, N,T]),Agent,KB,Proof):-var(P),!,fail.

/*
prove_goal(Predicate,Logic,Depth,Table,holds(Predicate,[H|_]),Agent,KB,Proof):-%true,
		     H=='Entity',!,fail.
prove_goal(Predicate,Logic,Depth,Table,holds(Predicate,[_,H|_]),Agent,KB,Proof):-
		     H=='Entity',!,fail.
*/
%holds(part, ['Entity', 'SaudiArabia']))

%finite_goal('.',Logic,Depth,Table,[H|T],Agent,KB,Proof):-!,
%	infer_backchain(Logic,Depth,Table,[H|T],Agent,KB,Proof).

% ============================================================
% Invert Not
% ============================================================
prove_goal(not,true,Depth,Table,not(NewQuery),Agent,KB,Proof):-!,
	invert_logic(Logic,NewLogic),
	functor(NewQuery,Predicate,_),!,
	inferGoal(Predicate,NewLogic,Depth,Table,NewQuery,Agent,KB,Proof).

% ==============================================   
% Time space Limits
% ==============================================   

prove_goal(Predicate,Logic,Depth,Table,Var,Agent,KB,Proof):-
	(
	(getCputime(Now),cpuend(Then),Now>Then,writeDebug(timeout(Now>Then)))
	;
	(var(Var),once(writeDebug(vaR)))
	;catch((Depth<(-1),once(writeDebug(too_far))),_,true)
	),
	%asserta(noexistencials),
	!,fail.


%prove_goal(Predicate,Logic,Depth,Table,Var,Agent,KB,Proof):-trace,fail



	

% ==========================================
% Member INFERENCE OPTIMIZATION
% ==========================================
/*
finite_goal(member,Logic,Depth,Table,member( X,Y),Agent,KB,P ):-!,
	prove_goal_member(Logic,Depth,Table,X,Y,Agent,KB,P ).

% ==========================================
% Attribute INFERENCE OPTIMIZATION
% ==========================================

finite_goal(attribute,Logic,Depth,Table,attribute( X,Y),Agent,KB,P ):-!,
	prove_goal_attribute(Logic,Depth,Table,X,Y,Agent,KB,P ).
  */


% ==========================================
% Instance INFERENCE OPTIMIZATION
% ==========================================

prove_goal(_,Logic,Depth,Table,instance( X,Y),Agent,KB,P ):-
	prove_goal_instance(Logic,Depth,Table,X,Y,Agent,KB,P ).
	

% ================================================
% True: Check Database for Goal
% ================================================
prove_goal(Predicate,true,Depth,Table,Goal,Agent,KB,Proof):-
	inferSurfaceGuarded(Predicate,true,Goal,Agent,KB,Proof),
	writeDebug(green,found(Goal)).

% ================================================
% False: Check Database for Not Goal (Success)
% ================================================
prove_goal(Predicate,false,Depth,Table,Goal,Agent,KB,Proof):-
	inferSurfaceGuarded(Predicate,false,Goal,Agent,KB,Proof),
	writeDebug(green,found(not(Goal))).
	     


   
prove_goal(Predicate,_,Depth,Table,Goal,Agent,KB,Proof):-
	(disabled_backchain(Predicate),!,fail).

prove_goal(_,_,Depth,Table,holds(Predicate,_,_),Agent,KB,Proof):-
	(disabled_backchain(Predicate),!,fail).

prove_goal(Predicate,_,Depth,Table,Goal,Agent,KB,Proof):-
	atom(Predicate),
	disabled_backchain(Predicate),!,fail.
	
disabled_backchain(temporalPart).
disabled_backchain(part).
disabled_backchain(result).
disabled_backchain(equal).
disabled_backchain(copy).
disabled_backchain(manner).
disabled_backchain('piece'). 

disabled_backchain(A):-atom_codes(A,[115,117,98|_]). %sub*

prove_goal(temporalPart,true,Depth,Table,holds(temporalPart,Arg,Arg),Agent,KB,incode(holds(temporalPart,Arg,Arg),'sameness')).


%prove_goal(_,Logic,Depth,Table,subclass( 'Entity',Y),Agent,KB,P ):-!,fail.

prove_goal(holdsDuring,true,Depth,Table,holds(holdsDuring,TimePosition,Situation),Agent,KB, Proof):- 
	once((writeDebug(subgoal(true,Situation,Agent,KB)),
        once(query_compile((Situation),NewQuery,KB,Agent,UVars,Given)),
	writeDebug(pink,subgoal(NewQuery,Agent,KB)), 
	functor(NewQuery,Predicate,_))),
        inferGoal(Predicate,true,Depth,Table,NewQuery,Agent,KB,Proof).

% ==========================================
% Transition FINITE GOAL
% ==========================================

/*
prove_goal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	nobackchain_on(Predicate),!,
	writeDebug(red,nobackchain_on(PR)),fail.
*/

/*
prove_goal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
        not(not(memberchk(Goal,Table))),!,
	writeDebug(red,memberchk(Goal,Table)),fail.
*/
	
prove_goal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P):-
	instanciate_finate(Predicate,Logic,Depth,Table,Goal,Agent,KB),
	finite_goal(Predicate,Logic,Depth,Table,Goal,Agent,KB,P).
	%After chain completers term must be ground
	%ground(Goal).

instanciate_finate(Predicate,Logic,Depth,Table,VAR,Agent,KB):-ground(VAR),!.


%instanciate_finate(instance,Logic,Depth,Table,instance(E,C),Agent,KB):-!,
%	inferSurface(instance,true,instance(E,C),Agent,KB,_).
instanciate_finate(Predicate,Logic,Depth,Table,instance(E,C),Agent,KB):-!,
	inferInstanceTable(KB,C,'Class',_),not(C='Entity').
	%inferSurface_dc(instance,true,instance([C,'Class']),Agent,KB,Proof).

instanciate_finate(Predicate,Logic,Depth,Table,holds(holdsDuring,T,S),Agent,KB):-!,
	nonvar(T),nonvar(S).
		
	%%instance_all(KB,Predicate,Arg,SO). %,ground(Arg,SO).
instanciate_finate(Predicate,Logic,Depth,Table,holds(Predicate,Arg,SO),Agent,KB):-%trace,
	instance_all(KB,Predicate,[Arg,SO]). %,ground(Arg,SO).

instanciate_finate(Predicate,Logic,Depth,Table,holds(Predicate,Arg,S,O),Agent,KB):-
	instance_all(KB,Predicate,[Arg,S,O]). %,ground(Arg,SO).

% ==========================================
% PROVE FINITE GOAL
% ==========================================

finite_goal(Predicate,Logic,Depth,Table,Var,Agent,KB,Proof):-
	once(writeDebug(finite_goal(Predicate,Logic,Var,Depth,Agent,KB,Table))),fail.

/*
% ================================================
% True: Check Database for Not Goal (only ground)
% ================================================
finite_goal(Predicate,true,Depth,Table,Goal,Agent,KB,Proof):-
		inferSurfaceGuarded(Predicate,false,Goal,Agent,KB,P),!,writeDebug(countered(Goal)),!,fail.

% ================================================
% False: Check Database for Goal (if there is then fail)
% ================================================
finite_goal(Predicate,false,Depth,Table,Goal,Agent,KB,Proof):-
		inferSurfaceGuarded(Predicate,true,Goal,Agent,KB,P),!,writeDebug(encountered(Goal)),!,fail.
*/




% ==========================================
% "holdsDuring" INFERENCE
% http://www.google.com/search?q=cache:hRA54mmgLc8:www-formal.stanford.edu/guha/guha-thesis.pls+holdsDuring+PRolog&hl=en
% ==========================================

/*
% Break a holdsDuring on Argument Type Violation
finite_goal(holdsDuring,Logic,Depth,Table,holdsDuring(TimePosition,Situation),Agent,KB, Proof):-
	not(inferGoal(instance,true,Depth,Table,instance(TimePosition,'TimePosition'),Agent,KB, ITime)),!,fail.

% Break a holdsDuring on Free Situation
finite_goal(holdsDuring,Logic,Depth,Table,holdsDuring(TimePosition,false),Agent,KB, Proof):- !,fail.

% holdsDuring(TimePosition,and(Situation1,Situation2)) => and(holdsDuring(TimePosition,Situation1),holdsDuring(TimePosition,Situation))
*/
/*
finite_goal(holdsDuring,Logic,Depth,Table,holdsDuring(TimePosition,and(Situation1,Situation2)),Agent,KB, Proof):- 
	inferGoal(and,Logic,Depth,Table,
	and(holdsDuring(TimePosition,Situation1),holdsDuring(TimePosition,Situation)),
	Agent,KB, Proof).
*/
    
% Situation => holdDuring(AnyTime,Situation) 
	      
/*
infer_holdsDuring(Predicate,Logic,Depth,Table,TimePosition,Situation,Agent,KB,Proof ):-
	writeDebug(infer_holdsDuring(Predicate,Logic,holdsDuring,TimePosition,Situation)),fail.


infer_holdsDuring(Predicate,Logic,Depth,Table,TimePosition,Situation,Agent,KB,ITime * Proof * ConditionalProof ):-true,
	writeDebug(green,[holdsDuring,TimePosition,Logic,Situation]),
	client_rulebase(holdsDuring,Logic,holdsDuring(TimePosition,Situation), Agent,KB, Conditions, Proof, F),
	writeDebug(green,[Conditions=>holdsDuring,TimePosition,Logic,Situation]),
	functor(Situation,NewPredicate),
	infer_holdsDuring(NewPredicate,Logic,Depth,Table,TimePosition,Conditions,Agent,KB,ConditionalProof).
  */   
  

% ==========================================
% "entails" INFERENCE
% ==========================================


finite_goal(entails,true,Depth,Table,entails(Pre,Goal),Agent,KB, Proof):-!,
	((nonvar(Goal),functor(Goal,Predicate,_));nonvar(Pre)),
	writeDebug(green,entails(Pre,Goal)),!,
	infer_entails(Predicate,true,Depth,Table,Pre,Goal,Agent,KB,Proof ),
	writeDebug(green,succeed_infers(true,Pre,Goal)).


infer_entails(Predicate,Logic,Depth,Table,Pre,Goal,Agent,KB,Proof ):-
	writeDebug(infer_entails(Predicate,Logic,entails(Pre,Goal),Depth,Table,Agent,KB,Proof )),fail.
    
infer_entails(Predicate,true,Depth,Table,attribute(O,R),not(attribute(O,L)),Agent,KB,Proof):-
	inferGoal(contraryProperty,true,Depth,Table,contraryProperty(L,R),Agent,KB,Proof ).

infer_entails(Predicate,true,Depth,Table,Pre,Goal,Agent,KB, Proof ):-
	Pre==true,!,inferGoal(Predicate,true,Depth,Table,Goal,Agent,KB, Proof).

infer_entails(Predicate,true,Depth,Table,Pre,Goal,Agent,KB, Proof ):-
	Pre==false,!,fail.  %Absurdy

infer_entails(not,true,Depth,Table,Pre,not(Goal),Agent,KB, Proof ):-!,nonvar(Goal),
	functor(Goal,Predicate,_),
	infer_entails(Predicate,false,Depth,Table,Pre,Goal,Agent,KB, Proof ).

infer_entails(not,false,Depth,Table,Pre,not(Goal),Agent,KB, Proof ):-!,nonvar(Goal),
	functor(Goal,Predicate,_),
	infer_entails(Predicate,true,Depth,Table,Pre,Goal,Agent,KB, Proof ).

infer_entails(Predicate,Logic,Depth,Table,Reqs,Goal,Agent,KB, Proof ):-
	client_entailmentbase(Logic,Depth,Table,Reqs,Goal,Agent,KB, Proof ),
	not(memberchk(Reqs,Table)),
	writeDebug(silver,found_entails(Reqs,Logic,Goal)).
	
		
remove_conj(and(Pre,LeftOver),Pre,LeftOver).
remove_conj(and(LeftOver,Pre),Pre,LeftOver).
remove_conj(Pre,Pre,true).

              
/*
% ==========================================
% Range INFERENCE	(Transitive closure)
% ==========================================       
finite_goal(Predicate,true,Depth,Table,range( Function, SubClass),Agent,KB,(Proof1 * Proof2 * Proof3)):-
	nonvar(Function),
	inferTransitiveClosure_PartialOrderingRelation(KB,Agent,subrelation,Function,Super,Proof1),
	Function \= Super,
	inferGoal(Predicate,true,Depth,Table,range( Super, Class),Agent,KB,Proof2),
	%write(Class),
	inferTransitiveClosure_PartialOrderingRelation(KB,Agent,subclass,SubClass,Class,Proof3).
	
% =====================================================
% RANGE TRAVELS UP TO SUPERRELATION (If no assertions Found)
% =====================================================
finite_goal(Predicate,true,Depth,Table,range( Function, Class),Agent,KB,Proof1 * Proof2):-
	var(Function),nonvar(Class),
	inferGoal(Predicate,true,Depth,Table,range( Super, Class),Agent,KB,Proof2),
	inferTransitiveClosure_PartialOrderingRelation(KB,Agent,subrelation,Function,Super,Proof1).

*/
% ==========================================
% CONTENT INFERENCE   
% ==========================================

/*
finite_goal(Predicate,true,Depth,Table, subsumesContentInstance( S1, S2),Agent,KB,KB:TN:('=>'( S1, S2))^Vars):-
		inferSurfaceGuarded(Predicate,true,surface,(S1=>S2),Vars,KB,Agent,KB,TN,Author,Accept).
finite_goal(Predicate,true,Depth,Table, equivalentContentInstance( S1, S2),Agent,KB,KB:TN:('<=>'( S1, S2))^Vars):-
		inferSurfaceGuarded(Predicate,true,surface,(S1<=>S2),Vars,KB,Agent,KB,TN,Author,Accept).

finite_goal(Predicate,true,Depth,Table,assertedFormula(Form),Agent,KB,assertedFormula(Form)):-in_cache(Form,KB,Agent,KB,Proof).

finite_goal(Predicate,true,Depth,Table,assertedTermFormulas(Goal,Form),Agent,KB,assertedTermFormulas(Goal,Form)):-in_cache(Form,KB,Agent,KB,Proof),once(getConstants(atomic,Form,Lists,_,_)),memberchk(Goal,Lists).


finite_goal(Predicate,Depth,Table,'<=>'(L,St),Agent,KB,Proof):-!,
        writeDebug(goal('<=>'(L,St))),
	infer_backchain(Depth,Table,'<=>'(L,St),Agent,KB,Proof).

*/

% ===================================================
% Stop Crazy Existentials 
% ===================================================

/*
finite_goal(Predicate,Logic,Depth,Table,holds(Predicate,[Arg1,Arg2]),Agent,KB,Proof):-
	((nonvar(Arg1),functor(Arg1,_,2));(nonvar(Arg2),functor(Arg2,_,2))),
	!,writeDebug(red,unwind(holds(Predicate,Arg1,Arg2))),!,fail.	       
*/
% ==========================================
% Special Predicate INFERENCE
% ==========================================

/*
finite_goal(Predicate,Logic,Depth,Table,holds(Predicate,Arg1,Arg2),Agent,KB,Proof):-
	fail,
	inference_math(Depth,['RelationExtendedToQuantities'|Table],holds(Predicate,Arg1,Arg2),Agent,KB,Proof).
*/


% ===================================================
% No Backchain if Defered
% ===================================================
finite_goal(Predicate,Logic,Depth,Table,holds(Predicate,Arg1,Arg2),Agent,KB,Proof):-
		 memberchk(defering_to_gafs(Logic,Predicate),Table),
		 writeDebug(red,backchain_defered(Predicate)),!,fail.

% ===================================================
% No Backchain Ever if uncommented
% ===================================================
%finite_goal(Predicate,Logic,Depth,Table,holds(Predicate,Arg1,Arg2),Agent,KB,Proof):-!,fail.



/*
%Quick Negation check (finites are ground)  obnly for non constaints
finite_goal(Predicate,TF,Depth,Table,holds(Predicate,Arg1,Arg2),Agent,KB,Proof ):-
	not(constraint_functor(Predicate)),
	backchain_goal(Predicate,false,Depth,Table,holds(Predicate,Arg1,Arg2),Agent,KB,Proof),
	writeDebug(pink,refuted(holds(Predicate,Arg1,Arg2))),
	!,fail.

*/

% ===================================================
% Modus Ponens
% ===================================================
finite_goal(Predicate,true,Depth,Table,Fact,Agent,KB,Proof * Proof2 ):-
	writeDebug(calling_backchain(Fact,Table)),
	confirm_callable((Fact)),
	Depth2 is Depth -1,!,  %trace,
	client_rulebase_spec(Predicate,true,Fact, Agent,KB, Conditions, Proof, F,Type),
	confirm_rule(Predicate,true,Fact, Agent,KB, Conditions, Proof, F,Type,Depth,Table,NewTable,NewConds),
	inferGoal(holds,true,Depth2,[Fact|NewTable],NewConds,Agent,KB,Proof2),
	confirm_ground(Conditions).
	

%constraint_functor(X):-not(atom(X)),!.
constraint_functor(instance).
constraint_functor(domain).
constraint_functor(valence).
constraint_functor(subclass).
constraint_functor(subrelation).
constraint_functor(disjoint).
constraint_functor(Sub):-atom_codes(Sub,[115,117,98|_]).


nobackchain_on(X):-not(atom(X)),!.
nobackchain_on(subrelation).

nobackchain_on(domain).
nobackchain_on(domainSubclass).
nobackchain_on(subclass).
nobackchain_on(Sub):-atom_codes(Sub,[115,117,98|_]).

% ============================================================
% Modus Tollens
% ============================================================
finite_goal(Predicate,false,Depth,Table,Fact,Agent,KB,Proof * Proof2 ):-
	writeDebug(calling_backchain(not(Fact),Table)),
	confirm_callable(not(Fact)),
	Depth2 is Depth -1,!,
	client_rulebase_spec(Predicate,false,Fact, Agent,KB, Conditions, Proof, F,Type),
	confirm_rule(Predicate,false,Fact, Agent,KB, Conditions, Proof, F,Type,Depth,Table,NewTable,NewConds),
	inferGoal(holds,true,Depth2,[Fact|NewTable],NewConds,Agent,KB,Proof2),
	confirm_ground(Conditions).

/*

confirm_callable(not(holds(Predicate,Arg1,Arg2))):-
	not(constraint_functor(Predicate)),
	writeDebug(red,no_constraint_functor(holds(Predicate,Arg1,Arg2))),!,
	fail.
*/	
/*
confirm_callable(not(holds(Predicate,Arg1,Arg2))):-
	not(('zzskFn'(_,_)=Arg1)),
	not(('zzskFn'(_,_)=Arg2)),
	not(('zzfn'(_,_)=Arg1)),
	not(('zzfn'(_,_)=Arg2)),
	writeDebug(red,no_zzt(holds(Predicate,Arg1,Arg2))),!,
	fail.
*/	
confirm_callable(_):-!.



confirm_rule(Predicate,TF,I, Agent,KB, Conditions, Proof, F,Type,Depth,Table,NewTable,NewConds):-
	memberchk(ident(equal),Type),
	writeDebug(red,no_ident(I,Conditions)),
	!,fail.


confirm_rule(Predicate,_,_, Agent,KB, Conditions, Proof, F,Type,Depth,Table,NewTable,NewConds):-
	memberchk(on(_),Type),!,fail.


% Succeed
confirm_rule(Predicate,false,holds(Predicate,Arg1,Arg2), Agent,KB, Conditions, Proof, F,Type,Depth,Table,[defering_to_gafs(false,Predicate)|Table],Conditions):-
	writeDebug(purple,Type:(not(holds(Predicate,Arg1,Arg2)):modus_tollens:Conditions)),!.

% Succeed
confirm_rule(Predicate,true,holds(Predicate,Arg1,Arg2), Agent,KB, Conditions, Proof, F,Type,Depth,Table,[defering_to_gafs(true,Predicate)|Table],Conditions):-
	writeDebug(blue,Type:(holds(Predicate,Arg1,Arg2):modus_ponens:Conditions,Proof)),!.


confirm_rule(Predicate,L,G, Agent,KB, Conditions, Proof, F,Type,Depth,Table,NewTable,NewConds):-
	G=..[Predicate,Arg1,Arg2],!,
	confirm_rule(Predicate,L,holds(Predicate,Arg1,Arg2), Agent,KB, Conditions, Proof, F,Type,Depth,Table,NewTable,NewConds).
	

% Never gets here
confirm_rule(Predicate,false,holds(Predicate,Arg1,Arg2), Agent,KB, Conditions, Proof, F,Type,Depth,Table,NewTable,NewConds):-
	memberchk('zzskFn'(_),Type),
	writeDebug(red,no_skolems_on_modus_tollens(holds(Predicate,Arg1,Arg2),Conditions)),
	!,fail.

confirm_rule(Predicate,TF,holds(holds,Arg1,Arg2), Agent,KB, Conditions, Proof, F,Type,Depth,Table,NewTable,NewConds):-
	memberchk(ident(_),Type),
	writeDebug(red,no_ident(holds(Predicate,Arg1,Arg2),Conditions)),
	!,fail.

confirm_rule(Predicate,false,holds(Predicate,Arg1,Arg2), Agent,KB, Conditions, Proof, F,Type,Depth,Table,NewTable,NewConds):-
	memberchk('zzfn'(_),Type),
	writeDebug(red,no_functions_on_modus_tollens(holds(Predicate,Arg1,Arg2),Conditions)),
	!,fail.
	
confirm_rule(Predicate,TF,holds(Predicate,Arg1,Arg2), Agent,KB, and(equals(_,_),_), Proof, F,Type,Depth,Table,NewTable,NewConds):-
	writeDebug(red,no_functions_on_modus_tollens(holds(Predicate,Arg1,Arg2),Conditions)),
	!,fail.
	

confirm_rule(Predicate,TF,holds(Predicate,Arg1,Arg2), Agent,KB, Conditions, Proof, F,Type,Depth,Table,NewTable,NewConds):-
	memberchk('2nd'(_),Type),
	memberchk('zzskFn'(_,_),[Arg1,Arg2]),
	writeDebug(red,Type:(TF:holds(Predicate,Arg1,Arg2),Conditions)),
	!,fail.
	

confirm_rule(Predicate,false,holds(Predicate,Arg1,Arg2), Agent,KB, Conditions, Proof, F,Type,Depth,Table,[defering_to_gafs(false,Predicate)|Table],Conditions):-
	writeDebug(purple,Type:(not(holds(Predicate,Arg1,Arg2)):modus_tollens:Conditions)),!.

confirm_rule(Predicate,true,holds(Predicate,Arg1,Arg2), Agent,KB, Conditions, Proof, F,Type,Depth,Table,[defering_to_gafs(true,Predicate)|Table],Conditions):-
	writeDebug(blue,Type:(holds(Predicate,Arg1,Arg2):modus_ponens:Conditions,Proof)),!.


confirm_ground(subclass('Entity',B)):-!,fail.
confirm_ground(subclass(_,foobar)):-!,fail.
confirm_ground(subclass(A,B)):-!,
	inferTransitiveClosure_PartialOrderingRelation(KB,Agent,subclass,A,B,Proof1).
	
confirm_ground(and(A,B)):-!,
	confirm_ground(A),
	confirm_ground(B).
	
confirm_ground(Conditions):-ground(Conditions),
	writeDebug(green,confirm_ground(Conditions)).
confirm_ground(Conditions):-!,fail,
	writeDebug(pink,nonground(Conditions)),!,fail.



finite_goal(Predicate,Logic,Depth,Table,Call,Agent,KB,Proof):-
	writeDebug(blue,completed_table(Logic,Call)),!,fail.


client_rulebase_spec(Predicate,Logic,Post, _Agent,KB, Pre, Proof,Post ,Type):-
	ado_cache_rule(Predicate,Post, Pre, RuleDepth, Logic, KB, _AgentDisjoin, Proof,Type).


% Backchain



%reorderAnteceedants(_,Best,Best):-ground(Best),!.
       
not_in(Goal,Table):-not(member(Goal,Table)).

exact_memberchk(X,[Y]):-X==Y,!.
exact_memberchk(X,[_|Y]):-exact_memberchk(X,Y),!.

in_rule(Goal,Goal).
in_rule(Goal,and(A,B)):-!,
	(in_rule(Goal,A);in_rule(Goal,B)),!.

not_in_rule(Goal,Rule):-not(in_rule(Goal,Rule)).



	
			%client_rulebase(Predicate,Logic,holds(Predicate,ArgS), Agent,KB, ConditionsO, ProofO, F,A).
/*
client_rulebase_spec(Predicate,Logic,holds(Predicate,ArgS), _Agent,KB, ConditionsO, ProofO, _,TypeO):-
	ArgS=Arg,SO,
	findall(O,
		((
			client_rulebase(Predicate,Logic,holds(Predicate,ArgS), Agent,KB, Conditions, Proof, F,A),
			indiviguals(holds(Predicate,ArgS),Conditions,Proof,A,O)		
		)),Rules),!,
	format('RULES: ~q~n',[Rules]),
	iterate_rulesout(Logic,holds(Predicate,ArgS),Rules,ConditionsO, ProofO).
  */

iterate_rulesout(Logic,Goal,[],ConditionsO, ProofO):-!,fail.
iterate_rulesout(Logic,Goal,Rules,ConditionsO, ProofO):-
	member(rule(Mark,ConditionsOM,ProofO),Rules),
	once(make_markings(Logic,Goal,Mark,ConditionsOM,Rules,ConditionsO)).

indiviguals(Goal,Conditions,(surf(KB,Mark) * HOW),_,rule(Mark,Conditions,(surf(KB,Mark) * HOW))).
/*
indiviguals(Goal,Conditions,(surf(_,Mark) * _),i(Mark,N,Each)):-
	indiviguals0(Conditions,ALL,N),!,
	member(Each,ALL).
*/

				 /*
indiviguals0(blah,_,0):-!,fail.
indiviguals0(and(and(and(and(A,B),C),D),E),[A,B,C,D,E],5).
indiviguals0(and(and(and(A,B),C),D),[A,B,C,D],4).
indiviguals0(and(and(A,B),C),[A,B,C],3).
indiviguals0(and(A,B),[A,B],2).
indiviguals0(Each,[Each],1).
				   */
	


:-was_indexed(make_markings(0,0,0,1,0,0)).

make_markings(Logic,Goal,Mark,V,Rules,V):-var(V),!.
make_markings(Logic,Goal,Mark,and(A,B),Rules,and(AA,BB)):-!,
	make_markings(Logic,Goal,Mark,A,Rules,AA),
	make_markings(Logic,Goal,Mark,B,Rules,BB),!.
make_markings(Logic,Goal,Mark,or(A,B),Rules,or(AA,BB)):-!,
	make_markings(Logic,Goal,Mark,A,Rules,AA),
	make_markings(Logic,Goal,Mark,B,Rules,BB),!.
make_markings(Logic,Goal,Mark,exists(A,B),Rules,exists(AA,BB)):-!,
	make_markings(Logic,Goal,Mark,A,Rules,AA),
	make_markings(Logic,Goal,Mark,B,Rules,BB),!.
make_markings(Logic,Goal,Mark,forall(A,B),Rules,forall(AA,BB)):-!,
	make_markings(Logic,Goal,Mark,A,Rules,AA),
	make_markings(Logic,Goal,Mark,B,Rules,BB),!.
make_markings(Logic,Goal,Mark,entails(A,B),Rules,entails(AA,BB)):-!,
	make_markings(Logic,Goal,Mark,A,Rules,AA),
	make_markings(Logic,Goal,Mark,B,Rules,BB),!.
make_markings(Logic,Goal,Mark,=>(A,B),Rules,=>(AA,BB)):-!,
	make_markings(Logic,Goal,Mark,A,Rules,AA),
	make_markings(Logic,Goal,Mark,B,Rules,BB),!.
make_markings(Logic,Goal,Mark,<=>(A,B),Rules,<=>(AA,BB)):-!,
	make_markings(Logic,Goal,Mark,A,Rules,AA),
	make_markings(Logic,Goal,Mark,B,Rules,BB),!.
make_markings(Logic,Goal,Mark,Ante,Rules,skip):-Goal==Ante,!.
make_markings(Logic,Goal,Mark,Ante,Rules,gafs(Logic,Ante)):-
	not(not(Goal=Ante)),!.
make_markings(Logic,Goal,Mark,Ante,Rules,delay(Logic,Ante)):-
	not(not(defered_fact(_,Agent,KB,Ante))),!.
make_markings(Logic,Goal,Mark,ConditionsO,Rules,ConditionsO):-!.


/*
iterate_rulesout(Logic,[partof(ConditionsO, ProofO)|Rest],partof(ConditionsO, ProofO)).
iterate_rulesout(Logic,[_|Rest],ConditionsO, ProofO):-iterate_rulesout(Logic,Rest,ConditionsO, ProofO).

	member(,Rules),not(ConditionsO=i(_)),
	member(partof(i(Each),ProofO),Rules),
	ispartof(Each,)
*/	
	




/*

% ===================================================
% ReflexiveRelation True
% ===================================================
prove_holds(true,Depth,Flags,Predicate,Arg1,[Arg2],Table,Agent,KB,P):-
	not_in('ReflexiveRelation',Table),
	memberchk('ReflexiveRelation',Flags),
	Arg1==Arg2.


% ===================================================
% ReflexiveRelation false
% ===================================================
prove_holds(false,Depth,Flags,Predicate,Arg1,[Arg2],Table,Agent,KB,P):-
	not_in('ReflexiveRelation',Table),
	memberchk('ReflexiveRelation',Flags),
	Arg1==Arg2,!,fail.


% ===================================================
% SymmetricRelation (True/False)
% ===================================================
prove_holds(true,Depth,Flags,Predicate,Arg1,[Arg2],Table,Agent,KB, P * Proof):-
	not_in('SymmetricRelation',Table),
	memberchk('SymmetricRelation',Flags),
	inferGoal(Predicate,true,Depth,['SymmetricRelation'|Table],holds(InvPredicate,[Arg2,Arg1]),Agent,KB,Proof).

	
% ===================================================
% PartialOrderingRelation
% ===================================================
prove_holds(true,Depth,Flags,Predicate,Sub,[Super],Table,Agent,KB,P * Proof):-
	memberchk('PartialOrderingRelation',Flags),
	not(memberchk('TotalOrderingRelation',Flags)),
	inferTransitiveClosure_PartialOrderingRelation(KB,Agent,Predicate,Sub,Super,Proof),sendNote(Predicate).
	*/
/*
% ===================================================
% TotalOrderingRelation
% ===================================================
prove_holds(true,Depth,Flags,Predicate,Sub,[Super],Table,Agent,KB, P * Proof):-
	memberchk('TotalOrderingRelation',Flags),
	inferTransitiveClosure_TotalOrderingRelation(KB,Agent,Predicate,Sub,Super,Proof),sendNote(Predicate).
*/	
% ===================================================
% TransitiveRelation
% ===================================================
				  /*
prove_holds(true,Depth,Flags,Predicate,Sub,[Super],Table,Agent,KB, P * Proof):-
	memberchk('TransitiveRelation',Flags),
	not(memberchk('PartialOrderingRelation',Flags)),
	inferTransitiveClosure_TransitiveRelation(KB,Agent,Predicate,Sub,Super,Proof).	
				       */
% ===================================================
% RelationExtendedToQuantities 
% ===================================================

/*
prove_holds(false,Depth,Flags,Predicate,Sub,[Super],Table,Agent,KB, Proof):-
	memberchk('RelationExtendedToQuantities',Flags),
        ((
	inferGoal(instance,true,2,[prove_holds,inverse(Predicate)|Table],instance(Sub,'Quantity'),Agent,KB,_)
	;
	inferGoal(instance,true,2,[prove_holds,inverse(Predicate)|Table],instance(Super, 'Quantity'),Agent,KB,_)
	)),
	
	((	inference_math(Depth,['RelationExtendedToQuantities'|Table],holds(Predicate,[Sub|Super]),Agent,KB,Proof),!,fail)
	;
	Proof=incode(not(Head),'RelationExtendedToQuantities')).
*/   
/*
% ===================================================
% IrreflexiveRelation False
% ===================================================
prove_holds(false,Depth,Flags,Predicate,Arg1,[Arg2],Table,Agent,KB,P * Proof * bullet(not(holds(Predicate,[Arg1,Arg2])))):-
	not_in('IrreflexiveRelation',Table),
	memberchk('IrreflexiveRelation',Flags),
	Arg1==Arg2,
	Proof=sfindi((instance(Predicate, 'IrreflexiveRelation')=>
		forall(Arg1, not holds(Predicate, Arg1, Arg2)))).
*/

/*
% ===================================================
% Inverse
% ===================================================
prove_holds(Logic,Depth,Flags,Predicate,Arg1,[Arg2],Table,Agent,KB,P * Proof):-
	%(memberchk('BinaryRelation',Flags);memberchk('BinaryPredicate',Flags)),
	%not(memberchk('TransitiveRelation',Flags)),
	not_in(inverse(Predicate),Table),
	((
	inferGoal(inverse,true,2,[prove_holds,inverse(Predicate)|Table],inverse(InvPredicate,Predicate ),Agent,KB,P)
	;
	inferGoal(inverse,true,2,[prove_holds,inverse(Predicate)|Table],inverse(Predicate, InvPredicate),Agent,KB,P)
	)),
	inferGoal(InvPredicate,Logic,Depth,[inverse(Predicate),inverse(InvPredicate)|Table],holds(InvPredicate,[Arg2,Arg1]),Agent,KB,Proof).
		       */

/*
% ===================================================
% True subrelation Child
% ===================================================
prove_holds(true,Depth,Flags,Predicate,Arg1,ArgS,Table,Agent,KB,P * Proof):- 
	not_in(subrelation(Predicate),Table),
	inferSurfaceGuarded(subrelation,true,subrelation(  Child ,Predicate  ),Agent,KB,P),
	Child \==  Predicate,
	writeDebug(Child \==  Predicate),
	inferGoal(Child,true,Depth,[subrelation(Predicate)|Table],holds(Child,Arg1,Arg2),Agent,KB,Proof).

% ===================================================
% False subrelation Parent
% ===================================================
prove_holds(false,Depth,Flags,Predicate,Arg1,ArgS,Table,Agent,KB,P * Proof):- 
	not_in(subrelation(Predicate),Table),
	inferSurfaceGuarded(subrelation,true,subrelation(  Predicate, Child  ),Agent,KB,P),
	Child \==  Predicate,
	writeDebug(Child \==  Predicate),
	inferGoal(Child,false,Depth,[subrelation(Predicate)|Table],holds(Child,Arg1,Arg2),Agent,KB,Proof).	
*/
/*
% ===================================================
% Temporal Inference
% ===================================================
prove_holds(Logic,Depth,Flags,Predicate,Arg1,[Arg2|ArgS],Table,Head,Agent,KB,P * Proof):-
	fail, 
	not_in(domain,Table),
	(inferGoal(Predicate,true,2,[prove_holds|Table],domain( Predicate, 1 , 'Formula' ),Agent,KB,P),
	getNegationForm(Arg1,NNF),
	writeDebug(getNegationForm(Arg1,NNF)),
	Arg1 \= NNF,
	once((Proto_call=..[Predicate,NNF,Arg2|ArgS])),
	inferGoal(Predicate,Logic,Depth,Flags,[domain|Table],Proto_call,Agent,KB,Proof))
	;
	(inferGoal(Predicate,true,2,[prove_holds|Table],domain( Predicate, 2 , 'Formula' ),Agent,KB,P),
	getNegationForm(Arg2,NNF),
	writeDebug(getNegationForm(Arg2,NNF)),
	Arg2 \= NNF,
	once((Proto_call=..[Predicate,Arg1,NNF|ArgS])),
	inferGoal(Predicate,Logic,Depth,Flags,[domain|Table],Proto_call,Agent,KB,Proof)).
*/
 

/*

prove_goal_rtype(holdsDuring,true,Depth,Flags,Table,holdsDuring(TimePosition,Situation),Agent,KB, Proof):-
	((nonvar(Situation),functor(Situation,Predicate,_));nonvar(TimePosition)), true,
	writeDebug(green,holdsDuring(TimePosition,Situation)),
	once(pos_neg(Situation,NewFact,Logic)),
	infer_holdsDuring(Predicate,Logic,Depth,Flags,Table,TimePosition,NewFact,Agent,KB,Proof ),
	writeDebug(green,succeed_holdsDuring(TimePosition,Situation)).


*/

% ==========================================================
% Not Subclass (Specialization)
% ==========================================================
/*
prove_goal_rtype(Predicate,false,Depth,Flags,Table,subclass(Sub,SubSuper),Agent,KB,Proof * Proof2):-
	Depth2 is Depth-1,
	inferGoal(Predicate,true,Depth2,[subclass(Sub,Super)|Table],disjoint(Sub,Super), Agent,KB,Proof),
	inferGoal(Predicate,true,Depth2,[subclass(SubSub,Super),disjoint(Sub,Super)|Table],subclass(SubSuper,Super),Agent,KB,Proof2).
*/  
% ==========================================================
% Is Disjoint
% ==========================================================
/*
prove_goal_rtype(Predicate,Logic,Depth,Flags,Table,disjoint(A,B),Agent,KB,Proof):-
	infer_disjoint(Logic,Depth,Flags,Table,A,B,Agent,KB,Proof).
*/       
/*
infer_disjoint(true,Depth,Flags,Table,A,B,Agent,KB,Proof):-
	infer_disjoint(true,Depth,Flags,Table,A,B,Agent,KB,Proof):-
	prove_holds(true,Depth,Flags,Predicate,Arg1,[Arg2],Table,Head,Agent,KB,P * Proof):-
 */       

% ==========================================================
% Single Value (Specialization)
% ==========================================================	

% TODO

% ====================================================	   
% make_all_subclasses(Agent,KB)
% ====================================================
/*

References:

Anderson, A. and Belnap, N., Entailment: The Logic of Relevance and Necessity, Princeton: Princeton University Press vol. 1 (1975), vol. 2 (1992) 
Barcan, R., "A Functional Calculus of First Order Based on Strict Implication," Journal of Symbolic Logic, 11 (1946): 1-16 
Bencivenga, E., "Free Logics," in Gabbay, D., and Guenthner, F. (eds.) Handbook of Philosophical Logic, Dordrecht: D. Reidel (1986): 3.6 
Bonevac, D., Deduction, Palo Alto, California: Mayfield Publishing Company (1987): Part II 
Boolos, G., The Logic of Provability, Cambridge, England: Cambridge University Press (1993) 
Bull, R. and Segerberg, Krister, "Basic Modal Logic," in Gabbay, D., and Guenthner, F. (eds.) Handbook of Philosophical Logic, Dordrecht: D. Reidel (1984): 2.1 
Carnap, R., Meaning and Necessity, Chicago: U. Chicago Press, 1947 
Chellas, Brian, Modal Logic: An Introduction, Cambridge, England: Cambridge University Press (1980) 
Cresswell, M. J., "Incompleteness and the Barcan formula", Journal of Philosophical Logic, 24 (1995): 379-403. 
Cresswell, M. J., "In Defence of the Barcan Formula," Logique et Analyse, 135-136 (1991): 271-282 
Fitting, M. and Mendelsohn, R., First Order Modal Logic, Dordrecht: Kluwer, (1998) 
Gabbay, D., Investigations in Modal and Tense Logics, Dordrecht: D. Reidel (1976) 
Gabbay, D., Temporal Logic: Mathematical Foundations and Computational Aspects, New York: Oxford University Press (1994) 
Garson, James, "Quantification in Modal Logic," in Gabbay, D., and Guenthner, F. (eds.) Handbook of Philosophical Logic, Dordrecht: D. Reidel (1984): 2.5 
Hintikka, J., Knowledge and Belief: An Introduction to the Logic of the Two Notions, Ithaca, N. Y.: Cornell University Press (1962) 
Hilpinen, R., Deontic Logic: Introductory and Systematic Readings, Dordrecht: D. Reidel (1971) 
Hughes, G. and Cresswell, M., An Introduction to Modal Logic, London: Methuen (1968) 
Hughes, G. and Cresswell, M., A Companion to Modal Logic, London: Methuen (1984) 
Hughes, G. and Cresswell, M., A New Introduction to Modal Logic, London: Routledge (1996) 
Kripke, Saul, "Semantical Considerations on Modal Logic," Acta Philosophica Fennica, 16, (1963): 83-94 
Konyndik, K., Introductory Modal Logic, Notre Dame: University of Notre Dame Press (1986) 
Kvart, I., A Theory of Counterfactuals, Indianapolis: Hackett Publishing Company (1986) 
Lemmon, E. and Scott, D., An Introduction to Modal Logic, Oxford: Blackwell (1977) 
Lewis, C.I. and Langford, C. H., Symbolic Logic, New York: Dover Publications, 1959 (1932) 
Lewis, D., Counterfactuals, Cambridge, Massachusetts: Harvard University Press (1973) 
Linsky, B. and Zalta, E., "In Defense of the Simplest Quantified Modal Logic," Philosophical Perspectives, 8, (Logic and Language), (1994): 431-458 
Prior, A. N., Time and Modality, Oxford: Clarendon Press (1957) 
Prior, A. N., Past, Present and Future, Oxford: Clarendon Press (1967) 
Quine, W. V. O., "Reference and Modality", in From a Logical Point of View, Cambridge, MA: Harvard University Press, 1953: 139-159 
Rescher, N, and Urquhart, A., Temporal Logic, New York: Springer Verlag (1971) 
Sahlqvist, H., "Completeness and Correspondence in First and Second Order Semantics for Modal Logic," in Kanger, S. (ed.) Proceedings of the Third Scandanavian Logic Symposium, Amsterdam: North Holland (1975): 110-143 
Van Benthem, J. F., The Logic of Time, Dordrecht: D. Reidel (1982) 
Zeman, J., Modal Logic, The Lewis-Modal Systems, Oxford: Oxford University Press (1973) 

*/


