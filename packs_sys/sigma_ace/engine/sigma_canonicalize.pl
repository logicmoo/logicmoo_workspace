/*
Rule Matrix Examples:


Example:

 (=>
          (instance ?OBJ Object) 
          (exists
             (?TIME1 ?TIME2) 
             (and
                (instance ?TIME1 TimePoint) 
                (instance ?TIME2 TimePoint) 
                (before ?TIME1 ?TIME2) 
                (forall
                   (?TIME) 
                   (=>
                      (and
                         (beforeOrEqual ?TIME1 ?TIME) 
                         (beforeOrEqual ?TIME ?TIME2) ) 
                      (time ?OBJ ?TIME) ) ) ) ) )
		      

  (=>
          (instance ?FUNCTION UnaryConstantFunctionQuantity) 
          (and
             (domain ?FUNCTION 1 ConstantQuantity) 
             (range ?FUNCTION ConstantQuantity) ) )
	     


 (=>
          (and
             (domain ?REL1 ?NUMBER ?CLASS1) 
             (domain ?REL2 ?NUMBER ?CLASS2) 
             (disjoint ?CLASS1 ?CLASS2) ) 
          (disjointRelation ?REL1 ?REL2) )


 (=>
          (and
             (subrelation ?PRED1 ?PRED2) 
             (domain ?PRED2 ?NUMBER ?CLASS2) 
             (domain ?PRED1 ?NUMBER ?CLASS1) ) 
          (subclass ?CLASS1 ?CLASS2) )

Items:
 
	Varible clasification:
	
	Term classes
	
	Term:  HV(n) SV(n) DV(n)
	
	a(H1,H2):-
		v(H1,v(S1)),v(H2,v(S2,v(DV,_))),
		b(H1,S1),
		c(H2,S1),
		d(S2,D1),
		e(D2).


	a(H1,H2):-
		b(H1,S1),
		c(H2,S1),
		d(S2,D1),
		e(D2).
		
	H1-1
	H2-1
	S1-2
	D1-3
	D2-Infinate
	
	   cost ordered
	(H1,H2),(S1),D1
	$2(	
	
	----------
	  |	  |
	H1-S1-H2
	      |
	     D1	   D2
	
	
	headvar plus a variables shared with another headvar

	headvar plus a variables shared with another headvar
	
	

	HeadSlots
	
	Universal in term with no head getPrologVars  

 
*/

:-include('sigma_header.pl').


isSigmaReadyForEdits:-isKBCompilerThread(KB,Progress),!,write_ln(false).
isSigmaReadyForEdits:-write_ln(true).


isUncanonicalized(KB):-fail.	%TODO
isSourceNewerThenImage(KB):-fail.	%TODO
isKnowledgeBaseLoaded(KB,Ctx):-sigmaCache(_, _, _,_,KB,Ctx, _, _, _),!.
isKbUntransfered(KB):-not(isKnowledgeBaseLoaded(KB,_)).
isKbLoading(KB,Status):-isSigmaThread(ID,loadKBfromSource(KB,ToplevelContext)),!.

isKBCompilerThread(KB,LastPercent):-isSigmaThread(ID,canonicalizeSigmaKBHTML(KB,ToplevelContext)),!,
			flag('$last_written_percent',LastPercent,LastPercent).

isKBCurrentlyInUse(KB,unloaded):-nonvar(KB),isKbUntransfered(KB),!,fail.
isKBCurrentlyInUse(KB,canonicalizing(Progress)):-isKBCompilerThread(KB,Progress),!.
isKBCurrentlyInUse(KB,loading(Status)):-isKbLoading(KB,Status),!.
isKBCurrentlyInUse(KB,_):-!,fail.

invokeKBCompilerThread(KB):-!.
invokeKBCompilerThread(KB):-not(isUncanonicalized(KB)),!.
invokeKBCompilerThread(KB):-
	sigmaThreadCreate(canonicalizeSigmaKBHTML(KB,ToplevelContext),
	canonicalizeSigmaKBHTML(KB,ToplevelContext),ID,[detatched(true)]),!.

% =================================================
% Divide Fact and Rules	(Mine logic and create proof node)
% =================================================
testrcan:-recanonicalizeSigmaKB('Merge').

setOperationLock(Obj,Locker):-!.
setOperationUnlock(Obj,Locker):-!.



expireTN(KB,TN):-
	logOnFailureIgnore(retractall(sigmaCache(Literal,_,KB,Ctx,TN))),  %Facts
	logOnFailureIgnore(retractall(sigmaCache(Literal,AnteLiteral,_,KB,Ctx,TN))).

changeSurfaceStatusX(KB,TN,Before,After):-
	expireTN(KB,TN),!,
	changeSurfaceStatus(KB,TN,Before,After).

changeSurfaceStatus(KB,TN,Before,After):-
	retract(sigmaCache(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,Before)),
	assertz(sigmaCache(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,After)),fail.
changeSurfaceStatus(KB,TN,Before,After):-!.
		


canonicalizeSigmaKBHTML(KB):-!.
canonicalizeSigmaKBHTML(KB):-
	tell('errors.html'),
	saveSigmaCache,
	logOnFailure(canonicalizeSigmaKB(KB)),
	saveSigmaCache,
	told.
canonicalizeSigmaKBHTML(KB):-!.


:-dynamic(canonicalizerWarnings(KB,_,_)).

clearCanonicalizerWarnings(KB):-retractall(canonicalizerWarnings(KB,_,_)),!.

sendCanonicalizerWarning(Warning,Data,Surface,Rule,CLID,Flags,KRVars,KB,Ctx,TN,Anontate,Matrix,
	[canonicalizerWarning(Warning,Data):-true]):-
       % writeq(Warning:Data),nl,
       not(not(canonicalizerWarnings(KB,Warning,Data)))  -> ifInteractive(write(','));
	(assertz(canonicalizerWarnings(KB,Warning,Data)),
	ifInteractive(writeObject(nv([nl,Warning,nl,writeq(Data),nl,Surface,nl,nl]),KRVars))),!.



rct:-canonicalizeSigmaKBReal('Merge').

canonicalizeSigmaKB(KB):-!.
canonicalizeSigmaKBReal(KB):-
	clearCanonicalizerWarnings(KB),
	writeDebug(starting(canonicalizeSigmaKB(KB))),
	flag(proof_id,_,1),
	recanonicalizeTN(KB,TN),
	writeDebug(done(canonicalizeSigmaKB(KB))),!.


recanonicalizeTN(KB,TN):-
	(retractall(sigmaCache(Literal,_,KB,Ctx,TN))),  %Facts
	(retractall(sigmaCache(Literal,AnteLiteral,_,KB,Ctx,TN))),	 %Rules 
	fail.
	
recanonicalizeTN(KB,TN):-
	flag('$sofar',_,1),
	countAssertions(sigmaCache(_,_,_,_,KB,Ctx,TN,_,_),Total),
	ifInteractive((writeFmt('\nCompiling ~w surface clauses in ~w\n',[Total,KB]),writePercentAndTimeReset)),
	ignore((sigmaCache(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,Result),  
	once((
	%	numbervars((Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,Result),'$VAR',0,_),!,
		canonicalizeClause(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,Result,AssertList),
		assertAll(AssertList),
	        ifInteractive((
			flag('$sofar',SoFar,SoFar+1),
			writePercentAndTime('\n ~1f% complete.  Estimated cpu "priority" seconds remaining: ~1f\n ',SoFar,Total,8)
			))
		)),fail)),
	ifInteractive((
		flag(proof_id,ProofID,ProofID),
		countAssertions(sigmaCache(_,_,KB,Ctx,TN),Facts),
		countAssertions(sigmaCache(_,_,_,KB,Ctx,TN),Rules),
		writeFmt('\n100% complete.  Examined ~w internal lemma structures for ~w. Facts:~w  Rules:~w  \n',[ProofID,KB,Facts,Rules]))),!.
	
writePercentAndTimeReset:-
	notrace((flag('$last_written_percent',_,1),getCputime(Now),!,flag('$cputime_percent_start',_,Now))).
	
writePercentAndTime(Format,SoFar,Total,Steps):-
        notrace((
		flag('$last_written_percent',LastPercent,LastPercent),
	        NewPercent is (SoFar/Total * 100),
		NextPercent is LastPercent + Steps, 
		writePercentAndTime(Format,SoFar,Total,NextPercent,NewPercent))).
		
writePercentAndTime(Format,SoFar,Total,NextPercent,NewPercent):- NextPercent > NewPercent,!,write(.).
writePercentAndTime(Format,SoFar,Total,NextPercent,NewPercent):-
				flag('$last_written_percent',_,NewPercent),
				getCputime(Now),flag('$cputime_percent_start',Start,Start),
				TimeLeft is (Total-SoFar)*2*((Now-Start)/SoFar),
				writeFmt(Format,[NewPercent,TimeLeft]),!.
				



ifInteractive(G):-ignore((thread_self(Id),(number(Id);(G,flush_output)),!)).

                                    
recanonicalizeTN(KB,TN):-!.
	
inferLegalToCan(Surface,_):-
	once(getConstants(atomic,Surface,Consts,_,_)),
	member(Word,Consts),
	inferDisabledSurfaceConst(Word),!,fail.
inferLegalToCan(Surface,_):-!.
	
:-dynamic(inferDisabledSurfaceConst/1).

%inferDisabledSurfaceConst('graphPart'). inferDisabledSurfaceConst('NormativeAttribute').

% In Sigma DB we have 3 types of Vars
% var(Var)
% Var='$VAR'(_).
% Var=..['$existential',V|Body],var(V)

% isSlot(X):-var(Var).


% ================================================================
% Convert Forms to AssertionList
% ================================================================
canonicalizeClause(Surface,true,Flags,Vars,KB,Ctx,TN,Author,Result,[]):-!.
canonicalizeClause(Surface,not(true),Flags,Vars,KB,Ctx,TN,Author,Result,[]):-!.
canonicalizeClause(Surface,Var,Flags,Vars,KB,Ctx,TN,Author,Result,[]):-isSlot(Var),!.

% KB |= CAN1 & CAN2
canonicalizeClause(Surface,and(CAN1,CAN2),Flags,Vars,KB,Ctx,TN,Author,Result,AssertionList):-!,
	(canonicalizeClause(Surface,CAN1,Flags,Vars,KB,Ctx,TN,Author,Result,AssertList1)),
	(canonicalizeClause(Surface,CAN2,Flags,Vars,KB,Ctx,TN,Author,Result,AssertList2)),!,
	append(AssertList1,AssertList2,AssertionList).     

canonicalizeClause(Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,Result,AssertList):-
	unnumbervars((Surface,CAN,Flags,Vars,KB,Ctx,TN,Author,Result),
			   (USurface,UCAN,UFlags,UVars,UKB,UCtx,UTN,UAuthor,UResult)),
	canonicalizeRule(USurface,UCAN,UFlags,UVars,UKB,UCtx,UTN,UAuthor,UResult,AssertList),!.

      
% =============================================================

ruleToPrologHeadBodyList(entails(Ante,Cons),Cons,Ante):-!.	
ruleToPrologHeadBodyList(Cons,Cons,true).      
	
% =============================================================
	
canonicalizeRule(Surface,Rule,Flags,KRVars,KB,Ctx,TN,Author,Result,Assertions):-!,
	ruleToPrologHeadBodyList(Rule,Cons,Ante),	
	conjunctsToList(Ante,NewAnteLA),!,
	%removeCompilables(Cons,NewAnteLA,NewAnteLA,NewAnteRM),!,
	removeCompilables(Cons,NewAnteLA,NewAnteLA,NewAnteR),!,
	canonicalizeLemme(Surface,Rule,Cons,Ante,NewAnteR,Flags,KRVars,KB,Ctx,TN,Author,Result,Assertions),!.


% =============================================================

deNeg(not Item,Item):-!.  deNeg(Item,Item).

% =============================================================


canonicalizeLemme(Surface,Rule,Cons,Ante,NewAnteR,Flags,KRVars,KB,Ctx,TN,Author,Result,[]):-
	member(Item,NewAnteR), deNeg(Item,NItem), nonvar(NItem),
	functor(NItem,F,_),(memberchk(F,['$taut',query])),
	ifInteractive(writeObject(implied(Rule),KRVars)),!.


% =============================================================

canonicalizeLemme(Surface,Rule,Cons,Ante,NewAnteR,Flags,KRVars,KB,Ctx,TN,Author,Result,Assertions):-
	%ifInteractive((NewAnteR \== NewAnteLA, (write('\n':NewAnteLA:'\n':NewAnteR),nl,nl,trace))),
	getPrologVars(Cons,HeadSlots,_,_),
	adjustSkolemOrder(HeadSlots,NewAnteR,NewAnteL),!,
	nonWrappedCan(HardCoded),
	convertToHolds([holds|HardCoded],Cons,EntailedHead),!,
	convertBodyToHolds(NewAnteL,ConditionalBody),!,
	selectSign(EntailedHead,Sign),!,
	copy_term(EntailedHead,USeed),
	convertNegations(lit,USeed,ProtoType,_),
	numbervars(ProtoType,'$',0,_),
	selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,EntailedHead,ConditionalBody,Matrix),!,
	flag(clause_id,CLID,CLID+1),
	setSigmaOption(putAttributeStructures,Surface:Rule:CLID:Flags:KRVars:KB:Ctx:TN),!,
	catch(putAttributeStructures(KB,Ctx,Flags,Matrix,EmbededArgs),
		sigmaException(argDomains,Type,Details,DebugOnError),
		(set_prolog_flag(debug_on_error, DebugOnError),
		sendCanonicalizerWarning(Type,Details,Surface,Rule,CLID,Flags,KRVars,
								KB,Ctx,TN,Ctx,Matrix,EmbededArgs))),!,
	convertNegations(lit,EmbededArgs,LiteralMatrix,_),
	createFunctionalClauses(Rule:KRVars,LiteralMatrix,Clauses),
	mergeClauses(ProtoType,Clauses,Rule,CLID,Flags,KRVars,KB,Ctx,TN,Assertions),!.


% =============================================================

adjustSkolemOrder(HeadSlots,[],[]):-!.
adjustSkolemOrder(HeadSlots,['$existential'(Var,Name,Form)|Rest],[VT|RestPlus]):-
	once((getPrologVars(Form,FormVars,_,_),set_partition(FormVars,HeadSlots,_,_,MustBeGround))),
	MustBeGround=[_|_], VT=..['$groundVars'|MustBeGround],!,
	adjustSkolemOrder(HeadSlots,Rest,RestS),
	append(RestS,['$existential'(Var,Name,Form)],RestPlus),!.
adjustSkolemOrder(HeadSlots,Other,Other):-!.

% =============================================================

mergeClauses(ProtoType,[],KR,CLID,Flags,KRVars,KB,Ctx,TN,Assertions).
mergeClauses(ProtoType,[Matrix|Rest],KR,CLID,Flags,KRVars,KB,Ctx,TN,Assertions):-!,
	getAssertMatrix(ProtoType,Matrix,KR,CLID,KRVars,KB,Ctx,TN,HL),!,
	mergeClauses(ProtoType,Rest,KR,CLID,Flags,KRVars,KB,Ctx,TN,SubAssertions),
	append(HL,SubAssertions,Assertions),!.

% =============================================================

getAssertMatrix(ProtoType,(C:-true),KR,CLID,KRVars,KB,Ctx,TN,[sigmaCache(C,ProofID:KRVars:KR,KB,Ctx,TN)]):-
	flag(proof_id,ProofID,ProofID+1).

getAssertMatrix(ProtoType,(C:-A),KR,CLID,KRVars,KB,Ctx,TN,[sigmaCache(C,A,ProofID:KRVars:KR,KB,Ctx,TN)]):-
	flag(proof_id,ProofID,ProofID+1).
getAssertMatrix(ProtoType,C,KR,CLID,KRVars,KB,Ctx,TN,[sigmaCache(C,ProofID:KRVars:KR,KB,Ctx,TN)]):-
	flag(proof_id,ProofID,ProofID+1).
		 
% =============================================================

createFunctionalClauses(Rule,[],[]).
createFunctionalClauses(Rule,[C:-true|More],[C|Clauses]):-!,
	createFunctionalClauses(Rule,More,Clauses).
createFunctionalClauses(Rule,[C:-A|More],Clauses):-!,
	createFunctionalClauses(Rule,C:-A,SomeClauses),!,
	createFunctionalClauses(Rule,More,MoreClauses),
	append(SomeClauses,MoreClauses,Clauses),!.
createFunctionalClauses(Rule,[C|More],Clauses):-!,
	createFunctionalClauses(Rule,C,SomeClauses),!,
	createFunctionalClauses(Rule,More,MoreClauses),
	append(SomeClauses,MoreClauses,Clauses),!.

/*
createFunctionalClauses(Rule,C:-A,[C:-Body]):-
	getMostGeneralSubsumption(C,A,TF,UnsignedC,Variant),Variant=[_|_],!,
	%copy_term(Variant,Copy),numbervars(Copy,'$',0,_),
       %  nl,writeq(Rule),nl,writeq(Variant),nl,nl,
	Body=..[TF,A,Variant].
*/

%createFunctionalClauses(Rule,C:-A,[C:-Body]):-!,writeq(A),
	
createFunctionalClauses(Rule,C,[C]).


% =============================================================

getMostGeneralSubsumption((C), Term,'table_',C,VL):- !,
	findall(Variant,getMostGeneralSubsumptionI((C), Term,(Variant)),VL),!.

/*
getMostGeneralSubsumption(not C, Term,'f_t',C,Variant):-!,
	getMostGeneralSubsumptionI((C), (Term),Variant).

getMostGeneralSubsumption(C, Term,'t_f',C,Variant):-
	getMostGeneralSubsumptionI(not(C), Term,not(Variant)),!.
*/
getMostGeneralSubsumptionI(X, Term,_):-not(compound(Term)),!,fail.
%getMostGeneralSubsumptionI(X, Term,_):-functor(Term,F,_),atom_concat('$',_,F),!,fail.
getMostGeneralSubsumptionI(X, Term,Term) :-%functor(X,F,Arity),functor(Term,F,Arity),!,
				not(not((X=Term))),!.
getMostGeneralSubsumptionI(X, Term,Variant) :-
	arg(_, Term, Arg),
	getMostGeneralSubsumptionI(X, Arg,Variant).

	
% =============================================================

nonWrappedCan([holdsDuring,entails,'include-context',instance,query,'$existential',false,true,domain,equal,subclass,subrelation,disjointDecomposition]).

% =============================================================

removeCompilables(Cons,Rule,[],[]).
removeCompilables(Cons,Rule,[EQ|AnteLA],AnteL):-Cons==EQ,!,
	removeCompilables(Cons,Rule,AnteLA,AnteL),!.
removeCompilables(Cons,Rule,[New|AnteLA],[New|AnteL]):-!,
	removeCompilables(Cons,Rule,AnteLA,AnteL),!.

/*
removeCompilables(Cons,Rule,[instance(Var,Atom)|AnteLA],AnteL):-isSlot(Var),atom(Atom),!,
	removeCompilables(Cons,Rule,AnteLA,AnteL).
removeCompilables(Cons,Rule,[instance(Var,'$Class'(Atom,Fixed))|AnteLA],AnteL):-isSlot(Var),atom(Atom),!,
	removeCompilables(Cons,Rule,AnteLA,AnteL),!.
removeCompilables(Cons,Rule,[subclass(Var1,Var2)|AnteLA],AnteL):-unify_with_occurs_check(Var1,Var2),!,
	removeCompilables(Cons,Rule,AnteLA,AnteL),!.
removeCompilables(Cons,Rule,[domain(Pred,N,Class)|AnteLA],['$grDomain'(Pred,N,Class)|AnteL]):-
	removeCompilables(Cons,Rule,AnteLA,AnteL),!.
*/

% =============================================================

mapOnConj(Goal,LogAnte,ConditionalBody):-var(LogAnte),var(ConditionalBody),!,
	Goal=..[F|Args],append([F|Args],[LogAnte,ConditionalBody],CallL),
	Call=..CallL,!,call(Call).
mapOnConj(Goal,[LogAnte],[ConditionalBody]):-var(LogAnte),var(ConditionalBody),!,
	Goal=..[F|Args],append([F|Args],[LogAnte,ConditionalBody],CallL),
	Call=..CallL,!,call(Call).

mapOnConj(_,[],[]):-!.
mapOnConj(Goal,[Log|Ante],[Conditional|Body]):-!,
	mapOnConj(Goal,Log,Conditional),
	mapOnConj(Goal,Ante,Body).
mapOnConj(Goal,LogAnte,ConditionalBody):-nonvar(LogAnte),
	compound(LogAnte),LogAnte=..[Connective,L|IST],isBodyConnective(Connective),!,
	mapOnConj(Goal,[L|IST],ConditionalBodyList),
	ConditionalBody=..[Connective|ConditionalBodyList],!.
mapOnConj(Goal,LogAnte,ConditionalBody):-nonvar(ConditionalBody),
	compound(ConditionalBody),ConditionalBody=..[Connective,L|IST],isBodyConnective(Connective),!,
	mapOnConj(Goal,LogAnteL,[L|IST]),
	LogAnte=..[Connective|LogAnteL],!.
mapOnConj(Goal,LogAnte,ConditionalBody):-!,
	Goal=..[F|Args],append([F|Args],[LogAnte,ConditionalBody],CallL),
	Call=..CallL,
	call(Call).
	
% =============================================================

convertBodyToHolds(LogAnte,ConditionalBody):-!,
	notrace((convertNegations(not,LogAnte,NottedLogAnte,_),
	nonWrappedCan(Nowrap),
	convertToHolds([holds|Nowrap],NottedLogAnte,ConditionalBody))).

convertToHolds(Flags,Mid,Term):-
	%notrace
	(mapOnConj(convertToHoldsProp(Flags),Mid,Term)),!.
	
convertToHoldsProp(Flags,(Term),(Term)):-isSlot(Term),!.
convertToHoldsProp(Flags,not(Mid),not(Term)):-!,convertToHolds(Flags,Mid,Term).
convertToHoldsProp(Flags,Mid,'$eval'(Term)):-Mid=..[F|Args],atom(F),atom_concat(_,'Fn',F),!,Term=..[F|Args].
convertToHoldsProp(Flags,(Term),(Term)):-functor(Term,F,_),atom_concat('$',_,F),!.

convertToHoldsProp(Flags,Mid,(Term)):-Mid=..[holds,F|Args],atom(F),atom_concat(_,'Fn',F),!,Term=..[function,F|Args].
convertToHoldsProp([_|Flags],Mid,Term):-Mid	=..[F|Args],memberchk(F,Flags),!,Term=..[F|Args].
convertToHoldsProp(Flags,Mid,Mid):-not(not(memberchk(Mid,Flags))),!.
convertToHoldsProp([orig|_],Mid,Mid):-!.
convertToHoldsProp([Wrap|_],Mid,Term):-Mid=..[Wrap,F|Args],atom(F),!,Term=..[F|Args],!.
convertToHoldsProp([Wrap|_],Term,Term):-!.

% =============================================================

containsSkolems(Flags,EntailedHeadLiteral):-
	notrace((getPrologVars(EntailedHeadLiteral,Vars,_,_),!,member(Each,Vars),member(replaceConsVar(Each,_),Flags),!)).

	
selectBestRuleMatrix(Sign,HeadSlots,Flags,_,EntailedHead,[],[EntailedHead:-guarded_(EntailedHead)]):-!.
selectBestRuleMatrix(Sign,HeadSlots,Flags,_,EntailedHead,[true],[EntailedHead]):-!.
selectBestRuleMatrix(Sign,HeadSlots,Flags,[],EntailedHead,Body,[EntailedHead:-novars_(Body)]):-!.
selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,EntailedHead,ConditionalBody,[EntailedHead:-NewBodListGuarded]):-
	getPrologVars(EntailedHead:ConditionalBody,AllVars,_,_),
	getPrologVars(ConditionalBody,BodyVars,BodyVarsSingleMaybeHead,BodyVarsConnectedAndMaybeHead),
	set_partition(HeadSlots,BodyVars,PrivateHead,PrivateBody,AllSharedHB),!,
	set_partition(HeadSlots,BodyVarsConnectedAndMaybeHead,UsedMoreThenOnceHeads,BodyOnlyConected,SplitHeadVar),!,
	set_partition(HeadSlots,BodyVarsSingleMaybeHead,UsedOnceHeads,UniversalBody,HeadSlotsSingleInBody),!,
	reorderAnteListAddPropositionalMechanisms(Sign,ConditionalBody,PrivateHead,PrivateBody,UniversalBody,HeadSlotsSingleInBody,BodyOnlyConected,SplitHeadVar,NewBodListWrappedL),!,
	sort(NewBodListWrappedL,NewBodListWrappedS),
	list_to_comma(NewBodListWrappedS,BodListWrappedC),
	Inner =.. [Sign,BodListWrappedC,PrivateHead,PrivateBody],
	selectBestBodyMechanism(Inner,HeadSlots,BodyOnlyConected,UniversalBody,NewBodListGuarded),!.
	%VarInfClause =.. ['$v'],
	%NewBodListGuarded=..[Major,BodListWrappedC|VarINF].
	%prologPartitionList(NewBodListWrappedS,Item,sharedVars(Item,HeadSlots),Shared,Unshared),
	%list_to_comma(Shared,SharedT),
	%list_to_comma(Unshared,UnsharedT),
	%selectRule(Sign,ConjPosNegFA,DiscPosNegFA,NewBodListGuarded),!.%trace.*/
	

% =============================================================
%			  uVar			  Hv				Bv		     CH
% =============================================================

intersectionMath(Start,Scale,L1,L2,L12,New):-set_partition(L1,L2,_,_,L12),!,
	length(L12,Length),!,New is (Length * Scale) + Start.

reorderAnteListAddPropositionalMechanisms(Sign,List,HeadSlots,BodyVars,UniversalBody,HeadSlotsSingleInBody,BodyOnlyConected,SplitHeadVar,Result):-
	reorderAnteListAddPropositionalMechanismEach(Sign,List,HeadSlots,BodyVars,UniversalBody,HeadSlotsSingleInBody,BodyOnlyConected,SplitHeadVar,KeyedResult),!,
	keysort(KeyedResult,SortedResult),%trace,
	removeKeys(SortedResult,Result),!.
	
removeKeys([],[]).
removeKeys([K-I|KRest],[I|Rest]):-
	  removeKeys(KRest,Rest),!.
removeKeys([I|KRest],[I|Rest]):-
	  removeKeys(KRest,Rest),!.
	  

reorderAnteListAddPropositionalMechanismEach(Sign,[],HeadSlots,BodyVars,UniversalBody,HeadSlotsSingleInBody,BodyOnlyConected,SplitHeadVar,[]).
reorderAnteListAddPropositionalMechanismEach(Sign,[Conditional|Body],HeadSlots,BodyVars,UniversalBody,HeadSlotsSingleInBody,BodyOnlyConected,SplitHeadVar,[Cost-ConditionalClass|BodyList]):-
	getPrologVars(Conditional,CVars,_,_),
	intersectionMath(0,1000,UniversalBody,CVars,CUniversalBody,Cost1), %U Irealivant
	intersectionMath(Cost1,100,BodyOnlyConected,CVars,CBodyOnlyConected,Cost2), %X Transfer
        intersectionMath(Cost2,10,HeadSlotsSingleInBody,CVars,CHeadSlotsSingleInBody,Cost3), %H  Head
        intersectionMath(Cost3,1,SplitHeadVar,CVars,CSplitHeadVar,Cost), %S split head over cluases
	selectBestPropositionMechanism(Sign,HeadSlots,BodyVars,Conditional,ConditionalClass,CUniversalBody,CBodyOnlyConected,CHeadSlotsSingleInBody,CSplitHeadVar),
	reorderAnteListAddPropositionalMechanismEach(Sign,Body,HeadSlots,BodyVars,UniversalBody,HeadSlotsSingleInBody,BodyOnlyConected,SplitHeadVar,BodyList).	

%CUniversalBody,CBodyOnlyConected,CHeadSlotsSingleInBody,CSplitHeadVar
selectBestPropositionMechanism(Sign,HeadSlots,BodyVars,Conditional,ConditionalClass,
	CUniversalBody,CBodyOnlyConected,CHeadSlotsSingleInBody,CSplitHeadVar):-!,
	ConditionalClass=..[Sign,Conditional,CUniversalBody,CBodyOnlyConected,CHeadSlotsSingleInBody,CSplitHeadVar].
	
			/*

% Works but is unused now
selectBestPropositionMechanism(Sign,Conditional,CVars,HeadSlots,ConditionalClass,
			CUniversalBody,CBodyOnlyConected,CHeadSlots):-
	appendFunctInf(''(Conditional),'U',CUniversalBody,Conditional1),!,
	appendFunctInf(Conditional1,'B',CBodyOnlyConected,Conditional2),!,
	appendFunctInf(Conditional2,'H',CHeadSlots,ConditionalClassEF),!,
	removeEmptyFunct(Sign,ConditionalClassEF,ConditionalClass).
			  */
selectBestBodyMechanism(Seed,HeadSlots,BodyOnlyConected,UniversalBody,VarClause):-!,
	appendFunctInf(Seed,'Head',HeadSlots,Next1),	
	appendFunctInf(Next1,'Bodyc',BodyOnlyConected,Next2),
	appendFunctInf(Next2,'Univb',UniversalBody,VarClauseEF),!,	
	removeEmptyFunct(VarClauseEF,VarClause).


removeEmptyFunct(''(VarClause),VarClause):-!.
removeEmptyFunct(VarClause,VarClause):-!.

removeEmptyFunct(_,''(VarClause),VarClause):-!.
removeEmptyFunct(Sign,VarClause,VarClauseO):-
	VarClause=..[BF|Args],atom_concat(Sign,BF,New),
	VarClauseO=..[New|Args].
	
% =============================================================

appendFunctInf(In,_,[],In).
appendFunctInf(In,F,VarList,Out):-
	In=..[PF|Rest],
	atom_concat(PF,F,NF),
	append(Rest,[VarList],OArgs),
	Out=..[NF|OArgs],!.

appendFunct(In,_,[],In).
appendFunct(In,F,_,Out):-
	In=..[PF|Rest],
	atom_concat(PF,F,NF),
	Out=..[NF|Rest],!.

appendFunctV(In,_,[],In).
appendFunctV(In,F,VL,Out):-
	VC=..['$v'|VL],
	In=..[PF|Rest],
	atom_concat(PF,F,NF),
	append(Rest,[VC],NewL),!,
	Out=..[NF|NewL],!.

       
% =============================================================

wFrame:-
	prolog_current_frame(Frame),!,
	prolog_frame_attribute(Frame, goal,Value),!,
	writeq(Value),nl.
     
	
fc_cmp(not X,not(Y),'f-f'):-!,too_eq(X,Y),!. 
fc_cmp(not X, Y,'f-t'):-!,too_eq(X,Y),!.
fc_cmp( X,not Y,'t-f'):-!,too_eq(X,Y),!.
fc_cmp( X, Y,'t-t'):-too_eq(X,Y),!.
	
too_eq(X,Y):-compareVariant(X,Y,U,Dif),!,
	nonvar(U), (Dif < (1)).
			   

selectSign(not NHead,'f~'):-!.
selectSign(NHead ,'t~'):-!.


	
selectRule(Sign,[],[],Sign).

selectRule(Sign,[],DiscPosNeg,Body):-
	DiscPosNeg=..[F|PosNegL],
	concat_atom([Sign,F,'_'],RH),!,
	Body=..[RH|PosNegL].

selectRule(Sign,ConjPosNeg,[],Body):-
	ConjPosNeg=..[F|PosNegL],
	concat_atom([Sign,F,'_'],RH),
	Body=..[RH|PosNegL],!.

selectRule(Sign,ConjPosNegFA,DiscPosNegFA,Body):-
	ConjPosNegFA=..[C|OnjPosNegFAL],
	DiscPosNegFA=..[D|IsjPosNegFAL],
	concat_atom([Sign,C,D,'_'],RH),
	append(OnjPosNegFAL,IsjPosNegFAL,BArgs),!,
	Body=..[RH|BArgs],!.
	
				
selectConjPosNegFA(AllSharedHB,FindPos,FindNeg,ConjPosNegFA):-
	selectConjPosNeg(FindPos,FindNeg,ConjPosNeg),
	selectConjPosNegVars(AllSharedHB,ConjPosNeg,ConjPosNegFA),!.


selectConjPosNeg([],[],[]).
selectConjPosNeg([],FindNeg,'CNeg'(FindNegC)):-list_to_comma(FindNeg,FindNegC).
selectConjPosNeg(FindPos,[],'CPos'(FindPosC)):-list_to_comma(FindPos,FindPosC).
selectConjPosNeg(FindPos,FindNeg,'Con'(FindPosC,FindNegC)):-
	list_to_comma(FindNeg,FindNegC),list_to_comma(FindPos,FindPosC),!.

selectConjPosNegVars([],ConjPosNeg,ConjPosNeg).
selectConjPosNegVars(AllSharedHB,[],'CVars'(Shared)):-Shared=..['$v'|AllSharedHB].
selectConjPosNegVars(AllSharedHB,ConjPosNeg,ConjPosNegFA):-
	Shared=..['$v'|AllSharedHB],
	ConjPosNeg=..[F|Args],
	ConjPosNegFA=..[F,Shared|Args].


selectDiscPosNegFA(AllSharedHB,FindPos,FindNeg,DiscPosNegFA):-
	selectDiscPosNeg(FindPos,FindNeg,DiscPosNeg),
	selectDiscPosNegVars(AllSharedHB,DiscPosNeg,DiscPosNegFA),!.

selectDiscPosNeg([],[],[]).
selectDiscPosNeg([],FindNeg,'DNeg'(FindNegC)):-list_to_comma(FindNeg,FindNegC).
selectDiscPosNeg(FindPos,[],'DPos'(FindPosC)):-list_to_comma(FindPos,FindPosC).
selectDiscPosNeg(FindPos,FindNeg,'Dis'(FindPosC,FindNegC)):-list_to_comma(FindNeg,FindNegC),list_to_comma(FindPos,FindPosC).

selectDiscPosNegVars([],DiscPosNeg,DiscPosNeg).
selectDiscPosNegVars(AllSharedHB,[],'DVars'(Shared)):-Shared=..['$v'|AllSharedHB].
selectDiscPosNegVars(AllSharedHB,DiscPosNeg,DiscPosNegFA):-
	Shared=..['$v'|AllSharedHB],
	DiscPosNeg=..[F|Args],
	DiscPosNegFA=..[F,Shared|Args].

                                                         

/*
replaceConsVar(xB, '$existential'([63, 84, 73, 77, 69, 50], exists(xC, and(instance(xC, 'TimePoint'), and(instance([63, 84, 73, 77, 69, 50], 'TimePoint'), and(before(xC, [63, 84, 73, 77, 69, 50]), forall(D, =>(and(beforeOrEqual(xC, D), beforeOrEqual(D, [63, 84, 73, 77, 69, 50])), time(A, D))))))))), 
replaceConsVar(xC, '$existential'([63, 84, 73, 77, 69, 49], and(instance([63, 84, 73, 77, 69, 49], 'TimePoint'), and(instance(xB, 'TimePoint'), and(before([63, 84, 73, 77, 69, 49], xB), forall(D, =>(and(beforeOrEqual([63, 84, 73, 77, 69, 49], D), beforeOrEqual(D, xB)), time(A, D))))))))], 

entails(not(instance(xC, 'TimePoint')), not(instance(A, 'Object')))
   
Prolog:  impossible(instance(_, 'Object')) :- \+ searchable(instance(_, 'TimePoint'))


entails(not(instance(xB, 'TimePoint')), not(instance(A, 'Object'))) 

Prolog: impossible(instance(_, 'Object')) :- impossible(instance(_, 'TimePoint'))


entails(not(before(xC, xB)), not(instance(A, 'Object'))), 

Prolog: impossible(instance(_, 'Object')) :- impossible(before(_,_))
 


*/

selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,EntailedHead,
		SharedPos,UnsharedPos,
		SharedNeg,UnsharedNeg,
	[impossible(EntailedHead) :- impossible(Negs)],
		HeadSlots,BodyVars,VAllShare,SharedPosV,SharedNegV,
		PrivateHead,PrivateBody,UnsharedPos,UnsharedNeg):-!.

selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,[],[not(Negs)],not(EntailedHead),
	[impossible(EntailedHead) :- impossible(Negs)],
		HeadSlots,BodyVars,
		PrivateHead,PrivateBody,
		SharedPos,SharedNeg,[]/*No shared Vars*/):-!.
	
selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,EntailedHead,
		SharedPos,UnsharedPos,
		SharedNeg,UnsharedNeg,
		Matrix,
		HeadSlots,BodyVars,VAllShare,SharedPosV,SharedNegV,
		PrivateHead,PrivateBody,UnsharedPos,UnsharedNeg):-!.



/*     selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,EntailedHead,
		/*SharedPos*/ SharedPos,/*SharedNeg*/ SharedNeg,
		/*UnsharedPos*/ UnsharedPos,/*UnsharedNeg*/ UnsharedNeg,
		Matrix,
		/*HeadSlots*/ HeadSlots,/*BodyVars*/ BodyVars,/*AllVars*/ AllVars,
		/*AllSharedHB*/ AllSharedHB,/*SharedPosV*/ SharedPosV,/*SharedNegV*/ SharedNegV,
		/*PrivateHead*/ PrivateHead,/*PrivateBody*/ PrivateBody),!.
selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,EntailedHead,
		/*SharedPos*/ SharedPos,/*SharedNeg*/ SharedNeg,
		/*UnsharedPos*/ UnsharedPos,/*UnsharedNeg*/ UnsharedNeg,
		[EntailedHead:- Body],
		/*HeadSlots*/ HeadSlots,/*BodyVars*/ BodyVars,/*AllVars*/ AllVars,
		/*AllSharedHB*/ AllSharedHB,/*SharedPosV*/ SharedPosV,/*SharedNegV*/ SharedNegV,
		/*PrivateHead*/ PrivateHead,/*PrivateBody*/ PrivateBody):-*/



/*

entails(not(domain(A, 1, 'ConstantQuantity')), not(instance(A, 'UnaryConstantFunctionQuantity'))),

Prolog: impossible(instance(A, 'UnaryConstantFunctionQuantity')):- impossible(domain(A, 1, 'ConstantQuantity'))


entails(not(range(A, 'ConstantQuantity')), not(instance(A, 'UnaryConstantFunctionQuantity'))), 

Prolog: neg(instance(A, 'UnaryConstantFunctionQuantity')):- neg(range(A, 'ConstantQuantity'))
 
*/
					       % no possitives
selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,[],[not(Negs)],not(EntailedHead),
	[impossible(EntailedHead) :- impossible(ConditionalBody)],
		HeadSlots,BodyVars,
		[],[],	% No varables private on head or body /*All shared Vars*/
		SharedPos,SharedNeg,AllSharedHB):-!.

/*
replaceConsVar(xB, '$existential'([63, 84, 73, 77, 69, 50], exists(xC, and(instance(xC, 'TimePoint'), and(instance([63, 84, 73, 77, 69, 50], 'TimePoint'), and(before(xC, [63, 84, 73, 77, 69, 50]), forall(D, =>(and(beforeOrEqual(xC, D), beforeOrEqual(D, [63, 84, 73, 77, 69, 50])), time(A, D))))))))), 
replaceConsVar(xC, '$existential'([63, 84, 73, 77, 69, 49], and(instance([63, 84, 73, 77, 69, 49], 'TimePoint'), and(instance(xB, 'TimePoint'), and(before([63, 84, 73, 77, 69, 49], xB), forall(D, =>(and(beforeOrEqual([63, 84, 73, 77, 69, 49], D), beforeOrEqual(D, xB)), time(A, D))))))))], 

entails(and(not(time(A, D)), and(beforeOrEqual(D, xB), instance(A, 'Object'))), not(beforeOrEqual(xC, D))), 

Prolog: neg(beforeOrEqual(xC, D)) :- beforeOrEqual(D, xB),  instance(A, 'Object'),  not(time(A, D))


entails(and(not(time(A, D)), and(beforeOrEqual(xC, D), instance(A, 'Object'))), not(beforeOrEqual(D, xB))))))))), 

Prolog: neg(beforeOrEqual(D, xB)) :- beforeOrEqual(xC, D),  instance(A, 'Object'),  not(time(A, D))
*/


selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,Pos,Negs,EntailedHead,
	[impossible(EntailedHead):- prove(ifThen([Item|Rest],Negs))],
		HeadSlots,BodyVars,
		PrivateHead,PrivateBody,
		[Shared|PosV],SharedNeg,[S|Hared]  /*One Or More shared Vars*/):-!.

selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,ConditionalBody,not(EntailedHead),
	[impossible(EntailedHead):- prove(ifThen([Item|Rest],Negs))],
	HeadSlots,BodyVars,PrivateHead,PrivateBody,[S|Hared]):-
	splitNegations(ConditionalBody,Pos,Negs), Pos=[_|_],   % At least one possitive
	prologEach(Negs,Item,sharedVars(Item,EntailedHead)),   % All negated body variables are shared with head
	prologAtLeastOne(Pos,Item,sharedVars(Item,EntailedHead)),!, % At least one positive item is shared
	subtractProp(Item,Pos,Rest),!. % we'll move that item first
	

/*


entails(and(not(subclass(E, D)), and(domain(A, C, E), domain(B, C, D))), not(subrelation(A, B)))

Prolog: neg(subrelation(A, B)):-  domain(A, C, E), domain(B, C, D), neg(subclass(E, D)).


entails(and(not(subclass(E, D)), and(subrelation(A, B), domain(A, C, E))), not(domain(B, C, D)))

Prolog: neg(domain(B, C, D)):- neg(subclass(E, D)), subrelation(A, B), domain(A, C, E)


entails(and(not(subclass(E, D)), and(subrelation(A, B), domain(B, C, D))), not(domain(A, C, E)))))),
 
Prolog: neg(domain(A, C, E)):- neg(subclass(E, D)), subrelation(A, B), domain(B, C, D)
 

entails(and(not(disjointRelation(A, D)), and(disjoint(C, E), domain(D, B, E))), not(domain(A, B, C))), 

Prolog: neg(domain(A, B, C)) :- disjoint(C, E), domain(D, B, E),  neg(disjointRelation(A, D))


and(entails(and(not(disjointRelation(A, D)), and(disjoint(C, E), domain(A, B, C))), not(domain(D, B, E))), 

Prolog: neg(domain(D, B, E)) :- disjoint(C, E), domain(A, B, C),  neg(disjointRelation(A, D))


entails(and(not(disjointRelation(A, D)), and(domain(A, B, C), domain(D, B, E))), not(disjoint(C, E)))))), 

Prolog: impossilbe(disjoint(C, E)) :- domain(A, B, C), domain(D, B, E), neg(disjointRelation(A, D)).

*/

selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,Pos,Negs,EntailedHead,
		Matrix,
		HeadSlots,BodyVars,
		PrivateHead,PrivateBody,
		SharedPos,SharedNeg,AllSharedHB):-!.


selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails((Body),not(EntailedHeadLiteral)),
		[impossible(EntailedHeadLiteral) :- ifThen(findall(Shared,Passed),Unshared)]):-
	notSkolemFlags(Flags),
	splitNegations(Body,Pos,[]),
	prologPartitionList(Pos,Item,sharedVars(Item,EntailedHeadLiteral),Passed,Unshared),
	sharedVars(Passed,EntailedHeadLiteral,Shared),!.

/*
replaceConsVar(xB, '$existential'([63, 84, 73, 77, 69, 50], exists(xC, and(instance(xC, 'TimePoint'), and(instance([63, 84, 73, 77, 69, 50], 'TimePoint'), and(before(xC, [63, 84, 73, 77, 69, 50]), forall(D, =>(and(beforeOrEqual(xC, D), beforeOrEqual(D, [63, 84, 73, 77, 69, 50])), time(A, D))))))))), 
replaceConsVar(xC, '$existential'([63, 84, 73, 77, 69, 49], and(instance([63, 84, 73, 77, 69, 49], 'TimePoint'), and(instance(xB, 'TimePoint'), and(before([63, 84, 73, 77, 69, 49], xB), forall(D, =>(and(beforeOrEqual([63, 84, 73, 77, 69, 49], D), beforeOrEqual(D, xB)), time(A, D))))))))], 

entails(instance(A, 'Object'), before(xC, xB))

Prolog: before(xC, xB) :- findall(A,instance(A, 'Object'))
	   searchable(before(_, _)):-searchable(instance(_, 'Object')).


entails(instance(A, 'Object'), instance(xC, 'TimePoint'))), 

Prolog: instance(xC, 'TimePoint') :- findall(A,instance(A, 'Object'))
	   searchable(instance(_,'TimePoint')):-searchable(instance(_, 'Object')).
	   

entails(instance(A, 'Object'), instance(xB, 'TimePoint'))), 

Prolog: instance(xB, 'TimePoint') :- findall(A,instance(A, 'Object'))
	   searchable(instance(_,'TimePoint')):-searchable(instance(_, 'Object')).
	   

*/

selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails([true],EntailedHeadLiteral),
		[EntailedHeadLiteral :- NewAnte,
		 searchable(EntailedHeadLiteral)]):-
	containsSkolems(Flags,EntailedHeadLiteral),
	getPrologVars(EntailedHeadLiteral,ConVars,_,_),
	add_skolems_to_body(Flags,true,EntailedHeadLiteral,ConVars,NewAnte),
%	skolemizeVars(Flags,KRVars,EntailedHeadLiteral,SkolemizedHead),
	sharedVars(EntailedHeadLiteral,Thing,SharedVars).

selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails(Thing,EntailedHeadLiteral),
		[EntailedHeadLiteral :- ifThen(findall(SharedVars,Thing),NewAnte),
		 searchable(EntailedHeadLiteral):-searchable(Thing)]):-
	containsSkolems(Flags,EntailedHeadLiteral),
	getPrologVars(EntailedHeadLiteral,ConVars,_,_),
	add_skolems_to_body(Flags,true,EntailedHeadLiteral,ConVars,NewAnte),
%	skolemizeVars(Flags,KRVars,EntailedHeadLiteral,SkolemizedHead),
	sharedVars(EntailedHeadLiteral,Thing,SharedVars).
	

/*
replaceConsVar(xB, '$existential'([63, 84, 73, 77, 69, 50], exists(xC, and(instance(xC, 'TimePoint'), and(instance([63, 84, 73, 77, 69, 50], 'TimePoint'), and(before(xC, [63, 84, 73, 77, 69, 50]), forall(D, =>(and(beforeOrEqual(xC, D), beforeOrEqual(D, [63, 84, 73, 77, 69, 50])), time(A, D))))))))), 
replaceConsVar(xC, '$existential'([63, 84, 73, 77, 69, 49], and(instance([63, 84, 73, 77, 69, 49], 'TimePoint'), and(instance(xB, 'TimePoint'), and(before([63, 84, 73, 77, 69, 49], xB), forall(D, =>(and(beforeOrEqual([63, 84, 73, 77, 69, 49], D), beforeOrEqual(D, xB)), time(A, D))))))))], 

entails(instance(A, 'Object'), instance(xB, 'TimePoint')) 

Prolog:  searchable(instance(_, 'TimePoint')) :- searchable(instance(_, 'Object')).

*/

selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails([true],not(Fact)),[impossible(EntailedHeadLiteral)]):-!.


selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails([true],(Fact)),[(NewCons)]):-!,
	skolemizeCons([],KRVars,Fact,Flags,NewCons).

			
selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails((ThingNotConnetedToHead),(EntailedHeadLiteral)),
		[searchable(EntailedHeadLiteral) :- searchable(ThingNotConnetedToHead)]):-
	not(sharedVars(EntailedHeadLiteral,ThingNotConnetedToHead)),
	getPrologVars(ThingNotConnetedToHead,_,_,[]),!.
	

	
/*
replaceConsVar(xB, '$existential'([63, 84, 73, 77, 69, 50], exists(xC, and(instance(xC, 'TimePoint'), and(instance([63, 84, 73, 77, 69, 50], 'TimePoint'), and(before(xC, [63, 84, 73, 77, 69, 50]), forall(D, =>(and(beforeOrEqual(xC, D), beforeOrEqual(D, [63, 84, 73, 77, 69, 50])), time(A, D))))))))), 
replaceConsVar(xC, '$existential'([63, 84, 73, 77, 69, 49], and(instance([63, 84, 73, 77, 69, 49], 'TimePoint'), and(instance(xB, 'TimePoint'), and(before([63, 84, 73, 77, 69, 49], xB), forall(D, =>(and(beforeOrEqual([63, 84, 73, 77, 69, 49], D), beforeOrEqual(D, xB)), time(A, D))))))))], 

entails(and(not(time(A, D)), and(beforeOrEqual(xC, D), beforeOrEqual(D, xB))), not(instance(A, 'Object'))),

Near Prolog: neg(instance(A, 'Object')) :- beforeOrEqual(xC, D), beforeOrEqual(D, xB),  not(time(A, D)).

Prolog: impossible(instance(A, 'Object')) :- union([D],searchable(beforeOrEqual(xC, D)), searchable(beforeOrEqual(D, xB))),  impossible(time(A, D)).

*/

selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails((OnlyNegationsConnetedToHead),not(EntailedHeadLiteral)),
		[impossible(EntailedHeadLiteral) :- searchable(Pos),impossible(Negs)]):-
	splitNegations(OnlyNegationsConnetedToHead,Pos,Negs),
	sharedVars(Negs,EntailedHeadLiteral,_),
	not(sharedVars(Pos,EntailedHeadLiteral,_)),!.
	
/*
replaceConsVar(xB, '$existential'([63, 84, 73, 77, 69, 50], exists(xC, and(instance(xC, 'TimePoint'), and(instance([63, 84, 73, 77, 69, 50], 'TimePoint'), and(before(xC, [63, 84, 73, 77, 69, 50]), forall(D, =>(and(beforeOrEqual(xC, D), beforeOrEqual(D, [63, 84, 73, 77, 69, 50])), time(A, D))))))))), 
replaceConsVar(xC, '$existential'([63, 84, 73, 77, 69, 49], and(instance([63, 84, 73, 77, 69, 49], 'TimePoint'), and(instance(xB, 'TimePoint'), and(before([63, 84, 73, 77, 69, 49], xB), forall(D, =>(and(beforeOrEqual([63, 84, 73, 77, 69, 49], D), beforeOrEqual(D, xB)), time(A, D))))))))], 

entails(and(beforeOrEqual(xC, D), and(beforeOrEqual(D, xB), instance(A, 'Object'))), time(A, D)), 

Prolog: time(A, D) :- prove(instance(A, 'Object'),beforeOrEqual(xC, D),beforeOrEqual(D, xB)).

*/


selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails((Body),(EntailedHeadLiteral)),
		[EntailedHeadLiteral:- prove(Body)]):-
	splitNegations(Body,Pos,[]),
	not(containsSkolems(Flags,EntailedHeadLiteral:Body)),
	prologEach(Pos,Item,sharedVars(Item,EntailedHeadLiteral)),!.
	
/*
	skolemizeVars(Flags,KRVars,Body,SkolemizedBody),
		
		skolemizeVars(Flags,KRVars,EntailedHeadLiteral,SkolemizedEntailedHeadLiteral),!.
  */



/*

entails(instance(A, 'UnaryConstantFunctionQuantity'), domain(A, 1, 'ConstantQuantity'))), 

Prolog: domain(A, 1, 'ConstantQuantity'):- instance(A, 'UnaryConstantFunctionQuantity').



entails(instance(A, 'UnaryConstantFunctionQuantity'), range(A, 'ConstantQuantity'))))

Prolog: range(A, 'ConstantQuantity') :- instance(A, 'UnaryConstantFunctionQuantity')



entails(and(subrelation(A, B), and(domain(A, C, E), domain(B, C, D))), subclass(E, D)) 

Prolog: subclass(E, D):- domain(A, C, E), domain(B, C, D), subrelation(A, B).

*/

selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails((Body),(EntailedHeadLiteral)),
		[(EntailedHeadLiteral) :- prove(Body)]):-
	sharedVars(EntailedHeadLiteral,Body,ListOfShared),
	getPrologVars(EntailedHeadLiteral,EntailedHeadLiteralAll,_,_),
	getPrologVars(Body,BodyAll,_,_),
	allUnion(EntailedHeadLiteralAll,BodyAll),!.


/*

entails(and(disjoint(C, E), and(domain(A, B, C), domain(D, B, E))), disjointRelation(A, D)), 

Prolog: disjointRelation(A, D):- domain(A, B, C), domain(D, B, E), disjoint(C, E).

*/

selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails((Body),(EntailedHeadLiteral)),
		[(EntailedHeadLiteral) :- ifThen(Passed,Failed)]):-
	splitNegations(Body,Pos,[]),
	prologPartitionList(Pos,Item,sharedVars(Item,EntailedHeadLiteral),Passed,Failed),!.



selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails(Body,not(Head)),[impossible(Head):-once(prove(NewAnte))]):-
	getPrologVars(Head,ConVars,_,_),!,
	add_skolems_to_body(Flags,Body,Head,ConVars,NewAnte),!.

selectBestRuleMatrix(Sign,HeadSlots,Flags,KRVars,entails(Body,Head),[Head:-unoptimized(NewAnte)]):-
	getPrologVars(Head,ConVars,_,_),!,
	add_skolems_to_body(Flags,Body,Head,ConVars,NewAnte),!.


notSkolemFlags(Flags):-not(member(replaceConsVar(_,_),Flags)).

splitNegations(List,Pos,Neg):-
	notrace((prologPartitionList(List,Item,isNegation(Item),NegT,PosT),!,NegT=Neg,PosT=Pos)).
	
sharedVars(Item1,Item2):-
	notrace((sharedVars(Item1,Item2,SharedVars,Item1Only,Item2Only),!,SharedVars=[_|_])).
	
sharedVars(Item1,Item2,SharedVars):-
	notrace((sharedVars(Item1,Item2,SharedVars,Item1Only,Item2Only),!)).
	
sharedVars(Item1,Item2,SharedVars,Item1Only,Item2Only):-
	notrace((getPrologVars(Item1,V1s,_,_),getPrologVars(Item2,V2s,_,_),!,
	set_partition(V1s,V2s,Item1Only,Item2Only,SharedVars),!)).

isNegation(Item):-notrace((isSlot(Item);(Item=not(V),nonvar(V));(functor(Item,F,A),atom_concat(~,_,F)))).
	













	
getGoalProfile(Name,Time):-flag(Name,Time,Time).
profileGoalTime(Name,Goal):-
	getCputime(Start),
	call(Goal),
	getCputime(End),
	flag(Name,Time,Time),
	Average is (Time+(End-Start))/2,
	flag(Name,_,Average).	

normalizeProp(Flags,In,Out,Cons):-
	ifThenElse(
		(member(holds,Flags);member(orig,Flags)),
		(convertNegations(not,In,Notted,_),
		convertToHolds(Flags,Notted,Held)),
		Held=In),
	convertNegations(Flags,Held,Out,Cons).

convertRuleBodyWFS(Flags,KB,RuleHead,[],!).

convertRuleBodyWFS(Flags,KB,RuleHead,[Cons],Rewritten):-
	normalizeProp(Flags,Cons,Rewritten,Bare).

convertRuleBodyWFS(Flags,KB,RuleHead,[H|T],(RH,RT)):-!,
	convertRuleBodyWFS(Flags,KB,RuleHead,H,RH),
	convertRuleBodyWFS(Flags,KB,RuleHead,T,RT),!.

convertRuleBodyWFS(Flags,KB,RuleHead,(H,T),(RH,RT)):-!,
	convertRuleBodyWFS(Flags,KB,RuleHead,H,RH),
	convertRuleBodyWFS(Flags,KB,RuleHead,T,RT),!.

convertRuleBodyWFS(Flags,KB,RuleHead,and(H,T),(RH,RT)):-!,
	convertRuleBodyWFS(Flags,KB,RuleHead,H,RH),
	convertRuleBodyWFS(Flags,KB,RuleHead,T,RT),!.

convertRuleBodyWFS(Flags,KB,RuleHead,or(H,T),(RH,RT)):-!,
	convertRuleBodyWFS(Flags,KB,RuleHead,H,RH),
	convertRuleBodyWFS(Flags,KB,RuleHead,T,RT),!.

convertRuleBodyWFS(Flags,KB,RuleHead,Cons,(Rewritten)):-
	normalizeProp(Flags,Cons,Rewritten,Bare).



getPredicateKey((LogConsq),(Key)):-
		convertNegations((not),LogConsq,NC,_),!,
		getPredicateKey2((NC),(Key)).
getPredicateKey2(not(LogConsq),not(Key)):-nonvar(LogConsq),!,getPredicateKey2((LogConsq),(Key)).
getPredicateKey2((LogConsq),(Key)):-functor(LogConsq,Key,_),!.


inferType(_,_):-fail.

convertListNotNeg(Var,holds('TruthFn',Var,'True')):-isSlot(Var),!.
convertListNotNeg(not(Var),holds('TruthFn',Var,'False')):-isSlot(Var),!.
convertListNotNeg([],[]):-!.
convertListNotNeg([not(X)|Y],[ (~ X)|YY]):-inferType(F,negationByFailure),!,convertListNotNeg(Y,YY).
convertListNotNeg([not(X)|Y],[ (XX)|YY]):- not(isSlot(X)),!,
		X=..[F|Args],!, atom_concat('~',F,FN),
		XX=..[FN|Args],!,convertListNotNeg(Y,YY).
convertListNotNeg([(X)|Y],[(X)|YY]):-!,convertListNotNeg(Y,YY).

reorderAnteceedants([NConsq|List],List,All):-
	reorderAnteceedants(NConsq,List,List,All),!.

reorderAnteceedants(Head,Body,[],[]):-!.
reorderAnteceedants(Head,Body,[AProto],[AProto]):-!.
reorderAnteceedants(Head,Body,AProto,Ordered):-
	predsort(reorderAnteceedants(Head,Body),AProto,Ordered).

reorderAnteceedants(Head,Body,Result,T1,T2):-
	compareVariant(T1,T2,M,C1,C2),
	compare(Result,C2,C1),
	not(Result = (=)),!.
reorderAnteceedants(Head,Body,Result,T1,T2):-
	compareVariant(T1,Head,M,C1,H1),
	compareVariant(T2,Head,M,C2,H2),
	compare(Result,C2,C1),
	not(Result = (=)),!.
reorderAnteceedants(Head,Body,<,T1,T2).


set_partition([],Set2,[],Set2,[]):-!.
set_partition(Set1,[],Set1,[],[]):-!.
set_partition([H|S1],Set2,O1,O2,[H|Intersection]):-
	memberchk(H,Set2,NewSet2),!,
	set_partition(S1,NewSet2,O1,O2,Intersection).
%set_partition(Set2,[H|S1],O2,O1,[H|Intersection]):-
%	memberchk(H,Set2,NewSet2),
%	set_partition(S1,NewSet2,O1,O2,Intersection).
set_partition([H|S1],Set2,[H|O1],O2,Intersection):-
	set_partition(S1,Set2,O1,O2,Intersection).

memberchk(H,[HH|List],ListO):- 
	'=='(HH,H)
		-> ListO=List ; 
		(memberchk(H,List,ListM),ListO =  [HH|ListM]).

	
convertFactHeadWFS(Flags,KB,Cons,Rewritten):-
	normalizeProp(Flags,Cons,Rewritten,Bare).
	

convertNegations(Var,In,Out,Bare):-var(Var),!,convertNegations(keep,In,Out,Bare).	
convertNegations(Var,In,In,In):-isSlot(In),!.
convertNegations(Var,not(In),not(In),true(In)):-isSlot(In),!.

convertNegations(Flags,In,Out,Bare):-member(neg(Neg),Flags),!,convertNegations(Neg,In,Out,Bare).
convertNegations([Neg|_],In,Out,Bare):-atom(Neg),!,convertNegations(Neg,In,Out,Bare).
convertNegations(keep,PTerm,PTerm,Cons):-!,convertNegations(not,PTerm,_,Cons).

convertNegations(Neg,[Log|Ante],[Conditional|Body],[Bare|BBody]):-!,
	convertNegations(Neg,Log,Conditional,BLog),
	convertNegations(Neg,Ante,Body,BBody).

convertNegations(Neg,LogAnte,ConditionalBody,Connective):-
	compound(LogAnte),LogAnte=..[Connective|List],isBodyConnective(Connective),!,
	convertNegations(Neg,List,ConditionalBodyList,_),!,
	ConditionalBody=..[Connective|ConditionalBodyList],!.

convertNegations(Other,PTerm,PTerm,Cons):-
	PTerm=..[Other,Cons],!.
	
convertNegations(Neg,\+(In),Out,Bare):-!,convertNegations(Neg,not(In),Out,Bare).
convertNegations(Neg,'~'(In),Out,Bare):-!,convertNegations(Neg,not(In),Out,Bare).
convertNegations(Neg,In,Out,Bare):-
	In=..[NegF|Args],atom_concat('~',Pos,NegF),
	Mid=..[Pos|Args],!,convertNegations(Neg,not(Mid),Out,Bare).

convertNegations(lit,not(PTerm),Lit,PTerm):-
	PTerm=..[Pos|Args],
	atom_concat('~',Pos,F),!,
	Lit=..[F|Args].
convertNegations(Neg,not(Cons), Term,Cons):-
	Term=..[Neg,Cons].	
convertNegations(_,Cons,Cons,Cons).


/*
selectBestBodyMechanism(''(Seed),[],[],[],[],'Novars'(Seed)):-!.
selectBestBodyMechanism(''(Seed),UniversalBody,[],[],[],T):-!,T=..['Zall',Seed|UniversalBody].
selectBestBodyMechanism(''(Seed),[],HeadSlotsSingleInBody,[],[],T):-!,T=..['Headall',Seed|HeadSlotsSingleInBody].
selectBestBodyMechanism(''(Seed),[],[],BodyOnlyConected,[],T):-!,T=..['Bodyall',Seed|BodyOnlyConected].
selectBestBodyMechanism(''(Seed),[],[],[],SplitHeadVar,T):-!,T=..['HeadAll',Seed|SplitHeadVar].
selectBestBodyMechanism(Seed,UniversalBody,HeadSlotsSingleInBody,BodyOnlyConected,SplitHeadVar,VarClause):-!,
	appendFunctInf(Seed,'Univb',UniversalBody,Next1),	
	appendFunctInf(Next1,'Headb',HeadSlotsSingleInBody,Next2),	
	appendFunctInf(Next2,'Bodyc',BodyOnlyConected,Next3),
	appendFunctInf(Next3,'Doubleb',SplitHeadVar,VarClauseEF),!,
	removeEmptyFunct(VarClauseEF,VarClause).
*/


/* 
selectBestPropositionMechanism(Sign,Conditional,CVars,HeadSlots,ConditionalClass,
	CUniversalBody,CHeadSlotsSingleInBody,CBodyOnlyConected,CSplitHeadVar):-%trace,
	appendFunct(''(Conditional),'Z_Uvar_',CUniversalBody,Conditional1),!,
	appendFunct(Conditional1,'amulitHeadVar_',CSplitHeadVar,Conditional2),
	appendFunct(Conditional2,'singleHeadVar_',CHeadSlotsSingleInBody,Conditional3),
	appendFunct(Conditional3,'bodyConnectedVar_',CBodyOnlyConected,ConditionalClass),!.
*/



