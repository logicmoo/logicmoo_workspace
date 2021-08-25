% ===============================================================
% AGENT QUERY (Chars)
% ===============================================================
:-include('sigma_header.pl').
      
tkq1:-agentQuery("(isa Joe ?Class)",'ToplevelContext','Merge','Dmiles',U,R,P).
tkq2:-agentQuery("(isa on Relation)",'ToplevelContext','Merge','Dmiles',U,R,P).


agentQuery(KIFCharsIn,Ctx,KB,User,UResultsSoFar,Result,Proof):-
	agentQuery(KIFCharsIn,Ctx,KB,User,UResultsSoFar,Result,Proof,Status),
	(Status = done(_) -> ! ; true).


agentQuery(KIFCharsIn,Ctx,KB,User,UResultsSoFar,Result,Proof,Status):-
	isCharCodelist(KIFCharsIn),!,
	string_clean(KIFCharsIn,KIFChars),
	logOnFailure(ask_parse_chars(KIFChars,FmlInOpen,Vars)),!,
	agentQuery(KIFCharsIn,FmlInOpen,Vars,Ctx,KB,User,UResultsSoFar,Result,Proof,Status).
	
	
agentQuery(KIFCharsIn,FmlInOpen,Vars,Ctx,KB,User,UResultsSoFar,Result,Proof,Status):-
	notrace((
	retractAllProlog(answer_found(_)),
	retractAllProlog(t_answer_found(_)),
	retractAllProlog(tabled_f(_)),
	retractAllProlog(sigmaCache(inferInstanceTable(KB,Class,Set))),
	ignore(findall(_,expireOptimizationsInKB(_,_,_),_)),
	retractAllProlog(table_g(_)),
	retractAllProlog(cpuend(_)),
	retractAllProlog(tabled_t(_)),
	retractAllProlog(table_g(_)),
	retractAllProlog(proving(_)),
	getDefaultKB(QKB),!,ignore((KB=QKB)),!,
	get_default_query_context(QCTX),!,ignore((Ctx=QCTX)),!,
	logOnFailure(ensureSigmaKB(KB,Ctx)),!,
	flag('$Returned Answers',_,0),
	flag('$UAnswers',_,0))),
	TN = User, % Tracks query based on 'User'
	destroyTN(KB,TN,_Ctx),	% Removes previous Query
	getOpenVariablesWFF(FmlInOpen,Vars,ChaseVars),
	getPrologVars(ChaseVars,QVars,_,_),
	QueryPrototype=..[query|QVars],
	notrace((not(not((
		getAssertionClauses(KB,Ctx,'<=>'(FmlInOpen,QueryPrototype),CAN,Vars,Flags),
		assert(sigmaCache(FmlInOpen,CAN,Flags,Vars,KB,Ctx,TN,User,Result)),!,
		(recanonicalizeTN(KB,TN)),    % Compile Inference
		assert(tq_attempted_query),   
		writeDebug(blue,'Stage 1 - Compilation ':CAN:Flags)))))),!,
	agentQueryEach(FmlInOpen,QueryPrototype,ChaseVars,Ctx,KB,User,UResultsSoFar,Result,Proof,Status),
	commitCleanProof(Proof,ProofOut).
	    

% ======================================================
% QUERY PROC POSITIVE/NEGATIVE
% ======================================================

% Check For Undefines
agentQueryEach(KIFCharsIn,Formula,Vars,Ctx,KB,User,UResultsSoFar,Result,Proof,Status):-  fail,
		once(getConstants(atomic,Formula,UsedConstants,_,_)),	
		logOnFailure(check_all_constants_have_type(Formula,UsedConstants,UnDefinedList)),
		UnDefinedList=[_|_],!,writeObject(undefined_constants(UnDefinedList),_),!,fail.		

% Check For Theorem
agentQueryEach(FmlInOpen,UQuery,UVars,Ctx,KB,User,1,['Result' = 'True'],formula(instance(FormulaIN,'Theorem')),done(true:thereom)):-
	resetTableFlags,
	writeDebug(purple,'Stage 2 - Theorem Check ':FmlInOpen),
	isTheorem(FmlInOpen,_),!,
	retain_yes,sendNote('(theorem)'),!.
      
% Call Inference
agentQueryEach(FmlInOpen,UQuery,UVars,Ctx,KB,User,UA, UVars,Proof,Result):-
	agentInference(FmlInOpen,UQuery,UVars,Ctx,KB,User,UA, UVars, Proof,Result).

% Query Failed
agentQueryEach(FmlInOpen,UQuery,UVars,Ctx,KB,User,0,['Result'='none'|_],'Unproven',done(possible:searchfailed)):-
	flag('$UAnswers',UA,UA),UA<1,!.

% Query Succeeded
agentQueryEach(FmlInOpen,UQuery,UVars,Ctx,KB,User,UA,['Result'='true'|_],found(UA),done(true:UA)):-!,
	flag('$UAnswers',UA,UA).      


% ======================================================
% ======================================================    	
commitCleanProof(deduced,deduced):-!.
commitCleanProof(Proof * deduced,ProofOut):-!,commitCleanProof(Proof,ProofOut).
commitCleanProof(deduced * Proof,ProofOut):-!,commitCleanProof(Proof,ProofOut).
commitCleanProof(Proof * '$VAR'(_),ProofOut):-!,commitCleanProof(Proof,ProofOut).
commitCleanProof('$VAR'(_) * Proof,ProofOut):-!,commitCleanProof(Proof,ProofOut).
commitCleanProof(Proof1 * Proof2,ProofOut1 * ProofOut2):-!,
	commitCleanProof(Proof1,ProofOut1),!,
	commitCleanProof(Proof2,ProofOut2),!.
commitCleanProof(Proof,Proof):-!.	
	




getOpenVariablesWFF(FmlInOpen,[],[]):-!.
getOpenVariablesWFF(FmlInOpen,V,[]):-var(V),!.
getOpenVariablesWFF(FmlInOpen,[K=Va|Rs],[K=Va|Vars]):-
		not(isVarClosedWFF(Va,FmlInOpen)),!,
		getOpenVariablesWFF(FmlInOpen,Rs,Vars),!.
getOpenVariablesWFF(FmlInOpen,[_|Rs],Vars):-	!,
		getOpenVariablesWFF(FmlInOpen,Rs,Vars),!.
		
isVarClosedWFF(Va,forall(VS,FmlInOpen)):-Va==VS,!.
isVarClosedWFF(Va,exists(VS,FmlInOpen)):-Va==VS,!.
isVarClosedWFF(Va,forall(_,FmlInOpen)):-!,isVarClosedWFF(Va,FmlInOpen).
isVarClosedWFF(Va,exists(_,FmlInOpen)):-!,isVarClosedWFF(Va,FmlInOpen).
isVarClosedWFF(Va,and(FmlIn,Open)):-!,
	isVarClosedWFF(Va,FmlIn);
	isVarClosedWFF(Va,Open).
isVarClosedWFF(Va,or(FmlIn,Open)):-!,
	isVarClosedWFF(Va,FmlIn);
	isVarClosedWFF(Va,Open).
isVarClosedWFF(Va,'=>'(FmlIn,Open)):-!,
	isVarClosedWFF(Va,FmlIn);
	isVarClosedWFF(Va,Open).
isVarClosedWFF(Va,'<=>'(FmlIn,Open)):-!,
	isVarClosedWFF(Va,FmlIn);
	isVarClosedWFF(Va,Open).
isVarClosedWFF(Va,xor(FmlIn,Open)):-!,
	isVarClosedWFF(Va,FmlIn);
	isVarClosedWFF(Va,Open).
isVarClosedWFF(Va,entails(FmlIn,Open)):-!,
	isVarClosedWFF(Va,FmlIn);
	isVarClosedWFF(Va,Open).
isVarClosedWFF(Va,known(Open)):-!,
	isVarClosedWFF(Va,Open).
isVarClosedWFF(Va,possible(Open)):-!,
	isVarClosedWFF(Va,Open).
isVarClosedWFF(Va,consistent(Open)):-!,
	isVarClosedWFF(Va,Open).
isVarClosedWFF(Va,not(Open)):-!,
	isVarClosedWFF(Va,Open).
isVarClosedWFF(Va,neg(Open)):-!,
	isVarClosedWFF(Va,Open).
	


getQueryDefaults(UQuery,OAnswers,BackchainsMax,Deductions):-
	notrace((
        (getSigmaOption(opt_backchains_max=BackchainsMax)),
        (getSigmaOption(opt_deductions_max=Deductions)),!,
        ignore((ground(UQuery) -> Answers=1 ; Answers=PAnswers)),
	(getSigmaOption(opt_answers_max=Answers)),!,
	ignore(BackchainsMax=30),ignore(Answers=60),OAnswers is Answers,!)).

set_quit_time(Num):-
	notrace((
	(getSigmaOption(opt_timeout=QuitTime)),!,ignore(QuitTime=5),!,
	retractAllProlog(cpuend(_)),    
	getCputime(Now),
	Then is Now + QuitTime*Num,
	asserta(cpuend(Then)),!)).
	
:-dynamic(noexistencials/0).



%edify_vars(X,X):-!.
edify_vars(Var,Var):-var(Var),!.
edify_vars([],[]):-!.
edify_vars([N=V|T],[N=RV|NT]):-
            eval_lr(V,RV),!,retain_answer(RV),
            edify_vars(T,NT),!.
edify_vars([H|T],[H|NT]):-
            edify_vars(T,NT),!.


