% ===================================================================
%  INVOKE QUERY STANDARD
% ===================================================================


:-dynamic(final_answer/1).
:-dynamic(addToResultBuffer/4).
:-dynamic(queryBuffer_db/4).	
:-dynamic(query_start_time/1).
:-dynamic(query_total_time/1).


invokeQueryToBuffer(findall(V,Formula),Ctx,TrackingAtom,KB,User,Vars,CPU):-!,
        invokeQueryToBuffer(Formula,Ctx,TrackingAtom,KB,User,Vars,CPU).
invokeQueryToBuffer(query(Formula),Ctx,TrackingAtom,KB,User,Vars,CPU):-!,
         invokeQueryToBuffer(Formula,Ctx,TrackingAtom,KB,User,Vars,CPU),!.
invokeQueryToBuffer(queryyn(Formula),Ctx,TrackingAtom,KB,User,Vars,CPU):-!,
         invokeQueryToBuffer(Formula,Ctx,TrackingAtom,KB,User,Vars,CPU),!.

invokeQueryToBuffer(canonicalize(Formula),Ctx,TrackingAtom,KB,User,Vars,CPU):-!,
	retractAllProlog(queryBuffer_db(_,_,_,_)),
	retractAllProlog(final_answer(_)),
        getAssertionClauses(PreQ,KB,Ctx,Formula,Out,Vars,P),
	retain_answer(Out),
	writeObject('$spacer',Vars),
	asserta(tq_passed),
	asserta(tq_attempted_query),
	asserta(tq_least_one_answer),
	writeObject_conj(Out,Vars).

			                                
invokeQueryToBuffer(Formula,Ctx,TrackingAtom,KB,User,Vars,CPU):-
	getCputime(Start),!,
	logOnFailure(createQueryResponseBuffer_proc(Formula,Ctx,TrackingAtom,KB,User,Vars)),
	getCputime(End),ignore(Start=0),ignore(End=0),                
	retractAllProlog(query_total_time(_)),
	CPU is End - Start,!,ignore(CPU=0),!,
	assert(query_total_time(CPU)),!.


createQueryResponseBuffer_proc(FmlInOpen,Ctx,TrackingAtom,KB,User,Vars):-
	assert(tq_attempted_query),
	retractAllProlog(queryBuffer_db(_,_,_,_)),
	retractAllProlog(final_answer((_))),
	agentQuery(KIFCharsIn,FmlInOpen,Vars,Ctx,KB,User,UResultsSoFar,Result,Proof,Status),
	ignore(logOnFailure(retain_answer(Result))),
        writeDebug(addToResultBuffer(UResultsSoFar,Result,Proof,Status)),
	addToResultBuffer(UResultsSoFar,Result,Proof,Status),!. % Buffer Calls Itteration via failure 	

createQueryResponseBuffer_proc(FmlInOpen,Ctx,TrackingAtom,KB,User,Vars):-!.


% ===========================================================
% Buffer Results
% ===========================================================
addToResultBuffer(UResultsSoFar,Result,Proof,Status):-var(Proof),!,fail.

addToResultBuffer(UResultsSoFar,Result,Proof,done(How)):-
	numbervars((UResultsSoFar,Result,Proof,Status),'$VAR',0,_),
	assert(queryBuffer_db(UResultsSoFar,Result,Proof,done(How))),
	assert(final_answer(How)),!.	%Finalize on done/1.
	
addToResultBuffer(UResultsSoFar,Result,Proof,Status):-
	once((numbervars((UResultsSoFar,Result,Proof,Status),'$VAR',0,_),
	assert(queryBuffer_db(UResultsSoFar,Result,Proof,Status)))),
	fail.




