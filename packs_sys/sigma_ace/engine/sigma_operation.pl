% ===================================================================
% File 'Sigma_operation.pl' 
% Authors: Douglas Miles; Jay Halcomb
% Contact: dmiles@teknowledge.com ; jhalcomb@teknowledge.com ; apease@teknowledge.com
% Version: 'Sigma_operation.pl' 1.0.0 
% History:
% Created - 2000/11/06 dmiles@teknowledge.com
% ===================================================================
% ===================================================================
% PURPOSE
% This File is the bootstrap for the Sigma Infence engine one first calls "[sigma_operation]"
% So first is loads the proper files and then starts up the system
% ===================================================================

:-include('sigma_header.pl').
		
% ===================================================================
% EXPORTS
% ===================================================================

register_inference_module(_ModuleName).


canonicalizer_module('sigma_normal').
compiler_module('sigma_byrd').
version_tag(kbl).

getDefaultKB('Merge').

%:-include('sigma_header.pl').
:-dynamic(title/1).

/* 
Loads and compiles a Authorial KIF file 
u_load_kif(+Filename,+ToplevelContext,+DefiningAuthor). 
*/

u_load_kif(Filename):-
	set_automation,
	agent_load_kif(Filename,Filename,Filename).

u_load_kif(Filename,ToplevelContext,User):-
	set_automation,
	agent_load_kif_quiet(Filename,ToplevelContext,User).


/*
Saves state of a Contet (crypt is a less human readable form) 
u_save(+FileName,+ToplevelContext) 
u_save_crypt(+FileName,+ToplevelContext) + Nearly Completed/Tested (actully just a QLF version =)  
*/                                                  

u_save(FileName,Ctx):-
	set_automation,
	agent_u_save(FileName,Ctx).	
    
	   
/*

Loads the saved state (crypted or not)  (adds assertions) they were originally stored with a context
u_load(+FileName) 
Clears a context (Leave unbound to clear all contexts) 
u_clear(?Context) 
We still have some discussion on how contexts are linked together so right now they are all active.. 
Mainly it gives us a way to do bulk load/unloads and separation user data from general system's like SUMO. 

*/


u_load(FileName):-
	set_automation,
	agent_u_load( FileName).



/* Assertion 
u_assert(+KIFCharsIn,+Ctx,+KB,+User). 
KIFCharsIn = "(instance virginia Computer)"  
Ctx = 'Computers and Things' 
KB = 'Merge' currently must be set to 'Merge' 
User = 'reynolds@teknowledge.com'  
*/

u_assert(KIFCharsIn,Ctx,KB,User):-
	set_automation,
	agent_tell(KIFCharsIn,Ctx,TN,KB,User).	


/*
Now the predicates that you most want 
Invokes a query 
u_query(+KIFCharsIn,+Ctx,+KB,+User,-UResultsSoFar,-Result,-Proof,-Status). 

Input variables: 
KIFCharsIn = "(instance ?What Computer)"  
Ctx = 'Computers and Things'  % Any legal Atom that helps you book keep 
KB = 'Merge' currently must be set to 'Merge' 
User = 'reynolds@teknowledge.com'  There are times when error messages about redundant assertions happen.. and for bookkeeping all assertions have an author atom.. perhaps you can even use the source file name it came from.

output variables: 
UResultsSoFar = 0 indexed value of answer number.. +1 would mean it is a repeated answer with a different proof.. you could choose not to display answers with negative numbers 
Result = Is the Prolog Variable Vector like ['What'=virginia|_] (can be written with writeObject(Verbose,getPrologVars(Result),Result))  
Proof = A proof object that can be printed with writeObject(Verbose,proof(Proof),Result) 
Status = is either 'true' or '!' ... true = more results.. '!' means no more results 
(Status is optional.. if you completely remove this argument and it will fail at the next call after the first solution returning !)  
*/



u_query(KIFCharsIn,Ctx,KB,User,UResultsSoFar,Result,Proof,Status):-
	set_automation,
	agentQuery(KIFCharsIn,Ctx,KB,User,UResultsSoFar,Result,Proof,Status).

u_query(KIFCharsIn,Ctx,KB,User,UResultsSoFar,Result,Proof):-
	set_automation,
	agentQuery(KIFCharsIn,Ctx,KB,User,UResultsSoFar,Result,Proof).



/*
% Lets you discover any errors that were created durring any operation 
read_errors(Message) 
+Will fail if no errors are in this buffer 
+When any of the above commands are first invoked the error buffer is cleared 
+Whenever there were errors and you read this buffer each one is deleted while it is read 
+Messages are stored as printable objects writeObject(Verbose,Message)  we can discuss a more programmatic approach to formats later based on how we want to use them. 
*/
:-dynamic(u_errors/1).
read_errors(Message):-retract(u_errors(Message)).

/*
Term Writting
writeObject(Verbose,+Object).
writeObject(Verbose,+Object,+Vars). 
Allows easy printing of variables and objects
*/

 
/*
Inference Variables
 
Set Inference Optional Variable
u_set_option(VariableName=NewValue).
*/
u_set_option(O):-setSigmaOption(O).
 
/*
Retrive Inference Optional Variable
u_get_option(VariableName=CurrentValue).
*/
u_get_option(O):-isSigmaOption(O).
 
/*
The defualts are shown here
opt_timeout=5  The query system will try for 5 seconds before stopping (RealNumber in seconds)
opt_answers_min=1 The system will at minumum try to come up with 1 answer
opt_answers_max=99 The system will at maximun will try to come up with 99 answers
opt_backchains_max=50 will try 50 prolog backchains aprocimately 12 authorial backchains on each deduction
opt_deductions_max=100 The system will try to come up with 100 non unique answers before stooping
*/ 

set_automation:-
		retractAllProlog(u_errors(_)),
	       !.
	       % (unsetSigmaOption(client=_)),
		%(setSigmaOption(client=automata)).


% ===================================================================
%  BATCH INTERFACE
% ===================================================================

invokeOperation(Verbose,surf,Ctx,Tracking,KB,User,Vars):-!.
invokeOperation(Verbose,Fml,Ctx,TN,KB,User,Vars):-!,
		once(invokeOperationProc(Verbose,Fml,Ctx,TN,KB,User,Vars)),!.

invokeOperationProc(Verbose,TERM,Ctx,TN,KB,User,Vars):-TERM =..[note|T],!,
		assert(title(T)),
		WT=..['note '|T],
		writeObject(Verbose,WT,Vars),!,
		writeObject(Verbose,'\n\n;; Assertions \n\n',Vars),!,
		clear_sigma_memory,set_promiscuous,!.

invokeOperationProc(Verbose,TERM,Ctx,TN,KB,User,Vars):-TERM =..[answer|T],!,batch_answer(T).

invokeOperationProc(Verbose,TERM,Ctx,TN,KB,User,Vars):-TERM =..[abort],!,assert(tq_ignored).
invokeOperationProc(Verbose,'infer-domains'(on),Ctx,TN,KB,User,Vars):-!,set_promiscuous,!.
invokeOperationProc(Verbose,'infer-domains'(true),Ctx,TN,KB,User,Vars):-!,set_promiscuous,!.
invokeOperationProc(Verbose,'infer-domains'(false),Ctx,TN,KB,User,Vars):-!,unset_promiscuous,!.
invokeOperationProc(Verbose,'infer-domains'(off),Ctx,TN,KB,User,Vars):-!,unset_promiscuous,!.

invokeOperationProc(Verbose,end_of_file,Ctx,TN,KB,User,Vars):-!.

invokeOperationProc(Verbose,'file_comment'(C),SourceCtx,SourceTN,KB,User,Vars):-!,nl,write(C),!.
invokeOperationProc(Verbose,'browser-only'(_),SourceCtx,SourceTN,KB,User,Vars):-!.
invokeOperationProc(Verbose,'execute-prolog'(Code),SourceCtx,SourceTN,KB,User,Vars):-!,
once(((%		writeq(Code),nl,
		atom_codes(Code,Codes),
%		writeq(Codes),nl,
		getUnquotedCodes(Codes,Chars),
%		writeq(Chars),nl,
		atom_codes(Atom,Chars),
		atom_to_term(Atom,Term,V)
		
		);writeFmt('could not parse ~w\n',[Code])),once(((once(catch(Term,_,fail)),writeFmt('Exec Suceeded',[]));writeFmt('Exec Failed',[])))).
invokeOperationProc(Verbose,'load-kb'(Filename),SourceCtx,SourceTN,KB,User,Vars):-!,fail,agent_load_kif_quiet(Filename,'ToplevelContext','Anonymous').
invokeOperationProc(Verbose,'load-kb-show'(Filename),SourceCtx,SourceTN,KB,User,Vars):-!,
	agent_load_kif(Filename,'ToplevelContext','Anonymous').

invokeOperationProc(Verbose,retract(nil),Ctx,TN,KB,User,Vars):-!.

invokeOperationProc(Verbose,query(nil),Ctx,TN,KB,User,Vars):-!.

invokeOperationProc(Verbose,(nil),Ctx,TN,KB,User,Vars):-!.

invokeOperationProc(Verbose,tell(nil),Ctx,TN,KB,User,Vars):-!.

invokeOperationProc(Verbose,assert(end_of_file),Ctx,TN,KB,User,Vars):- !.

invokeOperationProc(Verbose,assert(NEWFORM),Ctx,TN,KB,User,Vars):-  !,
        invokeOperationProc(Verbose,assert([trusted,canonicalize,to_mem],NEWFORM),Ctx,TN,KB,User,Vars).

invokeOperationProc(Verbose,assert(Flags,NEWFORM),Ctx,TN,KB,User,Vars):-  !,
        flag('Axioms Compiled',AC,AC+1),
	LN is AC + 1,
	flag(proof_linenumber,_,LN),
	writeObject(Verbose,nl,Vars),
	writeObject(Verbose,NEWFORM,Vars),
	writeObject(Verbose,nl,Vars),
	logOnFailure(invokeTell(Flags,surface,NEWFORM,Ctx,TN,KB,Vars,User)),
	ignore((getSigmaOption(opt_debug=off,on),Verbose=verbose,writeKnownFormsTN(Ctx,KB:TN))).


invokeOperationProc(Verbose,retract(Fml),Ctx,TN,KB,User,Vars):-
         flag('Axioms Compiled',AC,AC+1),
         writeObject(Verbose,nl,Vars),
         writeObject(Verbose,retract(Fml),Vars),
         writeObject(Verbose,nl,Vars),
         retract_pterm(Fml,Ctx,TN,KB,User,Vars).

invokeOperationProc(Verbose,canonicalize(Q),Ctx,TN,KB,User,Vars):-!,
         writeObject(Verbose,nl,Vars),
         writeObject(Verbose,canonicalize(Q),Vars),
         invokeQueryWithTime(canonicalize(Q),_,Ctx,TN,KB,User,Vars,CPU1), assert(findings(CPU1)).

invokeOperationProc(Verbose,query(Q),Ctx,TN,KB,User,Vars):-!, 
         writeObject(Verbose,'\n\n\n;; Query \n\n',Vars),
         writeObject(Verbose,'query '(Q),Vars),
         writeObject(Verbose,nl,Vars),
         invokeQueryWithTime(query(Q),Ctx,TN,KB,User,Vars,CPU1), assert(findings(CPU1)).

invokeOperationProc(Verbose,query(N,Q),Ctx,TN,KB,User,Vars):-!, 
         writeObject(Verbose,'\n\n\n;; Timed Query \n\n',Vars),
         writeObject(Verbose,'query '(Q),Vars),
         writeObject(Verbose,nl,Vars),
         invokeQueryWithTime(query(N,Q),Ctx,TN,KB,User,Vars,CPU1), assert(findings(CPU1)).

invokeOperationProc(Verbose,queryyn(Q),Ctx,TN,KB,User,Vars):-!,
         writeObject(Verbose,'\n\n\n;; Yes/No Query \n\n',Vars),
         writeObject(Verbose,'query '(Q),Vars),
         writeObject(Verbose,nl,Vars),
         invokeQueryWithTime(queryyn(Q),Ctx,TN,KB,User,Vars,CPU1), assert(findings(CPU1)).
							   

invokeOperationProc(Verbose,Fml,Ctx,TN,KB,User,Vars):-               % Default Left-over
             toFSCK(Fml,Ctx,TN,Assertion,SourceCtx,SourceTN),
             invokeOperation(Verbose,assert(Assertion),SourceCtx,SourceTN,KB,User,Vars).

invokeQueryWithTime(Q,Ctx,TN,KB,User,Vars,CPU):-
		invokeQueryAndWriteUserAgent(Q,Ctx,TN,KB,User,Vars,CPU),
		assert(query_time(CPU)).
		





