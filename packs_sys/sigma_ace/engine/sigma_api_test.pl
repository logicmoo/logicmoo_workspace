% ===================================================================
% File 'Sigma_api_test.pl' 
% Authors: Douglas Miles
% Contact: dmiles@teknowledge.com  apease@teknowledge.com
% Version: 'Sigma_api_test.pl' 1.0.0 
% History:
% Created - 2000/11/06 dmiles@teknowledge.com
% ===================================================================
% ===================================================================
% PURPOSE
% This File is the bootstrap for the Sigma Infence engine one first calls "[sigma_operation]"
% So first is loads the proper files and then starts up the system
% ===================================================================

%:-include('sigma_header.pl').

% ===================================================================
% EXPORTS
% ===================================================================


test_u_load_kif:-u_load_kif('IsApplicationExecutable.txt','test','tester').

/*

Saves state of a Context (crypt is a less human readable form) 
u_save(+FileName,+ToplevelContext) 
u_save_crypt(+FileName,+ToplevelContext) + Nearly Completed/Tested (actully just a QLF version =)  
*/
test_u_save:-u_save('testfile.pl','test').

	   
/*

Loads the saved state (crypted or not)  (adds assertions) they were originally stored with a context
u_load(+FileName) 
Clears a context (Leave unbound to clear all contexts) 
u_clear(?Context) 
We still have some discussion on how contexts are linked together so right now they are all active.. 
Mainly it gives us a way to do bulk load/unloads and separation user data from general system's like SUMO. 

*/

test_u_load:-u_load('testfile.pl').




/* Assertion 
u_assert(+KIFCharsIn,+Ctx,+KB,+User). 
KIFCharsIn = "(instance virginia Computer)"  
Ctx = 'Computers and Things' 
KB = 'Merge' currently must be set to 'Merge' 
User = 'dmiles@teknowledge.com'  

*/
	% u_assert(+KIFCharsIn,+Ctx,+KB,+User). 
test_u_assert:-u_assert("(instance hardwareType Relation)",'test','Merge','tester') .
test_u_assert1:-u_assert("(instance hardwareType Object)",'test','Merge','tester') .


/*
Now the predicates that you most want 
Invokes a query 
u_query(+KIFCharsIn,+Ctx,+KB,+User,-UResultsSoFar,-Result,-Proof,-Status). 

Input variables: 
KIFCharsIn = "(instance ?What Computer)"  
Ctx = 'Computers and Things'  % Any legal Atom that helps you book keep 
KB = 'Merge' currently must be set to 'Merge' 
User = 'dmiles@teknowledge.com'  There are times when error messages about redundant assertions happen.. and for bookkeeping all assertions have an author atom.. perhaps you can even use the source file name it came from.

output variables: 
UResultsSoFar = 0 indexed value of answer number.. +1 would mean it is a repeated answer with a different proof.. you could choose not to display answers with negative numbers 
Result = Is the Prolog Variable Vector like ['What'=virginia|_] (can be written with writeObject(getPrologVars(Result),Result))  
Proof = A proof object that can be printed with writeObject(proof(Proof),Result) 
Status = is either 'true' or '!' ... true = more results.. '!' means no more results 
(Status is optional.. if you completely remove this argument and it will fail at the next call after the first solution returning !)  
*/

		% u_query(+KIFCharsIn,+Ctx,+KB,+User,-UResultsSoFar,-Result,-Proof,-Status). 
test_u_query:-u_query("(hardwareType ?W ?U)",'test','Merge','tester',U,R,P),
	 writeObject(getPrologVars(R),R),writeObject(proof(P),R),fail.

test_u_query2:-
	 u_query("(hardwareType ?W ?U)",'test','Merge','tester',U,R,P),
	 not((U=(-1))),
	 writeObject(getPrologVars(R),R),
	 writeObject(proof(P),R),
	 fail.





/*
% Lets you discover any errors that were created durring any operation 
read_errors(Message) 
+Will fail if no errors are in this buffer 
+When any of the above commands are first invoked the error buffer is cleared 
+Whenever there were errors and you read this buffer each one is deleted while it is read 
+Messages are stored as printable objects writeObject(Message)  we can discuss a more programmatic approach to formats later based on how we want to use them. 
*/

show_errors:-read_errors(Message),writeObject(Message,_).

/*
 
 Term Writing

writeObject(+Object).
writeObject(+Object,+Vars). 

Allows easy printing of variables and objects
*/


 
/*
Inference Variables
 
Set Inference Optional Variable
u_set_option(VariableName=NewValue).
*/

u_set_option_test:-u_set_option(opt_timeout=2).

 
/*
Retrive Inference Optional Variable
u_get_option(VariableName=CurrentValue).
*/

u_get_option_test:-u_get_option(O),
	write_ln(O),fail.
u_get_option_test.
 
/*
The defaults are shown here
opt_timeout=5  The query system will try for 5 seconds before stopping (RealNumber in seconds)
opt_answers_min=1 The system will at minumum try to come up with 1 answer
opt_answers_max=99 The system will at maximun will try to come up with 99 answers
opt_backchains_max=50 will try 50 prolog backchains aprocimately 12 authorial backchains on each deduction
opt_deductions_max=100 The system will try to come up with 100 non unique answers before stooping
*/ 



