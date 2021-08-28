% ===================================================================
% ua_out(class(input),message(input),respoinse(output))
% generic call interface that was hooked into the inference engine with "ua_set_agent_callback(console_post)"
%This is not a predicate the useragent calls, but one that is called by the inference module to communicate  a question to the useragent or inform it of something.  
% The useragent decides if it can answer the a question and if not itself may ask a human user that is using it.
% There is three arguments to the my_callback predicate: Class, Message and Response
%
% Whenever the inference engine calls 'my_callback' only the first two arguments (Class,Message) are bound to supply information relevant to a Server invoked request.
%
% Class is a programmer defined message catagory  
% The Class is inteded to contain user defined message names that are sent as a callback function that is sent to the user's module consultation 
% Is is the type of Message catagory for the user agent.. A list of these are in TABLE 1.1 in <http://127.0.0.1/sigma_interface_advanced.html>
% (Class is always a ground Term)
%
% Message is a prolog term in the writeFmt defined by it's Class
% Each Class has a one known Message writeFmt shown in the table.   
% Message sometimes is ground term. 
%
%
% Response has normally has 2 response single_bindings: continue or abort
% This response is sent back to the inference_engine.
% If the inference_engine didn't receive 'abort', then it moves to the next stage in the command.
% 
% ===================================================================

:-include('sigma_header.pl').

% Assertion Time Errors

% Contradiction: The assertion contradicts other assertion(s) in the knowledge base. ; RAP note: this should be followed by a proof as per the XML element definition for "proof" 
% Syntax error: Illegal character in assertion 
% Syntax error: Unmatched parentheses in assertion 
% Syntax error: Missing parentheses in assertion 
% Syntax error: Unspecified 
% Argument type violation ; RAP note: this should be followed by a proof of the type violation as per the XML element definition for "proof" 
% Out of memory error 
% Broken socket: The connection between the web-based GUI and the inference engine is broken 
% Redundant assertion: ; RAP note: this should be followed by a proof of the type violation as per the XML element definition for "proof" 
% Undefined constant: Do you wish to add the constants to the KB? ; RAP note: this should be followed by a list of constants and a prompt to the user 


% ==========================================================
%  Sending Notes
% ==========================================================
%writeDebug(T):-!.  writeDebug(C,T):-!.
 
%writeDebug(T):-sigma_notrace(isSigmaOption(opt_debug=off)),!.
%writeDebug(C,T):-sigma_notrace(isSigmaOption(opt_debug=off)),!.

ttc(X):-thread_signal(X,trace).
ttc2:-thread_signal(2,trace).
ttc4:-thread_signal(4,trace).



writeModePush(Push):-!.

writeModePop(Pop):-!.

writeDebug(T):- dmsg(T),!.

writeDebug(T):-!,
	sigma_notrace((
	if_prolog(swi,
		(prolog_current_frame(Frame),
		prolog_frame_attribute(Frame,level,Depth),!,
		Depth2 = (Depth-25))),
	if_prolog(bp,Depth=2),
	writeFmt(';;',[T]),!,
	indent_e(Depth2),!,
	writeFmt('~q\n',[T]))),!.

indent_e(X):- catch((X < 2),_,true),write(' '),!.
indent_e(X):-XX is X -1,!,write(' '), indent_e(XX).

writeDebug(C,T):- dmsg(C,T),!.
writeDebug(C,T):-!,
	sigma_notrace((
	writeFmt('<font size=+1 color=~w>',[C]),
	writeDebug(T),
        writeFmt('</font>',[]))),!.

dumpstack_argument(T):-isSigmaOption(opt_debug=off),!.  
	
dumpstack_argument(Frame):-
	write(frame=Frame),write(' '),
	dumpstack_argument(1,Frame).

dumpstack_argument(1,Frame):-!,
	prolog_frame_attribute(Frame,goal,Goal),!,
	write(goal=Goal),write('\n').
	
dumpstack_argument(N,Frame):-
	prolog_frame_attribute(Frame,argument(N),O),!,
	write(N=O),write(' '),
	NN is N +1,
	dumpstack_argument(NN,Frame).
	
dumpstack_argument(N,Frame):-!,write('\n').
	
:-dynamic(mods/1).

write_response_begin:-!.
write_response_end:-!.

sendNote(X):-var(X),!.
sendNote(X):-mods(X),!.
%sendNote(X):-!,assert(mods(X)).
sendNote(X).			 

sendNote(To,From,Subj,Message):-sendNote(To,From,Subj,Message,_).

sendNote(To,From,Subj,Message,Vars):-
	not(not((sigma_numbervars((To,From,Subj,Message,Vars)),
	%writeDebug(sendNote(To,From,Subj,Message,Vars)),
	catch(sendNote_1(To,From,Subj,Message,Vars),E,
	writeFmt('send note ~w ~w \n <HR>',[E,sendNote(To,From,Subj,Message,Vars)]))))).


sendNote_1(To,From,Subj,surf,Vars):-!.
sendNote_1(To,From,[],surf,Vars):-!.
sendNote_1(To,From,[],end_of_file,Vars):-!.
sendNote_1(doug,From,_,_,Vars):-!.
sendNote_1(extreme_debug,From,_,_,Vars):-!.
sendNote_1(debug,'Inference',_,_,Vars):-!.

%sendNote_1(canonicalizer,From,Subj,Message,Vars):-!.


sendNote_1(canonicalizer,From,Subj,Message,Vars):-
            toMarkUp(kif,From,Vars,SFrom),
            toMarkUp(kif,nv(Subj),Vars,SS),
            toMarkUp(kif,nv(Message),Vars,SA),
            writeFmt('<font color=red>canonicalizer</font>: ~w "~w" (from ~w). \n',[SA,SS,SFrom]),!.

/*

sendNote_1(debug,From,Subj,Message,Vars):- %isSigmaOption(disp_notes_nonuser=on),!,
            toMarkUp(kif,From,Vars,SFrom),
            toMarkUp(kif,Subj,Vars,SS),
            toMarkUp(kif,Message,Vars,SA),
            writeFmt('% debug: ~w "~w" (from ~w). \n',[SA,SS,SFrom]).
sendNote_1(debug,From,Subj,Message,Vars):-!.
*/


sendNote_1(To,From,Subj,Message,Vars):- isSigmaOption(client=consultation),  !, 
            toMarkUp(kif,To,Vars,STo),
            toMarkUp(kif,From,Vars,SFrom),
            toMarkUp(kif,nv(Subj),Vars,S),
            toMarkUp(kif,nv(Message),Vars,A),
            fmtString(Output,'~w (~w from ~w) ',[A,S,SFrom]),
	    sayn(Output),!.

sendNote_1(To,From,'Rejected',Message,Vars):- isSigmaOption(client=automata),  !.

sendNote_1(To,From,Subj,Message,Vars):- isSigmaOption(client=automata),  !, 
            toMarkUp(kif,To,Vars,STo),
            toMarkUp(kif,From,Vars,SFrom),
            toMarkUp(kif,nv(Subj),Vars,S),
            toMarkUp(kif,nv(Message),Vars,A),
            writeFmt(user_error,'% ~w (~w from ~w) ',[A,S,SFrom]).

sendNote_1(To,From,Subj,Message,Vars):- isSigmaOption(client=html),  !, %  In Html
            toMarkUp(kif,To,Vars,STo),
            toMarkUp(kif,From,Vars,SFrom),
            toMarkUp(kif,nv(Subj),Vars,S),
            toMarkUp(html,nv(Message),Vars,A),
            writeFmt('<hr><B>To=<font color=green>~w</font> From=<font color=green>~w</font> Subj=<font color=green>~w</font></B><BR>~w\n',[To,From,S,A]),!.

sendNote_1(To,From,Subj,Message,Vars):- isSigmaOption(client=console),!, % In KIF
            toMarkUp(kif,To,Vars,STo),
            toMarkUp(kif,From,Vars,SFrom),
            toMarkUp(kif,nv(Subj),Vars,SS),
            toMarkUp(kif,nv(Message),Vars,SA),
            writeFmt(user_error,'; ~w: ~w "~w" (from ~w). \n',[STo,SA,SS,SFrom]),!.
  
sendNote_1(To,From,Subj,Message,Vars):-  % In KIF
            toMarkUp(kif,To,Vars,STo),
            toMarkUp(kif,From,Vars,SFrom),
            toMarkUp(kif,nv(Subj),Vars,SS),
            toMarkUp(kif,nv(Message),Vars,SA),
            writeFmt(user_error,'; ~w: ~w "~w" (from ~w). \n',[STo,SA,SS,SFrom]),!.

sendNote(To,From,Subj,Message,Vars):-!.


writeDebugS(X):-writeq(X),nl.

logOnFailureIgnore(X):- !,debugOnFailure(X).
logOnFailureIgnore(X):- ignore(logOnFailure(X)),!.

logOnFailure(X):- !,debugOnFailure(X).
logOnFailure(assert(X,Y)):- catch(assert(X,Y),_,Y=0),!.
logOnFailure(assert(X)):- catch(assert(X),_,true),!.
logOnFailure(assert(X)):- catch(assert(X),_,true),!.
%logOnFailure(X):-catch(X,E,trace),!.
logOnFailure(X):-catch(X,E,(writeFailureLog(E,X),!,catch((trace,X),_,fail))),!.
logOnFailure(X):- writeFailureLog('Predicate Failed',X),!.


writeFailureLog(E,X):-
		writeFmt(user_error,'\n% error:  ~q ~q\n',[E,X]),flush_output(user_error),!,
		%,trace.
		writeFmt('\n;; error:  ~q ~q\n',[E,X]),!,flush_output. %,say([E,X]).
		
/*
debugOnFailure(assert(X,Y)):- catch(assert(X,Y),_,Y=0),!.
debugOnFailure(assert(X)):- catch(assert(X),_,true),!.
debugOnFailure(assert(X)):- catch(assert(X),_,true),!.
*/
%logOnFailure(X):-catch(X,E,trace),!.
debugOnFailure(X):-catch(X,E,(writeFailureLog(E,X),fail)),!.
debugOnFailure(X):-trace,X.

debugOnFailure(arg_domains,CALL):-!,logOnFailure(CALL),!.
debugOnFailure(Module,CALL):-debugOnFailure(CALL),!.


noDebug(CALL):-CALL.
	


%unknown(Old, autoload).

% ================================================================
%   Serialize Objects to XML
% ================================================================


%%writeObject(OBJ,Vars):-!. %,writeq(OBJ),!.
%writeObject(OBJ,Vars):-!,catch(writeq(OBJ),_,true),nl,!.

writeObject(quiet,Term,Vars):-!.

writeObject(Verbose,Term,Vars):-writeObject(Term,Vars).

		
writeObject(OBJ,Vars):- isSigmaOption(client=html),!,
		((toMarkUp(html,OBJ,Vars,Chars),write(Chars))),!.
		
writeObject(OBJ,Vars):- isSigmaOption(client=atomata),!,
		((toMarkUp(kif,OBJ,Vars,Chars),write(Chars))),!.

writeObject(OBJ,Vars):- isSigmaOption(client=console),!,
		((toMarkUp(kif,OBJ,Vars,Chars),write(Chars))),!.

writeObject(OBJ,Vars):- isSigmaOption(client=consultation),!,
		(say(OBJ,Vars)),!.

writeObject(OBJ,Vars):- !,
		((toMarkUp(kif,OBJ,Vars,Chars),write(Chars))),!.


writeObject_conj(A,Vars):-isSlot(A),!,
	writeObject(A,Vars).

writeObject_conj(and(A,true),Vars):-!,
	writeObject_conj(A,Vars).

writeObject_conj(and(true,A),Vars):-!,
	writeObject_conj(A,Vars).

writeObject_conj(and(A,B),Vars):-!,
	writeObject_conj(A,Vars),
	writeObject_conj('\n\n Also \n\n ',Vars),
	writeObject_conj(B,Vars).

writeObject_conj(Output,Vars):-
	%write(Output),nl.
	writeObject(Output,Vars).


:-dynamic(resolve_skolem/2).


ua_out(C,P):-once_ignore(writeUAEvent(C,P,_)).

ua_out(C,M,Vars):-once_ignore(writeUAEvent(C,M,Vars)).



/*

15 Display options

disp_query
disp_note_user
disp_notes_nonuser
disp_compile
disp_modification
disp_debug
disp_trace
disp_answers
disp_proof_true
disp_truth_othe r
disp_result
disp_statistics
disp_errors
disp_success
disp_other

10 options

opt_answers_max=[0-n] Default is Unlimited
opt_answers_min=[0-n]  Default is 1
opt_backchain_depth_max=[0-n] Default is 10
opt_inferSurface_max=[0-n] Default is 1000
opt_timeout=[seconds] Default=60
opt_readonly=[true|false] Default is False
opt_deduce_assert=[true|false] Defualt is True
opt_language=[pnx_nf|getNegationForm|hylog|prolog|sigmese]   Default is Sigmese
opt_format=[kif|prolog]  Default is KIF
opt_compiled=[true|false]  Default is false

3 Callbacks

cb_error=[true|false] Default is false
cb_answer=[true|false] Default is false
cb_consult=[true|false] Default is false


*/


setSigmaOptionExpliciteWriteSettings:-
             setSigmaOption(disp_explicit='off'),
             setSigmaOption(disp_modification='off'),
             setSigmaOption(disp_debug='off'),
             setSigmaOption(disp_note_user='off'),
             setSigmaOption(disp_notes_nonuser='off'),
             setSigmaOption(disp_qresults='off'),
             setSigmaOption(disp_proof_true='off'),
             setSigmaOption(disp_proof_other='off'),
             setSigmaOption(disp_bindings='off'),
             setSigmaOption(disp_answers_num_yes='off'),
             setSigmaOption(disp_answers_num_no='off'),
             setSigmaOption(disp_answers_num_definate='off'),
             setSigmaOption(disp_answers_num_tries='off'),
             setSigmaOption(disp_cputime='off'),
             setSigmaOption(disp_compiled='off'),
             setSigmaOption(disp_ground_forms='off').

setSigmaOptionImplicitWriteSettings:-
             setSigmaOption(disp_explicit='off'),
             setSigmaOption(disp_modification='on'),
             setSigmaOption(disp_debug='on'),
             setSigmaOption(disp_note_user='on'),
             setSigmaOption(disp_notes_nonuser='on'),
             setSigmaOption(disp_proof_true='on'),
             setSigmaOption(disp_proof_other='off'),
             setSigmaOption(disp_bindings='on'),
             setSigmaOption(disp_answers_num_yes='on'),
             setSigmaOption(disp_answers_num_no='on'),
             setSigmaOption(disp_answers_num_definate='on'),
             setSigmaOption(disp_answers_num_tries='on'),
             setSigmaOption(disp_cputime='on'),
             setSigmaOption(disp_compiled='on'),
             setSigmaOption(disp_ground_forms='on').


setSigmaOption([]):-!.
setSigmaOption([H|T]):-!,
      setSigmaOption(H),!,
      setSigmaOption(T),!.
setSigmaOption(Var=_):-var(Var),!.
setSigmaOption(_=Var):-var(Var),!.
setSigmaOption((N=V)):-nonvar(N),!,setSigmaOption_thread(N,V),!.
setSigmaOption(N):-atomic(N),!,setSigmaOption_thread(N,true).
	
setSigmaOption(Name,Value):-setSigmaOption_thread(Name,Value).
setSigmaOption_thread(Name,Value):-
	sigma_notrace((thread_self(Thread),
	retractall('$SigmaOption'(Thread,Name,_)),
	asserta('$SigmaOption'(Thread,Name,Value)),!)).


unsetSigmaOption(Name=Value):-nonvar(Name),
	unsetSigmaOption_thread(Name,Value).
unsetSigmaOption(Name):-nonvar(Name),
	unsetSigmaOption_thread(Name,_).
unsetSigmaOption(Name):-sigma_notrace(retractall('$SigmaOption'(Thread,Name,Value))).


unsetSigmaOption_thread(Name):-
	unsetSigmaOption_thread(Name,Value).

unsetSigmaOption_thread(Name,Value):-
	thread_self(Thread),
	retractall('$SigmaOption'(Thread,Name,Value)).
	
getSigmaOption_nearest_thread(Name,Value):-
	getSigmaOption_thread(Name,Value),!.
getSigmaOption_nearest_thread(Name,Value):-
	'$SigmaOption'(_,Name,Value),!.
getSigmaOption_nearest_thread(Name,Value):-!.



isSigmaOption(Name=Value):-!,isSigmaOption(Name,Value).
isSigmaOption(Name):-!,isSigmaOption(Name,true).

isSigmaOption(Name,Value):-getSigmaOption_thread(Name,Value).

getSigmaOption_thread(Name,Value):-
	sigma_notrace((thread_self(Thread),
	'$SigmaOption'(Thread,Name,Value))),!.


getSigmaOption(Name=_Default,Value):-nonvar(Name),!,ensureSigmaOption(Name,Default,Value).
getSigmaOption(Name=Value):-nonvar(Name),!,ensureSigmaOption(Name,_,Value).
getSigmaOption(Name,Value):-nonvar(Name),!,ensureSigmaOption(Name,_,Value).


ensureSigmaOption(Name=Default,Value):-
	ensureSigmaOption(Name,Default,Value),!.
	
ensureSigmaOption(Name,Default,Value):-
	getSigmaOption_thread(Name,Value),!.
ensureSigmaOption(Name,Default,Default):-
	setSigmaOption_thread(Name,Default),!.
ensureSigmaOption(Name,Default,Value):-nonvar(Name),!,   
	setSigmaOption_thread(Name,Value),!.
ensureSigmaOption(Name,Default,Default).




write_val(Any,Vars):- isSigmaOption(client=html)
      -> write_val_xml(Any,Vars) ;
      write_sterm(Any,Vars).
      
write_val_xml(Any,Vars):-
      toMarkUp(leml,Any,Vars,Chars),write(Chars),nl.


         
writeUAEvent(_,_,_):-isSigmaOption(disp_hide_all=true),!.
writeUAEvent(_,_,_):-telling_file,!.
writeUAEvent(Class,_,_):-isSigmaOption(Class=false),!.
writeUAEvent(Class,_,_):-isSigmaOption(disp_explicit=true),not(isSigmaOption(_Class=true)),!.

writeUAEvent(query_start,Note,Vars):-!,
         (isSigmaOption(client=html) -> 
          (writeFmt('<Answer>\n'),le_push('Answer'));
          true).

writeUAEvent(query_end,(Result,Normal,Elapsed,Num,Bindings),Vars):-!, 
                  (isSigmaOption(client=html) -> 
                     ((    
                       (toMarkUp(leml,note('user',logicEngine,Result,(Result,Normal,Elapsed,Num,Bindings)),Vars,Chars),once((var(Chars);write(Chars)))),
                       writeFmt('<Summary result="~w" solutions="~d" bindings="~d" cpu="~f"/>\n</Answer>\n',[Result,Num,Bindings,Elapsed]),
                       le_pull('Answer')
                     ));
                       writeFmt('\n%%  ~w solutions="~d" bindings="~d" cpu="~f"\n',[Result,Num,Bindings,Elapsed])).

writeUAEvent(Class,Message,Vars):-not(isSigmaOption(client=html)),!, toMarkUp(kif,[Class,Message],Vars,Chars),write(Chars),nl.
writeUAEvent(Class,Message,Vars):-isSigmaOption(client=html),!, event_to_chars(leml,Class,_Message,Vars,Chars),write(Chars),nl.
writeUAEvent(cb_consultation, assertion([PredicateI|ConsultTemplate],_Context_atom,_SN), continue):- 
               agentConsultation(_Context_atom,[PredicateI|ConsultTemplate], _ListOfGafsAsserted).
writeUAEvent(_,_,_):-!.


/*toMarkUp(Sterm,VS,Chars):-
           once(( isSigmaOption(client=html) -> 
            toMarkUp(leml,Sterm,VS,Chars);
            toMarkUp(kif,Sterm,VS,Chars))).
  */

/*
Where the parameters are some string syntax or other straightforward data
structure and we've used I to signify a parameter that is used by the
function and O to signify a parameter that is returned by the
function.  If that were forall it had, we think that is sufficient for
the bulk of interactions.  Everything else is helpful but not strictly
essential.  Because of that, we believe that it is possible to run
our system with just the above commands after startup.

   We have shown a number of features implemented such as

  - proof trees
  - inference execution time and search controls
  - compilation
  - consultation mode

The expanded API is
*/          
%=================================================================
%  CONSULTATION MANAGEMENT DIRECTIVES
%=================================================================

/*
where the xxxNative versions take the disp_modification WFSform and the other
versions take SUMO.  Consultation mode has a simple default interface too:
*/



/* ; where the list is of arguments
missing that is requested from the user.  The default is to ask for
any and forall arguments that are missing

%TODO

ua_consultationModeEvery() ; ask the user for as many inputs as he's willing

to give
etc. ; other modes...

A further expansion to handle communication with a user agent external to
Prolog would add a message sent to a socket that process is listening to.
and a message string sent from Prolog to the user agent to request user input

"userInputRequest predicateName<cr>"

Where <cr> indicates a carriage return or some other suitable delimiter.

*/

:-setSigmaOptionImplicitWriteSettings.



