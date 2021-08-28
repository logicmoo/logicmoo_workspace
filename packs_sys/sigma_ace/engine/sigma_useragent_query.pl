% ===================================================================
%  INVOKE QUERY INTERFACE USED BY REQUEST AGENTS
% ===================================================================

%:-include('sigma_header.pl').

% =======================================================================================

invokeQueryAndWriteUserAgent(Formula,Ctx,TrackingAtom,KB,User,Vars,CPU):-
	 invokeQueryToBuffer(Formula,Ctx,TrackingAtom,KB,User,Vars,CPU),
	 writeUserAgentBuffer.

	
% ===========================================================
% Cite Buffer
% ===========================================================
writeUserAgentBuffer:-%trace,  
	retract(queryBuffer_db(UResultsSoFar,Result,Proof,Status)),
	once(writeAnswersUserAgent(UResultsSoFar,Result,Proof,Status)),fail.

% Call to write Summary
writeUserAgentBuffer:-
	final_answer(Logic:How),
	writeDebug(final_answer(Logic:How)),
	writeAnswersUserAgent(How, ['Summary'=Logic|_G14093],final_answer(Logic:How),final_answer(Logic:How) ).

writeUserAgentBuffer:-!.	
	

% ===========================================================
% Send to debugger
% ===========================================================
writeAnswersUserAgent(UResultsSoFar,Result,InProof,Status):-
	sigma_notrace((once(writeDebug(writeAnswersUserAgent(UResultsSoFar,Result,InProof,Status))),fail)).
	
% ===========================================================
% Hide certain returns
% ===========================================================
writeAnswersUserAgent(-1,Result,Proof,Status).

writeAnswersUserAgent(0, ['Result'=none|A], 'Unproven', done(possible:searchfailed)).
writeAnswersUserAgent(_, ['Result'=true|A], found(_), done(true:_)).

% ===========================================================
% Write Summaries
% ===========================================================
writeAnswersUserAgent(_, ['Summary'=true|_G5892], _, final_answer(Logic:NA)):-
	writeObject('$spacer',Vars), retain_yes,
	query_total_time(Total),
	writeObject(nv([' Yes. (Found ',NA,Logic,' in ',Total,' seconds.)']),['Total'=Total,'Answers'=NA,'Logic'=Logic]),!.

writeAnswersUserAgent(Proof, ['Summary'=false|_G5282], final_answer(Logic:Query:P), _):-   
	writeObject('$spacer',Vars), retain_no,
	query_total_time(Total),
	writeObject(nv([' No. (Found ',Logic,' in ',Total,' seconds.)']),['Total'=Total,'Answers'=NA,'Logic'=Logic]),!.

writeAnswersUserAgent(searchfailed, ['Summary'=Logic|_G4695], final_answer(possible:searchfailed), final_answer(possible:searchfailed)):-
	writeObject('$spacer',Vars), retain_unproven,
	query_total_time(Total),
	writeObject(nv([' Unproven. (Found no answers in ',Total,' seconds.)']),['Total'=Total,'Answers'=NA,'Logic'=Logic]),!.

% ===========================================================
% Write Answers
% ===========================================================
:-dynamic(show_all_proofs/0).


/*
writeAnswersUserAgent(UResultsSoFar,Result,InProof,Status):-
	format('<b>~q.</b>',writeAnswersUserAgent(UResultsSoFar,Result,InProof,Status)),!.
*/
	
writeAnswersUserAgent(UResultsSoFar,Result,InProof,Status):-not(show_all_proofs),
	writeObject('$spacer',Vars), 
	%writeObject(writeAnswersUserAgent(UResultsSoFar,Result,InProof,Status),Vars),nl,
	writeObject(getPrologVars(Result),Vars),
	length_proof(InProof,InLength),
	findall(Length-Proof,
		(retract(queryBuffer_db(_,Result,Proof,_)),
		 length_proof(Proof,Length)
		 ),KeyList),
	keysort([(InLength-InProof)|KeyList],[(_-ChoiceProof)|_]),
	writeObject(proof(ChoiceProof),Result).

writeAnswersUserAgent(UResultsSoFar,Result,InProof,Status):-show_all_proofs,
	writeObject('$spacer',Vars), 
	writeObject(getPrologVars(Result),Vars),!,
	writeObject(proof(InProof),Vars),!.

retain_no:-retain_answer('No'),retain_answer('no'). %,retain_answer('Unproven').
retain_yes:-retain_answer('Yes'),retain_answer('yes'). % ,retain_answer('Unproven').
retain_unproven:-retain_answer('Unproven'),retain_answer('unproven'). % ,retain_answer('Unproven').

	

length_proof(List,Len):-length(List,Len),!.
length_proof(deduced,3):- !.
length_proof(P * Proof,Length):- !,
	length_proof(P,PLength),
	length_proof(Proof,ProofLength),
	Length is PLength + ProofLength,!.
length_proof(_,1):- !.
	

setSigmaOptionDefaults:-
             (unsetSigmaOption(_)),
             setSigmaOption(opt_callback='sendNote'),
             setSigmaOption(cb_consultation='off'),
             setSigmaOption(opt_debug='off'),
             setSigmaOption(cb_error='off'),
             setSigmaOption(cb_result_each='off'),
             
% User Agent Defaults for Blank Variables
             setSigmaOption(opt_cxt_query='ToplevelContext'),
             setSigmaOption(opt_ctx_assert='ToplevelContext'),
             setSigmaOption(opt_tracking_number='generate'),
             setSigmaOption(opt_agent='ua_parse'),
             setSigmaOption(opt_precompiled='off'),
             getDefaultKB(KB),setSigmaOption(opt_kb=KB),
             setSigmaOption(opt_notation='kif'),
             setSigmaOption(opt_timeout=2),
             setSigmaOption(opt_readonly='off'),
             setSigmaOption(opt_debug='off'),
             setSigmaOption(opt_compiler='Byrd'),
             setSigmaOption(opt_language = 'pnx_nf'),
             
%Query Limits
             setSigmaOption(opt_answers_min=1),
             setSigmaOption(opt_answers_max=999), %TODO Default
             setSigmaOption(opt_backchains_max=5),
             setSigmaOption(opt_deductions_max=100),
             setSigmaOption(opt_backchains_max_neg=5),
             setSigmaOption(opt_deductions_max_neg=20),
             setSigmaOption(opt_forwardchains_max=1000),
             setSigmaOption(opt_max_breath=1000), %TODO Default

%Query Contexts
             setSigmaOption(opt_explore_related_contexts='off'),
             setSigmaOption(opt_save_justifications='off'),
             setSigmaOption(opt_deductions_assert='on'),
             setSigmaOption(opt_truth_maintence='on'),
             setSigmaOption(opt_forward_assertions='on'),
             setSigmaOption(opt_infer_domains='on'),
	     setSigmaOption(opt_notice_not_say=off),

             
%Query Pobibility
             setSigmaOption(opt_certainty_max=1),
             setSigmaOption(opt_certainty_min=1),
             setSigmaOption(opt_certainty=1),
             setSigmaOption(opt_resource_commit='on').

unset_promiscuous:- 
		setSigmaOption(opt_infer_domains='off').
set_promiscuous:- 
		setSigmaOption(opt_infer_domains='on').


