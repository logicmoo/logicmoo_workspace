
 
/*************************************************************************

         name: update_rules.pl 
  description: GoDiS-AOD update rules
      authors: Original code (June 2002) by Staffan Larsson (SL)
               Modifications by SL and David Hjelm (DH)

*************************************************************************/

/************************************************************************

HISTORY, started 030727

[SL:] After my thesis was done, I went back to "quick bugfix" mode
which resulted in lots of unnecessary hacks. Some of these bugs fixed
were bugs in trindikit rather than in godis.

However, some features have also been added, e.g. the "up" request.

021125 (SL) (retroactively added here by SL 050531)
- added plan construct change_domain

030131 (DH) (retroactively added here by SL 050531)
- added simple booleans to plan constructs if_then, if_then_else
- added plan constructs protect, forget, forget_except

0303 (DH)
- modified exec_devQuery tp pass $/shared/com to device, like dev_do

030520 (SL) (retroactively added here by SL 050531)
- added plan construct exec_bind

030727 (SL)
- simplified integration rules for ask, answer, request; they are now more
  similar to the description in IBDM thesis.
- removed a hack rule for integrating requests for "up" and "top" actions
- fixed the goUp rule; this did not work properly

030822 (SL)
- fixed rejectProp so it does not produce pos*und feedback; the system
could not handle non-positive response to this.

031016 (DH)
- added plan construct assume(P) which adds P to private/bel

031022 (SL)
- modified integrateUsrAccNegICM so that rejected(Q) is added to shared/com;
this can be used to prevent the question being asked again
- modifed integrateUsrAnswer so that multiple answers to the same question can be given in one turn; this was previously prohibited to avoid problems when recognition goes badly wrong; a new solution is needed which distinguishes self-corrections, and the recognition problem should be solved by the recognition grammar.

031024 (SL)
- added exec_assume_issue, similar to exec_assume but for questions
- commented out clearing of shared/actions in exec_forget - it didn't make any sense

031124 (DH)
- fixed exec_sequence so that sequences can be empty lists
- fixed downdateIssues2 - now only triggers when ISSUES has > 1 elements


031125 (SL)
- exec_forget_except now puts top plan on actions, like exec_forget_all

031201 (DH)
- modified rule integrateSysQuit; sets program_state to idle instead of quit

040108 (SL)
- added condition to integrateUsrAnswer: in( $latest_moves, answer(A) ).
  If the answer was not performed in latest utterance, the score is not valid and selectUndIntAnswer should trigger instead.
- changed selectIcmUndIntAnswer so it does not require matching Q on QUD, only on ISSUES; previously it did not work.
- NOTE: these changes fixed a bug that arose from separating out selection of und-int ICM from the integrateUsrAnswer rule. In IOD, the old solution is still used.

040115 (SL)
- added rule downdateActions2 to remove menu-chioce altQ in case the altQ was answered "no"; see rule for additional comments 

040209 (SL)
- moved removeFindout and removeRaise so that they are tried after exec_forget. This way
  removeFindout/Raise will not remove the actions findout(X^p(X)) and raise(X^p(X)) if forget(p(X))
  is first in the plan sequence. If implementation of removeFindout/Raise is changed so that
  they only look at first element of plan, forget actions can occur anywhere,
  yielding the same result.
  This will probably will make update algorithm (slightly) more efficient too,
  since removeFindout/Raise do not check all elems in plan.
  
040213 (SL)
- modified noFollowup so it takes care of actions properly; before, they were added to COM instead of ACTIONS in tmp/usr.

050302 (SL)
- added plan construct dev_queryAll which finds all answers to a query in a device

0504 (DH)
- modified to work with trindikit4

050627 (DH) 
- removed dependency on lexicon resource by replacing all $lexicon::yn_answer(A) with (A=yes or A=no)

050629 (SL)
- added exec_assume_shared to add propositions to SHARED/COM (exec_assume only adds to PRIVATE/BEL)
- When downdating ISSUES with Q, resolved(Q) is now added to SHARED/COM. This affects the downdateIssue rule. This makes it possible to include action postconditions of the form resolved(Q). The corresponding change has not been made for downdateIssues2 and 3, which deal with issue- and action-questions.

050630 (SL)
- Revised integrateUsrRequest to allow intelligent interpretation of request htypernyms, i.e. requests that mean different things depending on the current topmost goal action. This required adding a new predicate "hypernym" to the domain resource. Also, some changes were made to irrelevantFollowup and rejectAction to stop them from catching hypernym requests.

***************************************************************************/
:- discontiguous rule_class/2, rule/3, '==>'/2.


%trindikit 3 or trindikit4
:- ( current_module(tkit_properties)->
       use_module( trindikit(tkit_operators)); %trindikit4
       use_module( library(tis_operators) ) ). %trindikit3


rule_class( getLatestMoves, grounding ).

rule_class( retract, integrate ).

rule_class( integrateUsrReraise, integrate ).
rule_class( integrateUsrAsk, integrate ).
rule_class( integrateSysAsk, integrate ).

rule_class( integrateUsrRequest, integrate ).
rule_class( integrateConfirm, integrate ).
rule_class( integrateReport, integrate ).

% answers 
rule_class( integrateNegIcmAnswer, integrate ).
rule_class( integratePosIcmAnswer, integrate ).
rule_class( integrateUsrAnswer, integrate ).
rule_class( integrateSysAnswer, integrate ).
% ICM moves
rule_class( integrateAccommodationICM, integrate ).
rule_class( integrateUndPosICM, integrate ).
rule_class( integrateUndIntICM, integrate ).
rule_class( integrateUsrPerNegICM, integrate ).
rule_class( integrateUsrAccNegICM, integrate ).
rule_class( integrateOtherICM, integrate ).
% other moves
rule_class( integrateGreet, integrate ).
rule_class( integrateSysQuit, integrate ).
rule_class( integrateUsrQuit, integrate ).
rule_class( integrateNoMove, integrate ).

rule_class( accommodateIssues2QUD, accommodate ).
rule_class( accommodateQUD2Issues, accommodate ). 
rule_class( accommodatePlan2Issues, accommodate ).
rule_class( accommodateCom2Issues , accommodate ).
rule_class( accommodateCom2IssuesDependent, accommodate ).
rule_class( accommodateDependentIssue, accommodate ).
rule_class( accommodateDependentAction, accommodate ).

rule_class( downdateQUD, downdate_qud ).
rule_class( downdateISSUES, downdate_issues ).
rule_class( downdateISSUES2, downdate_issues ).
rule_class( downdateISSUES3, downdate_issues ).
rule_class( downdateActions, downdate_issues ).
rule_class( downdateActions2, downdate_issues ).

rule_class( recoverPlan, exec_plan ).
rule_class( recoverActionPlan, exec_plan ).
rule_class( findPlan, load_plan ).
rule_class( findPlan, exec_plan ).
rule_class( findActionPlan, load_plan ).
rule_class( goUp, load_plan ).



rule_class( exec_consultDB, exec_plan ).
rule_class( exec_if_then, exec_plan ).
rule_class( exec_if_then_else, exec_plan ).
rule_class( exec_sequence, exec_plan ).
rule_class( exec_bind, exec_plan). 
rule_class( exec_assume, exec_plan).
rule_class( exec_assume_shared, exec_plan).
rule_class( exec_assume_issue, exec_plan ).
rule_class( exec_protect, exec_plan).
rule_class( exec_forget_all, exec_plan ).
rule_class( exec_forget, exec_plan ).
rule_class( exec_forget_except, exec_plan ).
rule_class( exec_dev_get, exec_plan ).
rule_class( exec_dev_set, exec_plan ).
rule_class( exec_dev_do, exec_plan ).
rule_class( exec_dev_query, exec_plan ).
rule_class( exec_dev_queryAll, exec_plan ).
rule_class( exec_mode_only_speech1, exec_plan ). % Tram
rule_class( exec_mode_only_speech2, exec_plan ). % Tram
rule_class( exec_mode_only_graph, exec_plan ). % Tram
rule_class( exec_mode_both_speech_and_graph, exec_plan ). % Tram
rule_class( exec_set_speech_value, exec_plan ). % Tram
rule_class( exec_set_graph_value, exec_plan ). % Tram
rule_class( exec_change_language, exec_plan ).
rule_class( exec_change_domain, exec_plan ).

rule_class( removeFindout, exec_plan ).
rule_class( removeRaise, exec_plan ).



rule_class( selectIcmUndIntAsk, select_action ).
rule_class( selectIcmUndIntAnswer, select_action ).
rule_class( selectIcmUndIntRequest, select_action ).
rule_class( rejectIssue, select_action ).
rule_class( rejectAction, select_action ).
rule_class( rejectProp, select_action ).

%%%%%%%%%%%

rule( getLatestMoves,
      [ $latest_moves = LatestMovesQueue,
	$latest_speaker = DP,
	$score = Score,    
	$/shared/lu/moves = PrevMoves ],
      [ %set( /shared/lu/moves, oqueue(M) ), % grounding
	! $/private/nim = OldMoves,
	clear( /private/nim ),
	forall_do( in( LatestMovesQueue, M ), push( /private/nim, pair(DP, M) ) ),
	append( /private/nim, OldMoves ), % old moves tried last
	init_shift( /private/nim ),
	set( /shared/lu/speaker, DP ),
	clear( /shared/lu/moves ),
	set( /shared/pm, PrevMoves ),
	if_do( $$arity( $/shared/move_history ) == 6,
	       pop( /shared/move_history )),
	push( /shared/move_history, LatestMovesQueue ),
	if_do( DP = usr,                       
	       if_then_else( Score > 0.9, 
		             speechinput := min,
		             speechinput := max )
	     )
       ] ).


% Copy parts of IS to /private/tmp before integrating user utt
% if it's worth repeating in case usr does icm:per*neg later

rule( backupSharedUsr,
       [ $/shared/lu/speaker = usr,
	 $latest_moves = Moves,
	 % not if Moves contains contentful icm move
	 not in( Moves, icm:_:_ ),
	 % not if Moves contains reply to contenful icm move, or is no_move
	 not ( fst($/shared/qud, und(usr*GQ)) and
	     ( ( ( in( Moves, answer( A ) ) and $domain :: relevant( A, GQ ) ) or
	       ( in( Moves, ask( Q ) ) and $domain :: relevant( issue(Q), GQ ) ) ) or in( Moves, no_move ) ) )
	     ],
      [ /private/tmp/usr/qud := $/shared/qud,
	 /private/tmp/usr/issues := $/shared/issues,
	 /private/tmp/usr/actions := $/shared/actions,
	 /private/tmp/usr/com := $/shared/com,
	 /private/tmp/usr/agenda :=  $/private/agenda,
	 /private/tmp/usr/plan := $/private/plan ] ).


% irrelevant followup to ask-move or interoggative icm -> retract optimistic grounding
% except for grounding-questions?
rule( irrelevantFollowup,
      [ $latest_moves = Moves, 
	$/shared/lu/speaker == usr,
	not empty( Moves ),
	% non-interrogarive user icm is always relevant
	not( Moves/elem = icm:per*neg  ),
	not ( Moves/elem = icm:acc*neg:issue ),
	not ( Moves/elem = icm:acc*pos ),
	% previous move was an ask-move or interrogative icm
	in( $/shared/pm, QMove ),
	QMove = ask( Q1 ) or QMove = icm:und*int:usr*Q1,
	% the following followups are relevant to Q1:
	% asking a question on which Q1 depends
	not ( Moves/elem = ask( Q ) and $domain :: depends( Q1, Q ) ),
	% providing a relevant answer to Q1
	not ( Moves/elem = answer( A ) and $domain :: relevant( A, Q1 ) ),
	% requestion an action relevant to Q1 (in which case Q1 must be an action-question) 
	not ( Moves/elem = request( Act ) and $domain :: relevant( action(Act), Q1 ) ),
	% asking a question as a response to an issue-question
	not ( Moves/elem = ask(Q2) and $domain :: relevant( issue(Q2), Q1 ) ),
	% removed 030727 by SL; caused problems and seemed to have no function
%	not ( Moves/elem = ask(Q2) and $domain :: relevant( action(respond((Q2))), Q1 ) )
	 % SL050630: requesting a hypernym action 
	not ( Moves/elem = request( Act ) and $domain :: relevant( action(Act), Q1 ) )
      ],
      [  /shared/qud := $/private/tmp/sys/qud,
	 /shared/issues := $/private/tmp/sys/issues,
	 /shared/actions := $/private/tmp/sys/actions,%021120 SL
	 /shared/com := $/private/tmp/sys/com,
%	 clear( /private/nim ), init_shift( /private/nim ),% 021120 SL
	 if_do( QMove = ask( AltQ ) and in( AltQ, issue( SuperQ ) ) and
	      $domain :: plan( SuperQ, _ ),
		[ clear( /private/nim ),
		  init_shift( /private/nim ) ] )
      ] ).

rule( irrelevantFollowup,
      [ $latest_moves = Moves, 
	$/shared/lu/speaker == usr,
	not in( Moves, no_move ),
	not empty( Moves ),
	in( $/shared/pm, QMove ),
	QMove = ask( Q1 ) or QMove = icm:und*int:usr*Q1,
	not ( Moves/elem = ask( Q ) and $domain :: depends( Q, Q1 ) ),
	not ( Moves/elem = answer( A ) and $domain :: relevant( A, Q1 ) ),
	not ( Moves/elem = ask(Q2) and $domain :: relevant( issue(Q2), Q1 ) ),
	not ( Moves/elem = ask(Q2) and $domain :: relevant( action(respond(Q2)), Q1 ) ),
	not ( Moves/elem = request(Act) and $domain :: relevant( action(Act), Q1 ) ),
	not ( Moves/elem = icm:acc*neg:issue ) ],
      [  /shared/qud := $/private/tmp/sys/qud,
	 /shared/issues := $/private/tmp/sys/issues,
	 /shared/com := $/private/tmp/sys/com,
	 if_do( QMove = ask( AltQ ) and in( AltQ, issue( SuperQ ) ) and
	      $domain :: plan( SuperQ, _ ),
		[ clear( /private/nim ),
		  init_shift( /private/nim ) ] )
      ] ).


rule( unclearFollowup,
      [ $latest_moves = failed,
	$latest_speaker == usr,
	in( $/shared/lu/moves,  PrevMove ),
	q_raising_move( PrevMove )
      ],
      [  /private/plan := $/private/tmp/sys/plan,
	 /private/agenda := $/private/tmp/sys/agenda,
	 /shared/qud := $/private/tmp/sys/qud,
	 /shared/issues := $/private/tmp/sys/issues,
	 /shared/actions := $/private/tmp/sys/actions,
	 /shared/com := $/private/tmp/sys/com,
	 forall_do( in( $/private/agenda, A ) and not q_raising_action( A ),
		    del( /private/agenda, A ) )
      ] ).


rule( failedFollowup,
      [ $input = "'FAIL'",
	$latest_speaker == usr,
	in( $/shared/lu/moves,  PrevMove ),
	q_raising_move( PrevMove )
      ],
      [  /private/plan := $/private/tmp/sys/plan,
	 /private/agenda := $/private/tmp/sys/agenda,
	 /shared/qud := $/private/tmp/sys/qud,
	 /shared/issues := $/private/tmp/sys/issues,
	 /shared/actions := $/private/tmp/sys/actions,
	 /shared/com := $/private/tmp/sys/com,
	 forall_do( in( $/private/agenda, A ) and not q_raising_action( A ),
		    del( /private/agenda, A ) )
      ] ).


rule( noFollowup,
      [ in( $latest_moves, no_move ),
	fst( $/shared/qud, und(usr*Content) ) ],
      [ 
	if_do( in( $/shared/pm, icm:und*pos:usr*Content ),
	       % if Content is an issue, push it on ISSUES and QUD
	       if_then_else( Content = issue(Q1),
			     [ push( /private/tmp/usr/qud, Q1 ),
			       push( /private/tmp/usr/issues, Q1 ),
			       push( /private/tmp/usr/agenda, respond(Q1) ) ],
			     % else, if Content is an action, push it on ACTIONS
			     if_then_else( Content = action( A ),
					   [ push( /private/tmp/usr/actions, A ),
					     push( /private/tmp/usr/agenda, do(A) ) ],
					   % else, add it to COM
					   add( /private/tmp/usr/com, Content) ) ) ),
	if_do( fst( $/shared/issues, und(usr*Content) ),
	       pop( /shared/issues ) )
      ] ).

%enter idle state when there is no contact with user
rule( noContact,
      [ $contact = false ],
      [ program_state := idle ] ).


rule( removeYesNo,
      [ in( $/private/nim, Pair ),
	Pair/snd = answer(yes) or Pair/snd = answer(no) ],
      [ del( /private/nim, Pair ) ] ).

/*----------------------------------------------------------------------
       Move integration rules
----------------------------------------------------------------------*/

rule( retract,
      [ not empty( $/private/nim),
	$/private/nim/elem/snd = answer(A),
	in( $/shared/com, P1 ),
	fst( $/shared/issues, Q ),
	$domain :: relevant( P1, Q ),
	$domain :: relevant( A, Q ),
	$domain :: combine( Q, A, P ),
	$domain :: incompatible( P, P1 )],
      [ del( /shared/com, P1 ),
	forall_do( in( $/private/agenda, ICM_P1 ) and ICM_P1 = icm:und*_:_*P1,
		   del( /private/agenda, ICM_P1 ) ) ] ).




rule( integrateUsrAsk,
       [ not empty($/private/nim),
	 $/private/nim/fst = DPM,
	 DPM/fst == usr,
	 DPM/snd = ask(Q),
	 ! $score = Score,
	 Score >= 0.6, 
	 % ACCEPTABILITY
	 $domain :: plan( Q, _ ) ], 
       [ pop( /private/nim ),
	 push( /private/agenda, icm:acc*pos ),
	 add( /shared/lu/moves, ask(Q) ),
	 if_do( Score =< 0.9,
		push( /private/agenda, icm:und*pos:usr*issue(Q) ) ),
				% reraising of open issue
	 if_do( in( $/shared/issues, Q ) and not fst( $/shared/issues, Q ), push( /private/agenda, icm:reraise:Q ) ),
				% reraising of closed issue
	 if_do( in( $/shared/com, P ) and $domain :: resolves( P, Q ),
		[ del( /shared/com, P ),
		  if_do( in( $/private/bel, P ), del( /private/bel, P ) ),
		  push( /private/agenda, icm:reraise:Q ) ] ),
	 push( /shared/issues, Q ),
	 push( /shared/qud, Q ),
	 push( /private/agenda, respond(Q) )
       ] ).

rule( integrateSysAsk,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/fst == sys, 
	DPM/snd = ask(Q) ],
      [	pop( /private/nim ),
	add( /shared/lu/moves, ask(Q) ),
	push( /shared/qud, Q ),
	push( /shared/issues, Q )
      ] ).


       
      

% Short Answer to und-question

rule( integrateNegIcmAnswer,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = answer(A),
	fst( $/shared/issues, Q ),
	$domain :: resolves( A, Q ),
	fst( $/shared/qud, Q ),
	$domain :: combine( Q, A, P ),
	P = (not und(DP*Content)) 
	],
      [ pop( /private/nim ),
	add( /shared/lu/moves, answer(P) ),
	pop( /shared/issues ),	% resolving ICMINT
	if_do( in( $/shared/com, Content) or ( Content=issue(Q0) and in( $/shared/issues, Q0 ) ) or ( Content = action(Act) and in( $/shared/actions, Act ) ), % ICMINT
		      [
		       /shared/qud := $/private/tmp/DP/qud,
		       /shared/issues := $/private/tmp/DP/issues,
		       /shared/actions := $/private/tmp/DP/actions,
		       /shared/com := $/private/tmp/DP/com,
		       /private/plan := $/private/tmp/DP/plan,
		       % also reset agenda so remove confirmation of now-irrelevant info
		       % 021120 SL
		       /private/agenda := $/private/tmp/DP/agenda ] ),
	% 021126 SL: prop might have been stored many turns ago
%	if_do( in( $/shared/com, Content ), del( /shared/com, Content) ),
	       % this should be done for issues and actions too...
	if_do( Content = issue( SuperQ ) and $domain :: plan( SuperQ, _ ),
	       [ clear( /private/nim ),
		 init_shift( /private/nim ) ] ),
	push( /private/agenda, icm:und*pos:DP*(not Content) ) %, % ???
      ] ).

rule( integratePosIcmAnswer,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = answer(A),
	fst( $/shared/issues, Q ),
	$domain :: resolves( A, Q ),
	fst( $/shared/qud, Q ),
	$domain :: combine( Q, A, P ),
	P = und(DP*Content)
	],
      [ pop( /private/nim ),
	add( /shared/lu/moves, answer(P) ),
	pop( /shared/issues ),
	if_then_else( Content = issue(Q1),
		      [	push( /private/tmp/DP/qud, Q1 ),
			push( /private/tmp/DP/issues, Q1 ),
			push( /private/tmp/DP/agenda, respond(Q1) ) ],
		      if_then_else( Content = action( Act ),
				    [ push( /private/tmp/DP/actions, Act),
				      push( /private/tmp/DP/agenda, do(Act) ) ],
				    add( /private/tmp/DP/com, Content ) ) ),
	% changed "in" to "fst" for issues and actions 021120 SL
	if_do( not ( in( $/shared/com, Content) or ( Content=issue(Q0) and fst( $/shared/issues, Q0 ) ) or ( Content = action( Act ) and fst( $/shared/actions, Act ) ) ), 
	       if_then_else( Content = issue(Q1),
			     [ push( /shared/qud, Q1 ),
			       push( /shared/issues, Q1 ),
			       push( /private/agenda, respond(Q1) ) ],
			     if_then_else( Content = action( Act )
					 % acceptability!
					 and $domain :: plan(Act, _ ), %SL021126
					   [ push( /shared/actions, Act ),
					     push( /private/agenda, do(Act)) ],
					   add( /shared/com, Content )
			     ) ) )
      ] ).




rule( integrateUsrAnswer,
      [ fst( $/private/nim, DPM ),
	DPM/fst == usr,
	DPM/snd = answer(A),
	% sufficient score
	$score = Score,
	Score > 0.6,
	% answer performed in latest utterance, otherwise score is not valid
	% & selectUndIntAnswer should trigger instead
	in( $latest_moves, answer(A) ),
	% RELEVANCE
	fst( $/shared/issues, Q ),
	$domain :: relevant( A, Q ),
	% if it's not a proposition, mathcing Q must be on QUD
	not( ( not $domain :: proposition( A ) ) and
	   not in( $/shared/qud, Q ) ),
	$domain :: combine( Q, A, P ),
	% ACCEPTABILITY
%	$database :: validDBparameter( P ) or P = ( not _ )
	( $domain :: valid_parameter( P ) or P = ( not _ ) or
	  ( field($devices, SomeDev ) and $devices/SomeDev :: valid_parameter( P ) ) )
	],
      [ pop( /private/nim ),
	add( /shared/com, P ),
	add( /shared/lu/moves, answer(P) ),
	if_do( not in( $/private/agenda, icm:acc*pos),
	       push( /private/agenda, icm:acc*pos ) ),
	if_do( Score =< 0.9 and A \= yes and A \= no,
	       push( /private/agenda, icm:und*pos:usr*P ) )
	% prohibits more than one answer to a single Q in one turn
%	forall_do( in($/private/nim, DPM2) and DPM2/snd = answer(A2) and $domain::relevant(A2,Q), del( /private/nim, DPM2 ) )
      ] ).


rule( integrateSysAnswer,
      [ fst( $/private/nim, DPM ),
	DPM/snd = answer(P),
	DPM/fst == sys,
	$domain :: proposition( P ),
	% RELEVANCE
	fst( $/shared/issues, Q ),
	$domain :: relevant( P, Q ) ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, answer(P) ),
	add( /shared/com, P )
    ] ).



/*
rule( integrateUsrRequest,
       [ % pick out move
	 $/private/nim/fst = DPM,
	 DPM/fst == usr,
	 DPM/snd = request(A),
	 % check score; "up" and "top" actions always optimistically grounded
	 $score = Score,
	 Score > 0.6 or A == up or A == top,
	 % check acceptability; there is a plan for A
	 $domain :: plan( A, _ )], 
       [ pop( /private/nim ),
	 add( /shared/lu/moves, request(A) ),
	 % select ICM:
	 % 1. accepted
	 push( /private/agenda, icm:acc*pos ),
	 % 2. understood
	 if_do( Score =< 0.9 and ( A \== up and A \== top ),
		push( /private/agenda, icm:und*pos:usr*action(A) ) ),
	 % 3. reraising of open action
	 if_do( in( $/shared/actions, A ), %and not fst( $/shared/actions, A ), SL030727
		push( /private/agenda, icm:reraise:A ) ),
	 % 4. reraising of closed issue
	 if_do( in( $/shared/com, done(A) ),
		[ del( /shared/com, done(A) ),
%		  if_do( in( $/private/bel, P ), del( /private/bel, P ) ),
		  push( /private/agenda, icm:reraise:A ) ] ),
	 % update ACTIONS and AGENDA
	 push( /shared/actions, A ),
	 if_do( A\== up, push( /private/agenda, do(A) ) )
       ] ).
*/

rule( integrateUsrRequest,
       [ % pick out move
	 $/private/nim/fst = DPM,
	 DPM/fst == usr,
	 DPM/snd = request(A0),
	 % check score; "up" and "top" actions always optimistically grounded
	 $score = Score,
	 Score > 0.6 or A0 == up or A0 == top,
	 % check acceptability; there is a plan for A
	 ( ( $domain :: plan( A0, _ ) and A = A0 ) or
	 % SL 050630
	 % ...or A is a resolvable hypernym to an action for which there is a plan
	 ( fst( $/shared/actions, A2 ) and A2 \== top and 
	 $domain :: hypernym(A, A0) and
	 $domain :: dominates( A2, A ) ) )
	 ], 
       [ pop( /private/nim ),
	 add( /shared/lu/moves, request(A) ),
	 % select ICM:
	 % 1. accepted
	 push( /private/agenda, icm:acc*pos ),
	 % 2. understood
	 if_do( Score =< 0.9 and ( A \== up and A \== top ),
		push( /private/agenda, icm:und*pos:usr*action(A) ) ),
	 % 3. reraising of open action
	 if_do( in( $/shared/actions, A ), %and not fst( $/shared/actions, A ), SL030727
		push( /private/agenda, icm:reraise:A ) ),
	 % 4. reraising of closed issue
	 if_do( in( $/shared/com, done(A) ),
		[ del( /shared/com, done(A) ),
%		  if_do( in( $/private/bel, P ), del( /private/bel, P ) ),
		  push( /private/agenda, icm:reraise:A ) ] ),
	 % update ACTIONS and AGENDA
	 push( /shared/actions, A ),
	 if_do( A\== up, push( /private/agenda, do(A) ) )
       ] ).

rule( integrateConfirm,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = confirm(Act) ],
      [ pop( /private/nim ),
	add( /shared/com, done(Act) ) ] ).

% "report" is generalisation of "confirm"
% addresses issue X^status(Act, X)
rule( integrateReport,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = report(Act, Status) ],
      [ pop( /private/nim ),
	add( /shared/com, status(Act, Status) ) ] ).

rule( integrateAccommodationICM,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = icm:accommodate:Q ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, DPM/snd ),
	push( /shared/qud, und(usr*issue(Q) ) )] ).


rule( integrateUndPosICM,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = icm:und*pos:DP*Content ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, DPM/snd ),
	if_do( Content \= (not _) , push( /shared/qud, und(DP*Content) ) )] ).



rule( integrateUndIntICM,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = icm:und*int:DP*Content ], 
      [ del( /private/nim, DPM ),
	add( /shared/lu/moves, DPM/snd ),
	push( /shared/qud, und(DP*Content) ),
	push( /shared/issues, und(DP*Content) ) ] ). 



rule(  integrateUsrPerNegICM,
       [ $/shared/lu/speaker == usr,
	 not empty($/private/nim),
	 $/private/nim/fst = DPM,
	 DPM/fst = usr,
	 DPM/snd = icm:per*neg,
	 in( $latest_moves, DPM/snd ) ],
       [ pop( /private/nim ),
	 add( /shared/lu/moves, DPM/snd ),
	 /shared/qud := $/private/tmp/sys/qud,
	 /shared/issues := $/private/tmp/sys/issues,
	 /shared/actions := $/private/tmp/sys/actions,
	 /shared/com := $/private/tmp/sys/com,
	 /private/agenda := $/private/tmp/sys/agenda,
	 /private/plan := $/private/tmp/sys/plan
       ]).



rule(  integrateUsrAccNegICM,
       [ $/shared/lu/speaker == usr,
	 not empty($/private/nim),
	 $/private/nim/fst = DPM,
	 DPM/fst = usr,
	 DPM/snd = icm:acc*neg:issue,
	 in( $latest_moves, DPM/snd ),
	 in( $/shared/pm,  ask( Q ) ) ],
       [ pop( /private/nim ),
	 add( /shared/lu/moves, DPM/snd ),
	 if_do( in( $/private/plan, findout( Q ) ) and in( $/shared/qud, Q ), del( /private/plan, findout( Q ) ) ),
	 /shared/qud := $/private/tmp/sys/qud,
	 /shared/issues := $/private/tmp/sys/issues,
	 /shared/actions := $/private/tmp/sys/actions,
	 /shared/com := $/private/tmp/sys/com,
	 if_do( in( $/shared/issues, Q ), del( /shared/issues, Q ) ),
	 % SL031022
	 add( /shared/com, rejected(Q) )
	 ]).



rule( integrateOtherICM,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = icm:_ ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, DPM/snd )
	] ).


rule( integrateGreet,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = greet ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, DPM/snd ) ] ).


rule( integrateSysQuit,
       [ not empty($/private/nim),
	 $/private/nim/fst = DPM,
	 DPM/fst == sys,
	 DPM/snd = quit],
       [ pop( /private/nim ),
	 add( /shared/lu/moves, DPM/snd ),
	 program_state := idle ] ). %DH031201 - idle instead of quit


rule( integrateUsrQuit,
       [ not empty($/private/nim),
	 $/private/nim/fst = DPM,
	 DPM/fst == usr,
	 DPM/snd = quit ],
       [ pop( /private/nim ),
	 add( /shared/lu/moves, DPM/snd ),
	 push( /private/agenda, quit )] ).


rule( integrateNoMove,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = no_move ],
      [ pop( /private/nim ) ] ).


/*
rule( declineIssue,
      [ in( $/private/nim, M ),
	M/fst == usr,
	M/snd = ask(Q),
	not $domain :: plan( Q, _ ) ], 
      [ clear( /private/nim ),
	add( /shared/lu/moves, M/snd ),
	push( /private/agenda, icm:und*pos:usr*issue(Q) ),
	push( /private/agenda, icm:acc*neg:issue(Q) ) % isis2
      ] ).

rule( declineProp,
      [ in( $/private/nim, M ),
	M/fst == usr,
	M/snd = answer( A ), 
	fst( $/shared/issues, Q ),
	not( ( not $domain :: proposition( A ) ) and
	   not in( $/shared/qud, Q ) ),
	$domain :: relevant( A, Q ),
	$domain :: combine( Q, A, P ),
	not ( $database :: validDBparameter( P ) ) ], 
      [ del( /private/nim, M ),
	add( /shared/lu/moves, M/snd ),
	push( /private/agenda, icm:acc*neg:P ) 
      ] ).
*/

/*----------------------------------------------------------------------
       Accommodation
----------------------------------------------------------------------*/

rule( accommodateIssues2QUD,
      [ not empty($/private/nim),
	$/private/nim/elem/snd = answer(A),
	$latest_speaker == usr,
	%not $lexicon :: yn_answer(A),
	% no lexicon when using GF...
	not ( A=yes or A=no ),
	in( $/shared/issues, Q ),
	not in( $/shared/qud, Q ),
	$domain :: relevant( A, Q ),
	$domain :: combine( Q, A, P ),
	P \= A
	%not $domain :: proposition( A )
      ], 
      [ push( /shared/qud, Q ),
	raise( /shared/issues, Q) ] ).



rule( accommodatePlan2Issues,
      [ not empty($/private/nim),
	$/private/nim/elem/snd = answer(A),
	$latest_speaker == usr,
	%not $lexicon :: yn_answer(A),
	% no lexicon when using GF...
	not ( A=yes or A=no ),
	( in( $/private/plan, findout(Q) ) or
	    in( $/private/plan, raise(Q) ) or
	      in( $/private/plan, bind(Q) ) ),
	$domain :: relevant( A, Q ),
	not in( $/shared/issues, Q ),
	% default question, or...
	$domain :: default_question( Q ) or
	% only relevant to one action
 	( not ( ( in( $/private/plan,  findout(Q1) ) and
	      Q \= Q1 ) and 
	    $domain :: relevant( A, Q1 ) ) )
	],
      [ push( /shared/issues, Q )%,
      ] ).			

rule( accommodateCom2IssuesDependent,	
      [ not empty($/private/nim),
	$/private/nim/elem/snd = answer(P),
	$latest_speaker == usr,
	$domain :: proposition( P ),
	in( $/shared/com, P1 ),
	$domain :: question( Q ),
	$domain :: relevant( P, Q ),
	$domain :: relevant( P1, Q ),
	$domain :: depends( MQ, Q ),
	not in( $/shared/issues, MQ ),
	not in( $/private/agenda, icm:und*int:usr*issue(MQ) ),
	in( $/shared/com, MP ),
	$domain :: relevant( MP, MQ ) ],
      [ del( /private/bel, MP ),% ),
	del( /shared/com, MP ),
	push( /shared/issues, MQ ),
	push( /shared/issues, Q ),
	push( /private/agenda, respond( MQ ) ),
	push( /private/agenda, icm:und*pos:usr*issue(MQ) )
    ] ).		

rule( accommodateCom2Issues,
      [ not empty($/private/nim),
	$/private/nim/elem/snd = answer(P),
	$latest_speaker == usr,
	$domain :: proposition( P ),
	in( $/shared/com, P1 ),
	$domain :: question( Q ),
	$domain :: relevant( P, Q ),
	$domain :: relevant( P1, Q ),
	not in( $/shared/issues, Q ),
	( not $domain :: depends( _MQ, Q ) ) or
          ( $domain :: depends( MQ, Q ) and in( $/shared/issues, MQ ) ) or
      % 021120 SL
          ( in( $/shared/actions, Act ) and $domain :: depends( Act, Q )  )      ],
      [ push( /shared/issues, Q ) ] ). 





rule( accommodateQUD2Issues,
      [ not empty($/private/nim),
	$/private/nim/elem/snd = answer(P),
%	not $lexicon :: yn_answer(P),
	$latest_speaker == usr,
	in( $/shared/qud, Q ),
	$domain :: relevant( P, Q ),
	not in( $/shared/issues, Q ) ],
      [ push( /shared/issues, Q ) ] ).



rule( accommodateDependentIssue,
      [ $latest_speaker == usr,
	% no accommodation if icm on agenda
	% to prevent accommodation while requested action is being confirmed
	% 021120 SL
	not in($/private/agenda, icm:_),
	% exclude yesno
	% 021120 SL

	% no lexicon when using GF to interpret
	%setof( A, ( $/private/nim/elem/snd = answer(A) and ( not $lexicon :: yn_answer(A) ) ), AnswerSet ),
	setof( A, ( $/private/nim/elem/snd = answer(A) and ( not ( A=no or A=yes ) ) ), AnswerSet ),

	
	$$arity( AnswerSet ) > 0,
%%% 041119  trying without is_empty( $/private/plan )
%%% in order to accommodate just an answer to an issue
%%% always something on private/plan
	%%is_empty( $/private/plan ),
	$domain :: plan( SuperQ, Plan ),
	not $domain :: action( SuperQ ),
	forall( in( AnswerSet, A ),
		in( Plan, findout(Q) ) and
		$domain :: relevant( A, Q ) ),
	not ( 	( $domain :: plan( SuperQ1, Plan1 ) and
	        SuperQ1 \= SuperQ ) and
		forall( in( AnswerSet, A ), 
			in( Plan1, findout(Q) ) and
			$domain :: relevant( A, Q ) ) ),
	not in( $/private/agenda, icm:und*int:usr*issue(SuperQ) ) ],
      [ push( /shared/issues, SuperQ ),
	push( /private/agenda, icm:accommodate:SuperQ ),
	push( /private/agenda, icm:und*pos:usr*issue(SuperQ) ),
	set( /private/plan, Plan ),
	push( /private/agenda, icm:loadplan )	
	] ).



rule( accommodateDependentAction,			
      [ $latest_speaker == usr,
	% no accommodation if icm on agenda
	% to prevent accommodation while requested action is being confirmed
	% 021120 SL
	not in($/private/agenda, icm:_),
	% exclude yesno
	% 021120 SL

	%no lexicon when using GF
	%setof( A, ( $/private/nim/elem/snd = answer(A) and ( not $lexicon :: yn_answer(A) ) ), AnswerSet ),
	setof( A, ( $/private/nim/elem/snd = answer(A) and ( not ( A=no or A=yes ) ) ), AnswerSet ),
	
	$$arity( AnswerSet ) > 0,
	$domain :: plan( Action, Plan ),
	$domain :: action( Action ),
	forall( in( AnswerSet, A ),
		in( Plan, findout(Q) ) and
		$domain :: relevant( A, Q ) ),
	not ( 	( $domain :: plan( Action1, Plan1 ) and
	        Action1 \= Action ) and
		forall( in( AnswerSet, A ), 
			in( Plan1, findout(Q) ) and
			$domain :: relevant( A, Q ) ) ),
	not in( $/private/agenda, icm:und*int:usr*action(Action) ) ],
      [ push( /shared/actions, Action ),
	push( /private/agenda, icm:accommodate:Action ),
	push( /private/agenda, icm:und*pos:usr*action(Action) ),
	set( /private/plan, Plan ),
	push( /private/agenda, icm:loadplan )
	] ).




      

/*----------------------------------------------------------------------
       Downdate QUD
----------------------------------------------------------------------*/


rule( downdateQUD,
      [ $/shared/lu/speaker == usr ], 
      [ clear( /shared/qud ) ] ).

rule( downdateISSUES,
      [ fst( $/shared/issues, Q ), 
	in( $/shared/com, P ),
	$domain :: resolves( P, Q ) ],
      [ pop( /shared/issues ),
	% line below added 050629 SL
	add( /shared/com, resolved(Q) ) ] ).

rule( downdateISSUES2,
      [ in( $/shared/issues, IssueQ ), 
	fst( $/shared/issues, Q ),
	not fst( $/shared/issues, IssueQ ), %DH 031124
	$domain :: resolves( issue(Q), IssueQ ) ],
      [ del( /shared/issues, IssueQ ) ] ).

rule( downdateISSUES3,
      [ in( $/shared/issues, ActionQ ), 
	fst( $/shared/actions, A ),
	A \== top,
	$domain :: resolves( action(A), ActionQ )  ],
      [ del( /shared/issues, ActionQ ) ] ).



rule( downdateActions,
      [ fst( $/shared/actions, Action ),
	$domain :: postcond( Action, PC ),
	in( $/shared/com, PC )%or
      ],
      [ pop( /shared/actions ) ] ).

% should be: if only one action, and action non-leaf, don't pop

% added 040115 by SL:
% if all alternatives denied (by answering "no" to alt-q), return to top
% Only valid for "menu choice" altQ where the plan contains a single findout-q
% so also check that plan is empty
% NOTE: may overgenerate, should perhaps also check that AltQ is really an altq
rule( downdateActions2,
      [ fst( $/shared/actions, Action ),
	$domain :: plan( Action, Plan ),
	in( Plan, findout( AltQ ) ),
	$$arity( Plan ) == 1,
	in( $/shared/com, not(P) ),
	$domain :: resolves( not(P), AltQ ),
	empty( $/private/plan ) ],
      [ pop( /shared/actions ) ] ).


% allow going back to previous unfinished action (or menu node)
% hack 020702, modified 030724 (SL)

rule( goUp,
       [ fst( $/shared/actions, up ),
	 $$arity( $/shared/actions ) > 1 ], 
       [ pop( /shared/actions ),
	 pop( /shared/actions ),
	 clear( /private/plan )
       ] ).



/*----------------------------------------------------------------------
       Execute plan
----------------------------------------------------------------------*/

rule( recoverPlan,
      [ fst( $/shared/issues, Q ),
	is_empty( $/private/agenda ),
	is_empty( $/private/plan ),
	$domain :: plan( Q, Plan ),
	not ( in($/private/bel, P) and
	    $domain :: resolves( P, Q ) ) ],
      [ set( /private/plan, Plan ),
	push( /private/agenda, icm:reraise:Q ) ]
    ).

rule( recoverActionPlan,
      [ fst( $/shared/actions, A ),
	is_empty( $/shared/issues ),
	is_empty( $/private/agenda ),
	is_empty( $/private/plan ),
	$domain :: plan( A, Plan ),
	not ( ( in($/private/bel, PC ) or in($/shared/com, PC) )  and
	    $domain :: postcond( A, PC ) ) ],
      [ set( /private/plan, Plan ),
	push( /private/agenda, icm:reraise:A ) ]
    ).



rule( findPlan,
      [ in( $/private/agenda, respond(Q) ), % fst?
	$domain :: plan( Q, Plan ),
	not ( in( $/private/bel, P ) and
	    $domain::resolves(P, Q) ) ],
      [ del( /private/agenda, respond(Q) ),
	set( /private/plan, Plan ),
	if_do( ( in( $/private/plan, findout(Q1) ) or in( $/private/plan, raise(Q1 ) ) )
	     and ( not ( in( $/shared/com, P1 ) and $domain::resolves( P1, Q1 ) ) ) ,
	       push( /private/agenda, icm:loadplan ) )
      ]
     ).




rule( findActionPlan,
      [ in( $/private/agenda, do(A) ), % fst?
	$domain :: plan( A, Plan )%,
      ],
      [ del( /private/agenda, do(A) ),
	set( /private/plan, Plan ),
	push( /private/agenda, icm:loadplan ) 
      ]
     ).


rule( exec_consultDB,
      [ not empty($/private/plan),
	consultDB(Q) = $/private/plan/fst ],
      [! $/shared/com = Ps,
       ! $database :: consultDB( Q, Ps,Result ),
       add( /private/bel, Result ),
       pop( /private/plan ) ] ).


% get all answers to a question, SL 030502

rule( exec_consultDBx,
      [ not empty($/private/plan),
	consultDBAll(Q) = $/private/plan/fst ],
      [! $/shared/com = Ps,
       ! $database :: consultDBx( Q, Ps,Result ),
       extend( /private/bel, Result ),
       pop( /private/plan ) ] ).


% DH 030131
% complex boolean conditionals in plans using and, or and not. Must precede
% the simple conditionals

 %negation
 rule( exec_if_then,
       [ fst( $/private/plan, if_then( not C, A) ) ],
       [ pop( /private/plan ),
        push( /private/plan, if_then_else(C,[],A) ) ] ).

 rule( exec_if_then_else,
       [ fst( $/private/plan, if_then_else( not C, A ,B) )  ],
       [ pop( /private/plan ),
        push( /private/plan, if_then_else(C, B, A) ) ] ).

 %conjunction
 rule( exec_if_then,
       [ fst( $/private/plan, if_then( C1 and C2, A ) ) ],
       [ pop( /private/plan ),
        push( /private/plan, if_then( C1, if_then(C2, A ) ) ) ] ).

 rule( exec_if_then_else,
       [ fst( $/private/plan, if_then_else( C1 and C2, A , B ) ) ],
       [ pop( /private/plan ),
        push( /private/plan, if_then_else( C1, if_then_else(C2, A , B) , B) ) ] ).

 %disjunction
 rule( exec_if_then,
       [ fst( $/private/plan, if_then( C1 or C2, A ) ) ],
       [ push( /private/plan, if_then_else(C1 , A, if_then(C2, A) ) ) ] ).

 rule( exec_if_then_else,
       [ fst( $/private/plan, if_then_else( C1 or  C2, A , B) ) ],
       [ push( /private/plan, if_then_else(C1 , A, if_then_else(C2, A, B) ) ) ] ).

%simple conditionals

rule( exec_if_then,
      [ fst( $/private/plan, if_then( C, A ) ),
	in( $/private/bel, C ) or in( $/shared/com, C ) ],
      [ pop( /private/plan ),
	push( /private/plan, A ) ] ).

rule( exec_if_then,
      [ fst( $/private/plan, if_then( _C, _A ) ) ],
      [ pop( /private/plan ) ] ).

rule( exec_if_then_else,
      [ fst( $/private/plan, if_then_else( C, A1, _A2 ) ),
	in( $/private/bel, C ) or in( $/shared/com, C ) ],
      [ pop( /private/plan ),
	push( /private/plan, A1 ) ] ).

rule( exec_if_then_else,
      [ fst( $/private/plan, if_then_else( _C, _A1, A2 ) ) ],
      [ pop( /private/plan ),
	push( /private/plan, A2 ) ] ).

rule( exec_sequence,
      [ fst( $/private/plan, [X|Xs] ) ],
      [ pop( /private/plan ),
	prepend( /private/plan, stackset([X|Xs]) ) ] ).

%DH 031118 - sequences can also be empty
rule( exec_sequence,
      [ fst( $/private/plan, [] ) ],
      [ pop( /private/plan ) ] ).

%DH 030131
% The protect plan construct prevents actions in a plan from being
% accommodated. When it gets first in the plan, it is replaced with its argument
% (the protected action).
rule( exec_protect,
      [ fst( $/private/plan, protect(Action) ) ],
      [ pop( /private/plan ),
        push( /private/plan, Action ) ] ).


% SL 030520
% The "bind" plan construct allows questions in a plan to be  accommodated.
% When it gets first in the plan, it is simply deleted. Bound questions should
% be places last in the plan

rule( exec_bind,
      [ fst( $/private/plan, bind(_Q) ) ],
      [ pop( /private/plan ) ] ).



rule( exec_forget_all,
      [ fst( $/private/plan, forget_all ) ],
      [ pop( /private/plan ),
	clear( /private/bel ),
	clear( /shared/com ),
	clear( /shared/actions ),
	clear( /shared/issues ),%021120 SL
%	clear( /private/plan ),%021120 SL
%	clear( /private/agenda ),%021120 SL
	clear( /private/nim ), init_shift( /private/nim ), %021120 SL
				% hack
	clear( /shared/move_history ), 
	push( /shared/actions, top ) ] ).

%DH 030131
%everything except P is cleared from the IS
rule( exec_forget_except,
      [ fst( $/private/plan, forget_except(P) ) ,
        in( $/private/bel, P) or in( $/shared/com, P)],
      [ pop( /private/plan ),

        clear( /private/bel ),
        clear( /shared/com ),
        clear( /shared/actions ),
        add( /shared/com, P ),
	push( /shared/actions, top )%hack
	]).

%previous rule failed, P is not in IS
rule( exec_forget_except,
      [ fst( $/private/plan, forget_except(_P) ) ],
      [ pop( /private/plan ),
	
        clear( /private/bel ),
        clear( /shared/com ),
        clear( /shared/actions ),
	push( /shared/actions, top )%hack
      ] ).

%DH 030131
%P is cleared from the IS
rule( exec_forget,
      [ fst( $/private/plan, forget(P) ) ],
       [ pop( /private/plan ),
         % clear( /shared/actions ), commented out SL 031024
         if_do(in( $/private/bel,P) ,del( /private/bel, P )),
         if_do(in( $/shared/com,P) ,del( /shared/com, P ))
      ] ).


rule( exec_dev_get,
      [ fst( $/private/plan, dev_get(Dev, Var) ),
	% avoid doing actions until everything's been checked
	% hack for problem with optimistic grounding
	not in($/private/agenda, icm:und*_:_ ),
	not fst($/shared/qud, und(_) )
	],
      [ ! $devices/Dev :: dev_get( Var, Val ),
	pop( /private/plan ),
	del_all( /private/bel, dev_val( Dev, Var, _ ) ),
        %add( /private/bel, dev_val( Dev, Var, Val ) )
	% Val is e.g. play_status(playing)
	add( /private/bel, Val )] ).  


rule( exec_dev_query,
    [ fst( $/private/plan, dev_query(Dev, Query) ),
	% avoid doing actions until everything's been checked
	% hack for problem with optimistic grounding
	not in($/private/agenda, icm:und*_:_ ),
	not fst($/shared/qud, und(_) ) ],
      [ 
	% DH 0303: pass $/shared/com to device like  dev_do
	! ( $/shared/com = SharedCom ),
	!$devices/Dev :: dev_query( Query, SharedCom, Result ),
	pop( /private/plan ),
	add( /private/bel, Result )
      ] ).  

% get all answers to a query; SL 050302

rule( exec_dev_queryAll,
    [ fst( $/private/plan, dev_queryAll(Dev, Query) ),
	% avoid doing actions until everything's been checked
	% hack for problem with optimistic grounding
	not in($/private/agenda, icm:und*_:_ ),
	not fst($/shared/qud, und(_) ) ],
      [ ! ( $/shared/com = SharedCom ),
	!$devices/Dev :: dev_queryAll( Query, SharedCom, ResultSet ),
	pop( /private/plan ),
	extend( /private/bel, ResultSet )
      ] ).  


rule( exec_dev_set,
      [ fst( $/private/plan, dev_set(Dev, Var, Val) ),
	% avoid doing actions until everything's been checked
	% hack for problem with optimistic grounding
	not in($/private/agenda, icm:und*_:_ ),
	not fst($/shared/qud, und(_) ) ],
      [ pop( /private/plan ),
	devices/Dev :: dev_set( Var, Val ) ] ).



rule( exec_dev_do,
      [ fst( $/private/plan, dev_do(Dev, Action) ),
	% avoid doing actions until everything's been checked
	% hack for problem with optimistic grounding
	not in($/private/agenda, icm:und*_:_ ),
	not fst($/shared/qud, und(_) ) ],
      [ ! ( $/shared/com = SharedCom ),
	pop( /private/plan ),
	devices/Dev :: dev_do( Action, SharedCom ),
	add( /private/bel, done(Action) ) ] ).





rule( removeFindout,
      [ not empty( $/private/plan),
	findout(Q) = $/private/plan/elem,
	in( $/shared/com, P ),
	$domain :: resolves( P, Q ) ],
      [ del(/private/plan, findout(Q)) ]
     ).

rule( removeRaise,
       [ not empty( $/private/plan),
	 raise(Q) = $/private/plan/elem,
	 in( $/shared/com, P ),
	 $domain :: resolves( P, Q ) ],
      [ del(/private/plan, raise(Q)) ]
     ).

rule( exec_mode_only_speech1,
      [ fst( $/private/plan, mode_only_speech1 )
      ],
      [ pop( /private/plan ),
	graph := no,
	voice := max,
	del( /shared/actions, mode_only_speech1 ),
	clear( /private/agenda ),
	push( /private/agenda, icm:acc*pos )
      ]
    ).

rule( exec_mode_only_speech2,
      [ fst( $/private/plan, mode_only_speech2 )
      ],
      [ pop( /private/plan ),
	graph := no,
	voice := min,
	del( /shared/actions, mode_only_speech2 ),
	clear( /private/agenda ),
	push( /private/agenda, icm:acc*pos )
      ]
    ).

rule( exec_mode_only_graph,
      [ fst( $/private/plan, mode_only_graph )
      ],
      [ pop( /private/plan ),
	voice := no,
	graph := max,
	del( /shared/actions, mode_only_graph ),
	clear( /private/agenda ),
	push( /private/agenda, icm:acc*pos )
      ]
    ).

rule( exec_mode_both_speech_and_graph,
      [ fst( $/private/plan, mode_both_speech_and_graph )
      ],
      [ pop( /private/plan ),
	voice := min,
	graph := max,
	del( /shared/actions, mode_both_speech_and_graph ),
	clear( /private/agenda ),
	push( /private/agenda, icm:acc*pos )
      ]
    ).

rule( exec_set_speech_value,
      [ fst( $/private/plan, set_speech_value ),
	in( $/shared/com, speech_val(SV) )
      ],
      [ pop( /private/plan ),
	voice := SV,
	del( /shared/actions, set_speech_value ),
	clear( /private/agenda ),
	push( /private/agenda, icm:acc*pos )
      ] ).

rule( exec_set_graph_value,
      [ fst( $/private/plan, set_graph_value ),
	in( $/shared/com, graph_val(GV) )
      ],
      [ pop( /private/plan ),
	graph := GV,
	del( /shared/actions, set_graph_value ),
	clear( /private/agenda ),
	push( /private/agenda, icm:acc*pos )
      ] ).

rule( exec_change_language,
      [ fst( $/private/plan, change_language ),
	in( $/shared/com, language(Language) ),
	domain-Domain = $$underscore2dash($domain)
      ],
      [ pop( /private/plan ),
	language := Language,
	lexicon := $$dash2underscore(lexicon-Domain-Language),
	del( /shared/actions, change_language ),
	% clear agenda to avoid clutter
	% 021019 SL
	clear( /private/agenda ),
	push( /private/agenda, icm:acc*pos ) ] ).

% SL021125
rule( exec_change_domain,
      [ fst( $/private/plan, change_domain ),
	in( $/shared/com, domain(Domain) ),
	$language = Language
      ],
      [ pop( /private/plan ),
	domain := $$dash2underscore( domain-Domain ),
	lexicon := $$dash2underscore( lexicon-Domain-Language ),
	% line below added SL 050629
	add_field( devices, Domain, $$dash2underscore( device-Domain ) ),
	clear( /shared/actions ),
	clear( /shared/qud ),
	clear( /shared/issues ),
	clear( /shared/com ),
	clear( /private/nim ),
	clear( /private/plan ),
	push( /shared/actions, top ),
	push( /private/plan, do(top) ),
	% clear agenda to avoid clutter
	% 021019 SL
	clear( /private/agenda ),
	push( /private/agenda, greet ),
	push( /private/agenda, icm:acc*pos ) ] ).

% DH XXXXXX (?)
% this change_domain is quite different from the one above,
% when change_domain(Domain,Bel) is first in plan
% system changes domain to Domain and keeps a set of Beliefs
rule( exec_change_domain,
      [ fst( $/private/plan, change_domain(Domain,Beliefs) ),
        $language = Language
      ],
      
      [ pop( /private/plan ),
        clear( /private/bel ),
        clear( /shared/com ),
        clear( /shared/actions ),
        extend( /private/bel, set(Beliefs) ),
        lexicon := $$dash2underscore(lexicon-Domain-Language),
	asr_grammar := $$dash2underscore(asrg-Domain-Language),
        language := Language,
        domain := $$dash2underscore(domain-Domain),
        ! ($domain :: plan(top, TopPlan)),
        /private/plan := TopPlan,
        push(/private/agenda, greet)]   ).


% DH031016
% assume P (add P to /private/bel),
% system will then be able to answer questions ?X^P and ?P
rule( exec_assume, 
      [ fst($/private/plan, assume(P))],
      [ add(/private/bel, P),
	pop(/private/plan) ] ).


% SL050629
% assume P is shared (add P to /shared/com),
rule( exec_assume_shared, 
      [ fst($/private/plan, assume_shared(P))],
      [ add(/shared/com, P),
	pop(/private/plan) ] ).


% SL 031024
% assume Q is under discussion
% perhaps not OK w/ regards to grounding?
rule( exec_assume_issue, 
      [ fst($/private/plan, assume_issue(Q))],
      [ push(/shared/issues, Q),
	push( /private/agenda, respond( Q ) ),
	pop(/private/plan) ] ).

/*----------------------------------------------------------------------
       Action Selection
----------------------------------------------------------------------*/

rule( selectIcmUndIntAsk,
      [ $/shared/lu/speaker == usr,
	not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = ask(Q),
	$score =< 0.6 ], 
       [ pop( /private/nim ),
	 push( /private/agenda, icm:und*int:usr*issue(Q) ) ] ).

rule( selectIcmUndIntRequest,
      [ $/shared/lu/speaker == usr,
	not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = request(A),
	$score =< 0.6 ], 
       [ pop( /private/nim ),
	 push( /private/agenda, icm:und*int:usr*action(A) ) ] ).

rule( selectIcmUndIntAnswer,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd =  answer(A),
	$/shared/lu/speaker == usr,
	% RELEVANCE
	fst( $/shared/issues, Q ), % changed from QUD, SL040108
	$domain :: relevant( A, Q ),
	% ACCEPTABILITY
	$domain :: combine( Q, A, P ),
	% too low score or not in latest utterance
	$score =< 0.6 or (not in( $latest_moves, answer(A) ) )
      ], 
       [ pop( /private/nim ),
	 push( /private/agenda, icm:und*int:usr*P ) ] ).

rule( rejectIssue,
      [ not empty($/private/nim),
	$/private/nim/fst = DPM,
	DPM/snd = ask(Q),
	$/shared/lu/speaker = usr,
	not $domain :: plan( Q, _ ) ], 
      [ pop( /private/nim ),
	push( /private/agenda, icm:und*pos:usr*issue(Q) ),
	push( /private/agenda, icm:acc*neg:issue(Q) ) % isis2
      ] ).

% reject action because no plan
rule( rejectIssue,
      [ not empty($/private/nim),
	$/private/nim/elem = DPM,
	DPM/snd = ask(Q),
	$/shared/lu/speaker = usr,
	not $domain :: plan( Q, _ ) ], 
      [ del( /private/nim, pair(_,ask(Q)) ),
	push( /private/agenda, icm:und*pos:usr*issue(Q) ),
	push( /private/agenda, icm:acc*neg:issue(Q) ) 
      ] ).

rule( rejectAction,
      [ not empty($/private/nim),
	$/private/nim/elem = DPM,
	DPM/snd = request(A ),
	$/shared/lu/speaker = usr,
	% SL050630
	( ( not $domain :: plan( A, _ )) and
	( not ( $domain :: hypernym(A1, A) and ($domain :: plan(A1, _) ) ) ) )], 
      [ del( /private/nim, pair(_,request(A)) ),
	push( /private/agenda, icm:und*pos:usr*action(A) ),
	push( /private/agenda, icm:acc*neg:action(A) )
      ] ).

% rejection of short answers
rule( rejectProp,
      [ not empty($/private/nim),
	$/private/nim/elem = DPM,
	DPM/snd = answer( A ), 
	$/shared/lu/speaker = usr,
	fst( $/shared/qud, Q ),
	$domain :: relevant( A, Q ),
	$domain :: combine( Q, A, P ),
	not $domain :: valid_parameter( P ),
	not ( field( $devices, SomeDev ) and
	    $devices/SomeDev :: valid_parameter( P ) ), % 020702
	% don't reject ICM answers!
	not P = und(_),
	not ( P = not(und(_)) )
      ], 
      [ del( /private/nim, pair(_,answer( A )) ),
%	push( /private/agenda, icm:und*pos:usr*P ), removed 030822 SL
	push( /private/agenda, icm:acc*neg:P ) 
      ] ).
/*
% IOD variant:

rule( rejectProp,
      [ $/private/nim/elem/snd = answer( P ), 
	$domain :: proposition( P ),
	$/shared/lu/speaker = usr,
	fst( $/shared/issues, Q ),
	$domain :: relevant( P, Q ),
	not $domain :: valid_parameter( P ),
	not ( field( $devices, SomeDev ) and
	    $devices/SomeDev :: valid_parameter( P ) ), % 020702
	% don't reject ICM answers!
	not P = und(_),
	not ( P = not(und(_)) )
      ], 
      [ del( /private/nim, pair(_,answer( P )) ),
	push( /private/agenda, icm:und*pos:usr*P ),
	push( /private/agenda, icm:acc*neg:P ) 
      ] ).

*/

