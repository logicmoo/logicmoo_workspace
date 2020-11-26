
 
/*************************************************************************

         name: update_rules.pl 
  description: IBiS3 update rules
 
*************************************************************************/


:- discontiguous rule_class/2, rule/3, '==>'/2.
:- use_module( library(tis_operators) ).

rule_class( getLatestMoves, grounding ).

rule_class( retract, integrate ).
% ask
rule_class( integrateUsrAsk, integrate ).
rule_class( integrateSysAsk, integrate ).
% answer
rule_class( integrateNegIcmAnswer, integrate ).
rule_class( integratePosIcmAnswer, integrate ).
rule_class( integrateUsrAnswer, integrate ).
rule_class( integrateSysAnswer, integrate ).
% ICM
rule_class( integrateAccommodationICM, integrate ).
rule_class( integrateUndPosICM, integrate ).
rule_class( integrateUndIntICM, integrate ). 
rule_class( integrateUsrPerNegICM, integrate ).
rule_class( integrateUsrAccNegICM, integrate ).
rule_class( integrateOtherICM, integrate ).
% other
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

rule_class( downdateISSUES, downdate_issues ).
rule_class( downdateISSUES2, downdate_issues ).

rule_class( downdateQUD, downdate_qud ).

rule_class( recoverPlan, load_plan ).
rule_class( findPlan, load_plan ).

rule_class( removeFindout, exec_plan ).
rule_class( removeRaise, exec_plan ).
rule_class( exec_consultDB, exec_plan ).
rule_class( exec_if_then, exec_plan ). %DH 030131
rule_class( exec_if_then_else, exec_plan ).%DH 030131
rule_class( exec_sequence, exec_plan ).
rule_class( exec_bind, exec_plan). %SL 030520
rule_class( exec_protect, exec_plan). %DH 030131
rule_class( exec_forget_all, exec_plan ).
rule_class( exec_forget, exec_plan ).%DH 030131
rule_class( exec_forget_except, exec_plan ).%DH 030131



rule_class( selectIcmUndIntAsk, select_action ).
rule_class( selectIcmUndIntAnswer, select_action ).
rule_class( rejectIssue, select_action ).
rule_class( rejectProp, select_action ).


/*----------------------------------------------------------------------

----------------------------------------------------------------------*/

rule( getLatestMoves,
      [ $latest_moves = LatestMovesQueue,
	$latest_speaker = DP,
	$/shared/lu/moves = PrevMoves ],
      [ ! $/private/nim = OldMoves,
	clear( /private/nim ),
	forall_do( in( LatestMovesQueue, M ), push( /private/nim, pair(DP, M) ) ),
	append( /private/nim, OldMoves ), 
	init_shift( /private/nim ),
	
	set( /shared/lu/speaker, DP ),
	clear( /shared/lu/moves ),
	set( /shared/pm, PrevMoves ) ] ).




rule( irrelevantFollowup,
      [ $latest_moves = Moves, 
	$/shared/lu/speaker == usr,
	not( Moves/elem = icm:per*neg  ),
	not ( Moves/elem = icm:acc*neg:issue ),
	not ( Moves/elem = icm:acc*pos ),
	in( $/shared/pm, QMove ),
	QMove = ask( Q1 ) or QMove = icm:und*int:usr*Q1,
	not ( Moves/elem = ask( Q ) and $domain :: depends( Q, Q1 ) ),
	not ( Moves/elem = answer( A ) and $domain :: relevant( A, Q1 ) ),
	not ( Moves/elem = ask(Q2) and $domain :: relevant( issue(Q2), Q1 ) ) ],
      [  /shared/qud := $/private/tmp/sys/qud,
	 /shared/issues := $/private/tmp/sys/issues,
	 /shared/com := $/private/tmp/sys/com,
	 if_do( QMove = ask( AltQ ) and in( AltQ, issue( SuperQ ) ) and
	      $domain :: plan( SuperQ, _ ),
		[ clear( /private/nim ),
		  init_shift( /private/nim ) ] )
      ] ).


rule( unclearFollowup,
      [ $latest_moves == failed,
	$latest_speaker == usr,
	in( $/shared/lu/moves,  PrevMove ),
	q_raising_move( PrevMove )
      ],
      [  /private/plan := $/private/tmp/sys/plan,
	 /private/agenda := $/private/tmp/sys/agenda,
	 /shared/qud := $/private/tmp/sys/qud,
	 /shared/issues := $/private/tmp/sys/issues,
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
	 /shared/com := $/private/tmp/sys/com,
	 forall_do( in( $/private/agenda, A ) and not q_raising_action( A ),
		    del( /private/agenda, A ) )
      ] ).


rule( noFollowup,
      [ $input == "'TIMED_OUT'",
	in( $/shared/pm, icm:und*pos:usr*Content ) ],
      [ if_then_else( Content = issue(Q1),
		      [ push( /private/tmp/usr/qud, Q1 ),
			push( /private/tmp/usr/issues, Q1 ),
			push( /private/tmp/usr/agenda, respond(Q1) ) ],
		      add( /private/tmp/usr/com, Content) )
      ] ).

rule( backupSharedUsr,
       [ $latest_speaker = usr,
	 $latest_moves = Moves,
	 not in( Moves, icm:_),
	 not in( Moves, no_move ),
	 not ( fst($/shared/qud, und(usr*C)) and
	     in( Moves, answer( A ) ) and
	     $domain :: relevant( A, und(usr*C) ) ) ],
      [ /private/tmp/usr/qud := $/shared/qud,
	 /private/tmp/usr/issues := $/shared/issues,
	 /private/tmp/usr/com := $/shared/com,
	 /private/tmp/usr/agenda :=  $/private/agenda,
	 /private/tmp/usr/plan := $/private/plan ] ).

/*----------------------------------------------------------------------
       Move integration rules
----------------------------------------------------------------------*/

rule( retract,
      [ $/private/nim/elem/snd = answer(A),
	in( $/shared/com, P1 ),
	fst( $/shared/issues, Q ),
	$domain :: relevant( P1, Q ),
	$domain :: relevant( A, Q ),
	$domain :: combine( Q, A, P ),
	$domain :: incompatible( P, P1 )],
      [ del( /shared/com, P1 ),
	forall_do( in( $/private/agenda, ICM_P1 ) and ICM_P1 = icm:_:_*P1,
		   del( /private/agenda, ICM_P1 ) ) ] ).



rule( integrateUsrAsk,
       [ $/private/nim/fst = M,
	 M/fst == usr,
	 M/snd = ask(Q),
	 % ACCEPTABILITY
	 $domain :: plan( Q, _ ) ], 
       [ pop( /private/nim ),
	 push( /private/agenda, icm:acc*pos ),
	 ! $score = Score,
	 if_then_else( Score =< 0.7,
		       push( /private/agenda, icm:und*int:usr*issue(Q) ),
		       [ add( /shared/lu/moves, ask(Q) ),
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
			 push( /private/agenda, respond(Q) ),
			 add( /shared/lu/moves, ask(Q) )
		       ] )
       ] ).

rule( integrateSysAsk,
      [ $/private/nim/fst = M,
	M/fst == sys, 
	M/snd = ask(Q) ],
      [	pop( /private/nim ),
	add( /shared/lu/moves, ask(Q) ),
	push( /shared/qud, Q ),
	push( /shared/issues, Q )
      ] ).


       
   

rule( integrateNegIcmAnswer,
      [ $/private/nim/fst/snd = answer(A),
	fst( $/shared/issues, Q ),
	$domain :: resolves( A, Q ),
	fst( $/shared/qud, Q ),
	$domain :: combine( Q, A, P ),
	P = (not und(DP*Content)) 
	],
      [ pop( /private/nim ),
	add( /shared/lu/moves, answer(P) ),
	pop( /shared/issues ),
	if_do( in( $/shared/com, Content) or ( Content=issue(Q0) and in( $/shared/issues, Q0 ) ), 
		      [
		       /shared/qud := $/private/tmp/DP/qud,
		       /shared/issues := $/private/tmp/DP/issues,
		       /shared/com := $/private/tmp/DP/com,
		       /private/plan := $/private/tmp/DP/plan ] ),
	if_do( Content = issue( SuperQ ) and $domain :: plan( SuperQ, _ ),
	       [ clear( /private/nim ),
		 init_shift( /private/nim ) ] ),
	push( /private/agenda, icm:und*pos:DP*(not Content) )
      ] ).


rule( integratePosIcmAnswer,
      [ $/private/nim/fst/snd = answer(A),
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
		      add( /private/tmp/DP/com, Content ) ),
	if_do( not ( in( $/shared/com, Content) or ( Content=issue(Q0) and in( $/shared/issues, Q0 ) ) ),
	       if_then_else( Content = issue(Q1),
			     [ push( /shared/qud, Q1 ),
			       push( /shared/issues, Q1 ),
			       push( /private/agenda, respond(Q1) ) ],
			     [ add( /shared/com, Content ) ] ) )
      ] ).






rule( integrateUsrAnswer,
      [ fst( $/private/nim, DP_M ),
	DP_M/fst == usr,
	DP_M/snd = answer(A),
	% RELEVANCE
	fst( $/shared/issues, Q ),
	$domain :: relevant( A, Q ),
	not( ( not $domain :: proposition( A ) ) and
	   not in( $/shared/qud, Q ) ),
	$domain :: combine( Q, A, P ),
	% ACCEPTABILITY
	$database :: validDBparameter( P ) or P = ( not _ ) 
	],
      [ pop( /private/nim ),
	if_then_else(
		      in( $latest_moves, answer(A) ),
		      !$score = Score,
		      ! Score = 0.6 ),
	if_then_else( Score =< 0.7,
		      push( /private/agenda, icm:und*int:usr*P ),
		      [ add( /shared/com, P ),
			add( /shared/lu/moves, answer(P) ),
			if_do( not in( $/private/agenda, icm:acc*pos),
			       push( /private/agenda, icm:acc*pos ) ),
			if_do( Score =< 0.9 and A \= yes and A \= no,
			       push( /private/agenda, icm:und*pos:usr*P ) )
		      ] )
      ] ).



rule( integrateSysAnswer,
      [ fst( $/private/nim, M ),
	M/snd = answer(P),
	M/fst == sys,
	$domain :: proposition( P ),
	% RELEVANCE
	fst( $/shared/issues, Q ),
	$domain :: relevant( P, Q ) ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, answer(P) ),
	add( /shared/com, P )
    ] ).


rule( integrateAccommodationICM,
      [ $/private/nim/fst = M,
	M/snd = icm:accommodate:Q ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, M/snd ),
	push( /shared/qud, und(usr*issue(Q) ) )] ).


rule( integrateUndPosICM,
      [ $/private/nim/fst = M,
	M/snd = icm:und*pos:DP*Content ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, M/snd ),
	if_do( Content \= (not _) , push( /shared/qud, und(DP*Content) ) )] ).


rule( integrateUndIntICM, 
      [ $/private/nim/fst = M,
	M/snd = icm:und*int:DP*Content ], 
      [ del( /private/nim, M ),
	add( /shared/lu/moves, M/snd ),
	push( /shared/qud, und(DP*Content) ),
	push( /shared/issues, und(DP*Content) ) ] ). 

rule(  integrateUsrPerNegICM,
       [ $/shared/lu/speaker == usr,
	 $/private/nim/fst = M,
	 M/fst = usr,
	 M/snd = icm:per*neg,
	 in( $latest_moves, M/snd ) ],
       [ pop( /private/nim ),
	 add( /shared/lu/moves, M/snd ),
	 /shared/qud := $/private/tmp/sys/qud,
	 /shared/issues := $/private/tmp/sys/issues,
	 /shared/com := $/private/tmp/sys/com,
	 /private/agenda := $/private/tmp/sys/agenda,
	 /private/plan := $/private/tmp/sys/plan
       ]).



rule(  integrateUsrAccNegICM,
       [ $/shared/lu/speaker == usr,
	 $/private/nim/fst = M,
	 M/fst = usr,
	 M/snd = icm:acc*neg:issue,
	 in( $latest_moves, M/snd ),
	 in( $/shared/pm,  ask( Q ) ) ],
       [ pop( /private/nim ),
	 add( /shared/lu/moves, M/snd ),
	 if_do( in( $/private/plan, findout( Q ) ) and in( $/shared/qud, Q ), del( /private/plan, findout( Q ) ) ),
	 /shared/qud := $/private/tmp/sys/qud,
	 /shared/issues := $/private/tmp/sys/issues,
	 /shared/com := $/private/tmp/sys/com,
	 if_do( in( $/shared/issues, Q ), del( /shared/issues, Q ) )
	 ]).



rule( integrateOtherICM,
      [ $/private/nim/fst = M,
	M/snd = icm:_ ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, M/snd )
	] ).



rule( integrateGreet,
      [ $/private/nim/fst = M,
	M/snd = greet ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, M/snd ) ] ).


rule( integrateSysQuit,
       [ $/private/nim/fst = M,
	 M/fst == sys,
	 M/snd = quit],
       [ pop( /private/nim ),
	 add( /shared/lu/moves, M/snd ),
	 program_state := quit ] ).


rule( integrateUsrQuit,
       [ $/private/nim/fst = M,
	 M/fst == usr,
	 M/snd = quit ],
       [ pop( /private/nim ),
	 add( /shared/lu/moves, M/snd ),
	 push( /private/agenda, quit )] ).


rule( integrateNoMove,
      [ $/private/nim/fst/snd = no_move ],
      [ pop( /private/nim ) ] ).





/*----------------------------------------------------------------------
       Accommodation
----------------------------------------------------------------------*/


rule( accommodateIssues2QUD,
      [ $/private/nim/elem/snd = answer(A),
	$latest_speaker == usr,
	not $lexicon :: yn_answer(A),
	in( $/shared/issues, Q ),
	not in( $/shared/qud, Q ),
	$domain :: relevant( A, Q ),
	not $domain :: proposition( A )
      ], 
      [ push( /shared/qud, Q ),
	raise( /shared/issues, Q) ] ).


rule( accommodateQUD2Issues,
      [ $/private/nim/elem/snd = answer(P),
	$latest_speaker == usr,
	in( $/shared/qud, Q ),
	$domain :: relevant( P, Q ),
	not in( $/shared/issues, Q ) ],
      [ push( /shared/issues, Q ) ] ).

rule( accommodatePlan2Issues,
      [ $/private/nim/elem/snd = answer(A),
	$latest_speaker == usr,
	not $lexicon :: yn_answer(A),
	( in( $/private/plan, findout(Q) ) or
	    in( $/private/plan, raise(Q) ) or
	      in( $/private/plan, bind(Q) ) ),
	$domain :: relevant( A, Q ),
	not in( $/shared/issues, Q ),
	$domain :: default_question( Q ) or
 	( not ( ( in( $/private/plan,  findout(Q1) ) and
	      Q \= Q1 ) and 
	    $domain :: relevant( A, Q1 ) ) )
	],
      [ push( /shared/issues, Q )
      ] ).			

rule( accommodateCom2Issues,	
      [ $/private/nim/elem/snd = answer(P),
	$latest_speaker == usr,
	$domain :: proposition( P ),
	in( $/shared/com, P1 ),
	$domain :: question( Q ),
	$domain :: relevant( P, Q ),
	$domain :: relevant( P1, Q ),
	not in( $/shared/issues, Q ),
	( not $domain :: depends( _MQ, Q ) ) or
          ( $domain :: depends( MQ, Q ) and in( $/shared/issues, MQ ) ) ],
      [ push( /shared/issues, Q ) ] ).


rule( accommodateCom2IssuesDependent,
      [ $/private/nim/elem/snd = answer(P),
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
      [ del( /private/bel, MP ),
	del( /shared/com, MP ),
	push( /shared/issues, MQ ),
	push( /shared/issues, Q ),
	push( /private/agenda, respond( MQ ) ),
	push( /private/agenda, icm:und*pos:usr*issue(MQ) )
    ] ).			





rule( accommodateDependentIssue,
      [ $latest_speaker == usr,
	setof( A, $/private/nim/elem/snd = answer(A), AnswerSet ),
	$$arity( AnswerSet ) > 0,
	is_empty( $/private/plan ),
	$domain :: plan( SuperQ, Plan ),
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




      

/*----------------------------------------------------------------------
       Downdate QUD and ISSUES
----------------------------------------------------------------------*/

rule( downdateQUD,
      [ $/shared/lu/speaker == usr ],
      [ clear( /shared/qud ) ] ).

rule( downdateISSUES,
      [ fst( $/shared/issues, Q ), 
	in( $/shared/com, P ),
	$domain :: resolves( P, Q ) ],
      [ pop( /shared/issues ) ] ).

rule( downdateISSUES2,
      [ in( $/shared/issues, IssueAltQ ), 
	fst( $/shared/issues, Q ),
	in( IssueAltQ, issue(Q) ) ],
      [ del( /shared/issues, IssueAltQ ) ] ).







/*----------------------------------------------------------------------
       Manage plan
----------------------------------------------------------------------*/

rule( recoverPlan,
      [ fst( $/shared/issues, Q ),
	is_empty( $/private/agenda ),
	is_empty( $/private/plan ),
	$domain :: plan( Q, Plan ),
	not ( in($/private/bel, P) and
	    $domain :: resolves( P, Q ) ) ],
      [ set( /private/plan, Plan ),
	push( /private/agenda, icm:reraise:Q ),
	push( /private/agenda, icm:loadplan )]
    ).


rule( findPlan,
      [ in( $/private/agenda, respond(Q) ), 
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

% simple format
/*
rule( exec_consultDB,
      [ consultDB(Q) = $/private/plan/fst ],
      [! $/shared/com = Ps,
       ! $database :: consultDB( Q, Ps,Result ),
       add( /private/bel, Result ),
       pop( /private/plan ) ] ).
*/



rule( exec_consultDB,
      [ consultDB(Q) = $/private/plan/fst ],
      [! $/shared/com = Ps,
       ! $database :: consultDBx( Q, Ps,Result ),
       extend( /private/bel, Result ),
       pop( /private/plan ) ] ).




rule( removeFindout,
       [ findout(Q) = $/private/plan/elem,
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
      [ fst( $/private/plan, bind(Q) ) ],
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
        %hack
        push( /shared/actions, top )]).

%DH 030131
%P is cleared from the IS
rule( exec_forget,
      [ fst( $/private/plan, forget(P) ) ],
       [ pop( /private/plan ),
         clear( /shared/actions ),
         if_do(in( $/private/bel,P) ,del( /private/bel, P )),
         if_do(in( $/shared/com,P) ,del( /shared/com, P ))
      ] ).


% DH031016
% assume P (add P to /private/bel),
% system will then be able to answer questions ?X^P and ?P
rule( exec_assume, 
      [ fst($/private/plan, assume(P))],
      [ add(/private/bel, P),
	pop(/private/plan) ] ).

/*----------------------------------------------------------------------
       Action Selection
----------------------------------------------------------------------*/


rule( selectIcmUndIntAsk,
      [ $/shared/lu/speaker == usr,
	$/private/nim/elem/snd = ask(Q),
	$score =< 0.7 ], 
       [ pop( /private/nim ),
	 push( /private/agenda, icm:und*int:usr*issue(Q) ) ] ).


rule( selectIcmUndIntAnswer,
      [ $/private/nim/elem/snd = answer(A),
	$/shared/lu/speaker == usr,
	% RELEVANCE
	fst( $/shared/qud, Q ),
	$domain :: relevant( A, Q ),
	% ACCEPTABILITY
	$domain :: combine( Q, A, P ),
	$score =< 0.7 ], 
       [ pop( /private/nim ),
	 push( /private/agenda, icm:und*int:usr*P ) ] ).

rule( rejectIssue,
      [ $/private/nim/elem/snd = ask(Q),
	$/shared/lu/speaker = usr,
	not $domain :: plan( Q, _ ) ], 
      [ del( /private/nim, pair(_,ask(Q)) ),
	push( /private/agenda, icm:und*pos:usr*issue(Q) ),
	push( /private/agenda, icm:acc*neg:issue(Q) ) 
      ] ).

rule( rejectAction,
      [ $/private/nim/elem/snd = request(A ),
	$/shared/lu/speaker = usr,
	not $domain :: plan( A, _ ) ], 
      [ del( /private/nim, pair(_,request(A)) ),
	push( /private/agenda, icm:und*pos:usr*action(A) ),
	push( /private/agenda, icm:acc*neg:action(A) )
      ] ).


rule( rejectProp,
      [ $/private/nim/elem/snd = answer( A ), 
	$/shared/lu/speaker = usr,
	fst( $/shared/qud, Q ),
	$domain :: relevant( A, Q ),
	$domain :: combine( Q, A, P ),
	not( $database :: validDBparameter( P ) ),
	% don't reject ICM answers!
	not P = und(_),
	not ( P = not(und(_)) )
      ], 
      [ del( /private/nim, pair(_,answer( A )) ),
%	push( /private/agenda, icm:und*pos:usr*P ), removed 030822 SL
	push( /private/agenda, icm:acc*neg:P ) 
      ] ).

rule( rejectProp,
      [ $/private/nim/elem/snd = answer( P ), 
	$domain :: proposition( P ),
	$/shared/lu/speaker = usr,
	fst( $/shared/issues, Q ),
	$domain :: relevant( P, Q ),
	not $database :: validDBparameter( P ),
	% don't reject ICM answers!
	not P = und(_),
	not ( P = not(und(_)) )
      ], 
      [ del( /private/nim, pair(_,answer( P )) ),
%	push( /private/agenda, icm:und*pos:usr*P ),, removed 030822 SL
	push( /private/agenda, icm:acc*neg:P ) 
      ] ).
