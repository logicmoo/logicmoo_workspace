
 /*************************************************************************

         name: update_rules.pl 
  description: The update rules
 
*************************************************************************/

:- discontiguous rule_class/2, rule/3, '==>'/2.
:- use_module( library(tis_operators) ).

rule_class( getLatestMoves, grounding ).


% ask
rule_class( integrateUsrAsk, integrate ).
rule_class( integrateSysAsk, integrate ).
% answer
rule_class( integrateNegIcmAnswer, integrate ).
rule_class( integratePosIcmAnswer, integrate ).
rule_class( integrateUsrAnswer, integrate ).
rule_class( integrateSysAnswer, integrate ).
% ICM
rule_class( integrateUndIntICM, integrate ).
rule_class( integrateUsrPerNegICM, integrate ).
rule_class( integrateUsrAccNegICM, integrate ).
rule_class( integrateOtherICM, integrate ).
% other
rule_class( integrateGreet, integrate ).
rule_class( integrateSysQuit, integrate ).
rule_class( integrateUsrQuit, integrate ).
rule_class( integrateNoMove, integrate ).

rule_class( downdateQUD, downdate_qud ).
rule_class( downdateQUD2, downdate_qud ).

rule_class( recoverPlan, load_plan ).
rule_class( findPlan, load_plan ).

rule_class( removeFindout, exec_plan ).
rule_class( exec_consultDB, exec_plan ).


rule( getLatestMoves,
      [ $latest_moves = Moves,
	$latest_speaker = DP,
	$/shared/lu/moves = PrevMoves ],
      [ set( /private/nim, Moves ),
	set( /shared/lu/speaker, DP ),
	clear( /shared/lu/moves ),
	set( /shared/pm, PrevMoves )
	 ] ).


rule( irrelevantFollowup,
      [ $/private/nim = Moves, 
	$/shared/lu/speaker == usr,
	not ( Moves/elem = icm:_ ),
	in( $/shared/pm, QMove ),
	QMove = ask( Q1 ) or ( QMove = icm:und*int:DP*C and Q1 = und(DP*C)),
	not ( Moves/elem = ask( Q ) and $domain :: depends( Q, Q1 ) ),
	not ( Moves/elem = answer( A ) and $domain :: relevant( A, Q1 ) )
      ],
      [  /shared/qud := $/private/tmp/qud,
	 /shared/com := $/private/tmp/com ] ).


      
rule( unclearFollowup,
      [ $latest_moves = failed or $input = "'FAIL'",
	$latest_speaker == usr,
	in( $/shared/lu/moves,  PrevMove ),
	q_raising_move( PrevMove )
      ],
      [  /private/plan := $/private/tmp/plan,
	 /private/agenda := $/private/tmp/agenda,
	 /shared/qud := $/private/tmp/qud,
	 /shared/com := $/private/tmp/com,
	 forall_do( in( $/private/agenda, A ) and not q_raising_action( A ),
		    del( /private/agenda, A ) )
      ] ).






/*----------------------------------------------------------------------
       Move integration rules
----------------------------------------------------------------------*/

rule( integrateSysAsk,
      [ $/shared/lu/speaker == sys,
	fst( $/private/nim, ask(Q) ) ],
      [	pop( /private/nim ),
	add( /shared/lu/moves, ask(Q) ),
	push( /shared/qud, Q )
      ] ). 


rule( integrateUsrAsk,
       [ $/shared/lu/speaker == usr,
	 fst( $/private/nim, ask(Q) ),
	 !$score = Score,
	 Score > 0.7,
	 $domain :: plan( Q, _ ) ], 
      [ pop( /private/nim ),
	 push( /private/agenda, icm:acc*pos ),
	 add( /shared/lu/moves, ask(Q) ),
	if_do( Score =< 0.9,
	       push( /private/agenda, icm:und*pos:usr*issue(Q) ) ),
	if_do( in( $/shared/qud, Q ) and not fst( $/shared/qud, Q ),
	       push( /private/agenda, icm:reraise:Q ) ),
	 push( /shared/qud, Q ),
	 push( /private/agenda, respond(Q) )
       ] ).



rule( integrateNegIcmAnswer,
      [ fst( $/private/nim, answer(no) ),
	fst( $/shared/qud, und(DP*Content) ) ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, answer(und(DP*Content)) ),
	pop( /shared/qud ),
	push( /private/agenda, icm:und*pos:DP*(not Content) ) 
      ] ).

rule( integratePosIcmAnswer,
      [ fst( $/private/nim, answer(yes) ),
	fst( $/shared/qud, und(DP*Content) )
      ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, answer(und(DP*Content)) ),
	pop( /shared/qud ),
	if_then_else( Content = issue(Q1),
		      [ push( /shared/qud, Q1 ),
			push( /private/agenda, respond(Q1) ) ],
		      add( /shared/com, Content ) )
      ] ).




rule( integrateUsrAnswer,
      [ fst( $/private/nim, answer(A) ),
	$/shared/lu/speaker == usr,
	!$score = Score,
	Score > 0.7,
	fst( $/shared/qud, Q ),
	$domain :: relevant( A, Q ),
	$domain :: combine( Q, A, P ),
	$database :: validDBparameter( P ) or P = ( not _ )
	],
      [ pop( /private/nim ),
	add( /shared/lu/moves, answer(P) ),
	push( /private/agenda, icm:acc*pos ),
	if_do( Score =< 0.9 and A \= yes and A \= no,
	       push( /private/agenda, icm:und*pos:usr*P ) ),
	add( /shared/com, P )
      ] ).






rule( integrateSysAnswer,
      [ fst( $/private/nim, answer(P) ),
	$/shared/lu/speaker == sys,
	$domain :: proposition( P ),
	fst( $/shared/qud, Q ),
	$domain :: relevant( P, Q ) ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, answer(P) ),
	add( /shared/com, P )
    ] ).




rule( integrateUndIntICM, 
      [ fst( $/private/nim, icm:und*int:DP*Content ) ], 
      [ pop( /private/nim ),
	add( /shared/lu/moves, icm:und*int:DP*Content ),
	push( /shared/qud, und(DP*Content) ) ] ). 

   

rule(  integrateUsrPerNegICM,
       [ $/shared/lu/speaker == usr,
	 fst( $/private/nim, icm:per*neg ) ],
       [ pop( /private/nim ),
	 add( /shared/lu/moves, icm:per*neg ),
	 /shared/qud := $/private/tmp/qud,
	 /shared/com := $/private/tmp/com,
	 /private/agenda := $/private/tmp/agenda,
	 /private/plan := $/private/tmp/plan
       ]).



rule(  integrateUsrAccNegICM,
       [ $/shared/lu/speaker == usr,
	 fst( $/private/nim, icm:acc*neg:issue ),
	 in( $/shared/pm,  ask( _Q ) ) ],
       [ pop( /private/nim ),
	 add( /shared/lu/moves, icm:acc*neg ),
	 /shared/qud := $/private/tmp/qud,
	 /shared/com := $/private/tmp/com
	 ]).




rule( integrateOtherICM,
      [ fst( $/private/nim, icm:X ) ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, icm:X ) ] ).




rule( integrateGreet,
      [ fst( $/private/nim, greet ) ],
      [ pop( /private/nim ),
	add( /shared/lu/moves, greet ) ] ).



rule( integrateSysQuit,
       [ fst( $/private/nim, quit ),
	 $/shared/lu/speaker == sys ],
       [ pop( /private/nim ),
	 add( /shared/lu/moves, quit ),
	 program_state := quit ] ).


rule( integrateUsrQuit,
       [ fst( $/private/nim, quit ),
	 $/shared/lu/speaker == usr ],
       [ pop( /private/nim  ),
	 add( /shared/lu/moves, quit ),
	 push( /private/agenda, quit )] ).


rule( integrateNoMove,
      [ fst( $/private/nim, no_move ) ],
      [ pop( /private/nim ) ] ).


      

/*----------------------------------------------------------------------
       Downdate QUD
----------------------------------------------------------------------*/



rule( downdateQUD,
      [ fst( $/shared/qud, Q ), 
	in( $/shared/com, P ),
	$domain :: resolves( P, Q ) ],
      [ pop( /shared/qud ) ] ).

% Q on QUD resolves Issue alt-q
rule( downdateQUD2,
      [ in( $/shared/qud, IssueAltQ ), 
	fst( $/shared/qud, Q ),
	in( IssueAltQ, issue(Q) ) ],
      [ del( /shared/qud, IssueAltQ ) ] ).







/*----------------------------------------------------------------------
       Manage plan
----------------------------------------------------------------------*/

rule( recoverPlan,
      [ fst( $/shared/qud, Q ),
	is_empty( $/private/agenda ),
	is_empty( $/private/plan ),
	$domain :: plan( Q, Plan ),
	not ( in($/private/bel, P) and
	    $domain :: resolves( P, Q ) ) ],
      [ set( /private/plan, Plan ),
	push( /private/agenda, icm:reraise:Q ),
	push( /private/agenda, icm:loadplan )
]
    ).



rule( findPlan,
      [ in( $/private/agenda, respond(Q) ), 
	$domain :: plan( Q, Plan ),
	not ( in( $/private/bel, P ) and
	    $domain::resolves(P, Q) ) ],
      [ del( /private/agenda, respond(Q) ),
	set( /private/plan, Plan ),
	push( /private/agenda, icm:loadplan )
      ]
     ).


rule( exec_consultDB,
      [ consultDB(Q) = $/private/plan/fst ],
      [! $/shared/com = Ps,
       ! $database :: consultDB( Q, Ps,Result ),
       add( /private/bel, Result ),
       pop( /private/plan ) ] ).

/*
rule( exec_consultDB,
      [ consultDB(Q) = $/private/plan/fst ],
      [! $/shared/com = Ps,
       ! $database :: consultDB( Q, Ps,Result ),
       extend( /private/bel, Result ),
       pop( /private/plan ) ] ).
*/



rule( removeFindout,
       [ findout(Q) = $/private/plan/elem,
	 in( $/shared/com, P ),
	 $domain :: resolves( P, Q ) ],
      [ del(/private/plan, findout(Q)) ]
     ).






 