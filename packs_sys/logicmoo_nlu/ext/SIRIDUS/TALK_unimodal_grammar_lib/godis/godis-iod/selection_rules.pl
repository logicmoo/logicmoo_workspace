
 
/*************************************************************************

         name: selection_rules.pl 
  description: The selection rules
 
*************************************************************************/

:- use_module( library(tis_operators) ).

rule_class( clarifyIssue, select_action ).
rule_class( clarifyDependentIssue, select_action ).
rule_class( selectRespond, select_action ).
rule_class( forgetIssue, select_action ).
rule_class( reraiseIssue, select_action ).
rule_class( selectFromPlan, select_action ).

rule_class( selectIcmConNeg, select_icm ).
rule_class( selectIcmPerNeg, select_icm ).
rule_class( selectIcmSemNeg, select_icm ).
rule_class( selectIcmUndNeg, select_icm ).
rule_class( selectIcmOther, select_icm ).


rule_class( selectQuit, select_move ).
rule_class( selectAnswer, select_move ).
rule_class( selectAsk, select_move ).
rule_class( selectGreet, select_move ).
rule_class( selectIcmOther, select_move ). 

%%%%%%%%%%%%

rule( backupSharedSys,
       [ ],
       [ /private/tmp/sys/qud := $/shared/qud,
	 /private/tmp/sys/issues := $/shared/issues,
	 /private/tmp/sys/com := $/shared/com,
	 /private/tmp/sys/agenda :=  $/private/agenda,
	 /private/tmp/sys/plan := $/private/plan,
	 if_do( in( $/private/tmp/sys/agenda, icm:sem*neg ),
		del( /private/tmp/sys/agenda, icm:sem*neg ) ) ] ).


/*----------------------------------------------------------------------
       Action selection
----------------------------------------------------------------------*/

rule( clarifyIssue, 
      [ in( $/private/nim, M ),
	M/fst == usr,
	M/snd = answer( A ),
	setof( Q, ( in( $/private/plan, findout(Q) ) and
		  $domain :: relevant( A, Q ) ), Qs ),
	$$arity( Qs ) > 1 ],
      [ ! setof( P, in( Qs, Q1 ) and ($domain :: combine(Q1, A, P)), Ps ),
	push( /private/agenda, findout(Ps) ),
	del( /private/nim, M ) 
	] ).

rule( clarifyDependentIssue,
      [ in( $/private/nim, pair(usr,answer(A) ) ), 
	is_empty( $/shared/issues ),
	setof( MQ, 
	       $domain :: depends( MQ, Q ) and
	       $domain :: relevant( A, Q ), MQs ),
	remove_unifiables( MQs, MQs1 ),
	$$arity( MQs1 ) > 1 ],
      [ ! setof( P, in( MQs1, MQ1 ) and (P=issue(MQ1)), Ps ),
	push( /private/agenda, findout(Ps) )
	] ).


rule( selectRespond,
      [ is_empty( $/private/plan ),
	fst( $/shared/issues, Q ),
	in( $/private/bel, P ),
	not in( $/shared/com, P ), 
	$domain :: resolves(P, Q ),
	not in( $/private/agenda, respond( Q ) )
      ],
      [ push( /private/agenda, respond( Q ) ) ] ).

rule( selectFromPlan,
       [ Action = $/private/plan/fst ],
       [ push( /private/agenda, Action ) ]
     ).

% forget clarification issues if NIM is empty
% 020918
rule( forgetIssue,
      [ fst( $/shared/issues, Q ),
	in( Q, issue(_) ), % Q is a clarification question
	is_empty( $/private/nim ) ],
      [ del( /shared/issues, Q ) ] ).

rule( reraiseIssue,
       [ fst( $/shared/issues, Q ),
	 not $domain::plan( Q, _ ),
	 not ( in( $/shared/com, P ) and $domain :: resolves( P, Q ) ),
	 not in( $/private/agenda, respond( Q ) )
       ],
      [ push( /private/agenda, icm:reraise ),
	push( /private/agenda, raise(Q) ) ] ).

/*----------------------------------------------------------------------
       ICM selection
----------------------------------------------------------------------*/



rule( selectIcmConNeg,
      [ $input = "'TIMED_OUT'",
	is_empty( $next_moves ),
	is_empty( $/private/agenda ) ],
      [ push( next_moves, icm:con*neg ) ]
    ).

rule( selectIcmPerNeg,
      [ $input = "'FAIL'",
	not in( $next_moves, icm:per*neg ) ],
      [ push( next_moves, icm:per*neg ) ]
    ).
	

rule( selectIcmSemNeg, 
      [ $latest_moves = failed,
	$input = Input,
	not in( $next_moves, icm:sem*neg ),
	not in( $next_moves, icm:per*neg )
      ],
      [ push( next_moves, icm:per*pos:Input ),
	push( next_moves, icm:sem*neg ) ]
    ).

rule( selectIcmUndNeg,
      [ not in( $next_moves, icm:und*neg ),	
	not in( $next_moves, icm:sem*neg ),	
	not in( $next_moves, icm:per*neg ),	
	not empty( $latest_moves ),
	forall( $latest_moves/elem =  M, $/private/nim/elem/snd = M ),
	not in( $latest_moves, ask(_) ),
	forall( $latest_moves/elem =  answer( A ),
		not( fst( $/shared/issues, Q) and $domain :: relevant( A, Q ) ) ) ],
      [ forall_do( $latest_moves/elem =  M, push( next_moves, icm:sem*pos:M ) ),
	push( next_moves, icm:und*neg ),
	forall_do( in( $latest_moves, M1 ) and
		   ( M1 = answer(A) and
 		   ( ( $lexicon :: yn_answer(A) )  and
		     ( in( $/private/nim, M1Pair ) and
		         M1Pair/snd = M1 ) ) ),
		   del( /private/nim, M1Pair ) )  ] ).



rule( selectIcmOther, 
      [ in( $/private/agenda, icm:ICM ),
	not ( in( $next_moves, A ) and q_raising_icm(A) ),
	not ( in( $next_moves, A ) and A = ask( _ ) )
      ],
      [ push( next_moves, icm:ICM ),
	del( /private/agenda, icm:ICM ),
	if_do( ICM = und*pos:_, timeout := 1.0 ),
	if_do( ICM = loadplan and is_empty( $/private/plan ), del( next_moves, icm:ICM ) ) ]
    ).


/*----------------------------------------------------------------------
       Move selection
----------------------------------------------------------------------*/

rule( selectQuit,
      [ in( $/private/agenda, quit )],
      [ clear( next_moves ),
	push( next_moves, quit ),
	clear( /private/agenda ) ]
    ).

rule( selectAnswer, 
      [ fst( $/private/agenda, respond(Q) ),
	in( $/private/bel, P ),
	not in( $/shared/com, P ), 
	$domain :: resolves( P, Q ) ],
      [ forall_do( in( $/private/bel, P1 ) and ($domain :: relevant( P1, Q ) ),
		   push( next_moves, answer( P1 ) ) ),
	pop( /private/agenda ) ]
    ).


rule( selectAsk, 
      [ or( fst( $/private/agenda, findout(Q) ),
	    fst( $/private/agenda, raise(Q) ) ),
	not ( in( $next_moves, Move ) and q_raising_move( Move ) ) ],
      [ push( next_moves, ask(Q) ),
	pop( /private/agenda ),
	if_do( fst( $/private/plan, raise(Q) ), pop( /private/plan ) )
      ] ).


rule( selectGreet, 
      [ fst( $/private/agenda, greet )],
      [ push( next_moves, greet ),
	pop( /private/agenda ) ]
    ).

