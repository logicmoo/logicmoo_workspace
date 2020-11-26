
 
/*************************************************************************

         name: selection_rules.pl 
  description: GoDiS-AOD selection rules
      authors: Original code (June 2002) by Staffan Larsson (SL)
               Modifications by SL and David Hjelm (DH)

*************************************************************************/

/************************************************************************

HISTORY, started 050531

This history log was unfortunately started much later than the one in
update_rules.pl. Some reconstruction has been attempted

021119 (SL)
- modified reraiseIssue rule so it does not reraise Q if Q irrelevant
to current topmost action

021120 (SL)
- moved selectRespond rule
- modified rule clarifyAction so it does not trigger if actions
nonempty (except for top)
- modified selectFromPlan to exclude upnp actions
- modified selectAnswer to only select resolving answers (and exluding
relevant but not resolving ones)

0311 (SL)
- cancelled rule clarifyDependentIssue - overlaps with clarifyAction, need to fix

0504 (DH)
- modified to work with trindikit4

050414 (SL)
- changed selectFromPlan to accommodate this change in update_rules:
added plan construct dev_queryAll which finds all answers to a query
in a device

050627 (DH) 
- removed dependency on lexicon resource by replacing all $lexicon::yn_answer(A) with (A=yes or A=no)
************************************************************************/

%trindikit 3 or trindikit4
:- ( current_module(tkit_properties)->
       use_module( trindikit(tkit_operators)); %trindikit4
       use_module( library(tis_operators) ) ). %trindikit3

rule_class( selectIcmConNeg, select_icm ).
rule_class( selectIcmPerNeg, select_icm ).
rule_class( selectIcmSemNeg, select_icm ).
rule_class( selectIcmUndNeg, select_icm ).
rule_class( selectIcmOther, select_icm ).

rule_class( selectRespond, select_action ). % moved here fromefter clarifyAction 021120 SL
rule_class( selectConfirmAction, select_action ).
rule_class( selectReportAction, select_move ).
rule_class( clarifyIssue, select_action ).
%rule_class( clarifyDependentIssue, select_action ). % SL added nov 03; overlaps with clarifyAction, need to fix
rule_class( clarifyAction, select_action ).
%rule_class( forgetIssue, select_action ).
rule_class( reraiseIssue, select_action ).
rule_class( selectFromPlan, select_action ).

rule_class( selectQuit, select_move ).
rule_class( selectAnswer, select_move ).
rule_class( selectAsk, select_move ).
rule_class( selectConfirm, select_move ).
rule_class( selectReport, select_move ).
rule_class( selectGreet, select_move ).
rule_class( selectIcmOther, select_move ).

rule_class( selectInform, select_move ). %D feb 2003. maybe not needed


rule( clarifyIssue, % icm-und-int-Ps -> findout(Ps) [object-level]
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
	       $domain :: issue( MQ ) and
	       $domain :: relevant( A, Q ), MQs ),
	remove_unifiables( MQs, MQs1 ),
	$$arity( MQs1 ) > 1 ],
      [ ! setof( P, in( MQs1, MQ1 ) and (P=issue(MQ1)), Ps ),
	push( /private/agenda, findout(Ps) )
	] ).

rule( clarifyAction,
      [ in( $/private/nim, pair(usr,answer(A) ) ),  %M ),
	% don't trigger if actions nonempty (except top)s
	% 021120 SL
	not (in($/shared/actions, SomeAct) and SomeAct\==top),
	setof( Goal, %( $domain :: plan( MQ, Plan ) and
		  %( in( Plan, findout(Q) ) and ))
	       $domain :: depends( Goal, Q ) and
	       $domain :: relevant( A, Q ), Goals ),
	remove_unifiables( Goals, Goals1 ),
	$$arity( Goals1 ) > 1 ],
      [ ! setof( IssueProp, in( Goals1, Issue1 ) and
	            (not $domain::action(Issue1) ) and
	            ( IssueProp = issue(Issue1)),
		 IssueProps ),
	! setof( ActProp, in( Goals1, Act1 ) and
	            $domain::action(Act1)  and
	            ( ActProp = action(Act1) ),
		 ActProps ),
	! union( ActProps, IssueProps, PropSet ),
	push( /private/agenda, findout(PropSet) )
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


rule( selectConfirmAction,
      [ is_empty( $/private/plan ),
	fst( $/shared/actions, Act ),
	in( $/private/bel, P ),
	not in( $/shared/com, P ), 
	$domain :: postcond( Act, P ),
	not in( $/private/agenda, confirm(Act) )
      ],
      [ push( /private/agenda, confirm( Act ) ) ] ).

rule( selectReportAction,
      [ fst( $/private/plan, report(Act, Status) ),
%	fst( $/shared/actions, Act ),
	not in( $/private/agenda, report(Act, Status) )
      ],
      [ pop( /private/plan ),
	push( /private/agenda, report( Act, Status ) ) ] ).


% refill the agenda with the next action on the plan

rule( selectFromPlan,
       [ not empty($/private/plan),
	Action = $/private/plan/fst,
	 % no upnp actions; should be done by accessing resources?
	 % 021120 SL
	 Action \= dev_do(_,_),
	 Action \= dev_get(_,_),
	 Action \= dev_set(_,_,_),
	 Action \= dev_query(_,_),
	 Action \= dev_queryAll(_,_) % added 050415
	 ],
       [ push( /private/agenda, Action ) ]
     ).

% forget clarification issues if NIM is empty
% 020918
%rule( forgetIssue,
%      [ fst( $/shared/issues, Q ),
%	in( Q, issue(_) ), % Q is an issue clarification question
%	is_empty( $/private/nim ) ],
%      [ del( /shared/issues, Q ) ] ).

rule( reraiseIssue, 
	 [ in( $/shared/issues, Q ),
	   Q \= und(_),
	   Q \= set(_), % 020918: no clarification questions!
	 not $domain::plan( Q, _ ),
	 % don't reraise something that was just resolved, or that will be resoloved soon
	 not ( in( $/shared/com, P ) and $domain :: resolves( P, Q ) ),
	 not in( $/private/agenda, respond( Q ) ),
    % 021119 SL: don't reraise if irrelevant to current topmost action
	 not ( ( fst( $/shared/actions, Action ) and
	         $domain :: plan( Action, Plan ) ) and
	       not in( Plan, findout(Q) ) )

	   ],
      [ push( /private/agenda, icm:reraise ),
	push( /private/agenda, raise(Q) ) ] ).




rule( backupSharedSys,
       [ ],
       [ /private/tmp/sys/qud := $/shared/qud,
	 /private/tmp/sys/issues := $/shared/issues,
	 /private/tmp/sys/actions := $/shared/actions,
	 /private/tmp/sys/com := $/shared/com,
	 /private/tmp/sys/agenda :=  $/private/agenda,
	 /private/tmp/sys/plan := $/private/plan,
	  % delete "pardon"; it should never be repeated anyway
	 if_do( in( $/private/tmp/sys/agenda, icm:sem*neg ),
		del( /private/tmp/sys/agenda, icm:sem*neg ) ) ] ).



% select_move


rule( selectAnswer, 
      [ fst( $/private/agenda, respond(Q) ),
	in( $/private/bel, P ),
	not in( $/shared/com, P ), 
	$domain :: resolves( P, Q ) ],
      [ forall_do( in( $/private/bel, P1 ) and ($domain :: resolves( P1, Q ) ), %changed from "relevant" 021120 SL
		   push( next_moves, answer( P1 ) ) ),
	if_do( in( $next_moves, icm:acc*pos ), del( next_moves, icm:acc*pos ) ), % 020702
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

rule( selectConfirm, 
      [ fst( $/private/agenda, confirm(A) ) ], %,
      [ push( next_moves, confirm(A) ),
	pop( /private/agenda )
      ] ).
rule( selectReport, 
      [ fst( $/private/agenda, report(Act, Status) ) ], %,
      [ push( next_moves, report(Act, Status) ),
	pop( /private/agenda )
      ] ).

%% needed for tvGoDiS at least now
rule( selectInform,
      [ fst( $/private/agenda, inform(P) ) ],
      [ push( next_moves, answer(P)  ) ,
        pop( /private/agenda ),
        if_do( fst( $/private/plan, inform(P) ), pop( /private/plan ) )]
    ).



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
	

% no semantic interpretation

rule( selectIcmSemNeg, 
      [ $latest_moves = failed,
	$input = Input,
	not in( $next_moves, icm:sem*neg ),
	not in( $next_moves, icm:per*neg )
      ],
      [ push( next_moves, icm:per*pos:Input ),
	push( next_moves, icm:sem*neg ) ]
    ).

% no part of LU was understood (pragmatically)

rule( selectIcmUndNeg,
      [ not in( $next_moves, icm:sem*neg ),	
	not in( $next_moves, icm:und*neg ),	
	not in( $next_moves, icm:per*neg ),	
	not empty( $latest_moves ),
	% all moves in LU are still in NIM
	forall( in( $latest_moves, M), in($/private/nim, MPAIR) and snd(MPAIR,M) ),
	% there is no ask-move in LU (all ask-moves are understood,
	% so the only reason it could still be there is if it was not accepted)
	not in( $latest_moves, ask(_) ),
	% for all answers in LU, there is no Q on ISSUES
	forall( in( $latest_moves, answer( A ) ),
		not( fst( $/shared/issues, Q) and $domain :: relevant( A, Q ) ) ) ],
      [ % icm:sem*pos for all moves in LU
	forall_do( in( $latest_moves, M), push( next_moves, icm:sem*pos:M ) ),
	push( next_moves, icm:und*neg ),
	% delete all yes/no answer moves in LU from NIM
	% - they may be misunderstood if allowed to stay
	forall_do( in( $latest_moves, M1 ) and
		   ( M1 = answer(A) and

		   %no lexicon when using GF to interpret
 		  % ( ( $lexicon :: yn_answer(A) )  and
		   ( ( A=yes or A=no ) and
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
	if_do( ICM = und*pos:_, timeout := 2.0 ),
	% remove load_plan ICM if plan  is empty (because of accommodation)
	if_do( ICM = loadplan and is_empty( $/private/plan ), del( next_moves, icm:ICM ) ) ]
    ).



rule( selectQuit,
      [ in( $/private/agenda, quit )],
      [ clear( next_moves ),
	push( next_moves, quit ),
	clear( /private/agenda ) ]
    ).

rule( selectGreet, 
      [ fst( $/private/agenda, greet )],
      [ push( next_moves, greet ),
	pop( /private/agenda ) ]
    ).


%D
