
 
/*************************************************************************

         name: selection_rules.pl 
      version: Apr 7, 1999
  description: The selection rules
       author: Peter Bohlin, Staffan Larsson, Stina Ericsson
 
*************************************************************************/

:- use_module( library(tis_operators) ).


%rule_class( deleteICM, select_move ).
%rule_class( deleteICM, select_icm ).

rule_class( rejectIssue, select_action ).
rule_class( rejectProp, select_action ).

rule_class( selectIcmUndIntAsk, select_action ).
rule_class( selectIcmUndIntAnswer, select_action ).

rule_class( selectRespond, select_action ).
rule_class( selectFromPlan, select_action ).
rule_class( reraiseIssue, select_action ).

rule_class( selectIcmConNeg, select_icm ).
rule_class( selectIcmPerNeg, select_icm ).
rule_class( selectIcmSemNeg, select_icm ).
rule_class( selectIcmUndNeg, select_icm ).
rule_class( selectIcmOther, select_icm ).


rule_class( selectAnswer, select_move ).
rule_class( selectAsk, select_move ).
rule_class( selectOther, select_move ).
rule_class( selectIcmOther, select_move ). % to select ICM which has appear as consequence of action selection

%rule_class( _, 'NONE' ).

% select action (just one)

rule( rejectIssue,
      [ in( $/private/nim, ask(Q) ),
	$/shared/lu/speaker = usr,
	not $domain :: plan( Q, _ ) ], 
      [ del( /private/nim, ask(Q) ),
	push( /private/agenda, icm:und*pos:usr*issue(Q) ),
	push( /private/agenda, icm:acc*neg:issue(Q) ) % isis2
      ] ).

rule( rejectProp,
      [ in( $/private/nim, answer( A ) ), % full or underspecified,
	$/shared/lu/speaker = usr,
	fst( $/shared/qud, Q ),
	$domain :: relevant( A, Q ),
	$domain :: combine( Q, A, P ),
	not ( $database :: validDBparameter( P ) ) ], 
      [ del( /private/nim, answer( A ) ),
	push( /private/agenda, icm:und*pos:usr*P ),
	push( /private/agenda, icm:acc*neg:P ) 
      ] ).

% add respond-action to agenda if resolving answer is private but not shared
% and answer( P ) is not already selected in next_moves (might alerady have
% been selected )

rule( selectRespond,
      [ is_empty( $/private/plan ),
	fst( $/shared/qud, Q ),
	in( $/private/bel, P ),
	not in( $/shared/com, P ), 
	$domain :: resolves(P, Q ),
	not in( $/private/agenda, respond( Q ) )
      ],
      [ push( /private/agenda, respond( Q ) ) ] ).

% refill the agenda with the next action on the plan

rule( selectFromPlan,
       [ Action = $/private/plan/fst ],
       [ push( /private/agenda, Action ) ]
     ).

% if q has been asked by system but not yet answered [sys has no plan]
% NOT if q has been asked by user but nothing currently being dealt with [empty plan field] - this should be handled by reloadPlan!!!
% NOT if q asked by user & being dealt with by system
% 3rd condition cannot be checked directly, so check 1 and 2
rule( reraiseIssue, % only if plan is empty
       [ in( $/shared/qud, Q ),
	 % only reraise unresolved issues
	 not ( in( $/shared/com, P ) and $domain :: resolves( P, Q ) ),
	 % only reraise unresolved issues
	 not ( in( $/private/nim, answer(A) ) and $domain :: resolves( A, Q ) ),
	 % Q is a system2user question..
	 not $domain::plan( Q, _ ),
	 not in( $/private/agenda, respond( Q ) )
       ],
      [ push( /private/agenda, icm:reraise ),
	push( /private/agenda, raise(Q) ) ] ).



% Copy parts of IS to /private/tmp/sys 
% before optimistically integrating system utterance
% if it's worth repeating in case usr does icm:sem*neg later

rule( backupShared,
       [ ],
       [ /private/tmp/qud := $/shared/qud,
	 /private/tmp/com := $/shared/com,
	 /private/tmp/agenda :=  $/private/agenda,
	 /private/tmp/plan := $/private/plan
	  % delete "pardon"; it should never be repeated anyway
	 %if_do( in( $/private/tmp/agenda, icm:sem*neg ),
		%del( /private/tmp/agenda, icm:sem*neg ) )
	 ] ).



% select_move

%%% answer a question

rule( selectAnswer, 
      [ fst( $/private/agenda, respond(Q) ),
	in( $/private/bel, P ),
	not in( $/shared/com, P ), 
	$domain :: resolves( P, Q ) ],
      [ push( next_moves, answer( P ) ),
	pop( /private/agenda ) ]
    ).

%%% findout/raise questions
rule( selectAsk, 
      [ or( fst( $/private/agenda, findout(Q) ),
	    fst( $/private/agenda, raise(Q) ) ),
	not ( in( $next_moves, Move ) and q_raising_move( Move ) ) ],
      [ push( next_moves, ask(Q) ),
	pop( /private/agenda ),
	if_do( fst( $/private/plan, raise(Q) ), pop( /private/plan ) )
      ] ).


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

rule( selectIcmUndIntAsk,
      [ $/shared/lu/speaker == usr,
	fst( $/private/nim, ask(Q) ),
	$score =< 0.7 ], 
       [ pop( /private/nim ),
	 push( /private/agenda, icm:und*int:usr*issue(Q) ) ] ).

rule( selectIcmUndIntAnswer,
      [ fst( $/private/nim, answer(A) ),
	$/shared/lu/speaker == usr,
	% RELEVANCE
	fst( $/shared/qud, Q ),
	$domain :: relevant( A, Q ),
	% ACCEPTABILITY
	$domain :: combine( Q, A, P ),
	$score =< 0.7 ], 
       [ pop( /private/nim ),
	 push( /private/agenda, icm:und*int:usr*P ) ] ).


rule( selectIcmUndNeg,
      [ %not in( $next_moves, icm:_ICM ),
%	not in( $next_moves, icm:sem*neg ),	
	not in( $next_moves, icm:und*neg ),	
%	not $latest_moves == failed,
	in( $latest_moves, answer(_) ),
	% all moves in LU are still in NIM
	forall( $latest_moves/elem =  M, $/private/nim/elem = M ),
	% there is no ask-move in LU (all ask-moves are understood,
	% so the only reason it could still be there is if it was not accepted)
%	not in( $latest_moves, ask(_) ),
	% for all answers in LU, there is no Q on ISSUES
	forall( $latest_moves/elem =  answer( A ),
		not( fst( $/shared/qud, Q) and $domain :: relevant( A, Q ) ) ) ],
      [ % icm:sem*pos for all moves in LU
	forall_do( $latest_moves/elem =  M, push( next_moves, icm:sem*pos:M ) ),
	push( next_moves, icm:und*neg ) ] ).



rule( selectIcmOther, 
      [ in( $/private/agenda, icm:ICM ),
	not ( in( $next_moves, A ) and q_raising_icm(A) ),
	not ( in( $next_moves, A ) and A = ask( _ ) )
      ],
      [ push( next_moves, icm:ICM ),
	del( /private/agenda, icm:ICM ) ]
    ).



rule( selectOther, 
      [ fst( $/private/agenda, M ),
	( M = greet or M = quit )],
      [ push( next_moves, M ),
	pop( /private/agenda ) ]
    ).

