
 
/*************************************************************************

         name: update_rules.pl 
  description: IBiS1 update rules
 
*************************************************************************/

:- module(update_rules, [rule/3, rule_class/2]).
:- discontiguous rule_class/2, rule/3.
:- use_module( trindikit(tkit_operators) ).

rule_class( getLatestMove, grounding ).

rule_class( integrateUsrAsk, integrate ).
rule_class( integrateSysAsk, integrate ).
rule_class( integrateAnswer, integrate ).
rule_class( integrateGreet, integrate ).
rule_class( integrateSysQuit, integrate ).
rule_class( integrateUsrQuit, integrate ).

rule_class( downdateQUD, downdate_qud ).
% only needed for "how can I help you" (see p.63)
rule_class(downdateQUD2, downdate_qud).

rule_class( recoverPlan, load_plan ).
rule_class( findPlan, load_plan ).
rule_class( removeFindout, exec_plan ).
rule_class( removeRaise, exec_plan ).
rule_class( exec_consultDB, exec_plan ).



/*----------------------------------------------------------------------

----------------------------------------------------------------------*/

rule( getLatestMove,
      [ $latest_moves = M,
	$latest_speaker = DP  ],
      [ set( /shared/lu/moves, M ),
	set( /shared/lu/speaker, DP ) ] ).


/*----------------------------------------------------------------------
       Move integration rules
----------------------------------------------------------------------*/




rule( integrateSysAsk,
      [ $/shared/lu/speaker == sys,
	in( $/shared/lu/moves, ask(Q) ) ],
      [	%del( /shared/lu/moves, ask(Q) ),
	push( /shared/qud, Q ) ] ). % optimism(acc) 

% 0508
rule( integrateUsrAsk,
       [ $/shared/lu/speaker == usr,
	 in( $/shared/lu/moves,  ask(Q) ) ],
       [ %del( /shared/lu/moves, ask(Q) ),
	 push( /shared/qud, Q ),
	 push( /private/agenda, respond(Q) )  % obligation-level skipped
     ] ).





% Answer

rule( integrateAnswer,
      [ in( $/shared/lu/moves,  answer( A ) ),
	fst( $/shared/qud, Q ),
	$domain :: relevant( A, Q ) ],
      [ %del( /shared/lu/moves, answer( A ) ),
	! $domain :: combine(Q, A, P),
	add( /shared/com, P )

      ] ).

% Greet

rule( integrateGreet,
      [ in( $/shared/lu/moves,  greet ) ],
      [ %del( /shared/lu/moves, greet )
	] ).


% Quit

rule( integrateSysQuit,
       [ $/shared/lu/speaker == sys,
       	 in( $/shared/lu/moves,  quit ) ],
       [ %del( /shared/lu/moves, quit ),
	 program_state := quit ] ).


rule( integrateUsrQuit,
       [ $/shared/lu/speaker == usr,
	 in( $/shared/lu/moves,  quit ) ],
       [ %del( /shared/lu/moves,  quit ),
	 push( /private/agenda, quit )] ).


/*----------------------------------------------------------------------
       Downdate QUD
----------------------------------------------------------------------*/


% Downdate the QUD with questions whose answer has been inferred 
% (e.g. through task accommodation)

rule( downdateQUD,
      [ fst( $/shared/qud, Q ), 
	in( $/shared/com, P ),
	$domain :: resolves( P, Q ) ],
      [ pop( /shared/qud ) ] ).

rule( downdateQUD2,
      [ in( $/shared/qud, IssueQ ), 
	fst( $/shared/qud, Q ),
	IssueQ \= Q, 
	$domain :: resolves( Q, IssueQ )  ],
      [ del( /shared/qud, IssueQ ) ] ).







/*----------------------------------------------------------------------
       Manage plan
----------------------------------------------------------------------*/


rule( recoverPlan,
      [ fst( $/shared/qud, Q ),
	is_empty( $/private/agenda ),
	is_empty( $/private/plan ),
	$domain :: plan( Q, Plan ),
	not ( in($/private/bel, P) and $domain :: relevant( P, Q ) )
	],
%      [ push( /private/agenda, respond(Q) ) ]
      [ set( /private/plan, Plan ) ]
    ).



rule( findPlan,
      [ fst( $/private/agenda, respond(Q) ),
	not ( in( $/private/bel, P ) and $domain::resolves(P, Q) ),
	$domain :: plan( Q, Plan ) ],
      [ pop( /private/agenda ),
	set( /private/plan, Plan ) ]
     ).

%/*
% normal database search
rule( exec_consultDB,
      [ fst( $/private/plan, consultDB(Q) ) ],
      [! $/shared/com = Ps,
       ! $database :: consultDB( Q, Ps,Result ),
       add( /private/bel, Result ),
       pop( /private/plan ) ] ).
%*/

% extended database search format (see p.68)
rule( exec_consultDB,
      [ fst( $/private/plan, consultDB(Q) )  ],
      [! $/shared/com = Ps,
       ! $database :: consultDBx( Q, Ps,Result ),
       extend( /private/bel, Result ),
       pop( /private/plan ) ] ).


% remove "findout" action when question has been answered

rule( removeFindout,
       [ fst( $/private/plan, findout(Q) ),
	 in( $/shared/com, P ),
	 $domain :: resolves( P, Q ) ],
       [ pop(/private/plan) ]
     ).

rule( removeRaise,
       [ fst( $/private/plan, raise(Q) ),
	 in( $/shared/com, P ),
	 $domain :: resolves( P, Q ) ],
       [ pop(/private/plan) ]
     ).






