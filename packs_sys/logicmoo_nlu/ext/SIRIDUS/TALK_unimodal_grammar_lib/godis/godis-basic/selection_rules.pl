
 
/*************************************************************************

         name: selection_rules.pl 
  description: The selection rules
 
*************************************************************************/

:- use_module( library(tis_operators) ).

rule_class( selectRespond, select_action ).
rule_class( selectFromPlan, select_action ).
rule_class( reraiseIssue, select_action ).

rule_class( selectAnswer, select_move ).
rule_class( selectAsk, select_move ).
rule_class( selectOther, select_move ).


% add respond-action to agenda if resolving answer is private but not shared

rule( selectRespond,
      [ is_empty( $/private/agenda ),
	is_empty( $/private/plan ),
	fst( $/shared/qud, Q ),
	in( $/private/bel, P ),
	not in( $/shared/com, P ), 
	$domain :: relevant(P, Q ) ],
      [ push( /private/agenda, respond( Q ) ) ] ).

% refill the agenda with the next action on the plan

rule( selectFromPlan,
       [ is_empty( $/private/agenda ),
	 fst( $/private/plan, Action ) ],
       [ push( /private/agenda, Action ) ]
     ).



rule( reraiseIssue, % only if plan is empty
       [ fst( $/shared/qud, Q ),
	 not $domain::plan( Q, _ )
	 %not in( $/private/agenda, respond( Q ) )
       ],
      [ push( /private/agenda, raise(Q) ) ] ).

%%% answer a question

rule( selectAnswer, 
      [ fst( $/private/agenda, respond(Q) ),
	in( $/private/bel, P ),
	not in( $/shared/com, P ), 
	$domain :: relevant( P, Q ) ],
      [ add( next_moves, answer( P ) ) ]
    ).

%%% findout/raise questions
rule( selectAsk, 
      [ or( fst( $/private/agenda, findout(Q) ),
	    fst( $/private/agenda, raise(Q) ) )],
      [ add( next_moves, ask(Q) ),
	if_do( fst( $/private/plan, raise(Q) ), pop( /private/plan ) ) ]
    ).



rule( selectOther, 
      [ fst( $/private/agenda, M ),
	( M = greet or M = quit )],
      [ add( next_moves, M ) ]
    ).
