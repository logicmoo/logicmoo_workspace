
 
/*************************************************************************

         name: update_rules.pl 
      version: Apr 7 - Nov 29, 1999
  description: The update rules
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

:- module(update_rules, [rule/3, rule_class/2]).


:- op(800, fx, ['!', not]).
:- op(850, xfx, ['$=', '$==', and, or] ).

rule_class( assumeSysMovesGrounded, grounding ).

rule_class( integrate_instruct_check, integrate ).
rule_class( integrate_instruct_exec, integrate ).
rule_class( integrateSysInform, integrate ).
rule_class( integrateSysGreet, integrate ).
% rule_class( integrateSysQuit, integrate ).
rule_class( autoConfirm, integrate ).

rule_class( findPlan, manage_plan ).
rule_class( removeTask, manage_plan ).
rule_class( parse_if_then, manage_plan ).
rule_class( parse_subaction, manage_plan ).

rule_class( downdateAgenda(instruct_check), refill ).
rule_class( downdateAgenda(instruct_exec), refill ).
rule_class( downdateAgenda(inform), refill ).
rule_class( refillAgendaFromPlan, refill ).

rule_class(saveShared, store).


/*----------------------------------------------------------------------
       Grounding (cautious)
----------------------------------------------------------------------*/

% optimism about user recognizing system contributions

rule( assumeSysMovesGrounded,
       [ latest_speaker $== sys ],
       [ set#rec( shared^lu^speaker, sys),
	 clear#rec( shared^lu^moves ),
	 forall( in( latest_moves, Move),
		 add#rec( shared^lu^moves, Move, false) ) ] ).



/*----------------------------------------------------------------------
       Integration of the system's moves
       
----------------------------------------------------------------------*/

rule( integrate_instruct_exec,
      [ val#rec( shared^lu^speaker, sys ),
	assoc#rec( shared^lu^moves, instruct_exec(A), false ),
	fst#rec( private^agenda, instruct_exec(A) ) ],
      [ push#rec( shared^actions, A ), 
	set_assoc#rec( shared^lu^moves, instruct_exec(A), true) ] ).

rule( integrate_instruct_check,
      [ val#rec( shared^lu^speaker, sys ),
	assoc#rec( shared^lu^moves, instruct_check(P), false ),
	fst#rec( private^agenda, instruct_check(P) ) ],
      [ push#rec( shared^actions, check(P) ), 
	set_assoc#rec( shared^lu^moves, instruct_check(P), true) ] ).

rule( integrateSysInform,
      [ val#rec( shared^lu^speaker, sys ),
	assoc#rec( shared^lu^moves, inform(P), false ),
	fst#rec( private^agenda, inform(P) ) ],
      [	add#rec( shared^com, P ), % optimism(acc)
	set_assoc#rec( shared^lu^moves, inform(P), true) ] ).

rule( integrateSysGreet,
       [ val#rec( shared^lu^speaker, sys ),
       	 assoc#rec( shared^lu^moves,  greet, false ),
	 fst#rec( private^agenda, greet ) ],
       [ pop#rec( private^agenda ),
	 set_assoc#rec( shared^lu^moves, greet, true ) ] ).

rule( integrateSysQuit,
       [ val#rec( shared^lu^speaker, sys ),
       	 assoc#rec( shared^lu^moves,  quit, false ),
	 fst#rec( private^agenda, quit ) ],
       [ pop#rec( private^agenda ),
	 set( program_state, quit ),
	 set_assoc#rec( shared^lu^moves, quit, true ) ] ).





rule( autoConfirm,
      [ fst#rec( shared^actions, A ) ],
      [ pop#rec( shared^actions ),
	add#rec( shared^com, done(A) ) ] ).

	 



/*----------------------------------------------------------------------
       manage_plan
----------------------------------------------------------------------*/

rule( removeTask,
      [ in#rec( shared^com, task(T) ),
	domain :: tplan( T, G, _ ),
	in#rec( shared^com, G ) ],
      [ del#rec( shared^com, task(T) ) ] ).

rule( findPlan,
       [ empty#rec( private^plan ),
	 in#rec( shared^com, task(E) ),
	 ! ( domain :: tplan( E, _, Plan ) ) ],
      [ set#rec( private^plan, Plan )
	%, add#rec( shared^com, task(E),
       ]
     ).

rule( parse_if_then, % P true
      [ fst#rec( private^plan, if_then( P, A ) ),
	in#rec( shared^com, P ) ],
      [ pop#rec( private^plan ),
	push#rec( private^plan, A ) ] ).

rule( parse_if_then, % P false
      [ fst#rec( private^plan, if_then( P, A ) ),
	in#rec( shared^com, (not P) ) ],
      [ pop#rec( private^plan ) ] ).

rule( parse_if_then,		% (not P) true; horrible hack until we have some inference...
      [ fst#rec( private^plan, if_then( (not P), A ) ),
	in#rec( shared^com, P ) ],
      [ pop#rec( private^plan ) ] ).

rule( parse_subaction,
      [ fst#rec( private^plan, A ),
	domain :: tplan( A, _, Plan ) ],
      [ pop#rec( private^plan ),
	% add#rec( shared^com, task(A) ), % NEED TO HANDLE MULTIPLE TASKS!
	extend#rec( private^plan, Plan ) ] ).



/*----------------------------------------------------------------------
       Refill agenda
----------------------------------------------------------------------*/


% it would be possible to have a general rule which checked the n-1th move and if it was the one on the agenda, took it off. however, this relies on knowing the n-1th move which depends on grounding strategy.

% THIS DEFINES THE GOALS OF ACTIONS!!! shows that raise should be called "findout" or similar

rule( downdateAgenda(raise),
       [ fst#rec( private^agenda, raise(P) ),
	 ( in#rec( shared^com, P ) or in#rec( shared^com, (not P) ) ) ],
       [ pop#rec( private^agenda ) ]
     ).

%whq
rule( downdateAgenda(raise),
       [ fst#rec( private^agenda, raise(X^P) ),
	 in#rec( shared^com, P ) ],
       [ pop#rec( private^agenda ) ]
     ).

rule( downdateAgenda(instruct_check),
       [ fst#rec( private^agenda, instruct_check(A) ),
	 in#rec( shared^com, done(check(A)) ) ],
       [ pop#rec( private^agenda ) ]
     ).

rule( downdateAgenda(instruct_exec),
       [ fst#rec( private^agenda, instruct_exec(A) ),
	 in#rec( shared^com, done(A) ) ],
       [ pop#rec( private^agenda ) ]
     ).

rule( downdateAgenda(inform),
       [ fst#rec( private^agenda, inform(P) ),
	 in#rec( shared^com, P ) ],
       [ pop#rec( private^agenda ) ]
     ).


rule( refillAgendaFromPlan,
       [ empty#rec( private^agenda ),
	 fst#rec( private^plan, A ) ],
       [ pop#rec( private^plan ),
	 push#rec( private^agenda, A ) ] ).


/*----------------------------------------------------------------------
       Store
----------------------------------------------------------------------*/

rule( saveShared,
       [ latest_speaker $== sys ],
       [ copyRec( shared, private^tmp )] ).
