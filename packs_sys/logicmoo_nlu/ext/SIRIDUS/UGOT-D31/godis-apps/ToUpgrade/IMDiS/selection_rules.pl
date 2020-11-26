
 
/*************************************************************************

         name: selection_rules.pl 
      version: Apr 7, 1999
  description: The selection rules
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

:- op(800, fx, '!').


/*----------------------------------------------------------------------
      Port to TrindiKit 1.0  rule syntax
----------------------------------------------------------------------*/

rule( RuleName, Preconds, Effects ):-
	srule( RuleName, _, Preconds, Effects0, Moves ),
	% Effects = [(next_moves: set( set(Moves) )) | Effects0].
        Effects = [ set(next_moves, set(Moves) ) | Effects0].

rule_class( RuleName, select ):-
	srule( RuleName, _, _, _, _ ).


/*----------------------------------------------------------------------
     Definitions of s-rules, syntax:
     srule( RuleName, RuleType, PrecondList, EffectsList, MoveList )
----------------------------------------------------------------------*/



% system could not interpret

srule( select( reqRep(understanding)), _,
	       [not(input $== ""),
		latest_moves $== failed],
	       [],
	       [reqRep(understanding)]).
 
% system could not integrate info 
/*
srule( select( reqRep(relevance) ), _,
       [ not( input $== ""),
	 not( lm_integrated ) ],	
       [],
       [ reqRep( relevance ) ] ).
*/

%%% raise questions

srule( select( ask(Q) ), _, 
       [ % emptyRec( private^tmp^lm^moves ),
	 fstRec( private^agenda, raise(Q) ) ],
       [],
       [ ask(Q) ]
     ).

%%% repeat latest move

srule( select( repeat(M) ), _, 
       [ % emptyRec( private^tmp^lm^moves ),
	 fstRec( private^agenda, repeat(M) ) ],
       [ popRec( private^agenda )],
       [ repeat(M) ]
     ).

%%% quit

srule( select( quit ), _,
       [ fstRec( private^agenda, quit ) ],
       [],
       [ quit ]
     ).

%%% Any other move

srule( select( M ), _,
       [ fstRec( private^agenda, M ) ],
       [],
       [ M ]
     ).

/*
%%% nothing to do -> ask for new task

srule( select( ask(X^(task=X)) ), _,
       [ emptyRec( private^agenda ) ],
       [],
       [ ask(X^(task=X)) ]
     ).
*/

%%% nothing to do -> quit

srule( select( quit ), _,
       [ emptyRec( private^agenda ) ],
       [ set( program_state, quit ) ],
       [ quit ]
     ).

