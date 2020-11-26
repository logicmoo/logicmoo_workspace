 
/*************************************************************************

         name: domain_icecream.pl 
  description: ICECREAM domain file
 
*************************************************************************/

:- module( domain_icecream, [ plan/2,
			      issue/2,
			      sort_restr/1,
			      isa/2,
			      depends/2
			  ] ).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

:- ensure_loaded( library( semsort_icecream ) ).



/*----------------------------------------------------------------------
     Dialogue plans

     plan( ?ActionOrIssue, ?Plan )
----------------------------------------------------------------------*/


default_question(dummy).

% menus

plan( top,
      [ forget_all,
	raise( X^flavour(X) ) ] ).


/*--------------------------------------------------------------
     Conceptual knowledge
----------------------------------------------------------------------*/

% sortal restrictions; determine relevance and resolvement

sort_restr( flavour( X ) ) :- sem_sort( X, flavour ).
% negation

sort_restr( not P ) :- sort_restr( P ).


% action

sort_restr( action( X ) ) :- sem_sort( X, action ).
sort_restr( action( respond(Q) ) ) :- sort_restr( issue(Q) ).

% issue

sort_restr( issue(Q) ):- plan( Q, _ ),
	\+ sort_restr( action( Q ) ).

sort_restr( issue(Q) ):-
	plan( _, Plan ),
	member( findout(Q), Plan ),
	\+ sort_restr( action( Q ) ).

% metaissue

sort_restr( und(_DP*P) ):- sort_restr(P).

% sloppy, but allows "no" as answer to clarification alt-q
% could be replaced by general rule saying that not(set([p1, ..., ])) is sortally correct,
% menaing the same as (not P1 and not p2 and...)

sort_restr( not und(_DP*set(_))).




	
