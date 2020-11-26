
:- module( domain_telvcrlogin, [ plan/2,
			       issue/2,
			       sort_restr/1,
			       isa/2,
			       postcond/2,
			       depends/2
			     ] ).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

:- ensure_loaded( library( semsort_telvcrlogin ) ).
:- ensure_loaded( library( plan_operators ) ).

/*----------------------------------------------------------------------
     Dialogue plans

     plan( ?ActionOrIssue, ?Plan )
----------------------------------------------------------------------*/
default_question(dummy).
depends(dummy,dummy).

%Actions

% plan(top,
%      [ inform(usage),
%	do(login)
%      ]).



%postcond( top, none ).

%27/9 protect construction added (see update_rule exec_protect)
    
plan( top,
      [ protect( findout(X^user_name(X)) ),
	protect( findout(Y^password(Y)) ),
	if_then( user_name(X) and password(Y), 
		      [ dev_query( vcr_login, validUser(X,Y,Res) ),
			if_then_else( validUser(X,Y,yes) and user_name(X),
				      [	report(vcr_login, validUser(X,Y,yes)),
					change_domain(telvcr,[user_name(X)])],
				      [ report(vcr_login, validUser(X,Y,no))]
			       )
		      ]
	       ),
	forget_all ] ).

postcond(login , user_logged_in(true)).
postcond(login , user_logged_in(false)).


%output f report
%update_rule change_domain




			       





/*--------------------------------------------------------------
     Conceptual knowledge
----------------------------------------------------------------------*/

% sortal restrictions; determine relevance and resolvement
sort_restr( user_name( X ) ) :- sem_sort( X, alphadigit ).
sort_restr( password( X ) ) :- sem_sort( X, alphadigit ).
sort_restr( alphadigit( X ) ) :- sem_sort( X, alphadigit ).

% metaissue
sort_restr( und(_DP*P) ):- sort_restr(P).

% sloppy, but allows "no" as answer to clarification alt-q
% could be replaced by general rule saying that not(set([p1, ..., ])) is sortally correct,
% menaing the same as (not P1 and not p2 and...)

sort_restr( not und(_DP*set(_))).


% action

sort_restr( action( X ) ) :- sem_sort( X, action ).
sort_restr( action( respond(Q) ) ) :- sort_restr( issue(Q) ).


sort_restr( issue(Q) ):- plan( Q, _ ),
        \+ sort_restr( action( Q ) ).

sort_restr( issue(Q) ):-
        plan( _, Plan ),
        member( Plan, findout(Q) ),
        \+ sort_restr( action( Q ) ).


valid_parameter( user_name(N) ):- sem_sort( N, alphadigit ).
valid_parameter( password(N) ):- sem_sort( N, alphadigit ).
valid_parameter( action(A) ):- sem_sort(A, action).
