 
/*************************************************************************

         name: domain_medical.pl 
  description: Medical domain file
 
*************************************************************************/

:- module( domain_medical, [ plan/2,
			     issue/2,
			     sort_restr/1,
			     isa/2,
			     postcond/2,
			     depends/2,
			     incompatible/2
			   ] ).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

:- ensure_loaded( library( semsort_medical ) ).



/*----------------------------------------------------------------------
     Dialogue plans

     plan( ?ActionOrIssue, ?Plan )
----------------------------------------------------------------------*/


plan( top,
      [ forget_all,
	findout( set( [ issue(X^disease(X)), issue(X1^info(X1) ) ] ) ) ] ).
postcond( top, [] ).


plan( X1^disease(X1),
      [ % try to get a best question from db
	dev_query( medical, X2^bestq(X2) ),
	% if there is a best question, try to  find out the answer
	if_then( bestq(Q),
		 [ findout(Q),
		   % clean up
		   forget( bestq(Q) ) ] ),
	% check if db can determine a disease
	dev_query( medical, X3^disease(X3)),
	% if successful, GoDiS will answer disease issue
	% if not, clean up
	if_then( fail(X5^disease(X5),_),
		      forget(fail(X5^disease(X5),_) ) ),
	% enable accommodation, since findout is embedded in "if_then"
	bind( X4^symptom(X4) ),
	bind( X7^medicalHistory(X7) ) ] ).

plan( confirmed_by_interview,
      [ %findout(X^disease(X)), %V2
	% try to get a best question from db
	dev_query( medical, X2^bestq(X2) ),
	% if there is a best question, try to  find out the answer
	if_then( bestq(Q),
		 [ findout(Q),
		   % clean up
		   forget( bestq(Q) ) ] ),
	% check if db can determine a disease
	dev_query( medical, confirmed_by_interview ),
	% if successful, GoDiS will answer disease issue
	% if not, clean up
	if_then( fail(confirmed_by_interview),
		 forget( fail(confirmed_by_interview ) ) ),
	% if there are no more questions, the result was not confirmed
	if_then( fail(X1^bestq(X1)),
		 assume( not(confirmed_by_interview) ) ),
	% enable accommodation, since findout is embedded in "if_then"
	bind( X4^symptom(X4) ),
	bind( X7^medicalHistory(X7) ) ] ).



plan( confirmed_by_tests,%positive_diagnosis, %
      [ % try to get a best labtest from db
	dev_query( medical, X9^best_labtest(X9) ),
	% if there is a best labtest, get nurse to perform test 
	if_then( best_labtest(T), % T is y/n question
		 [ % tell the user that test is pending
		   report( take_test(T), pending ),
				% assume that test-result issue is now shared
%		   assume_issue( X1^test_result(X1) ),
		   assume_issue( test_result(T) ),
				% activate nurse-device to take the test
		   dev_do( medical, take_test(T) ),
				% get results of test
%		   dev_query( medical, X6^test_result(X6) ),
		   dev_query( medical, test_result(T) ),
		   % clean up
		   forget( status(T, pending ) ),
		   forget( best_labtest(T) )
		 ] ),
	% chec if db can determine a positive (or negative) diagnosis
	dev_query( medical, confirmed_by_tests ), % returns p_d, not(p_d) or fail(p_d)
	% if successful, GoDiS will answer positive_diagnosis issue
	% if not, clean up (and start again)
	if_then( fail( confirmed_by_tests ),
		 forget( fail(confirmed_by_tests) ) ),
	if_then( fail(X2^best_labtest(X2)),
		 assume( not(confirmed_by_test) ) )
	] ).	
      


% binds X^info_disease(X), but does not ask if X^disease(X) resolved
plan( X^info(X),
      [ findout( X1^info_disease(X1) ),
	if_then_else( disease(D),
		      assume(info(D)),
		      if_then( info_disease(D),
			       assume(info(D)) ) ) ]).

% binds X^info_disease(X), but does not ask if X^disease(X) resolved
plan( X^treatment(X),
      [ findout( X1^info_disease(X1) ),
	if_then_else( disease(D),
		      assume(treatment(D)),
		      if_then( info_disease(D),
			       assume(treatment(D)) ) ) ]).


plan( up, [] ).



/*--------------------------------------------------------------
     Conceptual knowledge
----------------------------------------------------------------------*/

% sortal restrictions; determine relevance and resolvement

sort_restr( disease( X ) ) :- sem_sort( X, disease ).
sort_restr( bestq( X ) ) :- of_type( X, question ).
sort_restr( symptom( X ) ) :- sem_sort( X, symptom ).
sort_restr( info( X ) ) :- sem_sort( X, disease ).
sort_restr( medicalHistory( X ) ) :- sem_sort( X, medicalHistory ).
sort_restr( test_result( X ) ) :- sem_sort( X, test ).
sort_restr( info( X ) ) :- sem_sort( X, disease ).
sort_restr( info_disease( X ) ) :- sem_sort( X, disease ).
sort_restr( treatment( X ) ) :- sem_sort( X, disease ).

% GENERAL STUFF

% 0-place predicates
sort_restr( Pred0 ) :- Pred0 =.. [_].

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


% valid parameter same as sortal restrictions
% so everything that's understood is accepted

valid_parameter( Prop ):- sort_restr( Prop ).



default_question(dummy).
depends( dummy, dummy ).

% never two diseases at once
incompatible(disease(D1), disease(D2)) :-
	D1 \= D2.
