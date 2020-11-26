 
/*************************************************************************

         name: domain_autoroute.pl 
      version: 
  description: An example domain file
       author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

:- module( domain_autoroute, [ relevant_to_task/3,
			       relevant_answer/2,
			       plan/2,
			       reduce/3
			     ] ).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).





/*----------------------------------------------------------------------
     Dialogue plans

     plan( ?Name, ?Plan )
----------------------------------------------------------------------*/

plan( top, [ findout(X^task(X)) ] ).

plan( get_trip_info, [ findout(X1^(from(X1))),
		       findout(X2^(to(X2))),
		       findout(X3^(time(X3))),
		       findout(X4^(quickshort(X4))),
		       consultDB(Y^dist(Y)),
		       consultDB(Z^dur(Z)),
		       if_then_else( quickshort(shortest),
				     respond(X5^(dist(X5))),
				     respond(Dur^(dur(Dur))) ),
		       thank,
		       forget,
		       exec(top)
		     ] ).

sysaction(dummy).
/*----------------------------------------------------------------------
     relevant_answer( -Question, +Answer )
     -- Returns (if it succeeds) a Question to which
        the Answer is relevant 
----------------------------------------------------------------------*/


relevant_answer( X^(to(X)), A) :-
	user:val(lexicon,Lexicon),
	Lexicon: location( A ).
relevant_answer( X^(to(X)), (to(A))) :-
	user:val(lexicon,Lexicon),
	Lexicon: location( A ).

relevant_answer( X^(from(X)), A) :-
	user:val(lexicon,Lexicon),
	Lexicon: location( A ).
relevant_answer( X^(from(X)), (from(A))) :-
	user:val(lexicon,Lexicon),
	Lexicon: location( A ).

relevant_answer( X^(time(X)), A) :-
	user:val(lexicon,Lexicon),
	Lexicon: time( A ).
relevant_answer( X^(time(X)), (time(A))) :-
	user:val(lexicon,Lexicon),
	Lexicon: time( A ).

relevant_answer( X^(quickshort(X)), A) :-
	user:val(lexicon,Lexicon),
	Lexicon: quickshort( A ).
relevant_answer( X^(quickshort(X)), (quickshort(A))) :-
	user:val(lexicon,Lexicon),
	Lexicon: quickshort( A ).

relevant_answer( X^(task(X)), A):-
	user:val(lexicon,Lexicon),
	Lexicon: task( A ).
relevant_answer( X^(task(X)), (task(A))):-
	user:val(lexicon,Lexicon),
	Lexicon: task( A ).

relevant_answer( X^(dist(X)), (dist(A))):-
	user:val(lexicon,Lexicon),
	Lexicon: dist( A ).

relevant_answer( X^(dur(X)), (dur(A))):-
	user:val(lexicon,Lexicon),
	Lexicon: dur( A ).







/***** GENERAL GODIS STUFF ******/




%%% Yes/no question P?

% yes
relevant_answer( Q, yes) :-
	ynq(Q).
% no
relevant_answer( Q, no) :-
	ynq(Q).
% P
relevant_answer( Q, P ) :-
	ynq(Q),
	P = Q.
% P
relevant_answer( Q, not(P) ) :-
	ynq(Q),
	P = Q.

% Alt-questions, full answer
relevant_answer( AltList, Alt ):-
	member( Alt, AltList ). 


%%% Definition of yes/no questions

ynq( YNQ ):-
	\+ YNQ = _^_,		% whq
	\+ YNQ = [_|_].		% altq


/*----------------------------------------------------------------------
     relevant_to_task( +Move, -Task, -Plan )
     -- Returns (if it succeeds) a Plan to which Move is relevant
----------------------------------------------------------------------*/


relevant_to_task( Move, Task, Plan ):-
	plan( Task, Plan ),
	Move = answer( A ),
	member( findout( Q ), Plan ),
	relevant_answer( Q, A ).


all_answers( Q, As ):-
	setof( A, relevant_answer( Q, A ), As ).


/*----------------------------------------------------------------------
     relevant_to_tasks( +Move, -Tasks )
     -- Returns (if it succeeds) a list of tasks Tasks to which Move is
     relevant; all elements in Task have the form task(T)
----------------------------------------------------------------------*/

relevant_to_task( Move, Task ):-
	plan( Task, Plan ),
	Move = answer( A ),
	member( findout( Q ), Plan ),
	relevant_answer( Q, A ).

relevant_to_tasks( Move, Tasks ):-
	setof( task(Task), relevant_to_task( Move, Task ), Tasks ).



/*----------------------------------------------------------------------
   dominates( T1, T2 )
   -- Task T1 dominates T2 in the menu hierarchy
----------------------------------------------------------------------*/

dominates0( T1, T2 ):-
	plan( T1, [ findout( Ts ) | _ ] ),
	member( task( T2 ), Ts ).

dominates( T1, T2 ):-
	dominates0( T1, T2 ).

dominates( T1, T3 ):-
	dominates0( T1, T2 ),
	dominates( T2, T3 ).
	

/*----------------------------------------------------------------------
   reduce( +Q, +A, -P )

   -- reduces a quesition and an answer to a proposition

   ** this should perhaps be in the definition of the datatypes "question"
   and "answer", as an operation which takes a question and an answer
   and yields a question
----------------------------------------------------------------------*/

/*
** Y/N-questions
*/

reduce( Q, yes, P ):-
	ynq( Q ),
	P = Q. 

reduce( Q, no, P ):-
	ynq( Q ),
	P = not( Q ).

reduce( Q, P, P ):-
	ynq( Q ),
	P = Q.

reduce( Q, not(P), not(P) ):-
	ynq( Q ),
	P = Q.

/*
** Alt-questions
*/

% findout([ A1, A2, ..., An ]) is answered by A1 or A2 or .... or An

reduce(AltList, A, P):-
	member(A, AltList),!,
	P = A.

% findout([Pred(_),...]) is answered by Pred(_)

reduce(AltList, A, P):-
	AltList = [ A0 | _ ],
	A0 =.. [ Pred | _ ],
	A =.. [ Pred | _ ],!,
	P = A.

/*
** WH-questions
*/

% X^Q is answered by an atom (ellipsis)

reduce(X^P, X, P):-
	atom(X).

% X^Q is answered by full proposition

reduce( X^Q, A, P ):-
	Q =.. [Y,X],
	A =.. [Y,X],
	P = Q,
	!.
