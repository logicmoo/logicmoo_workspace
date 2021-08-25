/*
 *	file:		funtest.pl
 *	version:	1.0
 *	date:		November 28, 1989
 *	creation:	-
 *	author:		Ullrich Hustadt
 *
 *	description:
 *	This file contains the definitions of predicate funtest.
 *
 *
 *	history:
 *	891010	js	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

/*----------------------------------------------------------------------*/
/* funtest(+Pred,+Term,+NumberOfCalls)					*/
/* generates the goal Pred(Term,Result), and forces backtracking until  */
/* fails or NumberOfCalls of Instantiations for Result have been 	*/
/* computed. The results are printed.					*/

funtest(Pred,Term,MaxCalls) :-
   callCounter:==1,
   Call=..[Pred,Term,X], 
   print('Results: '), nl, !,
   Call,
   print(X), nl,
   cont1(callCounter,Z1),
   ( Z1 = MaxCalls -> 
         !
         ;
         (Z2 is Z1+1, callCounter:==Z2, fail)
   ).
