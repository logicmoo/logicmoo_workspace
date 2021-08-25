/*
 *	file:		worlds.pl
 *	version:	1.5
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to handle worlds.
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

doState(P) :-
	statePredicate(S),
	C=..[P,S],
	callOnce(C),
	fail.

doState(_).


/*----------------------------------------------------------------------*/

statePredicate(C) :-
	changable(C).

statePredicate(T/2) :-
	relationTypes(Ts),
	member(T,Ts).

statePredicate(A/3) :-
	setof1(X,T^attribute(X,T),As),
	member(A,As).
	

/*----------------------------------------------------------------------*/

saveWorld(W) :-
	atom(W),
	statePredicate(P/N),
	functor(C,P,N),
	clause(C,Body),
	assertz(W:(C:-Body)),
	fail.
saveWorld(W) :-
	atom(W),
	assertz(world(W)).

storeClause(_,end_of_file):-
	!.
storeClause(_,(:-dynamic(_))):-
	!.
storeClause(_,(:-Goal)):-
	!,
	Goal,
	!.
storeClause(To,Clause):-
	stCl(To,Clause).

stCl(W,(H:-B)):-
	assertz(W:(H:-B)),
	!.
stCl(W,Cl):-
	W:Cl,
	!.
stCl(W,Cl):-
	assertz(W:Cl),
	!.


/*----------------------------------------------------------------------*/

restoreWorld(W) :-
	world(W),
	resetState,
	restoreClausesFor(W),
	integrityConstraint.
				% with any world of statePredicate predicates
				% an integrity constraint may be associated.
				% The latter is called upon any reinstallation
				% of a previously saved state
		

resetState:-
	initOpPrec,
%	dbInit,
	on_exception(_,eraseClauseAttributes,true),
	doState(abolish),
	initComplParams,
	retractMemoized,
	!.


retractMemoized:-
	repeat((	memoize(P/N),
			functor(Fact,P,N),
			(	retract((Fact:-!))
			;	retract(Fact)
			)
		)).

/*----------------------------------------------------------------------*/

restoreClausesFor(W) :-
	current_predicate(_,W:H),
	clause(W:H,B),
	assertz((H:-B)),
	fail.
restoreClausesFor(_).

/*----------------------------------------------------------------------*/

deleteWorld(user):-
	resetState,
	!.
deleteWorld(W) :-
	current_predicate(_,W:Q),
	functor(Q,P,N),
	abolish(W:P,N),
	fail.
deleteWorld(W):-
	retract(world(W)).

/*----------------------------------------------------------------------*/

listWorld :-
	write(':-cec_version('),
	cec_version(V,D),
	writeq(V),
	write(','),
	writeq(D),
	write(').'),
	nl,
	setof1(notation(O,P,N),notation(O,P,N),NL),
	mapA(writeOpDecl,NL),
	abolish(simplerProofExistsFor,2),
	doState(declareDynamic),
	doState(listing).



/*----------------------------------------------------------------------*/

declareDynamic(P/N) :-
	nl,
	write(':-dynamic '),
	writeq(P/N),
	write('.').

/*----------------------------------------------------------------------*/

writeOpDecl(notation(O,P,N)) :-
	nl,
	writeq((:-op(P,N,O))),
	write('.').

/*----------------------------------------------------------------------*/
/*	storeOpPrec : stores the operator precedences of the current	*/
/*		      system						*/
/*	initOpPrec :  restores the operator precedences to the		*/
/*		      previously stored operator precedences		*/
/*	deleteCurrentOpPrec : deletes the operator precedences of the	*/
/*			      current system				*/
/*----------------------------------------------------------------------*/

storeOpPrec :-
	setof(op(P,T,A), current_op(P,T,A), Set),
	assertz(opPrec(Set)).

initOpPrec :-
	deleteCurrentOpPrec,
	opPrec(Set),
	mapA(call,Set).

deleteCurrentOpPrec :-
	nullPrec(Prec),
	current_op(_,T,A),
	op(Prec,T,A),
	fail.
deleteCurrentOpPrec.

	


