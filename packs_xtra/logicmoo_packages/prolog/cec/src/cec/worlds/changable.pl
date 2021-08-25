/*
 *	file:		changable.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	Declares all cec-predicates that may change during execution.
 *	Declares the content of specification variable '$initial'
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

% changable predicates. 
% Note: for each of these predicates a combine-procedure must exist

changable(notation/3).          % must be first in list of state predicates
changable(cont/2).		%
changable(hhConstructor/1).	%
changable(domainAxiomHasBeenGenerated/1).
changable(ac_ist_A/1).		%
changable(ac_ist_C/1).		%
changable(ofType/2). 		%
changable(reduce/2).		%
changable(reduceI/3).		%

changable(specialLaw/1). 	%

changable(tps_state/1).		%
changable(kns_gt/2).		%
changable(kns_eq/2).		%
changable(kns_status/2).	%

changable(pretty/1).		%
changable(parse/2).		%

changable(clauseHasTerm/3).	%
changable(mayBeReducible/3).	%

changable(predicate/1).		%

changable('$maySuperpose'/7).	%

changable(subsort/2).
changable(inheritanceAxiom/2).

changable(action/2).
changable(actionNew/2).

changable(simplerProofExistsFor/2).

changable(parameterSort/1).
changable(generatingMsOps/2).
changable(unconstrainedSort/1).

/*----------------------------------------------------------------------*/

:- dynamic cont/2.
:- dynamic hhConstructor/1.
:- dynamic domainAxiomHasBeenGenerated/1.
:- dynamic kns_gt/2.
:- dynamic kns_eq/2.
:- dynamic kns_status/2.
:- dynamic notation/3.
:- dynamic ac_ist_A/1.
:- dynamic ac_ist_C/1.
:- dynamic ofType/2.
:- dynamic reduce/2.
:- dynamic reduceI/3.
:- dynamic specialLaw/1.

:- dynamic tps_state/2.

:- dynamic pretty/1.

:- dynamic parse/2.

:- dynamic clauseHasTerm/3.
:- dynamic mayBeReducible/3.

:- dynamic predicate/1.

:- dynamic '$maySuperpose'/7.

:- dynamic subsort/2.

:- dynamic inheritanceAxiom/2.

:- dynamic world/1.

:- dynamic narrterm/5.
:- dynamic narrterm/6.
:- dynamic solution/2.
:- dynamic job/1 .

:- dynamic action/2.
:- dynamic actionNew/2.

:- dynamic simplerProofExistsFor/2.

:- dynamic parameterSort/1.
:- dynamic generatingMsOps/2.
:- dynamic unconstrainedSort/1.

memoize(characteristicsW/6).
memoize(opsInTermW/6).
memoize(kns_isConsistent_positive/2).
memoize(kns_isConsistent_negative/2).
memoize(kns_eqstar_positive/3).
memoize(kns_eqstar_negative/3).
memoize(kns_gtOrEqstar_positive/3).
memoize(kns_gtOrEqstar_negative/3).
memoize(conditionReduceClause/3).


/*----------------------------------------------------------------------*/
% initial state of world $initial :
/*----------------------------------------------------------------------*/

world('$initial').


:-assert('$initial':tps_state(tps(neqkns,pol_state(1,[],[],[]),kns_state(on,prec(0,[]))))).


:-assert('$initial':kns_status('$r',ms)).
:-assert('$initial':kns_status('$r-ms',ms)).


:-assert('$initial':cont(ctr('$equation'),0)).
:-assert('$initial':cont(ctr('$rule'),0)).
:-assert('$initial':cont(ctr('$nonoperational equation'),0)).


