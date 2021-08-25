/*
 *	file:		robert.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the user interface to clausal completion.
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

:- module(cr,[cr_kb/0,cr_prove/1,cr_show/0,cr_complete/1,cr_completeAll/0]).

:-op(900,xfy,'  v  ').
:-op(1000,xfy,-->).
:-op(600,xfy,:).
:-op(950,xfx,=>).
:-op(850,xfy,and).
:-op(950,fx,'cons').
:-op(950,fx,'op').
:-dynamic(po_order/1).
:-dynamic(c_rule_num/1).

:-compile(rewrite).
:-compile(rpo).
:-compile(theorem).
:-compile(output).
:-compile(normbool).
:-compile(consult).
:-compile(complete).

:-ensure_loaded(library(subsumes)).
:-ensure_loaded(library(unify)).

:-dynamic(counter/1).
:-dynamic(new_rule/3).
:-dynamic(status_lexic/2).
:-dynamic(c_rule/5).

cr_kb :-
 	user:pushState(undo),
	translate_to_cr,
	kb.

cr_prove(Th) :- theorem(Th).

cr_show :- display_c_rules.


cr_completeAll :-
	user:currentPath(PA),
	user:listOfFiles(PA,'*','.eqn',A),
	user:time(mapP(cr_complete,A)).

cr_complete(Module):-
	user:sPrint("


****************** % ******************

",[Module]),
	user:in(Module),
	'cr_kb',
	!.

