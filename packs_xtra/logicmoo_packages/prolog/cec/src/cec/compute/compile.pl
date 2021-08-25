/*
 *	file:		compile.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the predicates for the compilation of a set
 * 	of rewrite rules.
 *
 *	history:
 *	891106	uh	Added this comment
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

compile :-
        tmpFileName(F),
	tell(F),
	outReduceClauses,
	told,
	no_style_check(multiple),
	no_style_check(single_var),
	compile(F),
	style_check(multiple),
	style_check(single_var),
        concAtomNames('rm ',F,Cmd),
	unix(system(Cmd)),
	!.

outReduceClauses :-
	(_ ofType (O:T)),
	length(T,N1),
	N is N1-1,
	N2 is N1+1,
	mkAtom('$%%',[N,O],O1),
	listingWithRename(O1/N1),
	listingWithRename(O1/N),
	listingWithRename(O1/N2),
	fail.
outReduceClauses :-
	listingWithRename(reduce/2),
        write('redR(T,NF) :- red(T,NF), !.
              '),
        write('redR(T,T)  :- !.
              '),
	write('eval(let Defs in Term,''$term''(NF)) :-
               !,
	       internalLetExpRep(let Defs in Term,PTerm),
	       reduceL(PTerm,NF,redR),
	       trace:=off,
               !.
              '),
	write('eval(T1,''$term''(NF)) :-
		!,
                (error:==none),
		internalTermRep1(T1,IT1,redR),
		redR(IT1,NF),
		trace:=off,
		!.
              '),
	write('eval(T1) :-
		!,
		eval(T1,''$term''(NF)),
		sPrint("The normalform of % is % .",[T1,''$osTerm''(NF)]).
              ').



listingWithRename(O/N) :-
	genVarList(N,Xs),
	Head=..[O|Xs],
	clause(Head,Body),
	renameOps((Head,Body),(Head1,Body1)),
	portray_clause((Head1:-Body1)),
	nl,
	fail.
listingWithRename(_).


renameOps(X,X) :-
	var(X),
	!.
renameOps(applicable(_,_),true) :-
	!.
renameOps(T,T1) :-
	T=..[O|Ts],
	renameOp(O,O1),
	map(renameOps,Ts,T1s),
	T1=..[O1|T1s],
	!.


renameOp(reduce,red) :-
	!.
renameOp(O,O1) :-
	name(O,[D,N|ON]),
	name('$',[D]),
	\+ name(i,[N]),
	name(O1,[D,D,N|ON]),
	!.
renameOp(O,O).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compileRules(F) :-
	nonvar(F) ->
		fileNameExt(F, rules, FileName),
		tell(FileName),
		outReduceClauses,
		told,
		!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loadRules(F) :-
	nonvar(F) ->
		fileNameExt(F, rules, FileName),
		no_style_check(single_var),
		no_style_check(multiple),
		compile(FileName),
		style_check(multiple),
		style_check(single_var),
		!.


