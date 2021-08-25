/*
 *	file:		panOutput.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains ..
 *
 *	history:
 *	891010	js	Added this comment
 *	891107  uwe	Renamed:
 *			    pan_deleteSortInfo -> pan_delSortInfoInString
 *			    pan_panndaNotation -> pan_deleteSortInfo
 *			    pan_mapPanndaNotation -> pan_mapDeleteSortInfo;
 *			"true" and "false" are no longer capitalized;
 *			Optimized pan_deleteSortInfo;
 *			Added "pan_displayq"
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% pan_writeTerm(Term)

pan_writeTerm(T) :-
  pan_deleteSortInfo(T, PT),
  pan_writeTermPrec(PT, 6).


% pan_writeTermPrec(Term, Prec)

pan_writeTermPrec(@(Var), _) :-
  !, write(Var).
pan_writeTermPrec(:(T,_), P) :-
  !, pan_writeTermPrec(T, P).
pan_writeTermPrec('$term'(T), P) :-
  !, pan_writeTermPrec(T, P).
     % rule inserted, since "norm" yields "'$term'(foo)"
     % (Sep 28, 1989, Uwe Waldmann)
pan_writeTermPrec('$inj'(T), P) :-
  !, pan_writeTermPrec(T, P).
pan_writeTermPrec(pan_forall(DomainList,Eqn), _) :-
  !, write('( for all '),
  pan_writeTermPrec(DomainList, 6),
  write(' => '),
  pan_writeTermPrec(Eqn, 6),
  write(')').
pan_writeTermPrec(pan_vs(Var,Sort), _) :-
  !, pan_writeTermPrec(Var, 5),
  write(' : '),
  pan_writeTermPrec(Sort, 5).
pan_writeTermPrec(term(T), P) :-
  !, pan_writeTermPrec(T, P).
pan_writeTermPrec(T, _) :-
  atomic(T),
  !, write(T).
pan_writeTermPrec(T, P) :-
  T =.. [Op, Arg1],
  pan_isPrefix(Op, P0, PR),
  !, (P0 > P -> write('(') ; true),
  write(Op),
  write(' '),
  pan_writeTermPrec(Arg1, PR),
  (P0 > P -> write(')') ; true).
pan_writeTermPrec(T, P) :-
  T =.. [Op, Arg1, Arg2],
  pan_isInfix(Op, PL, P0, PR),
  !, (P0 > P -> write('(') ; true),
  pan_writeTermPrec(Arg1, PL),
  write(' '),
  write(Op),
  write(' '),
  pan_writeTermPrec(Arg2, PR),
  (P0 > P -> write(')') ; true).
pan_writeTermPrec(T, _) :-
  T =.. [Op|L],
  write(Op),
  write('('),
  pan_writeTermList(L),
  write(')').


pan_writeTermList([T]) :-
  !, pan_writeTermPrec(T, 6).
pan_writeTermList([T1|L]) :-
  !, pan_writeTermPrec(T1, 6),
  write(', '),
  pan_writeTermList(L).


% pan_isInfix(Operator)

/* Precedences and non-terminals of the PAnndA-S grammar:
 *	0: secondary
 *	1: factor
 *	2: term
 *	3: simple_expression
 *	4: relation
 *	5: expression
 *	7: implication_expression
 */

pan_isInfix(('**'), 0, 1, 0).
pan_isInfix(('*'), 2, 2, 1).
pan_isInfix(('/'), 2, 2, 1).
pan_isInfix(('mod'), 2, 2, 1).
pan_isInfix(('rem'), 2, 2, 1).
pan_isInfix(('+'), 3, 3, 2).
pan_isInfix(('-'), 3, 3, 2).
pan_isInfix(('&'), 3, 3, 2).
pan_isInfix(('='), 3, 4, 3).
pan_isInfix(('/='), 3, 4, 3).
pan_isInfix(('<'), 3, 4, 3).
pan_isInfix(('<='), 3, 4, 3).
pan_isInfix(('>'), 3, 4, 3).
pan_isInfix(('>='), 3, 4, 3).
pan_isInfix(('and'), 5, 5, 4).
pan_isInfix(('or'), 5, 5, 4).
pan_isInfix(('xor'), 5, 5, 4).
pan_isInfix((';'), 5, 5, 4).
pan_isInfix(('->'), 6, 7, 6).
pan_isInfix(('<->'), 6, 7, 6).


% pan_isPrefix(Operator)

pan_isPrefix(('abs'), 1, 0).
pan_isPrefix(('not'), 1, 0).
pan_isPrefix(('+'), 3, 2).
pan_isPrefix(('-'), 3, 2).


% pan_deleteSortInfo(CecTerm, PanndaTerm)

pan_deleteSortInfo(CecAtom, PanndaAtom) :-
  atomic(CecAtom),
  !, name(CecAtom, L1),
  pan_delSortInfoInString(L1, L2),
  name(PanndaAtom, L2).
pan_deleteSortInfo(CecTerm, PanndaTerm) :-
  CecTerm =.. [CecFunctor|CecArgs],
  name(CecFunctor, L1),
  pan_delSortInfoInString(L1, L2),
  name(PanndaFunctor, L2),
  pan_mapDeleteSortInfo(CecArgs, PanndaArgs),
  PanndaTerm =.. [PanndaFunctor|PanndaArgs].
  
pan_mapDeleteSortInfo([], []) :-
  !.
pan_mapDeleteSortInfo([CT|CL], [PT|PL]) :-
  pan_deleteSortInfo(CT, PT),
  pan_mapDeleteSortInfo(CL, PL).

pan_delSortInfoInString([], []) :-
  !.
pan_delSortInfoInString([45,C|_], []) :-        % '-' followed by an alpha char
  pan_isAlpha(C),
  !.
pan_delSortInfoInString([C|R], [C|R0]) :-
  pan_delSortInfoInString(R, R0).


% pan_isAlpha(Char)

pan_isAlpha(C) :-
  ( 65 =< C, C =< 90 ; 97 =< C, C =< 122 ).          % A - Z, a - z


% because of a bug in the Quintus prolog "writeq", we need this
% (very primitive) output routine.
% Warning: The output is not to be read by human beings.

pan_displayq(T) :-
  ( atomic(T) ; var(T) ),
  !,
  write('('),
  writeq(T),
  write(')').
pan_displayq([X|Y]) :-
  !,
  write('['),
  pan_displayq(X),
  pan_displayqList(Y),
  write(']').
pan_displayq(T) :-
  T =.. [F|Args],
  writeq(F),
  write('('),
  pan_displayqArgs(Args),
  write(')').

pan_displayqArgs([Arg]) :-
  !,
  pan_displayq(Arg).
pan_displayqArgs([Arg|Rest]) :-
  pan_displayq(Arg),
  write(' , '),
  pan_displayqArgs(Rest).

pan_displayqList(X) :-
  var(X),
  !,
  write(' | '),
  pan_displayq(X).
pan_displayqList([]) :-
  !.
pan_displayqList([X|Y]) :-
  !,
  write(' , '),
  pan_displayq(X),
  pan_displayqList(Y).
pan_displayqList(X) :-
  write(' | '),
  pan_displayq(X).


% pan_toUpper and pan_toLower are not used anymore in this file;
% if panEvaluate gets replaced, both predicates possibly might be deleted.
% (Nov 6, 1989, Uwe Waldmann)

% pan_toUpper(OldString, NewString)

pan_toUpper([], []) :- !.
pan_toUpper([LC|LR], [UC|UR]) :-
  97 =< LC, LC =< 122,          % a - z
  !, UC is LC - 32,
  pan_toUpper(LR, UR).
pan_toUpper([C|LR], [C|UR]) :-
  !, pan_toUpper(LR, UR).


% pan_toLower(OldString, NewString)

pan_toLower([], []) :- !.
pan_toLower([LC|LR], [UC|UR]) :-
  65 =< LC, LC =< 90,          % A - Z
  !, UC is LC + 32,
  pan_toLower(LR, UR).
pan_toLower([C|LR], [C|UR]) :-
  !, pan_toLower(LR, UR).
