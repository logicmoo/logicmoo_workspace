/*
 *	file:		panSorts.pl
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
 *	891103	uwe	Replaced "pan_insertVS" by "pan_insert" (now in panMiscell.pl)
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% pan_getVarsAndSorts(Term, OldVarAndSortList, Sort, NewVarAndSortList)
  % should be called as: pan_getVarsAndSorts(Term, [], _, VSL)

pan_getVarsAndSorts(@(X), OldVSL, Sort, NewVSL) :-
  !, pan_insert(OldVSL, pan_vs(X,Sort), NewVSL).
pan_getVarsAndSorts(Term, OldVSL, _, NewVSL) :-
  Term =.. [Functor|Args],
  pan_getSorts(Functor, SortList, Args),
  pan_mapGetVarsAndSorts(Args, OldVSL, SortList, NewVSL).

pan_mapGetVarsAndSorts([], VSL, [], VSL) :- !.
pan_mapGetVarsAndSorts([Term|TList], OldVSL, [Sort|SList], NewVSL) :-
  pan_getVarsAndSorts(Term, OldVSL, Sort, VSL),
  pan_mapGetVarsAndSorts(TList, VSL, SList, NewVSL).

% pan_getSorts(Functor, SortList, Args)
  % SortList must have the same length as Args, if necessary, it is
  % padded with '_'

pan_getSorts(Functor, SortList, Args) :-
  name(Functor, String),
  pan_skipFunctorName(String, S0),
  pan_getSortsStr(S0, [_|SortList], [_|Args]).        % Range sort is skipped

% pan_getSortsStr(String, SortList, Args)

pan_getSortsStr(_, [], []) :-
  !.
pan_getSortsStr([], [_|RestSortList], [_|RestArgs]) :-
  !, pan_getSortsStr([], RestSortList, RestArgs).
pan_getSortsStr(String, [Sort|RestSortList], [_|RestArgs]) :-
  pan_getFirstSort(String, SortString, RestString),
  name(Sort, SortString),
  pan_getSortsStr(RestString, RestSortList, RestArgs).

% pan_getFirstSort(String, SortString, RestString)

pan_getFirstSort([], [], []).
pan_getFirstSort([45|Str], [], Str) :-
  !.
pan_getFirstSort([C|Str], [C|S], RestString) :-
  pan_getFirstSort(Str, S, RestString).

% pan_skipFunctorName(String, RestString)

pan_skipFunctorName([], []) :-
  !.
pan_skipFunctorName([45,C|R], [C|R]) :-        % '-' followed by a char
  pan_isAlpha(C),
  !.
pan_skipFunctorName([_|R], RestString) :-
  pan_skipFunctorName(R, RestString).

