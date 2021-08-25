/*
 *	file:		panEvaluate.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *
 *	history:
 *	891010	js	Added this comment
 *	891128	uh	Added description
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

% pan_makeNumerals(Term, NewTerm)
% transforms atoms like '123' into numerals like 123.

pan_makeNumerals(Atom, Number) :-
  atom_chars(Atom, L),
  number_chars(Number, L),
  !.
pan_makeNumerals(Atom, Atom) :-
  atomic(Atom).
pan_makeNumerals(Term, NewTerm) :-
  Term =.. [Functor|ArgList],
  pan_mapMakeNumerals(ArgList, NewArgList),
  NewTerm =.. [Functor|NewArgList].

pan_mapMakeNumerals([], []).
pan_mapMakeNumerals([Arg|ArgRest], [NewArg|NewArgRest]) :-
  pan_makeNumerals(Arg, NewArg),
  pan_mapMakeNumerals(ArgRest, NewArgRest).
