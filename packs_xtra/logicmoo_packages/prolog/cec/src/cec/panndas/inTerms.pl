/*
 *	file:		inTerms.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the predicate for reading a term from a file.
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

% syntax errors result in E = error
inTerms(Terms,E):-
        (read(Term),!,E1=success;E1=error),
        (Term\==end_of_file->
		(Term=(H:-B) ->
			assertz((H:-B)),
			inTerms(Terms,E)
		;       (Term=[T1|TR] ->
                        	inList([T1|TR], Terms, E)
			;	(expandTerm(Term,ETerm),!;ETerm=Term),
                                inTerms(Terms1,E2),
				Terms=[ETerm|Terms1]
			)
		)
	;       Terms=[],
		E2=success
	),
        (E1=E2 -> E=E1;E=error).

inList([], [], success).
inList([(H:-B)|R], Terms, E) :-
    assertz((H:-B)),
    !, inList(R, Terms, E).
inList([(_:-_)|_], [], error) :- !.
inList([T|R], [TExp|Rest], E) :-
    expandTerm(T,TExp),
    inList(R,Rest,E).

