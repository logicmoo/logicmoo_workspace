/*
 *	file:		rg_entry.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the entry point for the Runtime Generator
 *	version of CEC. This is a separate file from rg_env, because
 *	it must be defined in module user.
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

/* program entry point */
/* Note: This file exists because the predicate runtime_entry must be
 *       defined in module user, importing it into module user is not
 *       enough.
 */

runtime_entry(X) :-
	rg_env:runtime_entry(X).

runtimeGeneratorVersion.
