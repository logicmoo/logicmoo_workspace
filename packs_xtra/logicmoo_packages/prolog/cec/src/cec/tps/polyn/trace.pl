/*
 *	file:		trace.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for switching the trace mode for
 *	computation in POLY-part of cec on/off.
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

?- dynamic pol_trace/0.

pol_trace_on :-
	pol_trace,
	!.

pol_trace_on :-
	asserta(pol_trace).


pol_trace_off :-
	pol_retractall(pol_trace).
