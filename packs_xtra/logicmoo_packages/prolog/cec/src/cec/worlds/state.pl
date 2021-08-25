/*
 *	file:		state.pl
 *	version:	1.0
 *	date:		October 30, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains procedures for saving and restoring a
 *	CEC state.
 *
 *	history:
 *	891030	uh	Moved definitions of 
 *			saveCEC 		restoreCEC 
 *			from startup.pl into this file
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

saveCEC :-
	cont1(currentCECName,F), 
	!,
	saveCEC(F).
saveCEC :-
	currentPath(P),
	mkAtom('%%',[P,'/cec'],F),
	saveCEC(F).
	
saveCEC(F):-
	date(Date),
	cec_version(Prolog,Version),
	assert(versionPrologDate(Version,Prolog,Date)),
	currentCECName:==F,
	(prologFeature(hasSave) ->
	    (save(F, ReturnCode), ReturnCode = 0 -> 
		true
	    ;
		rg_init
	    )
	;
	    save_program(F, rg_init)
	).

restoreCEC(F):-
	restore(F).
