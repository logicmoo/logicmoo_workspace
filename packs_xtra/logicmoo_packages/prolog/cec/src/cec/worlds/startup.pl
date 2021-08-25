/*
 *	file:		startup.pl
 *	version:	1.5
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains procedures which are executed when
 *	creating and starting up CEC.
 *
 *	history:
 *	891010	js	Added this comment
 *	891116	uh	Moved definitions of 
 *			saveCEC/0	saveCEC/1	restoreCEC/1
 *			into file management/state.pl
 *	900322	js	added establish_handler to rg_init
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

prologFeature(hasSave,'2.4.2').

prologFeature(Feature) :-
	prologVersion(_,Version),
	prologFeature(Feature,Version).

cec_version(L,V):-
	thisVersion(V1),
	(	freezeCompatibleVersions(V,V1)
	;	freezeCompatibleVersions(V1,V)
	),
	!,
	cec_language(L).
cec_version(_,_) :-
	error("This state is not appropriate for this version of CEC.",[],freeze).


thisVersion('Version 1.9').

freezeCompatibleVersions(V,V).
freezeCompatibleVersions('Version 1.43','Version 1.5').


rg_init :-
	versionPrologDate(Version,Prolog,Date),
	sPrint("
  CEC % (%) saved on %
",[Version,Prolog,Date]),
	write('
  Copyright: PROSPECTRA group, University of Dortmund
       e-mail: hg@informatik.uni-dortmund.de

To obtain help, type  "??<space>.<return>"
'),
	abolish(cont1,2),
	(runtimeGeneratorVersion ->
	    retractall(libPath(P)),
	    assert((libPath(P) :- absolute_file_name(runtime(''),P))),
	    establish_handler(1)
	;
	    establish_handler(0)
	),
	absolute_file_name('.',HomeDir),
	assert(homeDir(HomeDir)),
	try(connect_and_serve_xdr).


createStandard:-
	(	libPath(P)
	;	absolute_file_name('.',P),
		assert(libPath(P))
	),
	concAtomNames(P,'/standard/standard',Standard),
	!,
	createStandard(Standard,neqkns),
	createStandard(Standard,poly1),
	createStandard(Standard,poly2),
	createStandard(Standard,poly3),
	deleteStack(undo),
	rs('$initial').


createStandard(Standard,kns) :-
	createStandard(Standard,neqkns).
createStandard(Standard,neqkns) :-
	rs('$initial'),
	enrich(Standard),
	moduleName:=standard,
	orderName :=noorder,	
	(	freeze(Standard)
	;
		nl,
		sPrint("no write permission for the file %",[Standard])
	),
	try(delete(standard,noorder)),
	try(delete(standard,neqkns)),
	nl.
createStandard(Standard,Poly) :-
	rs('$initial'),
	enrich(Standard,Poly),
	fileNameExt(Standard,Poly,StandardPoly),
	moduleName:=standard,
	orderName :=Poly,
	(	freeze(StandardPoly)
	;
		nl,
		sPrint("no write permission for the file %",[StandardPoly])
	),
	try(delete(standard,Poly)),
	nl.
createStandard(_Standard,_Order).
