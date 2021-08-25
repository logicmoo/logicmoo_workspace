/*
 *	file:		path.pl
 *	version:	1.5
 *	date:		November 6, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates for displaying and changing the current path.
 *
 *	history:
 *	891102	uh	Added this comment
 *	891106	uh	Moved definition of
 *			cd/0			cd/1			pathAsAtoms/2	
 *			exec/1			pwd/0			currentPath/1
 *			into this file
 *				
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

cd(D) :-
	atomic(D),
	!,
	(name(D,[S|_]),	name('/',[S]) ->
		DNew=D
	;	currentPath(DOld),
		(	DOld='' ->
			DSlash=''
		;	mkAtom('%/',[DOld],DSlash)
		),
		mkAtom('%%',[DSlash,D],DNew)
	),
	absolute_file_name(DNew,DNA),
	!,
	(dirExists(DNA) ->
		assign1(currentDir,DNA),
		print(DNA)
	;	error("% not a directory.",[DNA],cd),
		fail
	).

cd(A/B):-
	pathAsAtom(A/B,P),
	cd(P).

pathAsAtom(A,A):-
	atomic(A),
	!.
pathAsAtom(A/B,P):-
	pathAsAtom(A,A1),
	pathAsAtom(B,B1),
	mkAtom('%/%',[A1,B1],P).


(cd) :-
	homeDir(D),
	absolute_file_name(D,DNA),
	assign1(currentDir,DNA),
	assign1(currentDir,DNA),
	sPrint("directory now: %",[DNA]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

exec(C) :-
	currentPath(P),
	mkAtom('cd %; %',[P,C],C1),
	unix(system(C1)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pwd:-
	currentPath(P),
	write(P).

currentPath(A) :-
	(	cont1(currentDir,P)
	;	P=''
	),
	absolute_file_name(P,A),
	!.

