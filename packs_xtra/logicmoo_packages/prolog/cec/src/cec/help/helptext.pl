/*
 *	file:		helptext.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains predicates to display help files to the user.
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


/* ??
 * lists those topics and commands for which help is available
 */

'??' :-
	nl,
	write('Help is available under the following keys:'),
	nl,
	nl,
	showFunctionNames.


/* ??(PW)
 * lists only those topics containing PW
 * PW: Atom
 */

'??'(PW) :-
	nl,
	write('Help is available under the following keys:'),
	nl,
	nl,
	mkAtom('*%*',[PW],PAT),
	showFunctionNames(PAT).


showFunctionNames :-
       	showFunctionNames('*').


showFunctionNames(PAT) :-
	libPath(PA),
	concAtomNames(PA,'/help',HPA),
	listOfFiles(HPA,PAT,'.hlp',SF),
	transformKeys(SF,KeysGeneral,KeysF),
	write('General subjects:'),
	nl,
	write('-----------------'),
	nl,
	writeNames(KeysGeneral),
	nl,
	nl,
	write('CEC-Functions:'),
	nl,
	write('--------------'),
	nl,
	writeNames(KeysF),
	nl,
	nl,
	write('To obtain information about a key, type   ? key. '),
	nl.

transformKeys([],[],[]).
transformKeys([N | Rest],GList,FList) :-
	name(N,NS),
	name('old.',[O,L,D,P]),
	NS = [O,L,D,P | _NREST],
	transformKeys(Rest,GList,FList).
transformKeys([N|Rest],[New|GList],FList):-
	name(N,NS),
	name('@.',[K,P]),
	NS = [K,P | NRest],
	!,
	name(New,NRest),
	transformKeys(Rest,GList,FList).
transformKeys([N|Rest],GList,[N|FList]):-
	transformKeys(Rest,GList,FList).

writeNames([F1,F2,F3,F4|SF]):-
	nl,
	writeAtoms([F1,F2,F3,F4],19),
	writeNames(SF).
writeNames(SF):-
	nl,
	writeAtoms(SF,19),
	nl.


/* ?(+Topic) - lists help information about a topic.
   Topic: Atom
*/

?(F):-
	libPath(PA),
	concAtomNames(PA,'/help',HPA),
	listOfFiles(HPA,'*','.hlp',SF),
	(	name(F,FS),
		name('@.',[K,P]),
		name(F1,[K,P | FS]),
		member(F1,SF)
	;
		member(F,SF),
		F1=F
	),
	mkAtom('cat %/help/''%''.hlp',[PA,F1],Cmd),
	unix(system(Cmd)).
?(F):-
	write('No help information available for '),
	write(F),
	'??'.

/*--------------------------------------------------------------------
listOfFiles(+Path,+Pattern,+Suffix,-List)
Path:    Atom, directory where to look
Pattern: Atom, first part of csh search pattern, e.g. 'g*'
Suffix:  Atom, second part of search pattern
List:    List of Atoms, contains names of all files in Path matching
         Pattern+Suffix. Suffix is removed from the filenames.
*/

listOfFiles(PA,PAT,SUF,L) :-
	libPath(CPA),
	tmpFileName(TFN),
	mkAtom('%/list_of_files % ''%'' % > %',[CPA,PA,PAT,SUF,TFN],LCmd),
	unix(shell(LCmd)),
	see(TFN),
	read(L),
	seen,
        concAtomNames('rm ',TFN,RCmd),
	unix(shell(RCmd)).


/*
tmpFileName(-FileName)
FileName: Atom, a (hopefully) unique filename for a temporary file
*/

tmpFileName(TFN) :-
	statistics(runtime,[T,_]),
        concAtomNames('/tmp/cec',T,TFN).
