/*
 *	file:		panConnect.pl
 *	version:	1.0
 *	date:		October 10, 1989
 *	creation:	-
 *	author:		-
 *
 *	description:
 *	This file contains the predicates called from PAnndA-S to request
 *	CEC-services.
 *
 *	history:
 *	891010	js	Added this comment
 *	891103  uwe	Integrated preprocessor call
 *	891128	uh	Added description
 *	900222	uwe	Replaced pan_preprocessPanndacOutput by
 *                      pan_preprocessPanndacTermOutput in pan_eval, pan_norm
 *	900307	uwe	Removed call of pan_insertMissingDecls in pan_transfer
 *
 *	Copyright (C) 1989
 *		Hubert Bertling
 *		Harald Ganzinger
 *		Renate Schaefers
 *		University of Dortmund
 */

external(noserve, xdr, pan_noserve).
external(norm, xdr, pan_norm(+string)).
external(execgoal, xdr, pan_execGoal(+string)).
external(eval, xdr, pan_eval(+string, -integer)).
external(transfer, xdr, pan_transfer(+string,+string)).
external(show, xdr, pan_show).
external(complete, xdr, pan_complete).


:- dynamic pan_isTrace/0.


serve :- pan_ServeContinue.


pan_noserve :- pan_ServeOff.


pan_execGoal(FileName) :-
  pan_writeIfTrace('------------ goal ------------'),
  pan_getContents(FileName, [[Term]]),
  !, pan_delayGoal(Term),
  !.
pan_execGoal(_) :-
  write('illegal goal'),
  nl,
  fail.


pan_eval(FileName, AnswerCode) :-
  pan_writeIfTrace('------------ eval ------------'),
  pan_getContents(FileName, Contents),
  pan_preprocessPanndacTermOutput(Contents, [[Term]]),
  !, pan_callAndFlush(pan_evaluate(Term,NTerm,HasAnswer)),
  ( HasAnswer = yes ->
      pan_writeIfTrace(NTerm),
      pan_putTerm(FileName, NTerm),
      AnswerCode = 2
    ; AnswerCode = 1
  ),
  !.
pan_eval(_, _) :-
  write('illegal argument for "eval"'),
  nl,
  fail.


pan_norm(FileName) :-
  pan_writeIfTrace('------------ norm ------------'),
  pan_getContents(FileName, Contents),
  pan_preprocessPanndacTermOutput(Contents, [[Term]]),
  !, pan_callAndFlush(norm(Term,NTerm)),
  pan_writeIfTrace(NTerm),
  pan_putTerm(FileName, NTerm),
  !.
pan_norm(_) :-
  write('illegal argument for "norm"'),
  nl,
  fail.


pan_transfer(FileName,OrdName) :-
  pan_writeIfTrace('------------ transfer ------------'),
  ( pan_isRemote(HostName) ->
      name(HostName, HostNameAsString),
      name(FileName, FileNameAsString),
      append("rcp ", HostNameAsString, C1),
      append(C1, ":", C2),
      append(C2, FileNameAsString, C3),
      append(C3, " ", C4),
      append(FileNameAsString, ".", C5),
      append(C5, HostNameAsString, NewFileNameAsString),
      append(C4, NewFileNameAsString, CommandAsString),
      name(Command, CommandAsString),
      unix(system(Command)),
      name(NewFileName, NewFileNameAsString)
    ; NewFileName = FileName
  ),
  see(NewFileName),
  ( read(Contents) ->
      seen
    ; seen,
      fail
  ),
  pan_preprocessPanndacOutput(Contents, NewContents),
  getModName(Contents,ModName),
  fileNameExt(ModName,'eqn',ModFile),
  (OrdName == noorder ->
	OrdFile = noorder
  ;
	'prefixWithout@'(OrdName,Order),
	fileNameExt(ModName,OrdName,OF),
	fileNameExt(OF,'ord',OrdFile)
  ),
  tell(ModFile),
  pan_displayq(NewContents),
  % because of a bug in quintus prolog "writeq", we have to use "pan_displayq"
  write(' .'),
  nl,
  told,
  !,
  pan_callAndFlush(pan_in(ModFile,OrdFile,Order)),
  !.
pan_transfer(_,_) :-
  write('illegal argument for "transfer"'),
  nl,
  fail.

getModName([module(ModName)|_],ModName) :- !.
getModName([(module(ModName) using _)|_],ModName) :- !.
getModName(_,'$noName').

pan_in(ModFile,OrdFile,OrdName) :-
	deleteGarbage,
	(undoUponFail(in('',ModFile,OrdFile,OrdName,Error),Error) ->
	    deleteGarbage,
	    !,
	    (Error = none ->
		    sPrint("Specification accepted",[]),
		    nl
	    ;
		    sPrint("Transfer failed",[]),
		    nl,
		    !, fail
	    )
	;
	    sPrint("Transfer failed",[]),
	    nl,
	    !, fail
	).

pan_show :-
  pan_writeIfTrace('------------ show ------------'),
  pan_callAndFlush(show),
  !.

pan_complete :-
  pan_writeIfTrace('------------ c ------------'),
  pan_delayGoal(c),
  !.


pan_getContents(FileName, TermList) :-
  ( pan_isRemote(HostName) ->
      name(HostName, HostNameAsString),
      name(FileName, FileNameAsString),
      append("rcp ", HostNameAsString, C1),
      append(C1, ":", C2),
      append(C2, FileNameAsString, C3),
      append(C3, " ", C4),
      append(FileNameAsString, ".", C5),
      append(C5, HostNameAsString, NewFileNameAsString),
      append(C4, NewFileNameAsString, CommandAsString),
      name(Command, CommandAsString),
      unix(system(Command)),
      name(NewFileName, NewFileNameAsString),
      see(NewFileName)
  ;   see(FileName)
  ),
  pan_getCont(TL),
  pan_writeIfTrace(TL),
  TL = TermList,
  !, seen.
pan_getContents(_, TermList) :-
  seen,
  !, TermList = [].


pan_getCont(TL) :-
  read(T),
  ( T == end_of_file ->
      TL = []
    ; pan_getCont(TL0),
      TL = [T|TL0]
  ),
  !.


pan_putTerm(FileName, Term) :-
  pan_isRemote(HostName),
  !, pan_putTermRemote(HostName, FileName, Term).
pan_putTerm(FileName, Term) :-
  tell(FileName),
  pan_writeTerm(Term),
  !, told.
pan_putTerm(_, _) :-
  !, told.


pan_putTermRemote(HostName, FileName, Term) :-
  name(HostName, HostNameAsString),
  name(FileName, FileNameAsString),
  append(FileNameAsString, ".", C1),
  append(C1, HostNameAsString, NewFileNameAsString),
  append("rcp ", NewFileNameAsString, C2),
  append(C2, " ", C3),
  append(C3, HostNameAsString, C4),
  append(C4, ":", C5),
  append(C5, FileNameAsString, CommandAsString),
  name(Command, CommandAsString),
  name(NewFileName, NewFileNameAsString),
  tell(NewFileName),
  pan_writeTerm(Term),
  !, told,
  try(unix(system(Command))).
pan_putTermRemote(_, _) :-
  !, told.


pan_writeIfTrace(Term) :-
  pan_isTrace,
  !, nl,
  print(user, Term),
  nl(user).
pan_writeIfTrace(_).



pan_callAndFlush(Goal) :-
  call(Goal),
  !, ttyflush.
pan_callAndFlush(_) :-
  ttyflush,
  fail.



/* DUMMIES */

pan_isTrace.
