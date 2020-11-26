/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/


:- use_module(library(lists)).
:- use_module(library(statistics)).

/*
 * walking_file_list(+FileList,+Functor,+Arity)
 *
 *	Arg2 is the functor and Arg3 the Arity of a predicate
 *	that succeeds with a prolog file as the first argument,
 *	Arg1 is a list of files (or directories).
 *
 * 	walking_file_list/3 tries to call the predicate of Arg2 
 * 	and Arg3 on each file in Arg1 recursively.
 */    
walking_file_list([],_,_).
walking_file_list([File|Files],Functor,Arity):-
    check_file_or_dir(File,Functor,Arity),
    walking_file_list(Files,Functor,Arity),!.
  
/*
 *	check_file_or_dir(+File,+Functor,+Arity)
 *	Arg2 is the functor and Arg3 the Arity of a predicate
 *	that succeeds with a prolog file as the first argument.
 * 	
 *	If Arg1 is a prolog file the predicate is called on Arg1.
 *	If Arg1 is a directory the predicate is called on each prolog
 * 	file in Arg1 or its subdirectories.
 *	Else it simply succeeds.
 */    
check_file_or_dir(File,Functor,Arity):-    
    file_name_extension(_,Ext,File),
    prolog_file_type(Ext,prolog),
    !,
    functor(Term,Functor,Arity),
    arg(1,Term,File),
    (	catch(call(Term),_,true/*(write('.'),writeln(Term),true)*/)
    ;	true
    ).
check_file_or_dir(Dir,Functor,Arity):-
	walking_prolog_directory(Dir,Functor,Arity),!.
check_file_or_dir(_,_,_).
    
/*
 * walking_prolog_directory(+Dir,+Functor,+Arity)
 *	if Arg2 is the functor and Arg3 the Arity of a predicate
 *	that succeeds with a prolog file as the first argument,
 * 	walking_prolog_directory/3 calls this predicate on each
 *	prolog file in the directory Arg1 or its subdirectories.
 */
walking_prolog_directory(Dir,Functor,Arity):-
    exists_directory(Dir),
    string_concat(Dir,'/*',FileString), /**/
    expand_file_name(FileString,Files),
    forall(member(File, Files), 
 		   check_file_or_dir(File,Functor,Arity)).   

/*
 * do_nothing_on_file(+File)
 * does nothing to a file - just for runtime testing.
 */
do_nothing_on_file(_).


prolog_file(Dir,PLFile):- 
    exists_directory(Dir),
    string_concat(Dir,'/*',FileString), 
    expand_file_name(FileString,LocalFiles),
    member(File, LocalFiles), 
 	prolog_file_or_dir(File,PLFile). 

prolog_file_or_dir(File,File) :-   
    file_name_extension(_,Pl,File),
    prolog_file_type(Pl,prolog),
    !.
prolog_file_or_dir(Dir,File) :-
    prolog_file(Dir,File).

prolog_files(Dir, PrologFiles) :-
    findall( File, prolog_file(Dir,File), PrologFiles).
 
gogo :- time(consult_prolog_files('Z:/WorkspacePDT')).

consult_prolog_files(Dir) :-
    prolog_file(Dir, File), 
      consult(File),
    fail.
consult_prolog_files(_).   


