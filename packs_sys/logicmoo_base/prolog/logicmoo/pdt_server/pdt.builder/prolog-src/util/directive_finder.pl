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

/*
 * just for testing something with directives
 **/

:- consult(walking_prolog_files).

collect_directives(Dir):-
    prolog_files(Dir, Files),
    collect_for_files(Files,_,Directives),
    print_directives(Directives).
    
collect_for_files([File|Files],FormerDirectives,AllDirectives):-
    collect_for_files(Files,FormerDirectives,SomeDirectives),
    collect_for_a_file(File,SomeDirectives,AllDirectives).
collect_for_files([],_,[]).
    
collect_for_a_file(File,Former,All):-
    open(File,read,InStream),
    collect_from_term(InStream,[],Found),
    append(Found,Former,AllFound),
    list_to_set(AllFound,All),		
    close(InStream).
   
/*collect_from_term(Stream,Former,All):-
    read_term(Stream,Clause,[syntax_errors(dec10)]),
    (	Clause == end_of_file
    ->	All=Former
    ;	(	(	Clause =.. [(:-),Directive|_Rest],
    			Directive =.. [Functor|_]
   			;	Functor=''
   			),
   			collect_from_term(Stream,Former,Found),
   			All = [Functor|Found]
   		)
   	).*/
collect_from_term(Stream,Former,All):-
    read_term(Stream,Clause,[syntax_errors(dec10)]),
    (	Clause == end_of_file
    ->	All=Former
    ;	(	(	Clause =.. [(:-),Directive|_Rest],
    			Directive =.. [reexport|Args]
   			;	Args=''
   			),
   			collect_from_term(Stream,Former,Found),
   			All = [Args|Found]
   		)
   	).

    
print_directives([Directive|Directies]):-
    writeln(Directive),
    print_directives(Directies).

