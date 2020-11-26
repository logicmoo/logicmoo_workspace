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

%:- module(new_builder,[consult_entry_point_and_parse/2]).

:- use_module(parse_util).


/*
 * consult_entry_point_and_parse(+File, +Project)
 *    - loades the file represented by Arg1 and everything that has to 
 *   	be loaded together with it.
 *    - after that parses every file that was loaded in the first step inside 
 *      of the directory represented by Arg2 and builds the PEF-AST together
 *      with the edge informations 
 *      (see generate_facts/1 from parse_util_quick.pl).
 **/
consult_entry_point_and_parse(File, Project):-
    load_files(File,silent(true)),         
    %consult(File),
    findall(ToParse, 
    		(	source_file(ToParse),
				Project = Lc_Project,
      			atom_concat(Lc_Project,_,ToParse)
      		),
      		ParseList),
 %   	not(library_directory(Directory)),
    writeln(ParseList),
    generate_facts(ParseList).
%consult_entry_point_and_parse(_,_).



