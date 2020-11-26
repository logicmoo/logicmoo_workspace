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

:- module(load_graph,[build_new_load_graph/0, build_load_graph/0]).
:- ensure_loaded(parse_util).
:- use_module(library(lists)).


/*
 * build_new_load_graph/0
 *   retracts all load_edge/3 of a former load-graph and
 *   builds a new one with respect to all given
 *   load_dir/4 using build_load_graph/0.
 */
build_new_load_graph:-
    retractall(load_edge(_,_,_)),
    retractall(warning(_,'file not found in project',_)),
    retractall(warning(_,'guessed file reference',_)),
    retractall(warning(_,'link to external library',_)),
    build_load_graph.

/*
* build_load_graph/0
*   tries to build the corresponding load_edge/3 for each load_dir/4 it
*   can find.
*   If it cannot find the correct file that is loaded with the considered
*   directive it tries to guess the file: It compares the file name with each
*   file name it can find.
*   Only files that are parsed with the preparser are considered. If no such
*   file can be found a warning will be created, if it is not a reference 
*   to a library file. 
*   (Libraries will not be in the working context in most cases.) 
*
*	Finishes with retracting all load_dir/4.
**/

/* ToDo: inform the user about the guessing?
         use the given path for the guesseing? -> concat 
 */ 	 
build_load_graph:-
    load_dir(Directive,ToLoadFiles,Imports),
     	flatten(ToLoadFiles,ToLoadFilesFlatt),
		build_load_edges_for_list(ToLoadFilesFlatt,Imports,Directive),
    	fail.
build_load_graph.
%build_load_graph:-
%	retractall(load_dir(_,_,_,_)).

/*
 * build_load_edges_for_list(+ArgList,+LoadingFileId,+LoadingDirectiveId)
 *   builds the load edges for all Arguments inside of ArgList with the
 *   help of build_complex_load_edges/3. 
 **/
build_load_edges_for_list([File],Imports,Directive):-
    !,
    directiveT(Directive,LoadingId,_),
    lookup_complex_file_reference(File,LoadingId,FileId,Warning),
   	(	FileId = ''
   	->	true
	;	assert(load_edge(LoadingId,FileId,Imports,Directive))
    ),
    (	Warning = ''
    ->	true
    ;	assert(warning(Directive,Warning,File))
   	).
build_load_edges_for_list([A|B],LoadingId,Imports,Directive):-
    build_load_edges_for_list([A],LoadingId,Imports,Directive),
    build_load_edges_for_list(B,LoadingId,Imports,Directive).



lookup_complex_file_reference(Arg,LoadingId,FileId,''):-
    atom(Arg),
    !,
    lookup_direct_file_reference(Arg,LoadingId,FileId).
lookup_complex_file_reference(ToLoadConstruct,LoadingId,FileId,''):-
    ToLoadConstruct =.. [PathKey,File],
    compute_dir_with_file_search_path(PathKey,FlatDir),
    combine_two_path_elements(FlatDir,File,FileName),
	lookup_direct_file_reference(FileName,LoadingId,FileId),
	!.   
lookup_complex_file_reference(ToLoadConstruct,_LoadingId,FileId,'guessed file reference'):-
    ToLoadConstruct =.. [_,FilePath],
	get_path_with_prolog_file_ending(FilePath,FilePathPl),
	fileT_ri(AFile,FileId),					%ToDo: optimierbar?
	atom_concat(_,FilePathPl,AFile),
	!.    
lookup_complex_file_reference(library(_Name),_LoadingId,'','link to external library'):-
    				% if it is a reference to a library file 
 					% the library may not be inside the project we parse
    				% so there may be no fileT for it to find with build_direct_load_edge
    !.	
lookup_complex_file_reference(_Args,_LoadingId,'','file not found in project').




lookup_direct_file_reference(ToLoad,LoadingId,Id):-
    prolog_file_type(Pl,prolog),
    fileT(LoadingId,LoadingName,_),
    absolute_file_name(ToLoad,[extensions(Pl),relative_to(LoadingName)],FileName),	
	find_file_id_for_file_name(FileName,Id),	
	!.   


/*
 * find_file_id_for_file_name(?FileName, ?Id) is_det
 * 
 * This predicate can be used if one get's a file name via absolute_file_name/3.
 * absolute_file_name/3 generates lower case names even if the reference file
 * name (the name of the file to which you compare the relative file name) is given
 * as with an upper case absolute path.
 *
 * Either Arg2 is the id of a fileT fact with arg1 as respective file name, or 
 * id is the id of a fileT that has a file name that is equal to arg2 after 
 * converting all letters to lower case (for both names).
 *
 * (alternatively one could store names in fielT_ri as lower case, would be much faster)
 */
find_file_id_for_file_name(FileName,Id):-
    fileT_ri(FileName,Id).
find_file_id_for_file_name(FileName,Id):-
    downcase_atom(FileName,LowerFileName),
    fileT_ri(AFileName,Id),
    downcase_atom(AFileName,LowerAFileName),
    LowerFileName == LowerAFileName.

/*
 * compute_dir_with_file_search_path(+Key, -FinalDir)
 *   resolves the directory represented by Arg1
 *   with file_search_path/2.
 *   
 *   It does this recursivley, if the path given by
 *   file_search_path is not a plain path but a refernce
 *   with a key to another path stored in file_search_path/2.
 **/
compute_dir_with_file_search_path(Key,FinalDir):-
	file_search_path(Key,Dir),
    (	Dir =.. [InnerKey,DirPath]
    ->	compute_dir_with_file_search_path(InnerKey,InnerDir),
 		combine_two_path_elements(InnerDir,DirPath,FinalDir)
    ;	Dir = FinalDir
    ).    	
 
 
get_path_with_prolog_file_ending(FilePath,FilePathPl):-
    path_to_list(FilePath,List),
    last(List,File),
    append(PrePath,File,List),
    prolog_file_type(Pl,prolog),
    file_name_extension(File,Pl,FilePl),	
    append(PrePath,FilePl,ListPl),
    atomic_list_concat(ListPl,'/',FilePathPl).	
 
/*
 * combine_tow_path_elements(+First,+Second,-Combination,+Directive)
 *    Arg3 is the atom that begins with Arg1, is followed
 *    with a '/' and ends with Arg2. If Arg1 and Arg2 are
 *    terms their atom representation is used.
 *
 *    Arg4 is needed to compose some warnings if it stumbles
 *    over syntax errors.
 **/ 
/* combine_two_path_elements(First,Second,Combination,Directive):-
    (	not(atomic(First)), assert(warning(Directive,'is not atomic',[First])), writeln('first not atomic')
    ;	not(atomic(Second)), 
    	assert(warning(Directive,'is not atomic',[Second])), 
    	writeln('second not atomic'),
    	path_to_list(Second,Atomic_Second)
    ;	atomic(First), atomic(Second), writeln('both atomic'),
    	atomic_list_concat([First,'/',Second],Combination)
    ).*/


/*
 * combine_tow_path_elements(+First,+Second,-Combination)
 *    Arg3 is the atom that begins with Arg1, is followed
 *    with a '/' and ends with Arg2. If Arg1 and Arg2 are
 *    terms their atom representation is used.
 **/ 
combine_two_path_elements(First,Second,Combination):-
    listify_path_element(First,FirstList),
    listify_path_element(Second,SecondList),
	append(FirstList,SecondList,List),
	atomic_list_concat(List,'/',Combination).    
    
listify_path_element(Elem,List):-
    (	atomic(Elem)
    ->	List = [Elem]
    ;	path_to_list(Elem,List)
    ).
    
/*
 * path_to_list(+Path,?List)
 *	  This is more or less the reversion of atomic_list_concat/3 
 *    with '/' as seperator.
 *    Arg1 ia a term that should describe at least a part of a file path
 *    Arg2 is a list of all parts of the term, that are seperated by '/'. 
 **/
path_to_list(Path,List):-
    Path =.. [/,First,Second],
    listify_path_element(First,FirstList),
    listify_path_element(Second,SecondList),
    append(FirstList,SecondList,List).


