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

:-module(pl_ast_to_abba, [	write_facts_to_abba/1, 
							pl_test/0,
							pl_test/2]).
								
:- ensure_loaded(pdt_builder_analyzer('../parse_util')).

/*
 * write_facts_to_abba(+File)
 *   Arg1 has to be a full qualified file name. The file may not exist.
 *   The predicated collects the relevant informations about the following 
 *   facts and converts them into abba-sources for Bashaars Tool. This 
 *   sources are written into the file specified by arg1-
 *   The facts that are considered are:
 *   - fileT
 *	 - onlo
 **/
write_facts_to_abba(File):-
    open(File,write,OutStream,[type(text)]),
    write_files(OutStream),
    nl(OutStream),
    flush_output(OutStream),
    write_onloades(OutStream),
    nl(OutStream),
    flush_output(OutStream),
    write_predicates(OutStream),
    nl(OutStream),
    flush_output(OutStream),
    write_directives(OutStream),
    nl(OutStream),
    flush_output(OutStream),
    write_clauses(OutStream),
    nl(OutStream),
    flush_output(OutStream),
    write_hirarchy(OutStream),
    nl(OutStream),
    flush_output(OutStream),
    write_edges(OutStream),
    close(OutStream).
    
/*
 * write_files(+Stream)
 *    writes #### dito ####
 */
write_files(Stream):-
    forall(	fileT(Id,File,_),
    		write_node(Stream,Id,prolog_file,File)
    	  ).
		
write_predicates(Stream):-
	forall(	predicateT(Id,_,Functor,Arity,Module),
			(	write_node(Stream,Id,prolog_predicate,(Module:Functor/Arity)),
				filePosT(Id,Begin,Length),
				write_position(Stream,Id,Begin,Length)
			)
		).

write_onloades(Stream):-
	forall(	onloadT(Id,_,Module),
			(	write_node(Stream,Id,prolog_onload,Module),
				filePosT(Id,Begin,Length),
				write_position(Stream,Id,Begin,Length)
			)
		).
    
   	  
write_clauses(Stream):-
	forall(	headT(HeadId,Id,_,_,_,_),     
			(	termT(HeadId,Term),
				write_node(Stream,Id,prolog_clause,Term),
				filePosT(Id,Begin,Length),
				write_position(Stream,Id,Begin,Length)
			)
    	  ).      	  

write_directives(Stream):-
	forall(	directiveT(Id,_,_),
			(	termT(Id,Term),
				write_node(Stream,Id,prolog_directive,Term),
				filePosT(Id,Begin,Length),
				write_position(Stream,Id,Begin,Length)
			)
    	  ).  
 
write_hirarchy(Stream):-
    forall( onloadT(PredId,FileId,_),
    		write_within(Stream,FileId,PredId,'parent-child')
    	),
    forall(	predicateT(PredId,FileId,_,_,_),
 			write_within(Stream,FileId,PredId,'parent-child')
    	),
    forall( /*directiveT(DirectId,_,_), */onload_edge(DirectId,OnloadId),
    		write_within(Stream,OnloadId,DirectId,'parent-child')
    	),
    forall(	pred_edge(ClauseId,Id),
    		write_within(Stream,Id,ClauseId,'parent-child')
    	).
 
    	
write_edges(Stream):-
    call_edge(LId,CalleeId),
    literalT(LId,_,CallerId,_,_,_),
    termT(LId,Term),
    	write_edge(Stream,LId,CalleeId,CallerId,Term),
    fail.
write_edges(_).		

write_node(Stream,Id,Type,Attr):-
    write(Stream, 'node("'),
    write(Stream, Id),
    write(Stream, '", "'),
    write(Stream, Type),
    write(Stream,'", "'),
    write(Stream, Attr),
    write(Stream, '").'),
    nl(Stream).
    
write_position(Stream,Id,Begin,End):-
    write(Stream, 'property("'),
    write(Stream, Id),
    write(Stream, '", "position('),
    write(Stream, Begin),
    write(Stream,', '),
    write(Stream, End),
    write(Stream, ')").'),
    nl(Stream).

write_within(Stream,Id1,Id2,Text):-
    write(Stream, 'within("'),
    write(Stream, Id1),
    write(Stream, '", "'),
    write(Stream, Id2),
    write(Stream,'", "'),
    write(Stream, Text),
    write(Stream, '").'),
    nl(Stream).
    
write_edge(Stream,Id,Callee,Caller,Term):-
    write(Stream, 'edge("'),
    write(Stream, Id),
    write(Stream, '", "edge_call", "'),
    write(Stream, Term),
    write(Stream,'", "'),
    write(Stream, Callee),
    write(Stream,'", "'),
    write(Stream, Caller),
    write(Stream, '").'),
    nl(Stream).


pl_test:-
%    pl_test(['Z:/WorkspacePDT/pdt.runtime/library/pdt/xref'],'Z:/WorkspaceTeaching/bla/test.pl'). 
%    pl_test(['Z:/WorkspaceTeaching/svf.examples.prolog'],'Z:/WorkspaceTeaching/bla/test.pl'). 
%     pl_test(['Z:/WorkspaceTeaching/svf.examples.prolog/pl/Load.pl'],'Z:/WorkspaceTeaching/bla/test.pl'). 
    pl_test(['Z:/WorkspacePDT'],'Z:/WorkspaceTeaching/bla/test.txt').   
%    pl_test('Z:/WorkspacePDT/pdt.runtime/library/attic/org/cs3/pdt/metadata/mi_meta_ops.pl','Z:/WorkspaceTeaching/bla/test.pl').     
%    pl_test(['Z:/WorkspacePDT/pdt.runtime/library/attic/spike/socket/pifcom_codec.pl'],'Z:/WorkspaceTeaching/bla/test.pl').
%    pl_test(['Z:/WorkspaceTeaching/bla/seltsam.pl'],'Z:/WorkspaceTeaching/bla/test.pl').   
%    pl_test(['Z:/WorkspaceTeaching/bla/automat.pl'],'Z:/WorkspaceTeaching/bla/test.pl').   
%    pl_test(['Z:/WorkspacePDT/pdt.runtime/library/pdt/xref/parse_util_quick.pl'],'Z:/WorkspaceTeaching/bla/test.pl'). 
%   pl_test(['Z:/WorkspacePDT/jtransformer'],'Z:/WorkspaceTeaching/bla/test.pl'). 
      
 
pl_test(Project,Output):-
	parse_util:generate_facts(Project),
	writeln('generate abba sources'),
    time(write_facts_to_abba(Output)).     %Ergebnisdatei (abba-Format)
    


