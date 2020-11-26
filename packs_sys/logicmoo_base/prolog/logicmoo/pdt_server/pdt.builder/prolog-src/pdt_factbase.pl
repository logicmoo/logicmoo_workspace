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

:- module(pdt_factbase, [	fileT/3,
							predicateT/5, onloadT/3,
							directiveT/3, clauseT/5, literalT/6, metaT/6, headT/5,
							operatorT/8,
							dynamicT/2, transparentT/2, multifileT/2, meta_predT/2,
							termT/2,  
							call_edge/2, pred_edge/2, onload_edge/2, load_edge/4,
							call_built_in/4,
							fileT_ri/2, predicateT_ri/4, literalT_ri/4,  
							import_dir/2, export_dir/2, load_dir/3, property_dir/3, library_dir/3,
							pos_and_vars/3,
							filePosT/3,
							error/3, warning/3]).
							
:- reexport('util/ctc_admin_copy.pl').

:- dynamic fileT/3.			%fileT(Id,FileName,Module)

:- dynamic onloadT/3.		%onloadT(Id,FileId,Module)	
:- dynamic predicateT/5.	%predicateT(Id,FileId,Functor,Arity,Module)

:- dynamic directiveT/3.	%directiveT(Id,FileId,Module)
:- dynamic clauseT/5.		%clauseT(Id,ParentId,Module,Functor,Arity)
:- dynamic literalT/6.		%literalT(Id,ParentId,EnclosingId,Module,Functor,Arity)
:- dynamic metaT/6.			%metaT(Id,ParentId,EnclosingId,Module,Functor,Arity)		<-- da soll wahrscheinlich noch mehr rein...
:- dynamic headT/5.			%headT(Id,ClauseId,Module,Functor,Arity)

:- dynamic operatorT/8.		%operatorT(Id,ParentId,FileId,Module,Name,Arity,Type,Precedence)

:- dynamic dynamicT/2.		%dynamicT(PredicateId,DynamicId)  			
:- dynamic transparentT/2.	%transparentT(PredicateId,DynamicId)		
:- dynamic multifileT/2.	%multifileT(PredicateId,DynamicId) 
:- dynamic meta_predT/2.	%meta_predT(PredicateId,DynamicId)		

:- dynamic termT/2.			%termT(Id,Term)
:- dynamic filePosT/3.		%filePosT(Id,Pos,Len)    <-- should be coordinated with JTransformer slT/3 in the long run!!!!

:- dynamic call_edge/2.		%call_edge(PredId,LiteralId)
:- dynamic pred_edge/2.		%pred_edge(ClauseId,PredId)					
:- dynamic onload_edge/2.	%onload_edge(DirId,OId)						
:- dynamic load_edge/4.		%load_edge(LoadingId,FileId,Imports,Directive)

:- dynamic call_built_in/4.	%call_built_in(Functor, Arity, Module, LiteralId)

:- dynamic fileT_ri/2.		%fileT_ri(FileName,Id)
:- dynamic predicateT_ri/4.	%predicateT_ri(Functor,Arity,Module,Id)		
:- dynamic literalT_ri/4.   %literalT_ri(Functor, Arity, Module, LiteralId)

:- dynamic pos_and_vars/3.	%pos_and_vars(ClauseId,BodyPos,VarNames)

:- dynamic import_dir/2.	%import_dir(FileId,DirectiveId)
:- dynamic export_dir/2.	%export_dir(Predicates,DirectiveId)
:- dynamic load_dir/3.		%load_dir(DirectiveId,Args,Imports)
:- dynamic property_dir/3.	%property_dir(DirectiveId,Functor,Args)
:- dynamic library_dir/3.	%library_dir(LibName,LibDir,DirectiveId)

:- dynamic error/3.			%error(Error,Context,FileId)  
:- dynamic warning/3.		%warning(TreeElement,Type,AdditionalArgument)
		%TODO: warnings und errors mehr spezialisieren, so dass man sie spezieller updaten kann
		%		je nach Arbeitsschritt
		%		error aktuell nur zu Files (auf dauer wohl besser spezieller


