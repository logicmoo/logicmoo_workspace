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


:- module( pdt_manual_entry,
         [ predicate_manual_entry/5    % (_Module,Pred,Arity,Content)
         ]).
         
:- if(current_prolog_flag(dialect, swi)).

:- use_module(library(lists)).
%:- use_module(library(helpidx)).
:- use_module(library(memfile)).
:- use_module(library(quintus)).

% TODO 2012-07: are these still in use?
:- use_module(library(pldoc/doc_library)).
:- use_module(library(explain)).
:- use_module(library(help)).
:- use_module(library(make)).
:- use_module(library('pldoc')).
%:- use_module(library('pldoc/doc_html'),except([op_type/2])).
:- use_module(library('http/html_write')).

:- use_module(pdt_prolog_library(utils4modules)).


               /****************************************
                * GET THE MANUAL ENTRY FOR A PREDICATE *
                ****************************************/

%% predicate_manual_entry(+Module, +Pred,+Arity,-Content, -IsDeprecated) is det.
%
%
predicate_manual_entry(Module, Pred,Arity,Content, IsDeprecated) :-
    %pldoc:doc_comment(Module:Pred/Arity,_File:_,,Content),
    %TODO: The html code is now available:
    pldoc_process:doc_comment(Module:Pred/Arity,File:_,_Summary, Comment),
	(	atomic(Comment),
		sub_atom(Comment, _, _, _, '@deprecated')
	->	IsDeprecated = true
	;	IsDeprecated = false
	),
	gen_html_for_pred_(File,Pred/Arity,Content),
    !.
	
predicate_manual_entry(_Module,_Pred,_Arity,'nodoc', _).

gen_html_for_pred_(FileSpec,Functor/Arity,Html) :-    
	doc_file_objects(FileSpec, _File, Objects, FileOptions, [public_only(false)]),
	member(doc(Signature,FilePos,Doc),Objects),
	( Functor/Arity=Signature 
	; _Module:Functor/Arity=Signature
	),
	!,
	phrase(html([ 
	     		\objects([doc(Functor/Arity,FilePos,Doc)], FileOptions)
	     ]),List),
%	maplist(replace_nl_,List,AtomList),
%	atomic_list_concat(AtomList,Html), 
	with_output_to(atom(Html), print_html(List)).



%% manual_entry(Pred,Arity,Content) is det.
%
% TODO: Remove duplicate code by using predicate_manual_entry.
% Only difference: Use of stream_position versus use of seek.
%
manual_entry(Pred,Arity,Content) :-
    predicate(Pred,Arity,_,From,To),
    !,
    online_help:online_manual_stream(Manual),
    new_memory_file(Handle),
    open_memory_file(Handle, write, MemStream),
    stream_position(Manual, _, '$stream_position'(From, 0, 0)),
    Range is To - From,
    online_help:copy_chars(Range, Manual, MemStream),
    close(MemStream),
    memory_file_to_atom(Handle,Content),
    free_memory_file(Handle).
/*
manual_entry(Pred,Arity,Content) :-
    meta_data_help(_,Pred,Arity,ContentString),
    string_to_atom(ContentString,Content).

manual_entry(Pred,-1,Content) :-
    meta_data_module(_,Pred,ContentString),
    string_to_atom(ContentString,Content).
*/

:- else.

predicate_manual_entry(_Module,_Pred,_Arity,'nodoc').

:- endif.



