:- encoding(iso_latin_1).
/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Günter Kniesel (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

% Date: 06.10.2004

% 21.2.2011: Added option 'unique'

/*
 * This file contains predicates for caching results of goals
 * as facts in a specified module. By providing the option 
 * 'unique' in the optional Options list, one can enforce that the
 * generated facts are duplicate free.
 */
:- module( database_cache, 
   [ generate_all_in_module/3  % (+Module,+Template,+Call)
   , generate_all_in_module/4  % (+Module,+Template,+Call,+Options)
   , generate_in_module/3      % (+Module,+Template,+Call)
   , generate_in_module/4      % (+Module,+Template,+Call,Options)
   ]
).

:- use_module(general).       
:- use_module(utils4modules).
/*
 * generate_all_in_module(+Module,+Template,+Call)
 * generate_all_in_module(+Module,+Template,+Call,Options)
 *
 * Compute ALL results of an arbitrary goal (Arg3) 
 * that shares variables with the template (Arg2)
 * and assert the resulting template instances in 
 * the specified output module (Arg1). 
 *
 * If the Options list contains the element 'unique'
 * the generated facts will contain NO DUPLICATES.
 * If no Options are provided duplicates will be created.
 *
 * All previous facts for Template are retracted  
 * from Module before generating the new facts. 
 */
 
 :- use_module(library(lists)).
 
:- module_transparent generate_all_in_module/3,
                      generate_all_in_module/4.
                      
generate_all_in_module(Module,Template,Call) :- 
   generate_all_in_module(Module,Template,Call,[]).
   
generate_all_in_module(Module,Template,Call,Options) :- 
   retractall_generated_in_module(Module,Template),
   all( generate_in_module(Module,Template,Call,Options) ).


:- module_transparent retractall_generated_in_module/2.
 
retractall_generated_in_module(Module,Template) :- 
   ( is_list(Template)
     -> forall( member(T, Template), retractall_in_module(Module,T) )
      ; retractall_in_module(Module,Template)
   ) .


   
   
/*
 * generate_in_module(+Module,+Template,+Call)
 *
 * Compute a result of an arbitrary goal (Arg3) 
 * that shares variables with the template (Arg2)
 * and assert the resulting template instances in 
 * the specified output module (Arg1). 
 * If Template is a list, assert each element.
 */
:- module_transparent generate_in_module/3, 
                      generate_in_module/4.

generate_in_module(Module,Template,Call) :-
   generate_in_module(Module,Template,Call,[]).
                         
generate_in_module(Module,Template,Call,Options) :- 
   call(Call), 
   ( is_list(Template)
     -> forall( member(T, Template), assert_in_module(Module,T, Options) )
      ; assert_in_module(Module,Template,Options)
   ) .


%/* **********************************************************
% * Only for systems that do not support modules: 
% * Caching by prepending prefix to predicate name
% */
%
%   
%findall_and_generate_with_prefix(Prefix,Call) :- 
%   all( generate_with_prefix(Prefix,Call) ).
%
%generate_with_prefix(Prefix, Goal) :-
%    generatedName(Goal,Prefix,F/N,NewG),
%    dynamic(F/N),
%    call(Goal),
%    assert(NewG).
% 
%regenerate_with_prefix(Prefix, Goal) :-
%    generatedName(Goal,Prefix,_,NewG),
%    retractall(NewG),
%    call(Goal),
%    assert(NewG).
%        
%generatedName(G1,Prefix,F/N,G2) :-
%    functor(G1,FOld,N),
%    G1 =.. [FOld|Arg],
%    concat(Prefix,FOld,F),
%    G2 =.. [F|Arg].

