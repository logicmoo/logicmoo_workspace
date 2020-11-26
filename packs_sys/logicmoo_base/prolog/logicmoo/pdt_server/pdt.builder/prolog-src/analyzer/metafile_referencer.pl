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

:- module(metafile_referencer, [file_references_for_metacall/3,	%Arg1=ContextModule %Arg2=MetaCall %Arg3=References (see description)
								file_references_for_call/3,		%Arg1=ContextModule %Arg2=Term %Arg3=FileSet
								is_metaterm/3					%Arg1=ContextModule %Arg2=Literal %Arg3=MetaArgument
								]).
								
:- use_module(pdt_prolog_library(utils4modules)).

:- use_module(library(lists)).

:- dynamic user_defined_meta_pred/4.	%user_defined_meta_pred(Functor, Arity ,Module, MetaSpec)

% FÜR EVA: Dies Prädikat müssen wir noch mal besprechen. -- G.

file_references_for_metacall(Module,MetaTerm,References):-
    is_metaterm(Module,MetaTerm,MetaArgs),			
    length(MetaArgs,Length),
    length(References,Length),
    nth1(N,MetaArgs,MArg),
    MArg=(ArgNr,Term),
	file_references_for_term(Module, Term, FileSet),
    nth1(N,References,(ArgNr,Term,FileSet)).
    
    
/* *
 * is_metaterm(?Module, -Literal, ?MetaArguments ) is non_det
 * is_metaterm(?Module, +Literal, ?MetaArguments ) is det
 *  Arg1 is a literal representing a metacall and 
 *  Arg2 is the list of its meta-arguments each of the form:
 *      (Original_argument_position, Argument).
 */
is_metaterm(Module, Literal, MetaArguments) :-
   \+(var(Literal)), !,
   functor(Literal,Functor,Arity),	
   (	Module = user
   ->	visible_in_module(CurrentModule, Functor, Arity)
   ;	(	visible_in_module(Module,Functor,Arity),
   			CurrentModule = Module
   		)
   	),	
   %predicate_property(Module:Literal,meta_predicate(MetaTerm)),
   is_meta_pred(CurrentModule, Literal, MetaTerm),	
   Literal =.. [Functor|Args],
   MetaTerm =.. [Functor|MetaArgs],
   collect_meta_args(Args,MetaArgs, MetaArguments ).
is_metaterm(Module, Literal, MetaArguments) :-
   visible_in_module(Module,Functor,Arity),
   functor(Literal,Functor,Arity),	
   %predicate_property(Module:Literal,meta_predicate(MetaTerm)),
   is_meta_pred(Module, Literal, MetaTerm),
   Literal =.. [Functor|Args],
   MetaTerm =.. [Functor|MetaArgs],
   collect_meta_args(Args,MetaArgs, MetaArguments ).
   

%is_meta_pred(_, assert(_), assert(0)):- !.
%is_meta_pred(_, asserta(_), asserta(0)):- !.
%is_meta_pred(_, assertz(_), assertz(0)):- !.    
is_meta_pred(Module, Literal, MetaTerm):-	%TODO: auf built_in einschränken!
    predicate_property(Module:Literal,meta_predicate(MetaTerm)).
is_meta_pred(Module, Literal, MetaTerm):-    
    functor(Literal, Functor, Arity),
    user_defined_meta_pred(Functor, Arity, Module, MetaTerm).
    
/* *
* collect_meta_args(+Args,+MetaArgs,?MetaArguments) is det
* 
* MetaArguments is unified to a list of all elements of Args that are defined
* as meta-arguments via the corresponding elements of MetaArgs.
* (extract_meta_args/3 is used to select the corresponding elements and to 
* build the entries of MetaArguments.)
* Fails if no MetaArguments can be found.
*/
collect_meta_args(Args,MetaArgs, MetaArguments ) :- 
	bagof( 
        Meta,
        extract_meta_argument(Args,MetaArgs, Meta),
        MetaArguments
    ).
    
extract_meta_argument(Args,MetaArgs, (N,NewArg) ) :- 
    nth1(N,MetaArgs,MArg),
    nth1(N,Args,Arg),
    additonal_parameters(MArg,Arg,NewArg).

% If the meta-argument is not a variable,
% add as many parameters to it as indicated
% by the meta-argument specifier (0-9).
% Fail for (skip) parameters marked as ':'
% (= module-aware) but not as meta:
%additonal_parameters(':',Arg,Arg):- !.
additonal_parameters(0,Arg,Arg):- !.
additonal_parameters(N,Arg,Arg):-
    integer(N),
    var(Arg),!.
additonal_parameters(N,Arg,NewArg) :-
    integer(N),
	Arg =.. [Functor | Params],				
   	length(N_Elems,N),
   	append(Params,N_Elems,NewParams),
   	NewArg =.. [Functor | NewParams].
    
    
file_references_for_call(Module, Term, [(Module, 'any')]):-
    var(Term), !.
file_references_for_call(Module, Term, FileSet):-
    functor(Term,Name,Arity),
    findall( ContextFile,							
    		 (	defined_in_module(Module,Name,Arity),
    		    defined_in_file(Module,Name,Arity,_Nth,File,_Line),
    		 	ContextFile = (Module,File)
    		 ),
    		 Files
    ),
    not(Files = []), !,
    list_to_set(Files,FileSet).
file_references_for_call(Module, Term, FileSet):-
    functor(Term,Name,Arity),
    findall(	ContextFile,
    			(	(Module,Name,Arity,DeclModule),
    				module_property(DeclModule,file(File)),
    				predicate_property(DeclModule:Term,dynamic),
    				ContextFile = (Module,File)
    			),
    			Files
    ), 
    not(Files = []),  !,
    list_to_set(Files,FileSet).
file_references_for_call(Module, Term, [(Module, 'undefined')]):-
    functor(Term,Name,Arity),
	visible_in_module(Module,Name,Arity).   

	
	
	
:- dynamic(new_found_meta_predicate/1).	
	
find_unknown_meta_predicates:-
    retractall(new_found_meta_predicate(_)),
	current_predicate(RefModule:F/A),     % For all defined predicates	
	functor(RefHead,F,A),   
	\+ predicate_property(RefModule:RefHead, built_in),
	\+ predicate_property(RefModule:RefHead, autoload(_)),
    nth_clause(RefModule:RefHead,_,Ref),   % For all their clauses
   	'$xr_member'(Ref, Module:Meta_Head),                  % Get a term referenced by that clause
   	
   	functor(Meta_Head,Name,Arity),
	known_meta_predicate(Module,Name,Arity,_Definition),
    

	%hier müsste ein XReferencer jetzt rein,
	%wenn man es richtig macht, sollten die variablen-Bindungen auch zu finden sein
	% vielleicht mal mit den call_edges versuchen - oberste ebene sollte ich ja finden, geschachteltes im nächsten Schritt   	
   	assert(new_found_meta_predicate(RefModule:F/A)),
   	fail.
find_unknown_meta_predicates.




    
    


/*
 *	know_meta_predicat(?Module,?Name, ?Arity, ?MetaArgs) is nondet
 *
 *  succeds when the predicate represented by Arg1:Arg2/Arg3 
 *  is a meta-predicate. 
 *  In this case, Arg4 is the meta-predicate definition.
 **/
known_meta_predicate(Module,Name,Arity,Definition):-
    predicate_property(Module:Head,meta_predicate(Definition)),
    functor(Head,Name,Arity),
    visible_in_module(Module,Name,Arity).
    
    
    


