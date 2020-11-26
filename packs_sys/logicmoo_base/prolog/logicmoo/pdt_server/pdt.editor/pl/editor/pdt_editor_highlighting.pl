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

% Utility used by pdt/src/org/cs3/pdt/internal/editors/PLScanner.java

:- module( pdt_editor_highlighting,
         [ predicates_with_property/3  
         ]).
:- use_module(library(lists)).
:- use_module( prolog_connector_pl(split_file_path),
             [ split_file_path/5                % (File,Folder,FileName,BaseName,Extension)
             ] ).

:- op(600, xfy, ::).   % Logtalk message sending operator

               /************************************************
                * PREDICATE PROPERTIES FOR SYNTAX HIGHLIGHTING *
                ************************************************/
                

%% predicates_with_property(+Property,?FileName,-Predicates) is det.
%
% Look up all Predicates with property Property, including atomic
% properties (e.g. dynamic, built_in) AND properties that are 
% functions (e.g. meta_predicate(Head)).
%
% Property = undefined | built_in | dynamic | transparent | meta_predicate(_)    

% GK, 5. April 2011: Extended the implementation to deal with functors. 
% The combination of findall and setof is essential for 
% this added functionality. The findall/3 call finds all results
%   (even if the arguments are free variables -- note that setof/3
%   would return results one by one in such a case, not a full list!). 
% Then the setof/3 call eliminates the duplicates from the results
% of findall/3. 
% DO NOT CHANGE, unless you consider yourself a Prolog expert.

% Look for undefined predicates only in the local context 
% (of the file whose editor has just been opened):
%predicates_with_property(undefined, FileName, Predicates) :-
%    !,
%    module_of_file(FileName,Module), 
%	findall(Name, predicate_name_with_property_(Module,Name,undefined), AllPredicateNames),
%	make_duplicate_free_string(AllPredicateNames,Predicates).

predicates_with_property(Property, FileName, Predicates) :-
	(	split_file_path(FileName, _, _, _, lgt)
	;	split_file_path(FileName, _, _, _, logtalk)
	),
	!,
	current_predicate(logtalk_load/1),
	logtalk_editor_adapter::predicates_with_property(Property, FileName, AllPredicateNames),
	make_duplicate_free_string(AllPredicateNames,Predicates).
predicates_with_property(Property, _FileName, Predicates) :-
    findall(Name, predicate_name_with_property_(_Module,Name,Property), AllPredicateNames),
	make_duplicate_free_string(AllPredicateNames,Predicates).


    	
predicate_name_with_property_(Module,Name,Property) :-
    current_module(Module),
    current_predicate(Module:Name/Arity),
	Name \== [],
	Name \== (:),
	\+ atom_concat('$', _, Name),
	functor(Head,Name,Arity),
	predicate_property(Module:Head,Property).
	
make_duplicate_free_string(AllPredicateNames,Predicates) :-
    setof(Name, member(Name,AllPredicateNames), UniqueNames),
	format(atom(Predicates),'~w',[UniqueNames]).


% Below this line is apparently dead code. 
% TODO: 
% Check whether it is better than the one above.
% If yes use it, otherwise delete it. 

	
%% predicates_with_unary_property(+Property,?Predicates,?PropertyParams) is det.
%
% Look up all Predicates with the unary property Property, e.g. meta_predicate(Head) 
% The element at position i in Predicates is the name of a predicate that has  
% the property Property with the parameter at position i in PropertyParams.
%
% Author: GK, 5 April 2011
% TODO: Integrate into the editor the ability to show the params as tool tips,
% e.g. show the metaargument specifications of a metapredicate on mouse over.
predicates_with_unary_property(Property,Predicates,PropertyArguments) :-
	setof((Name,Arg),
	   predicate_name_with_unary_property_(Name,Property,Arg),
	   PredArgList),
	findall(Pred, member((Pred,_),PredArgList), AllProps),
	findall(Arg,  member((_,Arg), PredArgList), AllArgs),
	format(atom(Predicates),'~w',[AllProps]),
	format(atom(PropertyArguments),'~w',[AllArgs]).
	   	  
% helper
predicate_name_with_unary_property_(Name,Property,Arg) :-
    Property =.. [__F,Arg],
	predicate_property(_M:Head,Property),
	functor(Head,Name,_),
	Name \= '[]'.

