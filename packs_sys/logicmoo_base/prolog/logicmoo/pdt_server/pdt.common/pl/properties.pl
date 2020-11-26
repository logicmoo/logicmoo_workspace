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

 :- module( properties, 
         [ properties_for_predicate/4   % (+Module, +Name, +Arity, ?PropertyList)
         , entity_property/3            % (+Entity, predicate|module, ?PropertyList)
         ]).

%% properties_for_predicate(+Module, +Name, +Arity, ?PropertyList) is det
% Gives a  list of properties interest for the predicate that is referenced by Arg1:Arg2/Arg3 
% Selected properties of the predicate are elements of list arg4.
% 
properties_for_predicate(Module,Name,Arity,PropertyList):-
    functor(Head,Name,Arity),
    entity_property(Module:Head, predicate, PropertyList).
    
entity_property(Head, predicate, PropertyList):-
    predicate_property(Head,_), %Add a Cut here? Maybe it could also be a module?
    get_property_list(Head, predicate, [dynamic, multifile, exported, transparent, imported_from(_), 
    	file(_), number_of_clauses(_)], PropertyList), !.
entity_property(Name, module, PropertyList):-
    module_property(Name,_), !,
    get_property_list(Name, module, [file(_)], PropertyList).
    
    
get_property_list(Head, Kind, Properties, List):-
	add_properties_to_list(Head, Kind, Properties,[],List).    
    
    
add_properties_to_list(_, _, [], List, List):-!.
add_properties_to_list(Head, Kind, [Property|Rest_Properties], Orig_List, New_List):-
    add_properties_to_list(Head, Kind, Rest_Properties,Orig_List,Rest_List),
    (	look_for_kind_and_property(Head, Kind, Property)
    ->	New_List = [Property|Rest_List]
    ;	New_List = Rest_List
    ).
    
look_for_kind_and_property(Head,predicate,Property):- predicate_property(Head,Property).
look_for_kind_and_property(Head,module,   Property):-    module_property(Head,Property).
