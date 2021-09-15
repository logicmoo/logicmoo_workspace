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

:- module(pdt_smells, [smell_marker_pdt/7]).

%:- user:ensure_loaded(pdt_builder_analyzer('meta_pred_toplevel.pl')).

:- multifile smell_description_pdt/3.
:- multifile smell/5.

smell_marker_pdt(_Name, _Description, _QuickfixDescription, _QuickfixAction, _File, _Start, _Length) :- fail.
%	smell_description_pdt(Name, Description, QuickfixDescription),
%	smell(Name, File, Start, Length, QuickfixAction).
%		
%smell_description_pdt('MissingMetaPredicateDeclaration', SmellDescription, QuickfixDescription):-
%	SmellDescription = 'Missing meta-predicate declation', 
%	QuickfixDescription = 'Add missing meta-predicate declaration'.
%	
%    
%%QuickfixAction ist der Text, der unmittelbar vor dem Smell eingetragen werden muss (Zeilenumbruch muss mit angegeben werden)
%smell('MissingMetaPredicateDeclaration', File, Offset, 0, QuickfixAction) :-
%    find_undeclared_meta_predicates_position(File, Offset, Spec),
%    format(atom(QuickfixAction),':- meta_predicate(~w).~n', [Spec]).
    
    
    
    
% Dummy: Just for testing
%smell_description_pdt('hallo', 'beschreibung', 'fix beschreibung').
%smell('hallo', 'l:/work/noth/workspaces/runtime generics/dummdidumm/pl/heididdeliho.pl', 0, 5, '% fix itself\n').


