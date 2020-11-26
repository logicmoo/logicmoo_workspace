/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(completion_demo, []).

:- use_module(library(pldoc)).

:- doc_collect(true).

%% my_predicate_with_documentation(+Value) is det.
% 
% True if Value is an atom.
my_predicate_with_documentation(Value) :-
	atom(Value).

my_predicate_without_documentation(Value) :-
	atomic(Value).

% Use the auto-completion of the Prolog Editor by typing a prefix and pressing CTRL+Space.
% The auto-completion lists all matching predicates and modules.
% If the documentation of a selected predicate has been parsed it is shown right to the list of predicates
% and the variable names used in the documentation will be inserted as arguments of the predicate.
% E.g. type the prefix "my_pre" and press CTRL+Space.

