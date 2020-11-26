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

:- module(other_module, []).

% Start a global search for references to the module "lists" (in the menu Search->Search... or CTRL+H from the editor).
% You should find lots of results, including two results from this file:
% - the use_module/1 directive below and
% - the dummy/1 predicate below since it contains the call to lists:member/2.
% The search is performed on all consulted code. Therefore it finds lots of results contained in SWI Prolog code.
:- use_module(library(lists)).

abc(2).

dummy :-
	member(_, [_,_]).
