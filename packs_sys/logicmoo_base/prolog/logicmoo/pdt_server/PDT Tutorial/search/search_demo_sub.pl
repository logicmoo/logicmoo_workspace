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

% Start a global search for definitions of modules containing "search_demo" (in the menu Search->Search... or CTRL+H from the editor).
% Deselect the 'Exact matches only' option in the search dialog to search for modules containing the search string.
% You should find two modules, search_demo and search_demo_sub.
:- module(search_demo_sub, []).

:- use_module(search_demo).

abc(1).

% Start a global search for references to the module "search_demo".
% You should find three results:
% - the use_module/1 directive above
% - the predicate q/0 below since it calls a predicate from the module search_demo
% - the use_module/1 directive in load.pl
q :-
	search_demo:p(_).
