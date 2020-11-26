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

:- module(search_demo, [abc/1, p/1]).

% Select abc and call "Find all Declarations and Definitions" from the context menu.
% You should find results in the following modules:
% - search_demo (local): the clause below is found and this result is "local" because it is started from here
% - search_demo_sub (sub): this module imports search_demo and redefines abc/1, therefore this result is marked as sub
% - other_module (invisible): this module also defines a predicate abc/1, but this is not related to search_demo:abc/1
%
% You can also search globally for definitions of abc/1 (in the menu Search->Search... or CTRL+H from the editor).
% In this case you should find the same three results, but these have no visibility since the global search does not 
% have any context. 
abc(0).

% Select abc and call "Find References" from the context menu.
% You should find both clauses of p/1 as result because both clauses contain a term abc(_).
% 
% You can also start a global search for abc/1 which gives the same results.
p(X) :-
	abc(X).

p(Y) :-
	search_demo_sub:abc(Y).
	
