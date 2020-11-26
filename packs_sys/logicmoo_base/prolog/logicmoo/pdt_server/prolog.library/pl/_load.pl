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


 %:- use_module(pdt_support, [pdt_support/1]).
 :- use_module(pdt_support).  % <- NO! THIS BE FIRST!


 :- use_module('logging.pl', []).       % <- THIS BE FIRST!

:- use_module(compatibility, []).
  
 :- consult(compatiblitySWI).
  
 :- use_module(database, []).      % Assert, retract, ...
 :- use_module(files, []).         % File handling
 :- use_module(database_cache, []). 	    % Caching
 :- use_module(contains, []). 	    % Contains for Strings

 :- use_module(general, []).       % Various
 :- use_module(listing, []).       % Print clauses of preds
 :- use_module(lists, []).         % List handling
 :- use_module(count, []).         % Counting
 :- use_module(time, []).          % Runtime measurement
 :- use_module(utils4modules, []). % Module handling

 :- use_module(utils4modules_visibility, []). % Visibility handling

:- if(current_prolog_flag(dialect, swi)). 
 :- use_module(junitadapter, []).
:- endif.

% :- consult(pdt_xref_experimental).     % find_references, ...


