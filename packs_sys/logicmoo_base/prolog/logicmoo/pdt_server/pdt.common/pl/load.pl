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

:- ['metainference/load'].
:- ['callgraph/load'].
:- use_module(pdt_entry_points, []).
:- use_module(pdt_manual_entry, []).   % for quick outline (find_pred/5)
:- use_module(properties, []).
:- use_module('xref/pdt_xref', []).
:- use_module(pdt_search, []).

:- use_module(source_files, []).
:- use_module(pdt_common_reload_hook, []).

:- ['lgt/loader'].

