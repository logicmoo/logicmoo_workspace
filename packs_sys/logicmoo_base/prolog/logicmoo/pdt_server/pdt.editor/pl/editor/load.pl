:- encoding(iso_latin_1).
/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Günter Kniesel
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

%%%:- module(pdteditor,[]).  
%%% <--- TODO: If everything works, turn this into an own module



:- use_module(pdt_editor_breakpoints, []).
:- use_module(pdt_editor_highlighting, []).
:- use_module(pdt_editor_files, []).

:- ['lgt/loader'].

:- if(current_prolog_flag(dialect, swi)).
:- use_module(pdt_editor_edit_hook, []).
:- endif.
