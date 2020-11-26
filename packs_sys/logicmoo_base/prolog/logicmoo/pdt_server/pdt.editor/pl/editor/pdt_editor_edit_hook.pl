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


:- module(pdt_editor_edit_hook,[
]).

:- use_module(library(edit)).
:- use_module(library(lists)).

:- multifile(prolog_edit:edit_source/1).

prolog_edit:edit_source(Location) :-
    member(file(File), Location),
    (	member(line(Line), Location)
    ->	true
    ;	Line = 1
    ),
    format(atom(A), '~w ~w', [File, Line]),
    catch(process_observe:process_notify(pdt_edit_hook,A),_,true).


