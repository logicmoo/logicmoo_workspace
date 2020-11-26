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


:- module(pdt_entry_points,[
	pdt_entry_point/1,
	add_entry_point/1,
	remove_entry_points/1
]).

:- dynamic unfiltered_entry_point/1.

pdt_entry_point(File) :-
    unfiltered_entry_point(File),
    source_file(File).

add_entry_point(File) :-
    unfiltered_entry_point(File),
    !.
    
add_entry_point(File) :-
    assert(unfiltered_entry_point(File)).
    
remove_entry_points(File) :-
    retractall(unfiltered_entry_point(File)).



