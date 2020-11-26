/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(compatibility,[
	pdt_source_file/2
]).

:- if((current_prolog_flag(version_data, Version), Version @>= swi(7, 1, 15, []))).

:- meta_predicate(pdt_source_file(:, ?)).
pdt_source_file(PI, File) :-
	source_file(PI, File).

:- else.

pdt_source_file(M:Head, File) :-
	nonvar(M),
	nonvar(Head),
	!,
	(	M == user
	->	source_file(Head, File)
	;	source_file(M:Head, File)
	).

pdt_source_file(PI, File) :-
	source_file(Head, File),
	(	Head = _:_
	->	PI = Head
	;	PI = user:Head
	).

:- endif.