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

:- module(source_files, [pdt_source_files/1, pdt_source_file/2, pdt_source_file/3]).

:- use_module(library(lists), [
	member/2
]).

%% pdt_source_files(String) is nondet.
%
% TRHO: obsolete once improved parser is available (PDT-412)
pdt_source_files(String) :-
	findall(File,
		source_file(File),
		Files),
	ctc_lists:list_2_comma_separated_list(Files, String).

pdt_source_file(File, State) :-
	pdt_source_file(File, State, _).

pdt_source_file(File, State, loaded) :-
	source_file(File),
	(	exists_file(File)
	->	source_file_property(File, 	modified(ModifiedAtConsult)),
		time_file(File, ModifiedNow),
		(	ModifiedNow > ModifiedAtConsult + 0.001
		->	State = old
		;	State = current
		)
	;	State = current
	).

pdt_source_file(File, State, included) :-
	setof(
		F,
		P^L^(
			source_file_property(F, included_in(P, L)),
			\+ source_file(F)
		),
		Fs
	),
	member(File, Fs),
	(	exists_file(File)
	->	setof(
			Time,
			F^source_file_property(F, includes(File, Time)),
			Times
		),
		Times = [ModifiedAtConsult|_],
		time_file(File, ModifiedNow),
		(	ModifiedNow > ModifiedAtConsult + 0.001
		->	State = old
		;	State = current
		)
	;	State = current
	).
	