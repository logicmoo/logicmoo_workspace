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

:- module(analysis_example, []).

:- multifile(prolog_analysis_api:analysis_definition/4).
:- multifile(prolog_analysis_api:analysis_category/2).
:- multifile(prolog_analysis_api:analysis_result/4).

prolog_analysis_api:analysis_definition('Example Analysis', info, 'Example Category', 'This is an example analysis').

prolog_analysis_api:analysis_category('Example Category', 'This is an example category').

prolog_analysis_api:analysis_result('Example Analysis', File, Line, Description) :-
	nth_clause(outline_demo:likes(_, _), N, Ref),
	clause_property(Ref, file(File)),
	clause_property(Ref, line_count(Line)),
	format(atom(Description), 'This is the ~w. clause of likes/2', [N]).
