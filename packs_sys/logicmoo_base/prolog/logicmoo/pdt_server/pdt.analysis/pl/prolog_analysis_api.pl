/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(prolog_analysis_api, [
	analysis_marker/5
]).

%% analysis_category(Name, Description)
:- multifile(analysis_category/2).

%% analysis_definition(Name, Severity, Category, Description)
:- multifile(analysis_definition/4).

%% analysis_result(Name, File, Location, Description)
% Location is either a line number or Start-End
:- multifile(analysis_result/4).

%% analysis_marker(Name, Severity, File, Location, Description)
analysis_marker(Name, Severity, File, Location, Description) :-
	analysis_definition(Name, Severity, _Category, _Description),
	analysis_result(Name, File, Location0, Description),
	(	Location0 = Start-End
	->	format(atom(Location), '~w-~w', [Start, End])
	;	Location = Location0
	).
