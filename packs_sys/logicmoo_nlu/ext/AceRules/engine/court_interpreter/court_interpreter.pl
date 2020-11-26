% This file is part of AceRules.
% Copyright 2008-2012, Tobias Kuhn, http://www.tkuhn.ch
%
% AceRules is free software: you can redistribute it and/or modify it under the terms of the GNU
% Lesser General Public License as published by the Free Software Foundation, either version 3 of
% the License, or (at your option) any later version.
%
% AceRules is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
% the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
% General Public License for more details.
%
% You should have received a copy of the GNU Lesser General Public License along with AceRules. If
% not, see http://www.gnu.org/licenses/.


:- module(court_interpreter, [
		court_interpreter/3  % +Rules, -Answerset, -Trace
	]).

:- use_module('../logger').
:- use_module('../utils').
:- use_module('../list_utils').
:- use_module('../debug_output').
%:- use_module(eliminate_small_cycles).
:- use_module(transform_naf).
:- use_module(answerset_generator).
:- use_module(cycle_checker).

/** <module> Courteous interpreter

This module executes rules in courteous mode. The following step are performed.

1. Eliminate small cylces (_inactivated_). See eliminate_small_cycles.pl
2. Transformation of negation as failure. See transform_naf.pl
3. Generation of the answerset. See answerset_generator.pl
4. Check whether there was a cycle in the atom dependency graph. See cycle_checker.pl

@author Tobias Kuhn
@version 2007-02-06
*/


%% court_interpreter(+Rules, -Answerset, -Trace)
%
% Calculates answerset and trace for the rule set.

court_interpreter(Rules, Answerset, Trace) :-
	log('court-interpreter.start'),
	reset_cycle_checker,
	% --- STEP 1 --- INACTIVATED!
	% Eliminate small cycles.
	%log('court-interpreter.eliminate-small-cycles'),
	%eliminate_small_cycles(Rules, Rules1),
	% --- STEP 2 ---
	% Transformation of negation as failure.
	log('court-interpreter.transform-naf'),
	transform_naf(Rules, Rules2),
	debug(rules, 'Rules after Pre-Processing:', Rules2),
	% --- STEP 3 ---
	% Generation of the answerset.
	log('court-interpreter.answerset-generator'),
	get_answerset(Rules2, Trace),
	close_list(Trace),
	member(final(_, Answerset1), Trace), !,
	clean_factset(Answerset1, Answerset),
	debug(rules, 'Answer before Post-Processing:', Answerset),
	% --- STEP 4 ---
	% Check whether there was a cycle in the atom dependency graph.
	log('court-interpreter.cycle-check'),
	adg_is_acyclic,
	log('court-interpreter.finished').
