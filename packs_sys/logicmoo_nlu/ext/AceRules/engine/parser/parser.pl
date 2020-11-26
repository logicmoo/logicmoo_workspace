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


:- module(parser, [
		     parse/2,  % +InputCodes, -Rules
		     parse/3   % +InputCodes, -Rules, +Guess
		    ]).

:- use_module('../skolemizer/skolemizer').
:- use_module('../skolemizer/unskolemizer').
:- use_module('../logger').
:- use_module('../utils').
:- use_module('../list_utils').
:- use_module('../debug_output').
:- use_module(meta_preprocess).
:- use_module(priority_handler).
:- use_module(generate_drs).
:- use_module(drs_checker).
:- use_module(collect_templates).
:- use_module(group_predicates).
:- use_module(check_grouping).
:- use_module(condense_drs).
:- use_module(double_implication).
:- use_module(skolemize_drs).
:- use_module(rule_creator).

/** <module> Parser

Transforms an AceRules program into a rule structure using Prolog notation.
Error messages are raised if invalid structures are found. The following steps
are performed.

* 1.  Process the meta structures, i.e. labels and overrides-statements. See meta_preprocess.pl
* 2.  Using APE to parse the ACE rules and facts. See generate_drs.pl
* 3.  First check of the structure of the DRS (Level 1). See drs_checker.pl
* 4.  Transformation of double implications. See double_implication.pl
* 5.  Condense the DRS: reduction of the number of predicates and other transformations. See condense_drs.pl
* 6.  Collection of group templates. See collect_templates.pl
* 7.  Group predicates. See group_predicates.pl
* 8.  Check if the atom-restriction was violated. See check_grouping.pl
* 9.  Second check of the structure of the DRS (Level 2). See drs_checker.pl
* 10.  Skolemization of the variables. See skolemize_drs.pl
* 11. Third check of the structure of the DRS (Level 3). See drs_checker.pl
* 12. Creation of rules from the labeled DRS. See rule_creator.pl

@author Tobias Kuhn
@version 2007-03-19
*/


%% parse(+InputCodes, -Rules)
%
% Transforms the AceRules program (InputCodes) into a rule representation (Rules).
%
% @see parse/3

parse(InputCodes, Rules) :-
    parse(InputCodes, Rules, off).


%% parse(+InputCodes, -Rules, +Guess)
%
% Transforms the AceRules program (InputCodes) into a rule representation (Rules).
% Guess is 'on' or 'off' and defines if unknown word guessing should be applied.
%
% @see parse/2

parse(InputCodes, Rules, Guess) :-
	log('parser.start'),
	% --- STEP 1 ---
	% Process the meta structures, i.e. labels and overrides-statements.
	log('parser.meta-preprocess'),
	meta_preprocess(InputCodes, PlainText, LabelMap, OverridesPre),
	log('parser.priority_handler'),
	priority_handler(OverridesPre, Overrides),
	debug(list, 'Overrides Statements:', Overrides),
	% --- STEP 2 ---
	% Using APE to parse the ACE rules and facts.
	log('parser.generate-drs'),
	generate_drs(PlainText, Guess, DRS1),
	debug(drs, 'DRS Original:', DRS1),
	% --- STEP 3 ---
	% First check of the structure of the DRS (Level 1).
	log('parser.check-drs-1'),
	check_drs(DRS1, 1),
	% --- STEP 4 ---
	% Transformation of double implications.
	log('parser.double_implication'),
	transform_double_implication(DRS1, DRS2),
	% --- STEP 5 ---
	% Condense the DRS: reduction of the number of predicates and other transformations.
	log('parser.condense-drs'),
	condense_drs(DRS2, DRS3),
	debug(drs, 'DRS Condensed:', DRS3),
	% --- STEP 6 ---
	% Collection of group templates.
	log('parser.collect-templates'),
	clear_templates,
	collect_templates(DRS3),
	findall(Template, group_template(Template), Templates),
	debug(list, 'Group Templates:', Templates),
	% --- STEP 7 ---
	% Group predicates.
	log('parser.group-predicates'),
	copy_term(DRS3, DRS4),
	group_predicates(DRS4, DRS5),
	debug(drs, 'DRS Grouped:', DRS5),
	% --- STEP 8 ---
	% Check if the atom-restriction was violated.
	log('parser.check-grouping'),
	check_grouping(DRS5),
	% --- STEP 9 ---
	% Second check of the structure of the DRS (Level 2).
	log('parser.check-drs-2'),
	check_drs(DRS5, 2),
	% --- STEP 10 ---
	% Skolemization of the variables.
	log('parser.skolemize-drs'),
	skolemize_drs(DRS5),
	debug(drs, 'DRS Grouped & Skolemized:', DRS5),
	% --- STEP 11 ---
	% Third check of the structure of the DRS (Level 3).
	log('parser.check-drs-3'),
	check_drs(DRS5, 3),
	% --- STEP 12 ---
	% Creation of rules from the labeled DRS.
	log('parser.create-rules'),
	create_rules(DRS5, LabelMap, Rules1),
	get_setof(Rule, member(Rule, Rules1), Rules2),
	append(Rules2, Overrides, Rules),
	debug(rules, 'Rules:', Rules),
	log('parser.finished').
