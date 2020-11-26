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


:- module(stable_interpreter, [
		stable_interpreter/3,  % +Rules, +Mode, -Answerset
		stable_interpreter/4   % +Rules, +Mode, -Answerset, +MaxAnswers
	]).

:- use_module('../logger').
:- use_module('../list_utils').
:- use_module('../ground_rules').
:- use_module('../debug_output').
:- use_module(stable_checker).
:- use_module(rich_to_poor).
:- use_module(poor_to_rich).
:- use_module(get_stable_models).

/** <module> Stable interpreter

This module executes rules using stable model semantics, with or without strong negation.
The following steps are performed.

1. Check whether the program is valid for stable semantics. See stable_checker.pl
2. Generate variable-free rules.
3. Transform the rule into poor representation. See rich_to_poor.pl
4. Calculation of the answer set(s). See get_stable_models.pl
5. Transform the answer set(s) back to rich representation. See poor_to_rich.pl


---+++ Background

The external programs Smodels and Lparse are used for the actual answer set calculation. The steps
2 and 3 map to the format required for these programs. The step 5 translates back to the format
used by AceRules.

Lparse allows rules to have variables. But as it turned out, these variables have to occur in the
top-level of the predicates. Any variables that occur inside of functions are not handled correctly.
For that reason, we translate the rule set first into a variable-free form.

Atoms in Smodels and Lparse must contain only lower-case letters, numbers, and underscores. For that
reason we need to encode the atoms that contain for example hyphens or upper-case letters. An atom
like 'John' is encoded as xxx_074111104110. Every encoded atom starts with "xxx_" followed by numbers
that are the ASCII codes of the original atom. In this way, the transformation is reversible.

Smodels and Lparse do not support lists in Prolog notation: []. Thus, we have to encode lists as
predicates. A list like [a,b,c] is encoded as xxx_list(a,b,c).

The restricted format used by Smodels and Lparse, we call _poor_. In contrast, the format used by
AceRules that allows general atom names and lists we call _rich_. (Maybe these are not very good names...)

@author Tobias Kuhn
@version 2007-02-09
*/


%% stable_interpreter(+Rules, +Mode, -Answerset)
%
% Calculates the answer set for Rules. Mode has to be stable or stable_strong. At most
% one answer is returned.

stable_interpreter(Rules, Mode, Answersets) :-
    stable_interpreter(Rules, Mode, Answersets, 1).


%% stable_interpreter(+Rules, +Mode, -Answerset, +MaxAnswers)
%
% Calculates the answer set(s) for Rules. Mode has to be stable or stable_strong. MaxAnswers
% defines how many answers should be calculated as a maximum.

stable_interpreter(Rules, Mode, Answersets, MaxAnswers) :-
	log('stable-interpreter.start'),
	is_member(Mode, [stable, stable_strong]),
	% --- STEP 1 ---
	% Check whether the program is valid for stable semantics.
	log('stable-interpreter.stable-checker'),
	stable_checker(Rules, Mode),
	% --- STEP 2 ---
	% Generate variable-free rules.
	log('stable-interpreter.ground-rules'),
	ground_rules(Rules, GroundRules),
	% --- STEP 3 ---
	% Transform the rule into poor representation.
	log('stable-interpreter.rich-to-poor'),
	rich_to_poor(GroundRules, PoorRules),
	debug(rules, 'Rules after Pre-Processing:', PoorRules),
	% --- STEP 4 ---
	% Calculation of the answer set(s).
	log('stable-interpreter.get-stable-models'),
	get_stable_models(PoorRules, Mode, PoorModels, MaxAnswers),
	% --- STEP 5 ---
	% Transform the answer set(s) back to rich representation.
	log('stable-interpreter.postprocess-models'),
	post_process_models(PoorModels, Answersets),
	log('stable-interpreter.finished').


post_process_models([], []).

post_process_models([PoorModel|PoorModelsRest], [Answerset|AnswersetsRest]) :-
	debug(rules, 'Answer before Post-Processing:', PoorModel),
	poor_to_rich(PoorModel, Answerset),
    post_process_models(PoorModelsRest, AnswersetsRest).
