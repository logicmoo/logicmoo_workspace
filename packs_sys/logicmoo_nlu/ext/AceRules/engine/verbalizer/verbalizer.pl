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


:- module(verbalizer, [
		verbalize/2  % +Factset, -Text
	]).

:- style_check(-singleton).
:- style_check(-discontiguous).
:- use_module(ape('parser/tokenizer')).
:- use_module(ape('utils/drs_to_coreace')).
:- style_check(+singleton).
:- style_check(+discontiguous).
:- use_module('../skolemizer/skolemizer').
:- use_module('../skolemizer/unskolemizer').
:- use_module('../logger').
:- use_module('../utils').
:- use_module('../list_utils').
:- use_module('../debug_output').
:- use_module(clean_propernames).
:- use_module(drs_generator).
:- use_module(expand_drs).
:- use_module(attach_var_lists).

/** <module> Verbalizer

This module transforms answer sets in internal format into ACE texts. The following steps have
to be performed.

1. Remove unused proper names. See clean_propernames.pl
2. Transform the answer set into a DRS. See drs_generator.pl
3. Expand the predicates that have been condensed by the parser. See expand_drs.pl
4. Generate the correct variable lists for the DRS. See attach_var_lists.pl
5. Unskolemize the variables.
6. Translate the DRS into ACE sentences, using the ACE verbalizer.

@author Tobias Kuhn
@version 2008-11-24
*/


%% verbalize(+Factset, -Text)
%
% Translates an answer set (Factset) into an ACE text (Text).

verbalize(Factset, Text) :-
	log('verbalizer.start'),
	% --- STEP 1 ---
	% Remove unused proper names.
	log('verbalizer.clean-propernames'),
	clean_propernames(Factset, Factset2),
	% --- STEP 2 ---
	% Transform the answer set into a DRS.
	log('verbalizer.generate-drs'),
	generate_drs(Factset2, DRS1),
	debug(drs, 'DRS Preliminary:', DRS1),
	% --- STEP 3 ---
	% Expand the predicates that have been condensed by the parser.
	log('verbalizer.expand-drs'),
	expand_drs(DRS1, DRS2),
	% --- STEP 4 ---
	% Generate the correct variable lists for the DRS.
	log('verbalizer.attach-var-lists'),
	attach_var_lists(DRS2, DRS3),
	% --- STEP 5 ---
	% Unskolemize the variables.
	log('verbalizer.unskolemize'),
	unskolemize(v, DRS3, DRS4),
	debug(drs, 'DRS:', DRS4),
	% --- STEP 6 ---
	% Translate the DRS into ACE sentences, using the ACE verbalizer.
	log('verbalizer.drs-to-acetext'),
	% Using the private auxiliary predicate, because the public predicate is not usable in reverse direction.
	% This creates a "typed" representation where the type arguments are variables. The verbalizer is buggy for
	% untyped DRSs.
	drs_to_coreace(DRS4, TextPre1),
	concat_atom(TextPre1, '\n', TextPre2),
	atom_concat(TextPre2, '\n', Text),
	log('verbalizer.finished').
