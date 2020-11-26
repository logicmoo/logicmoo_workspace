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


:- module(generate_drs, [
		generate_drs/3   % +ACEText, +Guess, -DRS
	]).

:- use_module(ape('lexicon/clex')).
:- use_module(ape('parser/ace_to_drs')).
:- use_module(ape('parser/tokenizer.pl')).
:- ensure_loaded('../ulex_handler').
:- use_module('../logger').
:- use_module('../list_utils').

/** <module> DRS generator

This module invokes the APE parser to generate the DRS for an ACE text.

Furthermore, this module maps the error messages of APE to the format that is used by AceRules.

@author Tobias Kuhn
@version 2007-08-16
*/


%% generate_drs(+ACEText, +Guess, -DRS)
%
% Returns the DRS representation for the ACEText. Guess is 'on' or 'off' and defines if unknown word
% guessing should be applied.

generate_drs(Text, Guess, DRS) :-
	acetext_to_drs(Text, Guess, _, _, _, DRS, Messages, _),
	!,
	( member(message(error, _, _, _, _), Messages) ->
		transform_ape_error_messages(Messages, MessagesAtom, Code),
		atom_concat('Invalid ACE text:\n', MessagesAtom, ErrorText),
		throw(ar_error(Code, ErrorText))
	;
		true
	).


%% transform_ape_error_messages(+APEErrorMessages, -AceRulesErrorMessages, -Code)
%
% Transforms the APE-style error messages into the format used for AceRules.

transform_ape_error_messages([], '', 'parser.generate-drs.Unexpected').

transform_ape_error_messages([message(error, word, _, Word, 'Use the prefix n:, v:, a: or p:.')|Rest], Text, Code) :-
    Code = 'parser.generate-drs.UnknownWord',
    !,
    transform_ape_error_messages(Rest, TextRest, _),
    concat_list(['The word "', Word, '" is unknown.\n', TextRest], Text).

transform_ape_error_messages([message(error, word, _, WordBad, WordGood)|Rest], Text, Code) :-
    Code = 'parser.generate-drs.UnknownWord',
    !,
    transform_ape_error_messages(Rest, TextRest, _),
    concat_list(['The word "', WordBad, '" is unknown; did you mean "', WordGood, '"?\n', TextRest], Text).

transform_ape_error_messages([message(error, syntax, _, PN, 'A propername must not occur in apposition.')|Rest], Text, Code) :-
    Code = 'parser.generate-drs.PropernameInApposition',
    !,
    transform_ape_error_messages(Rest, TextRest, _),
    concat_list(['Propernames must not occur in apposition. The propername "', PN, '" occurs in apposition.\n', TextRest], Text).

transform_ape_error_messages([message(error, syntax, _, FN, 'A functionword must not occur in apposition.')|Rest], Text, Code) :-
    Code = 'parser.generate-drs.FunctionwordInApposition',
    !,
    transform_ape_error_messages(Rest, TextRest, _),
    concat_list(['Functionwords must not occur in apposition. The functionword "', FN, '" occurs in apposition.\n', TextRest], Text).

transform_ape_error_messages([message(error, sentence, Number-_, Sentence, 'This is the first sentence that was not ACE. Please rephrase it!')|Rest], Text, Code) :-
    Code = 'parser.generate-drs.InvalidSentence',
    !,
    transform_ape_error_messages(Rest, TextRest, _),
    concat_list(['The first incorrect sentence is number ', Number, ': "', Sentence, '"\n', TextRest], Text).

transform_ape_error_messages([message(error, refres, _, ErrorText, 'Identify correct accessible antecedent.')|Rest], Text, Code) :-
    atom_concat('Unresolved anaphor: ', Anaphor, ErrorText),
    Code = 'parser.generate-drs.UnresolvedAnaphor',
    !,
    transform_ape_error_messages(Rest, TextRest, _),
    concat_list(['The anaphor "', Anaphor, '" cannot be resolved.\n', TextRest], Text).

transform_ape_error_messages([message(error, refres, _, ErrorText, 'Assign unique variables.')|Rest], Text, Code) :-
    atom_concat('Redefined variable: ', VariableName, ErrorText),
    Code = 'parser.generate-drs.RedefinedVariable',
    !,
    transform_ape_error_messages(Rest, TextRest, _),
    concat_list(['The variable "', VariableName, '" is defined more than once.\n', TextRest], Text).

transform_ape_error_messages([message(error, _, _, Error, Desc)|Rest], Text, Code) :-
    Code = 'parser.generate-drs.APE-Error',
    !,
    transform_ape_error_messages(Rest, TextRest, _),
    concat_list(['"', Error, '": ', Desc, '\n', TextRest], Text).

transform_ape_error_messages([_|Rest], Text, Code) :-
    transform_ape_error_messages(Rest, Text, Code).
