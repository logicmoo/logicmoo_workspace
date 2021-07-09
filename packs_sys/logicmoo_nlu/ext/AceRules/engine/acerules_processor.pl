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


:- module(acerules_processor, [
		generate_output/7,  % +Input, +Semantics, +Options, -Rules, -Answersets, -Trace, -AnswerTexts
		generate_rules/3,   % +Input, +Options, -Rules
		verbalize_trace/2   % +Trace, -VerbTrace
	]).


:- prolog_load_context(directory,This),   
   absolute_file_name(This,Dir,[file_type(directory)]),
   asserta(user:file_search_path(ace_rules, Dir)).
:- [parameters].
:- parameter(ape_location, APELocation), 
   absolute_file_name(APELocation,APELocationABS,[file_type(directory)]),
   asserta(user:file_search_path(ape, APELocationABS)).


:- use_module(logger).
:- use_module(utils).
:- use_module(list_utils).
:- use_module('skolemizer/skolemizer').
:- use_module('parser/parser').
:- use_module('court_interpreter/court_interpreter').
:- use_module('stable_interpreter/stable_interpreter').
:- use_module('verbalizer/verbalizer').

/** <module> Interface module for AceRules

This modules provides interface predicates for the AceRules system. Prolog terms are used
for input and output.

@author Tobias Kuhn
@version 2007-08-17

@see run_acerules.pl
*/


%% generate_output(+Input, +Semantics, +Options, -Rules, -Answersets, -Trace, -AnswerTexts) is det
%
% Generates the answer set(s) and answer text(s) for the AceRules program.
%
% @param Input The AceRules program as a list of character codes (i.e. a Prolog string).
% @param Semantics The semantics to be used for the interpretation of the program. It has to be
%   court, stable, or stable_strong.
% @param Options A list of name=value pairs. The options guess=(on|off) and maxanswers=int are
%   supported.
% @param Rules Returns the list of rules that are parsed from the program.
% @param Answersets Returns the list of answersets (i.e. sets of facts that can be derived by the program)
%   in internal representation.
% @param Trace Returns a trace of the program execution in internal representation. This can be verbalized
%   with the verbalize_trace/2 predicate.
% @param AnswerTexts Returns a list of answers in ACE format. The elements correspond to the elements of the
%   parameter Answersets.

generate_output(InputCodes, Semantics, Options, Rules, Answersets, Trace, AnswerTexts) :-
	catch(
		generate_output_x(InputCodes, Semantics, Options, Rules, Answersets, Trace, AnswerTexts),
		ar_error(ErrorCode, ErrorText),
		( log(ErrorCode), throw(error(ErrorCode, ErrorText)) )
	),
	!.

generate_output(_, _, _, '', '', '', '') :-
    last_log_message(LogText),
    atom_concat(LogText, '.Unexpected', ErrorCode),
    concat_list(['Unexpected error in ', LogText, '.'], ErrorText),
    log(ErrorCode),
    throw(error(ErrorCode, ErrorText)).


%% generate_output_x(+InputCodes, +Semantics, +Options, -Rules, -Answersets, -Trace, -AnswerTexts) is det

generate_output_x(InputCodes, court, Options, Rules, [Answerset], Trace, [AnswerText]) :-
	reset_skolemizer,
	( member(guess=Guess, Options) ; Guess = off ), !,
	parse(InputCodes, Rules, Guess), !,
	court_interpreter(Rules, Answerset, Trace), !,
	verbalize(Answerset, AnswerText), !.

generate_output_x(InputCodes, Semantics, Options, Rules, Answersets, [], AnswerTexts) :-
    is_member(Semantics, [stable, stable_strong]),
	reset_skolemizer,
	( member(guess=Guess, Options) ; Guess = off ), !,
	parse(InputCodes, Rules, Guess), !,
	( member(maxanswers=MaxAnswers, Options) ; MaxAnswers = 1 ), !,
	stable_interpreter(Rules, Semantics, Answersets, MaxAnswers), !,
	verbalize_list(Answersets, AnswerTexts), !.


%% generate_rules(+Input, +Options, -Rules) is det
%
% Generates the rule representation for the AceRules program.
%
% @param Input The AceRules program as a list of character codes (i.e. a Prolog string).
% @param Options A list of name=value pairs. Only the option guess=(on|off) is supported.
% @param Rules Returns the list of rules that are parsed from the program.

generate_rules(InputCodes, Options, Rules) :-
	catch(
		generate_rules_x(InputCodes, Options, Rules),
		ar_error(ErrorCode, ErrorText),
		( log(ErrorCode), throw(error(ErrorCode, ErrorText)) )
	),
	!.

generate_rules(_, _, '') :-
    last_log_message(LogText),
    atom_concat(LogText, '.Unexpected', ErrorCode),
    concat_list(['Unexpected error in ', LogText, '.'], ErrorText),
    log(ErrorCode),
    throw(error(ErrorCode, ErrorText)).


%% generate_rules_x(+InputCodes, +Options, -Rules) is det

generate_rules_x(InputCodes, Options, Rules) :-
	reset_skolemizer,
	( member(guess=Guess, Options) ; Guess = off ), !,
	parse(InputCodes, Rules, Guess).


%% verbalize_list(+InputList, -VerbalizationList) is det

verbalize_list([], []).

verbalize_list([Answerset|AnswersetsRest], [AnswerText|AnswertextsRest]) :-
    verbalize(Answerset, AnswerText),
    verbalize_list(AnswersetsRest, AnswertextsRest).


%% verbalize_trace(+Trace, -VerbalizedTrace) is det
%
% Verbalizes a trace structure, as returned by the generate_output/7 predicate.
%
% @param Trace The trace structure as returned by generate_output/7.
% @param VerbTrace Returns verbalizations for the trace structure. It is a list that contains
%   terms of the forms raw(Step, RawText) and consistent(Step, ConsistenText). Step is the
%   step of execution. RawText and ConsistentText are ACE texts representing the two stages of
%   each step (in this order).

verbalize_trace(Trace, VerbTrace) :-
	catch(
		verbalize_trace_x(Trace, VerbTrace),
		ar_error(ErrorCode, ErrorText),
		( log(ErrorCode), throw(error(ErrorCode, ErrorText)) )
	),
	!.

verbalize_trace(_, _) :-
    last_log_message(LogText),
    atom_concat(LogText, '.Unexpected', ErrorCode),
    concat_list(['Unexpected error in ', LogText, '.'], ErrorText),
    log(ErrorCode),
    throw(error(ErrorCode, ErrorText)).


%% verbalize_trace_x(+Trace, -VerbalizedTrace) is det

verbalize_trace_x(Trace, VerbTrace) :-
    is_open_list(VerbTrace),
	member(final(Steps, _), Trace),
	verbalize_trace(Trace, 0, Steps, VerbTrace),
	close_list(VerbTrace).


%% verbalize_trace(+Trace, +Step, +LastStep, -VerbalizedTrace) is det

verbalize_trace(_, Step, LastStep, _) :-
    Step > LastStep,
    !.

verbalize_trace(Trace, Step, LastStep, VerbTrace) :-
    member(raw(Step, Raw), Trace),
    clean_factset(Raw, RawF),
    verbalize(RawF, RawText),
    member(raw(Step, RawText), VerbTrace), !,
    member(consistent(Step, Consistent), Trace),
    clean_factset(Consistent, ConsistentF),
    verbalize(ConsistentF, ConsistentText),
    member(consistent(Step, ConsistentText), VerbTrace), !,
    NextStep is Step + 1,
    verbalize_trace(Trace, NextStep, LastStep, VerbTrace).
