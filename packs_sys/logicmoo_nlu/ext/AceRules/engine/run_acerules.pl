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


:- module(run_acerules, [
		     run/3,  % +InFile, +OutFile, +Semantics
		     run/4   % +InFile, +OutFile, +Semantics, +Mode
		    ]).

:- use_module(acerules_processor).
:- use_module(ape('utils/drs_to_ascii')).
:- use_module(ulex_handler).
:- use_module('skolemizer/skolemizer').
:- use_module('parser/parser').
:- use_module('court_interpreter/court_interpreter').
:- use_module('verbalizer/verbalizer').
:- use_module(utils).
:- use_module(debug_output).
:- use_module(simple_rules).

/** <module> File-based interface module for AceRules

This is an interface module for AceRules. It reads the input from a file (or
from a pipe) and writes the output into a file (or into a pipe).

@author Tobias Kuhn
@version 2007-08-17
*/


%% run(+InFile, +OutFile, +Semantics)
%
% Runs an AceRules program. Normal mode is used.
%
% @param InFile The file or pipe containing the AceRules program.
% @param OutFile The file or pipe in which the output is written.
% @param Semantics Specifies which semantics is chosen: court, stable, or stable_strong.

run(InFile, OutFile, Semantics) :-
    run(InFile, OutFile, Semantics, normal).


%% run(+InFile, +OutFile, +Semantics, +Mode)
%
% Runs an AceRules program.
%
% @param InFile The file or pipe containing the AceRules program.
% @param OutFile The file or pipe in which the output is written.
% @param Semantics Specifies which semantics is chosen: court, stable, or stable_strong.
% @param Mode Specifies what kind of output is wanted: normal, debug, trace, or ace_trace.

run(InFile, OutFile, Semantics, Mode) :-
    open(InFile, read, In),
    open(OutFile, write, Out),
    init_ulex,
    process(In, Out, Semantics, Mode),
    close(In),
    close(Out),
    format(user_error, '~w processed.\n', InFile),
    !.

run(_, _, _, _) :-
    write(user_error, 'I/O error occured.').


%% process(+InStream, +OutStream, +Semantics, +Mode)
%
% Reads the rules from InStream and prints the output into OutStream. Mode
% is one of (normal, debug, trace, ace_trace) and specifies the desired output.
% In normal-Mode only the input text, the rules, the answerset and the answer
% text are printed as output. In debug-mode all intermediate results are
% printed. In trace-mode the single steps of the inference algorithm are
% printed.

process(In, Out, Semantics, normal) :-
    no_debug,
	read_stream_to_codes(In, Codes),
	catch(
		(
			generate_output(Codes, Semantics, [maxanswers=10], Rules, Answersets, _, AnswerTexts),
			simple_rules(Rules, SimpleRules),
			print_normal(Out, Codes, Rules, SimpleRules, Answersets, AnswerTexts)
		),
		error(_, ErrorMessage),
		format(Out, 'ERROR: ~w\n', ErrorMessage)
	).

process(In, Out, Semantics, debug) :-
    debug,
	read_stream_to_codes(In, Codes),
	catch(
		(
			generate_output(Codes, Semantics, [maxanswers=10], Rules, Answersets, _, AnswerTexts),
			simple_rules(Rules, SimpleRules),
			print_normal(Out, Codes, Rules, SimpleRules, Answersets, AnswerTexts)
		),
		error(_, ErrorMessage),
		format(Out, 'ERROR: ~w\n', ErrorMessage)
	).

process(In, Out, court, trace) :-
    no_debug,
	read_stream_to_codes(In, Codes),
	catch(
		(
			generate_output(Codes, court, [], _, _, Trace, _),
			member(final(InferenceSteps, _), Trace),
			print_trace(Out, 0, InferenceSteps, Trace)
		),
		error(_, ErrorMessage),
		format(Out, 'ERROR: ~w\n', ErrorMessage)
	).

process(_, Out, _, trace) :-
    write(Out, 'ERROR: Trace is not available for this mode.\n').

process(In, Out, court, ace_trace) :-
    no_debug,
	read_stream_to_codes(In, Codes),
	catch(
		(
			generate_output(Codes, court, [], _, _, Trace, _),
			member(final(InferenceSteps, _), Trace),
			verbalize_trace(Trace, VerbTrace),
			print_ace_trace(Out, 0, InferenceSteps, VerbTrace)
		),
		error(_, ErrorMessage),
		format(Out, 'ERROR: ~w\n', ErrorMessage)
	).

process(_, Out, _, ace_trace) :-
    write(Out, 'ERROR: ACE-trace is not available for this mode.\n').


print_normal(Out, InputCodes, Rules, SimpleRules, Answersets, AnswerTexts) :-
    atom_codes(InputText, InputCodes),
    format(Out, '\nINPUT TEXT:\n~w\nRULES:\n', InputText),
	write_rules(Out, Rules),
	write(Out, '\nSIMPLE RULES:\n'),
	write_rules(Out, SimpleRules), nl(Out),
	write_answersets(Out, Answersets, 1),
	write_answertexts(Out, AnswerTexts, 1).


%% write_answersets(+Stream, +Answersets, +Number)
%
% Writes the answerset onto the stream. The answersets are numbered.

write_answersets(_, [], _).

write_answersets(Out, [Answerset|AnswersetsRest], Num) :-
    write(Out, 'ANSWERSET #'),
    write(Out, Num),
    write(Out, ':\n'),
	write_terms(Out, Answerset), nl(Out),
	NextNum is Num + 1,
    write_answersets(Out, AnswersetsRest, NextNum).


%% write_answertext(+Stream, +AnswerTexts, +Number)
%
% Writes the answer-texts onto the stream. The answer-texts are numbered.

write_answertexts(_, [], _).

write_answertexts(Out, [AnswerText|AnswerTextsRest], Num) :-
    write(Out, 'ANSWERTEXT #'),
    write(Out, Num),
    write(Out, ':\n'),
	write(Out, AnswerText), nl(Out),
	NextNum is Num + 1,
    write_answertexts(Out, AnswerTextsRest, NextNum).


%% print_trace(+Stream, +Step, +LastStep, +Trace)
%
% Writes the trace information onto the stream, in internal representation.

print_trace(_, Step, LastStep, _) :-
    Step > LastStep,
    !.

print_trace(Out, Step, LastStep, TraceOutput) :-
    write(Out, '--- '), write(Out, Step), write(Out, ' ---\n\n'),
    member(raw(Step, Raw), TraceOutput),
    write(Out, 'RAW:\n'),
	write_terms(Out, Raw), nl(Out),
    member(deletelist(Step, Delete), TraceOutput),
    write(Out, 'DELETELIST:\n'),
	write_terms(Out, Delete), nl(Out),
    member(consistent(Step, Consistent), TraceOutput),
    write(Out, 'CONSISTENT:\n'),
	write_terms(Out, Consistent), nl(Out),
    NextStep is Step + 1,
    print_trace(Out, NextStep, LastStep, TraceOutput).


%% print_ace_trace(+Stream, +Step, +LastStep, +Trace)
%
% Writes the trace information onto the stream, verbalized in ACE.

print_ace_trace(_, Step, LastStep, _) :-
    Step > LastStep,
    !.

print_ace_trace(Out, Step, LastStep, VerbTrace) :-
    write(Out, '--- '), write(Out, Step), write(Out, ' ---\n\n'),
    member(raw(Step, RawText), VerbTrace),
    write(Out, 'RAW:\n'),
	write(Out, RawText), nl(Out),
    member(consistent(Step, ConsistentText), VerbTrace),
    write(Out, 'CONSISTENT:\n'),
	write(Out, ConsistentText), nl(Out),
    NextStep is Step + 1,
    print_ace_trace(Out, NextStep, LastStep, VerbTrace).
