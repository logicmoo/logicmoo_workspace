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


:- module(get_results, [
      get_results/2   % +SOAPInput, -SOAPOutput
   ]).

:- use_module(library('http/http_open')).

:- use_module('../acerules_processor').
:- use_module(ape('utils/drs_to_ascii')).
:- use_module(ape('lexicon/ulex')).
:- use_module(ape('logger/error_logger')).
:- use_module('../ulex_handler').
:- use_module('../utils').
:- use_module('../list_utils').
:- use_module('../verbalizer/verbalizer').
:- use_module('../simple_rules').
:- use_module(soap_utils).

/** <module> Webservice result composer

This module composes the result for a webservice request.

@author Tobias Kuhn
@version 2007-08-17
*/


ar_ns('http://attempto.ifi.uzh.ch/acerules').


%% get_results(+SOAPInput, -SOAPOutput)
%
% Processes the SOAP request and returns the result, again as a SOAP message.

get_results(SOAPInput, SOAPOutput) :-
    write_messages_log(SOAPInput),
    clear_messages,
    init_ulex,
    atom_to_memory_file(SOAPInput, InHandle),
    open_memory_file(InHandle, read, In),
    load_structure(stream(In), Message, [dialect(xmlns)]),
    close(In),
    free_memory_file(InHandle),
    get_element(Message, 'Envelope', Envelope), !,
    get_element(Envelope, 'Body', Body), !,
    get_element(Body, 'Request', Request), !,
    get_program(Request, Program), !,
    get_value(Request, 'Mode', [court, stable, stable_strong], court, Mode),
    get_value(Request, 'Guess', [on, off], off, Guess),
    get_pos_integer_value(Request, 'MaxAnswers', 1, MaxAnswers),
    get_value(Request, 'RulesOutput', [on, off], off, RulesOutput),
    get_value(Request, 'SimpleRulesOutput', [on, off], off, SimpleRulesOutput),
    get_value(Request, 'AnswersetOutput', [on, off], off, AnswersetOutput),
    get_value(Request, 'AnswertextOutput', [on, off], on, AnswertextOutput),
    get_value(Request, 'TraceOutput', [on, off], off, TraceOutput),
    get_value(Request, 'ACETraceOutput', [on, off], off, AceTraceOutput),
    catch(
    	(
    		load_lexicon(Request),
	    	( AnswersetOutput=off, AnswertextOutput=off, TraceOutput=off, AceTraceOutput=off ->
	        	generate_rules(Program, [guess=Guess], Rules)
	    	;
	        	generate_output(Program, Mode, [guess=Guess, maxanswers=MaxAnswers], Rules, Answersets, Trace, AnswerTexts)
	    	),
	    	( SimpleRulesOutput=on ->
	    		simple_rules(Rules, SimpleRules)
	    	; true ),
	        is_open_list(Results),
	        ( RulesOutput = on -> make_result(rules, Rules, Results) ; true ),
	        ( SimpleRulesOutput = on -> make_result(simplerules, SimpleRules, Results) ; true ),
	        ( AnswersetOutput = on -> make_result(answerset, Answersets, Results) ; true ),
	        ( AnswertextOutput = on -> make_result(answertext, AnswerTexts, Results) ; true ),
	        ( TraceOutput = on -> make_result(trace, Trace, Results) ; true ),
	        ( AceTraceOutput = on -> make_result(acetrace, Trace, Results) ; true ),
	        close_list(Results),
	        ar_ns(ARNS),
	        create_soap_message(element(ARNS:'Reply', [], Results), SOAPOutput)
		),
		error(ErrorCode, ErrorMessage),
        create_fault_soap_message(ErrorCode, ErrorMessage, SOAPOutput)
    ),
    write_messages_log(SOAPOutput).


get_program(Request, '') :-
    get_element(Request, 'Program', element(_, _, [])).

get_program(Request, Program) :-
    get_element(Request, 'Program', element(_, _, [Program])).


get_value(Request, Name, PossibleValues, _Default, Value) :-
    get_element(Request, Name, element(_, _, [Value])),
    member(Value, PossibleValues),
    !.

get_value(_Request, _Name, _PossibleValues, Value, Value).


get_pos_integer_value(Request, Name, _Default, Integer) :-
    get_element(Request, Name, element(_, _, [IntegerA])),
    atom_number(IntegerA, Integer),
    integer(Integer),
    Integer > 0,
    !.

get_pos_integer_value(_Request, _Name, Value, Value).


create_fault_soap_message(ErrorCode, ErrorMessage, SOAPMessage) :-
	atom_concat('ar:', ErrorCode, Code),
	create_fault_element(Code, ErrorMessage, FaultElement),
	create_soap_message(FaultElement, SOAPMessage).


load_lexicon(Request) :-
	get_element(Request, 'UserLexiconURL', element(_, _, [UserLexiconURL])),
	!,
	catch(
		http_open(UserLexiconURL, LexiconStream, []),
		_,
		throw(error('LexiconNotFound', 'The URL of the user lexicon is not valid.'))
	),
	read_ulex(LexiconStream),
	get_error_messages(ErrorMessages),
	( member(message(error, lexicon, _, _, Description), ErrorMessages) ->
		throw(error('InvalidLexicon', Description))
	;
		true
	),
	clear_messages.

load_lexicon(_).


assert_error(error(ErrorCode, ErrorMessage)) :-
    !,
    assert(error(ErrorCode, ErrorMessage)).

assert_error(_).


make_result(rules, Rules, Results) :-
    terms_to_atom(Rules, RulesAtom),
    ar_ns(ARNS),
    member(element(ARNS:'Rules', [], [RulesAtom]), Results),
    !.

make_result(simplerules, SimpleRules, Results) :-
    terms_to_atom(SimpleRules, RulesAtom),
    ar_ns(ARNS),
    member(element(ARNS:'SimpleRules', [], [RulesAtom]), Results),
    !.

make_result(answerset, Answersets, Results) :-
    make_answerset_results(Answersets, 1, Results).

make_result(answertext, AnswerTexts, Results) :-
    make_answertext_results(AnswerTexts, 1, Results).

make_result(trace, TraceList, Results) :-
    get_trace(TraceList, 0, Trace),
    ar_ns(ARNS),
    member(element(ARNS:'Trace', [], Trace), Results),
    !.

make_result(acetrace, TraceList, Results) :-
    get_acetrace(TraceList, 0, Trace),
    ar_ns(ARNS),
    member(element(ARNS:'ACETrace', [], Trace), Results),
    !.


make_answerset_results([], _, _).

make_answerset_results([Answerset|AnswersetsRest], Number, Results) :-
    terms_to_atom(Answerset, AnswersetAtom),
    ar_ns(ARNS),
    member(element(ARNS:'Answerset', [], [AnswersetAtom]), Results),
    !,
    NewNumber is Number + 1,
    make_answerset_results(AnswersetsRest, NewNumber, Results).


make_answertext_results([], _, _).

make_answertext_results([AnswerText|AnswerTextsRest], Number, Results) :-
    ar_ns(ARNS),
    member(element(ARNS:'Answertext', [], [AnswerText]), Results),
    !,
    NewNumber is Number + 1,
    make_answertext_results(AnswerTextsRest, NewNumber, Results).


terms_to_atom(Terms, Atom) :-
    new_memory_file(MemHandle),
    open_memory_file(MemHandle, write, S),
    write_rules(S, Terms),
    close(S),
    memory_file_to_atom(MemHandle, Atom).


get_trace(TraceList, Step, Trace) :-
    ar_ns(ARNS),
    member(raw(Step, Raw), TraceList),
    !,
    get_factset(Raw, RawC),
    terms_to_atom(RawC, RawAtom),
    RawEl = element(ARNS:'Raw', [], [RawAtom]),
    member(deletelist(Step, Delete), TraceList),
    get_factset(Delete, DeleteC),
    terms_to_atom(DeleteC, DeleteAtom),
    DeleteEl = element(ARNS:'Delete', [], [DeleteAtom]),
    member(consistent(Step, Consistent), TraceList),
    get_factset(Consistent, ConsistentC),
    terms_to_atom(ConsistentC, ConsistentAtom),
    ConsistentEl = element(ARNS:'Consistent', [], [ConsistentAtom]),
    NextStep is Step + 1,
    get_trace(TraceList, NextStep, TraceRest),
    Trace = [element(ARNS:'Step', [], [RawEl, DeleteEl, ConsistentEl]) | TraceRest].

get_trace(_, _, []).


get_acetrace(TraceList, Step, Trace) :-
    ar_ns(ARNS),
    member(raw(Step, Raw), TraceList),
    !,
    clean_factset(Raw, RawF),
    verbalize(RawF, RawText),
    RawEl = element(ARNS:'Raw', [], [RawText]),
    member(consistent(Step, Consistent), TraceList),
    clean_factset(Consistent, ConsistentF),
    verbalize(ConsistentF, ConsistentText),
    ConsistentEl = element(ARNS:'Consistent', [], [ConsistentText]),
    NextStep is Step + 1,
    get_acetrace(TraceList, NextStep, TraceRest),
    Trace = [element(ARNS:'Step', [], [RawEl, ConsistentEl]) | TraceRest].

get_acetrace(_, _, []).


write_messages_log(Message) :-
    make_directory_path(logs),
	open('logs/messages.log', append, F),
	format(F, '~w\n', Message),
	close(F).
