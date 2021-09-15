/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:-module(junitadapter, []).

:- use_module(library(plunit)).

:- dynamic file_to_test/1.

reset_file_to_test :-
   retractall(file_to_test(_)).

unit_test(UnitName,Name):-
    plunit:current_test_set(UnitName),
    plunit:unit_from_spec(_, UnitName, Tests, Module, _),
    Module:'unit test'(Name, _, _, _), plunit:matching_test(Name, Tests).


unit_test(UnitName,Name,File,Line):-
    (   file_to_test(File)
    *-> true
    ;   true
    ),
    plunit:current_test_set(UnitName),
    plunit:unit_from_spec(_, UnitName, Tests, Module, _),
    current_module(Module,File),
    Module:'unit test'(Name, Line,_, _), 
    plunit:matching_test(Name, Tests).
	
	
/*
	junit_adapter(+TestName,-ResultKind,-Comment,-File,-Line)
	
	see exception_kind/3 for result kinds
*/

junit_adapter(TestName,ResultKind,Comment):-
	catch(test(TestName), TestException, true),
	exception_kind(TestException,ResultKind,Comment).
	
	
/*
	exception_kind(+Exception,-Kind,-Comment)
	
	-Kind
		test succeeded:   'true'
		test failed:      'fail'
		thrown exception: 'exception'
*/

exception_kind(TestException,true,''):-
    var(TestException),
    !.

exception_kind(TestException,fail,TestComment):-
	TestException=assertion_failed(TestComment),
    !.

exception_kind(TestException,exception,Message):-
    message_to_string(TestException,MessageString),
    string_to_atom(MessageString,Message).

file_information(TestName,File,Line):-
%    nth_clause(test(TestName),_,Ref),
    clause(test(TestName),_,Ref),
	clause_property(Ref,file(File)),
	clause_property(Ref,line_count(Line)).

file_information(TestName,__File,__Line):-
    format(string(Msg), ' no test case ''~w'' defined in the factbase.',[TestName]),
    throw(Msg). 


test_failure(assertion,A,  Line):-
  plunit:failed_assertion(_Unit, _Test, _Line, _File:Line, _STO, Reason,Module:Goal),
  format(atom(A),'Failed assertion in line ~w,~n ~w of goal ~w in module ~w.',[Line,Reason,Goal,Module]).


test_failure(assertion,A,Line):-
  plunit:failed_assertion(_Unit, _Test, Line, _, _STO, Reason,Module:Goal),
  format(atom(A),'Failed assertion in line ~w, ~w of goal ~w in module ~w.',[Line,Reason,Goal,Module]).
  

test_failure(failed,A, Line):-
  plunit:failed(_,_,Line,Reason),
  format(atom(A),'Failed test in line ~w, ~w.',[Line,Reason]).

test_failure(blocked,A, Line):-
   plunit:blocked(_,_,Line,Reason),
  format(atom(A),'Blocked Assertion in line ~w, ~w.',[Line,Reason]).

	 


%mypred2(Info):-
%	prolog_current_frame(Frame),
%	stack_for_frame(Frame,Info).




