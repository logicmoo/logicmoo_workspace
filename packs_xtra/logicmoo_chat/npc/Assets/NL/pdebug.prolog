:- public pdebug/1.



%=autodoc
%% pdebug( ?WordList) is semidet.
%
% Pdebug.
%
pdebug(WordList) :-
   step_limit(100000),
   forall(nonterminal(N), test_nonterminal(N, WordList)).



%=autodoc
%% test_nonterminal( ?N, ?WordList) is semidet.
%
% Test Nonterminal.
%
test_nonterminal(N, WordList) :-
   parse(N, WordList),
   writeln(N),
   !.
test_nonterminal(N, _) :-
   functor(N, F, _A),
   writeln(F:fail).



%=autodoc
%% parse( +Nonterminal, ?WordList) is semidet.
%
% Parse.
%
parse(Nonterminal, WordList) :-
   call(Nonterminal, WordList, [ ]).



%=autodoc
%% parse( +Nonterminal, ?WordList, ?Completion) is semidet.
%
% Parse.
%
parse(Nonterminal, WordList, Completion) :-
   call(Nonterminal, WordList, Completion ).



%=autodoc
%% nonterminal( ?DialogAct) is semidet.
%
% Nonterminal.
%
nonterminal(utterance(_DialogAct)).
nonterminal(s(_LF, _M, _P, _T, _A)).
nonterminal(vp(_F, _LF, _S, _T, _A, nogap)).
nonterminal(aux_vp(_VP, _P, _A, _T, _)).
nonterminal(aux(_, _P, _Agreement, _T, _Aspect, _F, _M)).
nonterminal(np_chat(_LF, _C, _A, nogap, nogap)).

%%%
%%% Regression testing
%%%

:- dynamic(parser_tests/2).

:- public test_parser/0, try_completion/3, try_parse/3.



%=autodoc
%% test_parser is semidet.
%
% Test Parser.
%
test_parser :-
   forall(parser_tests(Nonterminal, String),
	  run_parser_test(Nonterminal, String)).



%=autodoc
%% try_parse( +Nonterminal, +String, ?Prototype) is semidet.
%
% Try Parse.
%
try_parse(Nonterminal, String, Prototype) :-
   nonterminal_args(Nonterminal, Prototype),
   word_list(String, Words),
   parse(Prototype, Words).



%=autodoc
%% try_completion( +Nonterminal, +String, ?CompletionText) is semidet.
%
% Try Completion.
%
try_completion(Nonterminal, String, CompletionText) :-
   nonterminal_args(Nonterminal, Prototype),
   word_list(String, Words),
   parse(Prototype, Words, CompletionWords),
   word_list(CompletionText, CompletionWords).



%=autodoc
%% run_parser_test( +Nonterminal, +String) is semidet.
%
% Run Parser Test.
%
run_parser_test(Nonterminal, String) :-
   nonterminal_args(Nonterminal, Prototype),
   word_list(String, Words),
   catch(once((parse(Prototype, Words) -> ansicall(green,writeln(Words:pass:Prototype)) ; ansicall(red,writeln(String:fail:Prototype)))),
	 Exception,
	 ansicall(yellow,writeln(String:Exception:Prototype))).

%% nonterminal_args(+Functor, -Prototype)
%  Prototype is the arguments for parsing a Functor nonterminal, sans input and output text.
nonterminal_args(Functor, Prototype) :-
   nonterminal(Prototype),
   functor(Prototype, Functor, _).

