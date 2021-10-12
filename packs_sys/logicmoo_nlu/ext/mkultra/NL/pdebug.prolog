:- public pdebug/1.

pdebug(WordList) :-
   step_limit(100000),
   forall(nonterminal(N), test_nonterminal(N, WordList)).

test_nonterminal(N, WordList) :-
   parse(N, WordList),
   writeln(N),
   !.
test_nonterminal(N, _) :-
   functor(N, F, _A),
   writeln(F:fail).

parse(Nonterminal, WordList) :-
   term_append(Nonterminal, [ WordList, [ ] ], Full),
   Full.

parse(Nonterminal, WordList, Completion) :-
   term_append(Nonterminal, [ WordList, Completion ], Full),
   Full.

nonterminal(np(_LF, _C, _A, nogap, nogap)).
nonterminal(aux(_, _P, _A, _T, _A, _F, _M)).
nonterminal(vp(_F, _LF, _S, _T, _A, nogap)).
nonterminal(aux_vp(_VP, _P, _A, _T, _)).
nonterminal(s(_LF, _M, _P, _T, _A)).
nonterminal(utterance(_DialogAct)).

%%%
%%% Regression testing
%%%

:- public test_parser/0, try_completion/3, try_parse/3.

test_parser :-
   forall(parser_tests(Nonterminal, String),
	  run_parser_test(Nonterminal, String)).

try_parse(Nonterminal, String, Prototype) :-
   nonterminal_args(Nonterminal, Prototype),
   word_list(String, Words),
   parse(Prototype, Words).

try_completion(Nonterminal, String, CompletionText) :-
   nonterminal_args(Nonterminal, Prototype),
   word_list(String, Words),
   parse(Prototype, Words, CompletionWords),
   word_list(CompletionText, CompletionWords).

run_parser_test(Nonterminal, String) :-
   nonterminal_args(Nonterminal, Prototype),
   word_list(String, Words),
   catch(once((parse(Prototype, Words) ; writeln(String:fail:Prototype))),
	 Exception,
	 writeln(String:Exception:Prototype)).

%% nonterminal_args(+Functor, -Prototype)
%  Prototype is the arguments for parsing a Functor nonterminal, sans input and output text.
nonterminal_args(Functor, Prototype) :-
   nonterminal(Prototype),
   functor(Prototype, Functor, _).

