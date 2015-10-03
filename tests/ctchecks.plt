:- begin_tests(ctchecks).

:- multifile
    user:message_property/2.

:- dynamic
    user:error_on_co/0.

user:message_property(_, stream(current_output)) :- user:error_on_co.

:- use_module(library(comment_data)).
:- use_module(library(call_in_module_file)).

:- comment_data:enable.

:- use_module(ctcex).

/* $ctcex$
Warning: -----------------
Warning: Check asssertions
Warning: ---------------------
Warning: The predicates below contains assertions that are inconsistent
Warning: with the  implementation. The reason is explained there.
Warning: 
ctcex.pl:18:4: In the body of ctcex:q/0:
ctcex.pl:12: Compile-Time failure in assertion for a(A,B).
	In *compat*, unsatisfied properties: 
		[list(B)].
	Because: 
		[B=b].
ctcex.pl:15: In the head of ctcex:a/2:
ctcex.pl:12: Compile-Time failure in assertion for a(A,B).
	In *compat*, unsatisfied properties: 
		[int(A),list(B)].
	Because: 
		[A=a,B=b].
ctcex.pl:30: In assertions of [ctcex:b/2]:
	[ctcex:is_3/1] are not properties
ctcex.pl:32: In assertions of [ctcex:b/2]:
	[ctcex:is_2/1] are not properties
ctcex.pl:36: In assertions of [ctcex:b/2]:
ctcex.pl:26: Compile-Time failure in assertion for is_num(A,B).
	In *compat*, unsatisfied properties: 
		[int(B)].
	Because: 
		[B=b].ctcex.pl:26: Compile-Time failure in assertion for is_num(A,B).
	In *compat*, unsatisfied properties: 
		[int(B)].
	Because: 
		[B=a].
*/

test(ctcex) :-
    %set_prolog_flag(check_assertions, [defined, is_prop, ctcheck]),
    set_prolog_flag(verbose, silent),
    assert(user:error_on_co),
    with_output_to(string(Result), showcheck(assertions, [module(ctcex)])),
    comment_data(ctcex, Pattern),
    module_property(ctcex, file(File)),
    directory_file_path(Dir, _, File),
    directory_file_path(Dir, '', AD),
    atom_string(AD, SD),
    replace_substrings(Result, SD, "", AResult1),
    replace_substrings(AResult1, "ERROR: ", "", AResult),
    assertion(Pattern == AResult),
    set_prolog_flag(verbose, normal),
    %set_prolog_flag(check_assertions, []).
    retractall(user:error_on_co).

replace_substrings(String, SubS, Repl, Result) :-
    ( sub_string(String, Before, _, After, SubS)
    ->sub_string(String, 0, Before, _, BeforeS),
      sub_string(String, _, After, 0, AfterS),
      replace_substrings(AfterS, SubS, Repl, Tail),
      string_concat(BeforeS, Repl, ResultHead),
      string_concat(ResultHead, Tail, Result)
    ; Result = String
    ).

:- comment_data:disable.

:- end_tests(ctchecks).
