:- begin_tests(ctchecks).

:- multifile
    user:message_property/2.

:- dynamic
    user:error_on_co/0.

user:message_property(_, stream(current_output)) :- user:error_on_co.

:- use_module(library(record_locations)).
:- use_module(library(comment_data)).
:- use_module(library(call_in_dir)).
:- use_module(checkers(checker)).

:- comment_data:enable.

:- use_module(ctcex).

/* $ctcex$
Warning: -----------------
Warning: Check asssertions
Warning: ---------------------
Warning: The predicates below contains assertions that are inconsistent
Warning: with the  implementation. The reason is explained there.
Warning: 
ctcex.pl:18: In the body of ctcex:q/0:
ctcex.pl:12: Assertion failure for a(A,B).
	In *compat*, unsatisfied properties: 
		[list(B)].
	Because: 
		[B=b].
ctcex.pl:15: In the head of ctcex:a/2:
ctcex.pl:12: Assertion failure for a(A,B).
	In *compat*, unsatisfied properties: 
		[int(A),list(B)].
	Because: 
		[A=a,B=b].
ctcex.pl:30: In assertions of [ctcex:b/2]:
	[ctcex:is_3/1] are not properties
ctcex.pl:32: In assertions of [ctcex:b/2]:
	[ctcex:is_2/1] are not properties
ctcex.pl:36: In assertions of [ctcex:b/2]:
ctcex.pl:26: Assertion failure for is_num(A,B).
	In *compat*, unsatisfied properties: 
		[int(B)].
	Because: 
		[B=a].
ctcex.pl:26: Assertion failure for is_num(A,B).
	In *compat*, unsatisfied properties: 
		[int(B)].
	Because: 
		[B=b].
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
    replace_noisy_strings(SD, Result, AResult),
    ( Pattern \== AResult
    ->format("~s", [AResult])
    ; true
    ),
    assertion(Pattern == AResult),
    set_prolog_flag(verbose, normal),
    %set_prolog_flag(check_assertions, []).
    retractall(user:error_on_co).

replace_noisy_strings(SD) -->
        replace_substrings(SD, ""),
	replace_substrings("ERROR: ", ""),
	replace_substrings("ctcex.pl:12:8:", "ctcex.pl:12:"),
	replace_substrings("ctcex.pl:18:4:", "ctcex.pl:18:"),
	replace_substrings("ctcex.pl:26:8:", "ctcex.pl:26:"),
	replace_substrings("ctcex.pl:30:8:", "ctcex.pl:30:"),
	replace_substrings("ctcex.pl:36:8:", "ctcex.pl:36:"),
	replace_substrings("ctcex.pl:32:8:", "ctcex.pl:32:").

replace_substrings(SubS, Repl, String, Result) :-
    ( sub_string(String, Before, _, After, SubS)
    ->sub_string(String, 0, Before, _, BeforeS),
      sub_string(String, _, After, 0, AfterS),
      replace_substrings(SubS, Repl, AfterS, Tail),
      string_concat(BeforeS, Repl, ResultHead),
      string_concat(ResultHead, Tail, Result)
    ; Result = String
    ).

:- comment_data:disable.

:- end_tests(ctchecks).
