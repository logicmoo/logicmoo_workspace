:- begin_tests(ctchecks).

:- multifile
    user:message_property/2.

:- dynamic
    user:error_on_co/0.

user:message_property(_, location_prefix(_, '', 'ERROR: ')).
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
ERROR: ctcex.pl:18:4: In the body of ctcex:q/0:
ERROR: ctcex.pl:12: Compile-Time failure in assertion for a(1,b).
ERROR: 	In *compat*, unsatisfied properties: 
ERROR: 		[list(b)].
ERROR: ctcex.pl:15: In the head of ctcex:a/2:
ERROR: ctcex.pl:12: Compile-Time failure in assertion for a(a,b).
ERROR: 	In *compat*, unsatisfied properties: 
ERROR: 		[int(a),list(b)].
ERROR: ctcex.pl:30:8: In assertions of [ctcex:b/2]:
ERROR: 	[ctcex:is_3/1] are not properties
ERROR: ctcex.pl:32:8: In assertions of [ctcex:b/2]:
ERROR: 	[ctcex:is_2/1] are not properties
ERROR: ctcex.pl:36:8: In assertions of [ctcex:b/2]:
ERROR: ctcex.pl:26: Compile-Time failure in assertion for is_num(_1,a).
ERROR: 	In *compat*, unsatisfied properties: 
ERROR: 		[int(a)].
ERROR: ctcex.pl:26: Compile-Time failure in assertion for is_num(_1,b).
ERROR: 	In *compat*, unsatisfied properties: 
ERROR: 		[int(b)].
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
    remove_substrings(Result, SD, AResult),
    assertion(Pattern == AResult),
    set_prolog_flag(verbose, normal),
    %set_prolog_flag(check_assertions, []).
    retractall(user:error_on_co).

remove_substrings(String, SubS, Result) :-
    ( sub_string(String, Before, _, After, SubS)
    ->sub_string(String, 0, Before, _, BeforeS),
      sub_string(String, _, After, 0, AfterS),
      remove_substrings(AfterS, SubS, Tail),
      string_concat(BeforeS, Tail, Result)
    ; Result = String
    ).

:- comment_data:disable.

:- end_tests(ctchecks).
