:- begin_tests(ctchecks).

:- multifile
    user:message_property/2.

user:message_property(_, stream(current_output)).

:- use_module(library(swi/ctchecks)).

:- use_module(comment_data).

:- comment_data:enable.

/* $ctcex$
ERROR: In assertions for [ctcex:a/2]
ERROR: /home/edison/prosyn_0/src/tools/rtchecks/tests/ctcex.pl:11: Compile-Time failure in assertion for ctcex:a(a,b).
ERROR: 	In *compat*, unsatisfied properties: 
ERROR: 		[int(a),list(b)].
ERROR: /home/edison/prosyn_0/src/tools/rtchecks/tests/ctcex.pl:14:0: Failed in ctcex:a(a,b).
ERROR: In assertions for [ctcex:a/2]
ERROR: /home/edison/prosyn_0/src/tools/rtchecks/tests/ctcex.pl:11: Compile-Time failure in assertion for ctcex:a(1,b).
ERROR: 	In *compat*, unsatisfied properties: 
ERROR: 		[list(b)].
ERROR: /home/edison/prosyn_0/src/tools/rtchecks/tests/ctcex.pl:17:4: Failed in ctcex:a(1,b).
*/

test(ctcex) :-
    set_prolog_flag(check_assertions, [defined, is_prop, ctcheck]),
    set_prolog_flag(verbose, silent),
    with_output_to(string(Result), [ctcex]),
    comment_data(ctcex, Pattern),
    assertion(Pattern == Result),
    set_prolog_flag(verbose, normal),
    set_prolog_flag(check_assertions, []).

:- comment_data:disable.

:- end_tests(ctchecks).
