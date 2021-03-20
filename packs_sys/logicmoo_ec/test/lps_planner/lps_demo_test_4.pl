
:- expects_dialect(lps).

maxTime(10).

fluents on(_,_), ontable(_), clear(_), handempty, holding(_).

fluents enabled_action_pick_up(X).

actions pick_up(X).

enabled_action_pick_up(X) at T if clear(X) at T, ontable(X) at T, handempty at T.
pick_up(X) initiates holding(X) if enabled_action_pick_up(X).
pick_up(X) terminates ontable(X) if enabled_action_pick_up(X).
pick_up(X) terminates clear(X) if enabled_action_pick_up(X).
pick_up(X) terminates handempty if enabled_action_pick_up(X).

fluents enabled_action_put_down(X).
actions put_down(X).
enabled_action_put_down(X) at T if holding(X) at T.
put_down(X) initiates clear(X) if enabled_action_put_down(X).
put_down(X) initiates handempty if enabled_action_put_down(X).
put_down(X) initiates ontable(X) if enabled_action_put_down(X).
put_down(X) terminates holding(X) if enabled_action_put_down(X).

fluents enabled_action_stack(X, Y).
actions stack(X, Y).
enabled_action_stack(X, Y) at T if holding(X) at T, clear(Y) at T.
stack(X, Y) initiates clear(X) if enabled_action_stack(X, Y).
stack(X, Y) initiates handempty if enabled_action_stack(X, Y).
stack(X, Y) initiates on(X, Y) if enabled_action_stack(X, Y).
stack(X, Y) terminates holding(X) if enabled_action_stack(X, Y).
stack(X, Y) terminates clear(Y) if enabled_action_stack(X, Y).

fluents enabled_action_unstack(X, Y).
actions unstack(X, Y).
enabled_action_unstack(X, Y) at T if on(X, Y) at T, clear(X) at T, handempty at T.
unstack(X, Y) initiates holding(X) if enabled_action_unstack(X, Y).
unstack(X, Y) initiates clear(Y) if enabled_action_unstack(X, Y).
unstack(X, Y) terminates clear(X) if enabled_action_unstack(X, Y).
unstack(X, Y) terminates handempty if enabled_action_unstack(X, Y).
unstack(X, Y) terminates on(X, Y) if enabled_action_unstack(X, Y).


initially clear(c), clear(a), clear(b), ontable(c),  ontable(a),  ontable(b), handempty.

% observe on(c, b), on(c, b)  from 8 to 9.
% observe on(b, a)  from 8 to 9.
% observe on(c, b)  from 8 to 9.
if true at 6 then on(b, a) at 6.
observe unstack(c,b) from 7.

:- multifile(ec:demo_test/3).
ec:demo_test(lps_demo_tests_4_run, lps_demo, [holds(on(b, a), T), holds(on(c, b),T)]).

baseKB:lps_demo_test_4_run:- abdemo([holds(on(b, a), T), holds(on(c, b),T)]).
baseKB:lps_demo_test_4_run_tweak:- abdemo([on(b, a),on(c, b)]).

/** <examples>
?- go(Timeline).
*/


