/* <module>
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(sanity_neg,[]).

:- expects_dialect(pfc).

:- process_this_script.

:- dynamic(fooBar/0).

~fooBar.

:- mpred_test(~fooBar).

fooBar.

\+ fooBar.

:- mpred_test(\+fooBar).

fooBar.

:- mpred_test(fooBar).

~fooBar.

:- mpred_test(\+fooBar).

:- mpred_nospy.


