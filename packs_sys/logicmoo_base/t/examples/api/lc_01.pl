/* <module>
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(sanity_neg,[]).

:- ensure_loaded(system:library(logicmoo_utils)).

b:-c.
c:-loop_check(a,true).
a:-b.
a:-loop_check(a,fail).


d:-loop_check(a,fail).
d:-dmsg(d).
:- listing(d).

?- d.
