/* <module>
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
*/

:- module(is_each_01,[]).

:- ensure_loaded(library(pfc)).

:- begin_pfc.

% :- fully_expand( (isEach([system(X),system(Y)]) :- related(X,Y)), _O).

% :- fully_expand( (isEach(aa(X),bb(Y)) :- related(X,Y)), _O).

==>(isEach(aa(X),bb(Y)) :- related(X,Y)).

:- listing([aa,bb]).


