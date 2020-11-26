reinitialise/0.

introduce(X) :- compile(X).
% introduce(X) :- consult(X).
:- use_module(library(system)).

:- introduce('code.pl').
:- introduce('ob_utils.pl').
:- introduce('test_domain.pl').

% introduce compile files
:- introduce('tools.pl').
:- introduce('assert_sort.pl').
:- introduce('consyn.pl').
:- introduce('op_syn_check.pl').
:- introduce('op_post_cond_check.pl').

compile:-
      assert_sort,
      start_syn,
      start_check_op,
      start_post_check.
