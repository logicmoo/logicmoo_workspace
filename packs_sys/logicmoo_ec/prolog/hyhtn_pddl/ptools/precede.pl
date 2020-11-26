:- op(100,xfy,'=>').
introduce(X) :- compile(X).

reinitialise/0.

:- introduce('code.pl').
:- introduce('ob_utils.pl').

:- introduce('fmacgr').
:- introduce('olprecede').
:- introduce('onprecede').

