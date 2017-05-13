:- module(rtc_external, [test_ex/0]).

:- use_module(library(assertions)).
:- use_module(library(basicprops)).
:- use_module(library(nativeprops)).

:- doc(author, "Edison Mera").

:- doc(module, "Example of runtime checking of an assertion about
        an imported (or even builtin) predicate.").

% :- pred functor(F, A, N) : nonvar(F) => (atom(A), nnegint(N)).

:- pred functor/3 : (nonvar * var * var) => (nonvar * atom * nnegint) + fails.

:- true prop patata/1 + no_signal.
:- meta_predicate patata(0).
patata(G) :- call(G).

:- pred display/1 + patata.

test_ex :-
        functor(0, A, N),
        display(A/N),
        nl.
