:- module(termtyping,
	  [type/2, add_1st_arg/3]).

:- use_module(library(assertions)).
:- use_module(library(basicprops)).
:- use_module(library(nativeprops)).

:- doc(title, "Extra-logical properties for typing").

:- doc(author, "Daniel Cabeza").
:- doc(author, "Manuel Hermenegildo").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module, "This library contains traditional Prolog predicates
        for testing types.  They depend on the state of instantiation of
        their arguments, thus being of extra-logical nature.").

:- true prop ground(X) + native
# "~w is currently ground (it contains no variables)."-[X].
:- trust success ground(X) => gnd(X).
:- trust comp ground/1 + (is_det, test_type(meta)).
:- true comp ground(@) + (sideff(free), native).
:- true comp ground(X) : ground(X) + eval.
:- true comp ground(X) : var(X) + equiv(fail).
:- true comp ground(X) : ground(X) + equiv(true).

% Compiled inline -- these are hooks for the interpreter.

:- true prop atom(X) + native
# "~w is currently instantiated to an atom."-[X].
:- trust success atom(X) => atm(X).
:- trust comp atom/1 + (is_det, test_type(arithmetic)).
:- true comp atom(@) + (sideff(free), native).
:- true comp atom(X) : nonvar(X) + eval.
:- true comp atom(T) : var(T) + equiv(fail).

:- true prop atomic(X) + native
# "~w is currently instantiated to an atom, a number.
In SWI-Prolog also to a string or the empty list."-[X].
:- trust success atomic(T) => constant(T).
:- trust comp atomic/1 + (is_det, test_type(meta)).
:- true comp atomic(@) + (sideff(free), native).
:- true comp atomic(X) : nonvar(X) + eval.
:- true comp atomic(T) : var(T) + equiv(fail).

:- true prop float(X) + native
# "~w is currently instantiated to a float."-[X].
:- trust success float(X) => flt(X).
:- trust comp float/1 + (is_det, test_type(meta)).
:- true comp float(@) + (sideff(free), native).
:- true comp float(X) : nonvar(X) + eval.
:- true comp float(T) : var(T) + equiv(fail).

:- true prop integer(X) + native
# "~w is currently instantiated to an integer."-[X].
:- trust success integer(X) => int(X).
:- trust comp integer/1 + (is_det, test_type(arithmetic)).
:- true comp integer(@) + (sideff(free), native).
:- true comp integer(X) : nonvar(X) + eval.
:- true comp integer(T) : var(T) + equiv(fail).

:- true prop nonvar(X) + native(not_free(X))
# "~w is currently a term which is not a free variable."-[X].
:- trust comp nonvar/1 + is_det.
:- true comp nonvar(@) + (sideff(free), native).
:- true comp nonvar(X) : nonvar(X) + eval.
:- true comp nonvar(T) : var(T) + equiv(fail).
:- true comp nonvar(T) : nonvar(T) + equiv(true).

:- true prop number(X) + native
# "~w is currently instantiated to a number."-[X].
:- trust success number(X) => num(X).
:- trust comp number/1 + (is_det, test_type(arithmetic)).
:- true comp number(@) + (sideff(free), native).
:- true comp number(X) : nonvar(X) + eval.
:- true comp number(T) : var(T) + equiv(fail).

:- true prop var(X) + native(free(X))
# "~w is a free variable."-[X].
:- trust comp var/1 + (is_det, test_type(meta)).
:- true comp var(@) + (native, sideff(free)).
:- true comp var(X) : nonvar(X) + eval.
:- true comp var(X) : nonvar(X) + equiv(fail).
:- true comp var(X) : var(X) + equiv(true).

:- true prop type(X, Y) + native
# "~w is internally of type ~w (@tt{var}, @tt{attv}, @tt{float},
      @tt{integer}, @tt{structure}, @tt{atom} or @tt{list})."-[X, Y].
:- trust comp type/2 + is_det.
:- true comp type/2 + (sideff(free), native).
:- true comp type(X, _) : nonvar(X) + eval.
:- meta_predicate type(?, :).

type(A, T) :-
    add_1st_arg(T, A, P),
    call(P).

add_1st_arg(M:T, A, M:P) :- !,
    add_1st_arg(T, A, P).
add_1st_arg(T, A, P) :-
    T =.. [F|Args],
    P =.. [F, A|Args].

% Help analyzers to identify this call:
prolog:called_by(type(A, MT), termtyping, CM, [M:P]) :-
    nonvar(A),
    nonvar(MT),
    strip_module(CM:MT, M, T),
    nonvar(T),
    add_1st_arg(T, A, P).

:- multifile
    unfold_calls:unfold_call_hook/4.

unfold_calls:unfold_call_hook(type(A, MT), termtyping, CM, M:P) :-
    strip_module(CM:MT, M, T),
    nonvar(T),
    add_1st_arg(T, A, P).
