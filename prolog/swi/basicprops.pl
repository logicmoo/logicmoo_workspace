:- module(basicprops, [list/2,
		       nlist/2,
		       sequence/2,
		       compat/2,
		       inst/2
		       ]).

%% A wrapper for the Ciao basic_props library

:- expects_dialect(ciao).
:- use_module(library(implementation_module)).
:- use_module(library(swi/assertions)).
:- use_module(library(engine/term_typing), [type/2]).
:- reexport(engine(basic_props),
	    except([list/2,
		    nlist/2,
		    sequence/2,
		    compat/2,
		    inst/2])).

% Note that the following redefinitions can not replace those in basic_props.pl,
% otherwise the ciao emulation in SWI-Prolog will not longer work --EMM

:- true prop list/2 + regtype.
:- meta_predicate list(?, :).

list([],_).
list([X|Xs], T) :-
        type(X, T),
        list(Xs, T).

:- true prop nlist/2 + regtype.
:- meta_predicate nlist(?, :).

nlist([], _).
nlist([X|Xs], T) :-
        nlist(X, T),
        nlist(Xs, T).
nlist(X, T) :-
	type(X, T).

:- true prop sequence/2 + regtype.
:- meta_predicate sequence(?, :).

sequence(E, T) :- type(E, T).
sequence((E,S), T) :-
        type(E, T),
        sequence(S,T).

:- true prop compat/2.
:- meta_predicate compat(?, :).

compat(T, P) :- \+ \+ type(P, T).

:- true prop inst/2.
:- meta_predicate inst(?, :).

inst(X, P) :-
	A = type(X, P),
	copy_term(A, AC),
	AC,
	subsumes_term(A, AC).

:- use_module(library(unfold_calls)).

unfoldable(list(_, _),     basicprops).
unfoldable(nlist(_, _),    basicprops).
unfoldable(sequence(_, _), basicprops).
unfoldable(compat(_, _),   basicprops).
unfoldable(inst(_, _),     basicprops).

prolog:called_by(Goal, basicprops, CM, CL) :-
    nonvar(Goal),
    implementation_module(CM:Goal, M),
    unfoldable(Goal, M),
    unfold_calls(Goal, CM, unfoldable, CL).
