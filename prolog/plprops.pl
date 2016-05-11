:- module(plprops, [det/1, semidet/1, nondet/1, multi/1, type/1, tlist/2,
		    char/1, keypair/1, keylist/1, arithexpression/1,
		    dupclauses/1]).

:- use_module(library(assertions)).
:- use_module(library(basicprops)).
:- use_module(library(nativeprops)).
:- use_module(library(send_check)).

% SWI-Like Properties:

:- true prop type/1.
:- meta_predicate type(0).
type(Goal) :- call(Goal).

:- true prop keypair/1 + type.
keypair(_-_).

:- true prop keylist/1 + type.
keylist(KL) :- list(KL, keypair).

:- true prop tlist/2 + type # "@var{L} is a list or a value of @var{T}s".
:- meta_predicate tlist(?, 1).
tlist(L, T) :- list(L, T).
tlist(E, T) :- call(T, E).

:- true prop char/1 is type.
char(A) :- atm(A). % size(A)=1

:- prop det(X) + equiv(not_fails(is_det(X))).
:- meta_predicate det(0).
% det(Goal) :- not_fails(is_det(Goal)).
det(Goal) :-
	Solved = solved(no),
	( true
	; arg(1, Solved, no) ->
	  send_comp_rtcheck(Goal, det, fails),
	  fail
	),
	prolog_current_choice(C0),
	Goal,
	prolog_current_choice(C1),
	( arg(1, Solved, no)
	-> true
	; send_comp_rtcheck(Goal, det, non_det)
	%% more than one solution!
	),
	( C0 == C1 -> !
	; nb_setarg(1, Solved, yes)
	).

:- prop semidet(X) + equiv(is_det(X)).

:- meta_predicate semidet(0).
semidet(Goal) :- is_det(Goal).

:- prop nondet/1.

:- meta_predicate nondet(0).
nondet(Goal) :- Goal.

:- prop multi(X) + equiv(not_fails(X)).

:- meta_predicate multi(0).
% multi(Goal) :- not_fails(Goal).
multi(Goal) :-
	Solved = solved(no),
	( true
	; arg(1, Solved, no) ->
	  send_comp_rtcheck(Goal, multi, fails),
	  fail
	),
	prolog_current_choice(C0),
	test_throw_2(Goal, multi, _, true),
	prolog_current_choice(C1),
	( C0 == C1 -> !
	; nb_setarg(1, Solved, yes)
	).

:- meta_predicate dupclauses(0).
:- prop dupclauses/1 is (type, eval)
    # "States that a predicate have repeated clauses".
dupclauses(M:Goal) :-
    ( functor(Goal, F, A),
      functor(Head1, F, A),
      functor(Head2, F, A),
      clause(M:Head1, Body1, Ref1),
      clause(M:Head2, Body2, Ref2),
      Ref1 \= Ref2,
      (M:Head1 :- Body1) =@= (M:Head2 :- Body2)
    ->true
    ; send_comp_rtcheck(Goal, dupclauses, not(dupclauses))
    ),
    call(Goal).

:- prop arithexpression/1 is type
    # "Represents an arithmetic expression, i.e., a term that could be
    an argument for an arithmetic predicate.".

arithexpression(X) :- number(X), !. % Optimization
arithexpression(X) :- num(X).
arithexpression(X) :-
    current_arithmetic_function(X),
    X =.. [_|Args],
    maplist(arithexpression, Args).
