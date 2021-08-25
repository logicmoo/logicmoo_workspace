/******************************************************************************
    This file is being distributed, by written permission of Quintus 
    Corporation, for use with the BACK system only.  This file may not
    be used with Prolog implementations other than Quintus Prolog except
    (a) as part of the BACK system, or (b) with the written permission
    of Quintus Corporation.  To obtain such written permission, please
    contact:

	Quintus Corporation
	2100 Geng Road
	Palo Alto,
	California  94303
	USA
	415-813-3800
	marketing@quintus.com
******************************************************************************/

%   Package: types.pl
%   Author : Richard A. O'Keefe
%   Updated: 10/11/91
%   Purpose: More and better type tests.

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1989, Quintus Computer Systems, Inc.  All rights reserved.

/*  This file adds the type tests through the predicates:

	must_be(+Type, +Term, +ArgNo, +Goal)
	must_be(+Type, +ArgNo, +Goal)

    Checks whether the the Term in ArgNo of Goal belongs to the
    indicated Type.  If it isn't, there are two cases:
    The Term may not be instantiated enough to tell yet, in which
        case an instantiation_error will be raised
    The Term may be definitely not of the type, in which case a
	type_error is raised.

    The types supported are 
	atom, atomic, between(L,U), callable, chars, text,
	compound, constant, float, ground, integer, nonneg, nonvar
	number, oneof(L), proper_list, proper_list, symbol, var

    Type checks in addition to the built-in types:

	symbol(Term)		-- atom(Term)
	constant(Term)		-- atomic(Term)
	proper_list(Term)	-- list and already ends with nil
	chars(Term)		-- proper list of character codes

*/

:- module(types, [
    /* 	callable/1,			% now a builtin		*/
	chars/1,
    /*	complex/1,			% for the future	*/
    /*	compound/1,			% now a builtin		*/
	constant/1,
    /*	ground/1,			% now a builtin		*/
	proper_list/1,
    /*	rational/1,			% for the future	*/
    /*	simple/1,			% now a builtin		*/
    /*	string/1,			% for the future	*/
	symbol/1,
	term_type/2,
	must_be/3,
	must_be/4,
    /*			THE REST ARE OBSOLESCENT		*/
	should_be/3,
	should_be/4,

	must_be_atom/3,
	must_be_between/5,
	must_be_callable/3,
	must_be_chars/3,
	must_be_compound/3,
	must_be_constant/3,
	must_be_ground/3,
	must_be_integer/3,
	must_be_nonneg/3,
	must_be_nonvar/3,
	must_be_number/3,
	must_be_oneof/4,
	must_be_proper_list/3,
	must_be_symbol/3,
	must_be_var/3
   ]).

sccs_id('"@(#)91/10/11 types.pl  65.1"').


/* 
callable(Term) :-
	nonvar(Term),
	functor(Term, Symbol, _Arity),
	atom(Symbol).


compound(Term) :-
	nonvar(Term),
	functor(Term, _Symbol, Arity),
	Arity > 0.
*/

constant(Term) :-			% Probably the same as atomic(Term),
	nonvar(Term),			% but I'm playing safe here.
	functor(Term, _Symbol, 0).	% atomic was defined as integer|atom

/* 
ground(Term) :-
	nonvar(Term),
	functor(Term, _, Arity),
	ground(Arity, Term).

ground(N, Term) :-
    (	N =:= 0 -> true
    ;	arg(N, Term, Arg),
	nonvar(Arg),
	functor(Arg, _, Arity),
	ground(Arity, Arg),
	M is N-1,
	ground(M, Term)
    ).
*/

proper_list(*) :- !, fail.		% catches variables
proper_list([]).
proper_list([_|L]) :-
	proper_list(L).

/*
simple(Term) :-
    (	var(Term) -> true
    ;	functor(Term, Term, 0)
    ).
*/

symbol(Term) :-
	atom(Term).


%   term_type(+Term, ?Type)
%   is a type testing predicate designed for incorporation into coroutining
%   Prologs like MU Prolog and NU Prolog.  In general, the rule is that when
%   a predicate *would* delay in a coroutining system, it *must* report an
%   instantiation fault in a non-coroutining system like Quintus Prolog.
%   This predicate is no exception: the Term must be instantiated when you
%   call term_type/2 because knowing the Type doesn't give you enough
%   information to reconstruct the Term.  If you supply the Type, as in
%   term_type(X, constant), this is a perfectly good logical test.  It is
%   not as fast as atomic(X) would be, but it is "sound".  In order to
%   preserve the logical character of this predicate, if you leave Type
%   unbound, all the types which Term has will be enumerated.

term_type(Term, Type) :-
	(   var(Term)  -> instantiation_fault(1, term_type(Term,Type))
	;   atom(Type) -> term_type_(Type, Term)
	;   var(Type)  -> term_type_(Type, Term)
	;   should_be(symbol, Type, 2, term_type(Term,Type))
	).


term_type_(compound, Term) :- compound(Term).
term_type_(callable, Term) :- callable(Term).
term_type_(constant, Term) :- atomic(Term).
term_type_(number,   Term) :- number(Term).
term_type_(symbol,   Term) :- atom(Term).
%         (string,   Term) :- string(Term).
term_type_(integer,  Term) :- integer(Term).
%         (rational, Term) :- rational(Term).
term_type_(float,    Term) :- float(Term).
%         (complex,  Term) :- complex(Term).



%   chars(+Term)
%   is true when Term is a proper list of ISO 8859/1 or EBCDIC character codes.
%   Do not confuse it with is_chars/1 in library(print_chars).  Any time we
%   say that something is or should be a "chars", _this_ is what we mean.

chars(*) :- !, fail.
chars([]).
chars([Char|Chars]) :-
	integer(Char),
	Char >= 0, Char =< 255,
	chars(Chars).


%-----------------------------------------------------------------------+
%   The following predicates are retained for backwards compatibility.	|
%   No new predicates of this form should be written, and these ones	|
%   will be removed from the library by 3.5 at the latest.		|
%-----------------------------------------------------------------------+

must_be_atom(Arg, N, Goal) :-
	must_be(atom, Arg, N, Goal).

must_be_between(L, U, Arg, N, Goal) :-
	must_be(between(L,U), Arg, N, Goal).

must_be_callable(Arg, N, Goal) :-
	must_be(callable, Arg, N, Goal).

must_be_chars(Arg, N, Goal) :-
	must_be(chars, Arg, N, Goal).

must_be_compound(Arg, N, Goal) :-
	must_be(compound, Arg, N, Goal).

must_be_constant(Arg, N, Goal) :-
	must_be(constant, Arg, N, Goal).

must_be_ground(Arg, N, Goal) :-
	must_be(ground, Arg, N, Goal).

must_be_integer(Arg, N, Goal) :-
	must_be(integer, Arg, N, Goal).

must_be_nonneg(Arg, N, Goal) :-
	must_be(nonneg, Arg, N, Goal).

must_be_nonvar(Arg, N, Goal) :-
	must_be(nonvar, Arg, N, Goal).

must_be_number(Arg, N, Goal) :-
	must_be(number, Arg, N, Goal).

must_be_oneof(Arg, List, N, Goal) :-
	must_be(oneof(List), Arg, N, Goal).

must_be_proper_list(Arg, N, Goal) :-
	must_be(proper_list, Arg, N, Goal).

must_be_symbol(Arg, N, Goal) :-
	must_be(symbol, Arg, N, Goal).

must_be_var(Arg, N, Goal) :-
	must_be(var, Arg, N, Goal).



%   must_be(Type, Term, ArgNo, Goal)
%   checks whether the Term belongs to the indicated Type (you will often
%   call must_be/2 when you know that this test will fail, but you can
%   leave it to must_be/4 to check for you).  If it isn't, there are two
%   cases:  the Term may not be instantiated enough to tell yet, in which
%   case an INSTANTIATION ERROR will be reported, or the Term may be
%   definitely not of the type, in which case a TYPE ERROR is reported.
%   You should use this in commands with side effects, and should arrange
%   that if this predicate does not succeed the side effect(s) will not
%   take place.

must_be(Type, Term, ArgNo, Goal) :-
	type_check(Type, Term, ArgNo, Goal).


%   must_be(Type, ArgNo, Goal)
%   can be used when the Term complained of is the ArgNo'th argument of Goal.
%   The only point is to make the caller a little less cluttered.

must_be(Type, ArgNo, Goal) :-
	arg(ArgNo, Goal, Term),
	type_check(Type, Term, ArgNo, Goal).


%   should_be(Type, Term, ArgNo, Goal)
%   checks whether the Term belongs to the indicated Type (you will often
%   call should_be/4 when you know that this test will fail, but you can
%   leave it to should_be/2 to check for you). 
%
%   For simplicity, should_be/[3,4] are implemented the same as
%   must_be/[3,4] now.

should_be(Type, Term, ArgNo, Goal) :-
	type_check(Type, Term, ArgNo, Goal).


%   should_be(Type, ArgNo, Goal)
%   can be used when the Term complained of is the ArgNo'th argument of Goal.
%   The only point is to make the caller a little less cluttered.

should_be(Type, ArgNo, Goal) :-
	arg(ArgNo, Goal, Term),
	type_check(Type, Term, ArgNo, Goal).


%.  type_check(Type, Term, ArgNo, Goal)
%   is the common type testing code for must_be/4 and should_be/4.

type_check(atom, X, _, _) :- atom(X), !.
type_check(atomic, X, _, _) :- atomic(X), !.
type_check(between(L,U), X, _, _) :- integer(X), X >= L, X =< U, !.
type_check(callable, X, _, _) :- nonvar(X), functor(X, F, _), atom(F), !.
type_check(chars, X, N, Goal) :-
	diagnose_chars(X, Problem),
	!,
	( nonvar(Problem) -> true ; instantiation_fault(N, Goal) ).
type_check(text, X, N, Goal) :-
	(   atom(X), !
    %	;   string(X), !
	;   diagnose_chars(X, Problem), !,
	    ( nonvar(Problem) -> true ; instantiation_fault(N, Goal) )
	).
type_check(compound, X, _, _) :- nonvar(X), functor(X, _, N), N > 0, !.
type_check(constant, X, _, _) :- atomic(X), !.
type_check(float, X, _, _) :- float(X), !.
type_check(ground, X, _, _) :- ground(X), !.
type_check(ground, _, N, Goal) :- !,
	instantiation_fault(N, Goal).
type_check(integer, X, _, _) :- integer(X), !.
type_check(nonneg, X, _, _) :- integer(X), X >= 0, !.
type_check(nonvar, X, _, _) :- nonvar(X), !.
type_check(number, X, _, _) :- number(X), !.
type_check(oneof(L), X, _, _) :- atom(X), mb_oneof(L, X), !.
type_check(proper_list, X, _, _) :- proper_list(X), !.
type_check(proper_list, X, N, Goal) :- \+ \+ length(X, _), !,
	instantiation_fault(N, Goal).
type_check(symbol, X, _, _) :- atom(X), !.
type_check(var, X, _, _) :- var(X), !.
type_check(_, X, N, Goal) :- var(X), !,
	instantiation_fault(N, Goal).
type_check(Type, X, N, Goal) :-
	raise_exception(type_error(Goal, N, Type, X)).

instantiation_fault(N, Goal) :-
	raise_exception(instantiation_error(Goal, N)).

mb_oneof([X|_], X) :- !.	%  This is memberchk/2,
mb_oneof([_|T], X) :-		%  but types.pl doesn't use basics.pl otherwise
	mb_oneof(T, X).



%.  diagnose_chars(Chars, Problem)
%   -- succeeds with Problem=[] if Chars is a proper list of character codes
%   -- succeeds with var(Problem) if the problem is an instantiation fault
%   -- fails if the error is a type failure 

diagnose_chars([Char|Chars], Problem) :-
	integer(Char), Char >= 0, Char =< 255,
	!,
	diagnose_chars(Chars, Problem).
diagnose_chars(Var, Var) :- var(Var), !.
diagnose_chars([], []).
diagnose_chars([Var|_], Var) :- var(Var).


end_of_file.

	/*  Here we have some test cases.  */

mbt :-
	mbt(Test, Succ, Type, Term),
	on_exception(Error,
	  must_be(Type, Term, 4, mbt(Test,Succ,Type,Term)), true),
	( Succ = true -> nonvar(Error)
	; var(Error)
	),
	format(user_error, '~N** Test ~w failed.~n', [Test]),
	fail
    ;	true.

mbt( 1, true, atom, a).
mbt( 2, fail, atom, _).
mbt( 3, fail, atom, 0).
mbt( 4, fail, atom, "a").
mbt( 5, fail, atom, f(a)).

mbt( 6, true, atomic, a).
mbt( 7, true, atomic, 4.5).
mbt( 8, fail, atomic, _).
mbt( 9, fail, atomic, "x").
mbt(10, fail, atomic, f(a)).

mbt(11, true, between(1,10),  1).
mbt(12, true, between(1,10), 10).
mbt(13, fail, between(1,10), 5.0).
mbt(14, fail, between(1,10), _).
mbt(15, fail, between(1,10), '5').

mbt(16, true, callable, a).
mbt(17, true, callable, "x").
mbt(18, true, callable, f(a)).
mbt(19, fail, callable, _).
mbt(20, fail, callable, 9).

mbt(21, true, chars, "").
mbt(22, true, chars, "abc").
mbt(23, fail, chars, [a,b,c]).
mbt(24, fail, chars, [a|_]).
mbt(25, fail, chars, [-1]).

mbt(26, true, compound, "x").
mbt(27, true, compound, f(a)).
mbt(28, fail, compound, a).
mbt(29, fail, compound, 4.5).
mbt(30, fail, compound, _).

mbt(31, true, constant, a).
mbt(32, true, constant, 4.5).
mbt(33, fail, constant, _).
mbt(34, fail, constant, "x").
mbt(35, fail, constant, f(a)).

mbt(36, true, float, 4.5).
mbt(37, true, float, -0.0).
mbt(38, fail, float, 0).
mbt(39, fail, float, '1.2').
mbt(40, fail, float, _).

mbt(41, true, ground, "abc").
mbt(42, true, ground, and(twas(brillig),did(the(slithy,toves),
				and(gyre,gimble),in(the(wabe))))).
mbt(43, true, ground, 'X').
mbt(44, fail, ground, _).
mbt(45, fail, ground, _+1+2+3+4).

mbt(46, true, integer, 5).
mbt(47, true, integer, -0).
mbt(48, fail, integer, 0.0).
mbt(49, fail, integer, '1').
mbt(50, fail, integer, _).

mbt(51, true, nonneg, 5).
mbt(52, true, nonneg, -0).
mbt(53, fail, nonneg, -1).
mbt(54, fail, nonneg, 1.0).
mbt(55, fail, nonneg, _).

mbt(56, true, nonvar, a).
mbt(57, true, nonvar, 2).
mbt(58, true, nonvar, [_|_]).
mbt(59, true, nonvar, f(_,_,_)).
mbt(60, fail, nonvar, _).

mbt(61, true, number, 1).
mbt(62, true, number, -2).
mbt(63, true, number, -0.0).
mbt(64, fail, number, '1').
mbt(65, fail, number, _).

mbt(66, true, oneof([a,b,c]), a).
mbt(67, true, oneof([a,b,c]), c).
mbt(68, fail, oneof([a,b,c]), d).
mbt(69, fail, oneof([a,b,c]), 9).
mbt(70, fail, oneof([a,b,c]), _).

mbt(71, true, proper_list, []).
mbt(72, true, proper_list, [a,b]).
mbt(73, true, proper_list, [_,_,_]).
mbt(74, fail, proper_list, _).
mbt(75, fail, proper_list, [_|_]).

mbt(76, true, atom, a).
mbt(77, fail, atom, _).
mbt(78, fail, atom, 0).
mbt(79, fail, atom, "a").
mbt(80, fail, atom, f(a)).

mbt(81, true, var, _).
mbt(82, fail, var, a).
mbt(83, fail, var, f(_,_)).
