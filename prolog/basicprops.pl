/*  Part of SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

%% Migrated from Ciao to SWI-Prolog

:- module(basic_props,
        [term/1, int/1, nnegint/1, flt/1, num/1, atm/1, struct/1,
	 gnd/1, gndstr/1, constant/1, % callable/1,
	 operator_specifier/1, list/1, list/2, nlist/2, % member/2,
	 sequence/2, sequence_or_list/2, character_code/1, % string/1,
	 num_code/1, predname/1, atm_or_atm_list/1, compat/2, inst/2,
	 iso/1, deprecated/1, not_further_inst/2, sideff/2, regtype/1,
	 native/1, native/2, rtcheck/1, rtcheck/2, no_rtcheck/1, eval/1,
	 equiv/2, bind_ins/1, error_free/1,memo/1,filter/2, flag_values/1,
	 pe_type/1]).

:- use_module(library(assertions)).
:- use_module(assertions(nativeprops)).
:- use_module(assertions(termtyping)).

:- doc(title,"Basic data types and properties").

:- doc(author,"Daniel Cabeza").
:- doc(author,"Manuel Hermenegildo").

:- doc(usage, "These predicates are builtin in Ciao, so nothing special
   has to be done to use them.").

:- doc(module,"@cindex{properties, basic} This library contains
   the set of basic properties used by the builtin predicates, and
   which constitute the basic data types and properties of the
   language.  They can be used both as type testing builtins within
   programs (by calling them explicitly) and as properties in
   assertions.").

:- doc(term/1, "The most general type (includes all possible terms).").

:- true prop term(X) + (regtype, native) # "~w is any term."-[X].
:- true comp term/1 + sideff(free).
:- true comp term/1 + eval.
:- true comp term/1 + equiv(true).
:- trust success term/1 => true.


term(_).

:- doc(int/1, "The type of integers. The range of integers is
        @tt{[-2^2147483616, 2^2147483616)}.  Thus for all practical
        purposes, the range of integers can be considered infinite.").

:- true prop int(T) + (regtype, native) # "~w is an integer."-[T].
:- true comp int/1 + sideff(free).
:- true comp int(T) : nonvar(T) + (eval, is_det).
:- trust success int(T) => int(T).
:- trust comp int/1 + test_type(arithmetic).


int(X) :-
        nonvar(X), !,
        integer(X).
int(0).
int(N) :- posint(I), give_sign(I, N).

posint(1).
posint(N) :- posint(N1), N is N1+1.

give_sign(P, P).
give_sign(P, N) :- N is -P.

:- doc(nnegint/1, "The type of non-negative integers, i.e.,
	natural numbers.").

:- true prop nnegint(T) + ( regtype, native )
	# "~w is a non-negative integer."-[T].
:- true comp nnegint/1 + sideff(free).
:- true comp nnegint(T) : nonvar(T) + eval.
:- trust success nnegint(T) => nnegint(T).
:- trust comp nnegint/1 + test_type(arithmetic).

nnegint(X) :-
        nonvar(X), !,
        integer(X),
	X >= 0.
nnegint(0).
nnegint(N) :- posint(N).


:- doc(flt/1, "The type of floating-point numbers. The range of
        floats is the one provided by the C @tt{double} type, typically
        @tt{[4.9e-324, 1.8e+308]} (plus or minus).  There are also three
        special values: Infinity, either positive or negative,
        represented as @tt{1.0e1000} and @tt{-1.0e1000}; and
        Not-a-number, which arises as the result of indeterminate
        operations, represented as @tt{0.Nan}").

:- true prop flt(T) + (regtype, native) # "~w is a float."-[T].
:- true comp flt/1 + sideff(free).
:- true comp flt(T) : nonvar(T) + (eval, is_det).
:- trust success flt(T) => flt(T).
:- trust comp flt/1 + test_type(meta).

flt(T) :- nonvar(T), !, float(T).
flt(T) :- int(N), T is N/10.

:- doc(num/1, "The type of numbers, that is, integer or floating-point.").

:- true prop num(T) + (regtype, native) # "~w is a number."-[T].
:- true comp num/1 + (sideff(free),bind_ins).
:- true comp num(T) : nonvar(T) + (eval, is_det).
:- trust success num(T) => num(T).
:- trust comp num/1 + test_type(arithmetic).

num(T) :- number(T), !.
num(T) :- int(T).
% num(T) :- flt(T). % never reached!

:- doc(atm/1, "The type of atoms, or non-numeric constants.  The
        size of atoms is unbound.").

:- true prop atm(T) + (regtype, native) # "~w is an atom."-[T].
:- true comp atm/1 + sideff(free).
:- true comp atm(T) : nonvar(T) + (eval, is_det).
:- trust success atm(T) => atm(T).
:- trust comp atm/1 + test_type(arithmetic).

% Should be current_atom/1
atm(T) :- atom(T), !.
atm(a).

:- doc(struct/1, "The type of compound terms, or terms with
non-zeroary functors. By now there is a limit of 255 arguments.").

:- true prop struct(T) + (regtype, native) # "~w is a compound term."-[T].
:- true comp struct/1 + sideff(free).
:- true comp struct(T) : nonvar(T) + eval.
:- trust success struct(T) => struct(T).

struct([_|_]):- !.
struct(T) :- functor(T, _, A), A>0. % compound(T).

:- doc(gnd/1, "The type of all terms without variables.").

:- true prop gnd(T) + (regtype, native) # "~w is ground."-[T].
:- true comp gnd/1 + sideff(free).
:- true comp gnd(T) : ground(T) + (eval, is_det).
:- trust success gnd(T) => gnd(T).
:- trust comp gnd/1 + test_type(meta).

gnd([]) :- !.
gnd(T) :- functor(T, _, A), grnd_args(A, T).

:- true prop gndstr(T) + (regtype, native) # "~w is a ground compound term."-[T].
:- true comp gndstr/1 + sideff(free).
:- true comp gndstr(T) : ground(T) + (eval, is_det).
:- trust success gndstr(T) => gndstr(T).

gndstr(A) :- gnd(A), struct(A).

grnd_args(0, _).
grnd_args(N, T) :-
        arg(N, T, A),
        gnd(A),
        N1 is N-1,
        grnd_args(N1, T).

:- true prop constant(T) + regtype
   # "~w is an atomic term (an atom or a number)."-[T].
:- true comp constant/1 + sideff(free).
:- true comp constant(T) : nonvar(T) + (eval, is_det).
:- trust success constant(T) => constant(T).

constant(T) :- atm(T).
constant(T) :- num(T).

:- true prop callable(T) + regtype
   # "~w is a term which represents a goal, i.e.,
        an atom or a structure."-[T].
:- true comp callable/1 + sideff(free).
:- true comp callable(T) : nonvar(T) + (eval, is_det).
:- trust success callable(T) => nonvar(T).

% callable(T) :- atm(T).
% callable(T) :- struct(T).

:- doc(operator_specifier/1, "The type and associativity of an
operator is described by the following mnemonic atoms:

@begin{description}

@item{@tt{xfx}} Infix, non-associative: it is a requirement that both of
the two subexpressions which are the arguments of the operator must be
of @em{lower} precedence than the operator itself.

@item{@tt{xfy}} Infix, right-associative: only the first (left-hand)
subexpression must be of lower precedence; the right-hand subexpression
can be of the @em{same} precedence as the main operator.

@item{@tt{yfx}} Infix, left-associative: same as above, but the other
way around.

@item{@tt{fx}} Prefix, non-associative: the subexpression must be of
@em{lower} precedence than the operator.

@item{@tt{fy}} Prefix, associative: the subexpression can be of the
@em{same} precedence as the operator.

@item{@tt{xf}} Postfix, non-associative: the subexpression must be of
@em{lower} precedence than the operator.

@item{@tt{yf}} Postfix, associative: the subexpression can be of the
@em{same} precedence as the operator.

@end{description}
").

:- true prop operator_specifier(X) + regtype # "~w specifies the type and
        associativity of an operator."-[X].
:- true comp operator_specifier/1 + sideff(free).
:- true comp operator_specifier(X) : nonvar(X) + (eval, is_det, relations(7)).
:- trust success operator_specifier(T) => operator_specifier(T).

operator_specifier(fy).
operator_specifier(fx).
operator_specifier(yfx).
operator_specifier(xfy).
operator_specifier(xfx).
operator_specifier(yf).
operator_specifier(xf).

:- doc(list/1, "A list is formed with successive applications of the
   functor @tt{'.'/2}, and its end is the atom @tt{[]}.  Defined as
   @includedef{list/1}").

:- true prop list(L) + regtype # "~w is a list."-[L].
:- true comp list/1 + sideff(free).
:- true comp list(L) : ground(L) + (eval, is_det).
:- trust success list(T) => list(T).

list([]).
list([_|L]) :- list(L).

:- doc(list(L,T), "~w is a list, and for all its elements,
   ~w holds."-[L, T]).

:- true prop list(L,T) + regtype # "~w is a list of ~ws."-[L, T].
:- true comp list/2 + sideff(free).
:- meta_predicate list(?, 1).
:- true comp list(L,T) : (ground(L),ground(T)) + eval.
:- trust success list(X,_) => list(X). % should be list(X,T), but does not work

list([],_).
list([X|Xs], T) :-
        type(X, T),
        list(Xs, T).

:- true prop nlist(L,T) + regtype #
	"~w is ~w or a nested list of ~ws.  Note that
	if ~w is term, this type is equivalent to term, this
	fact explain why we do not have a @pred{nlist/1} type"-[L, T, T, T].
:- true comp nlist/2 + sideff(free).
:- meta_predicate nlist(?, 1).
:- true comp nlist(L,T) : (ground(L),ground(T)) + eval.
:- trust success nlist(X,_) => term(X).

nlist([], _).
nlist([X|Xs], T) :-
        nlist(X, T),
        nlist(Xs, T).
nlist(X, T) :-
	type(X, T).

:- true prop member(X,L) # "~w is an element of ~w."-[X, L].
:- true comp member/2 + (sideff(free), bind_ins).
:- true comp member(_,L) : list(L) + eval.
:- trust success member(_,L) => list(L).
:- trust success member(X,L) : ground(L) => ground(X).

% member(X, [X|_]).
% member(X, [_Y|Xs]):- member(X, Xs).

:- doc(sequence/2, "A sequence is formed with zero, one or more
   occurrences of the operator @op{','/2}.  For example, @tt{a, b, c} is
   a sequence of three atoms, @tt{a} is a sequence of one atom.").

:- true prop sequence(S,T) + regtype # "~w is a sequence of ~ws."-[S, T].
:- true comp sequence/2 + sideff(free).

:- meta_predicate sequence(?, :).
:- true comp sequence(S, :) : ground(S) + eval.
:- trust success sequence(E, :) => (nonvar(E)).

sequence(E, T) :- type(E, T).
sequence((E,S), T) :-
        type(E, T),
        sequence(S,T).

:- true prop sequence_or_list(S,T) + regtype
   # "~w is a sequence or list of ~ws."-[S, T].
:- true comp sequence_or_list/2 + sideff(free).
:- meta_predicate sequence_or_list(?, 1).
:- true comp sequence_or_list(S,T) : (ground(S),ground(T)) + eval.
:- trust success sequence_or_list(E,T) => (nonvar(E),ground(T)).

sequence_or_list(E, T) :- list(E,T).
sequence_or_list(E, T) :- sequence(E, T).

:- true prop character_code(T) + regtype
   # "~w is an integer which is a character code."-[T].
:- true comp character_code/1 + sideff(free).
:- true comp character_code(T) : nonvar(T) + eval.
:- trust success character_code(I) => character_code(I).

character_code(I) :- int(I).

% :- doc(string/1, "A string is a list of character codes.  The usual
%         syntax for strings @tt{\"string\"} is allowed, which is
%         equivalent to @tt{[0's,0't,0'r,0'i,0'n,0'g]} or
%         @tt{[115,116,114,105,110,103]}.  There is also a special Ciao
%         syntax when the list is not complete: @tt{\"st\"||R} is
%         equivalent to @tt{[0's,0't|R]}.").

% :- true prop string(T) + regtype
%    # "@var{T} is a string (a list of character codes).".
% :- true comp string(T) + sideff(free).
% :- true comp string(T) : ground(T) + eval.
% :- trust success string(T) => string(T).

% string(T) :- list(T, character_code).

:- doc(num_code/1, "These are the ASCII codes which can appear in 
	decimal representation of floating point and integer numbers, 
	including scientific notation and fractionary part.").

:-  true prop num_code/1 + regtype.

num_code(0'0).
num_code(0'1).
num_code(0'2).
num_code(0'3).
num_code(0'4).
num_code(0'5).
num_code(0'6).
num_code(0'7).
num_code(0'8).
num_code(0'9).
num_code(0'.).
num_code(0'e).
num_code(0'E).
num_code(0'+).
num_code(0'-).

/*
:- doc(predname(P),"@var{P} is a Name/Arity structure denoting
	a predicate name: @includedef{predname/1}").
:- true prop predname(P) + regtype
   # "@var{P} is a predicate name spec @tt{atm}/@tt{int}.".
*/
:- true prop predname(P) + regtype
   # "~w is a Name/Arity structure denoting
	a predicate name: @includedef{predname/1}"-[P].
:- true comp predname/1 + sideff(free).
:- true comp predname(P) : ground(P) + eval.
:- trust success predname(P) => predname(P).

predname(P/A) :-
	atm(P),
	nnegint(A).

:- true prop atm_or_atm_list(T) + regtype
   # "~w is an atom or a list of atoms."-[T].
:- true comp atm_or_atm_list/1 + sideff(free).
:- true comp atm_or_atm_list(T) : ground(T) + eval.
:- trust success atm_or_atm_list(T) => atm_or_atm_list(T).

atm_or_atm_list(T) :- atm(T).
atm_or_atm_list(T) :- list(T, atm).


:- doc(compat/2,"This property captures the notion of type or
   @concept{property compatibility}. The instantiation or constraint
   state of the term is compatible with the given property, in the
   sense that assuming that imposing that property on the term does
   not render the store inconsistent. For example, terms @tt{X} (i.e.,
   a free variable), @tt{[Y|Z]}, and @tt{[Y,Z]} are all compatible
   with the regular type @pred{list/1}, whereas the terms @tt{f(a)}
   and @tt{[1|2]} are not.").

:- true prop compat(Term,Prop)
   # "~w is @em{compatible} with ~w"-[Term, Prop].
:- meta_predicate compat(?, 1).
% not complety sure that assertiong below is completely correct,
% unless side effects analysis understand pred(1) (metacalls).
%:- true comp compat(Term,Prop) + sideff(free).
:- true comp compat(Term,Prop) : (ground(Term),ground(Prop)) + eval.

compat(T, P) :- \+ \+ type(T, P).

:- prop compat(Prop) + no_rtcheck
# "Uses ~w as a compatibility property."-[Prop].

:- meta_predicate compat(0).

compat(_:H) :-
    % This first clause allows usage of atom/atomic and other test predicates as
    % compatibility check
    compound(H),
    arg(1, H, A),
    var(A), !.
compat(Goal) :- \+ \+ Goal.

% No comment necessary: it is taken care of specially anyway in the
% automatic documenter. (PBC: I guess this comment refers to compat/2)

:- true prop inst(Term,Prop)
	# "~w is instantiated enough to satisfy ~w."-[Term, Prop].
:- true comp inst/2 + sideff(free).
:- true comp inst(Term,Prop) : (ground(Term),ground(Prop)) + eval.

:- meta_predicate inst(?,1).

inst(X, Prop) :-
	A = type(X, Prop),
	copy_term(A, AC),
	AC,
	subsumes_term(A, AC).

:- true prop iso/1 # "@em{Complies with the ISO-Prolog standard.}".
:- true comp iso/1 + sideff(free).

:- meta_predicate iso(0).
iso(Goal) :- call(Goal).

:- doc(deprecated/1, "Specifies that the predicate marked with
   this global property has been deprecated, i.e., its use is not
   recommended any more since it will be deleted at a future
   date. Typically this is done because its functionality has been
   superseded by another predicate.").

:- true prop deprecated/1 # "@bf{DEPRECATED.}".
:- true comp deprecated/1 + sideff(free).

:- meta_predicate deprecated(0).
deprecated(Goal) :- call(Goal).

:- true prop rtc_status/1 + regtype # "Status of the runtime-check
	implementation for a given property. Valid values are:
 @begin{itemize}

 @item unimplemented: No run-time checker has been implemented for the
                      property. Althought it can be implemented
                      further.

 @item incomplete: The current run-time checker is incomplete, which
                   means, under certain circunstances, no error is
                   reported if the property is violated.

 @item unknown: We do not know if current implementation of run-time
                checker is complete or not.

 @item complete: The opposite of incomplete, error is reported always
                 that the property is violated. Default.

 @item impossible: The property must not be run-time checked (for
		   theoretical or practical reasons).

 @end{itemize}
".

rtc_status(unimplemented).
rtc_status(incomplete).
rtc_status(unknown).
rtc_status(exhaustive).
rtc_status(impossible).

:- true prop rtcheck(G, Status) : callable * rtc_status
    # "The runtime check of ~w have the status ~w."-[G, Status].

:- true comp rtcheck/2 + sideff(free).

:- meta_predicate rtcheck(0, ?).
rtcheck(Goal, _) :- call(Goal).

:- true prop rtcheck(G) : callable
    # "Equivalent to rtcheck(~w, complete)."-[G].

:- true comp rtcheck/1 + sideff(free).

:- meta_predicate rtcheck(0).
rtcheck(Goal) :- rtcheck(Goal, complete).

:- true prop no_rtcheck(G) : callable
    # "Declares that the assertion in which this comp property appears must not
    be checked at run-time.  Equivalent to rtcheck(~w, impossible)."-[G].

:- true comp no_rtcheck/1 + sideff(free).

:- meta_predicate no_rtcheck(0).
no_rtcheck(Goal) :- rtcheck(Goal, impossible).

:- true prop not_further_inst(G,V)
        # "~w is not further instantiated by ~w."-[V, G].
:- true comp not_further_inst/2 + (sideff(free), no_rtcheck).

:- meta_predicate not_further_inst(0, ?).
not_further_inst(Goal, _) :- call(Goal).

:- true comp sideff/2 + (native, sideff(free), no_rtcheck).
:- true prop sideff(G,X) : (callable(G), member(X,[free,soft,hard]))
# "~w is side-effect ~w."-[G, X].
:- doc(sideff(G,X),"Declares that ~w is side-effect ~w, free
   (if its execution has no observable result other than its success,
   its failure, or its abortion), soft (if its execution may have other
   observable results which, however, do not affect subsequent execution,
   e.g., input/output), or hard (e.g., assert/retract)."-[G, X]).
:- meta_predicate sideff(0,?).

sideff(Goal, _) :- call(Goal).

% Built-in in CiaoPP
:- true prop regtype/1 # "Defines a regular type.".
:- true comp regtype/1 + sideff(free).
:- meta_predicate regtype(0).

regtype(Goal) :- call(Goal).

% Built-in in CiaoPP
:- true prop native(_, Key)
   # "This predicate is understood natively by CiaoPP as ~w."-[Key].
%%   # "Predicate @var{Pred} is understood natively by CiaoPP as @var{Key}.".
:- true comp native/2 + sideff(free).
:- meta_predicate native(0,?).

native(Goal, _) :- call(Goal).

% Built-in in CiaoPP
:- true prop native/1
   # "This predicate is understood natively by CiaoPP.".
%%   # "Predicate @var{Pred} is understood natively by CiaoPP.".
:- true comp native/1 + sideff(free).
:- meta_predicate native(0).

native(X) :- native(X, X).

:- true prop eval(Goal) # "~w is evaluable at compile-time."-[Goal].

:- meta_predicate eval(0).
eval(Goal) :- call(Goal).

:- true prop equiv(Goal1,Goal2)
	# "~w is equivalent to ~w."-[Goal1, Goal2].

:- meta_predicate equiv(0,0).

equiv(Goal, _) :- call(Goal).

:- true prop bind_ins(Goal) # "~w is binding insensitive."-[Goal].

:- meta_predicate bind_ins(0).
bind_ins(Goal) :- call(Goal).

:- true prop error_free(Goal) # "~w is error free."-[Goal].

:- meta_predicate error_free(0).
error_free(Goal) :- call(Goal).

:- true prop memo(Goal) # "~w should be memoized (not unfolded)."-[Goal].

:- meta_predicate memo(0).
memo(Goal) :- call(Goal).

:- true prop filter(_, Vars) # "~w should be filtered during
	global control)."-[Vars].

:- meta_predicate filter(0, ?).
filter(Goal, _) :- call(Goal).

:- true prop flag_values/1 + regtype # "Define the valid flag values".

flag_values(atom).
flag_values(integer).
flag_values(L):- list(L,atm).

:- true prop pe_type(Goal) # "~w will be filtered in partial
	evaluation time according to the PE types defined in the
	assertion."-[Goal].

:- meta_predicate pe_type(0).
pe_type(Goal) :- call(Goal).


:- use_module(library(implementation_module)).
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
