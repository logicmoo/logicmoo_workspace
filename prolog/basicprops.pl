/*  Part of Assertion Reader for SWI-Prolog

    Author:        The Ciao Development Team, port and additions by Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.

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

:- module(basicprops,
          [inst/2, num_code/1, atm_or_atm_list/1, compat/2,
           not_further_inst/2, sideff/2, (regtype)/1, bind_ins/1,
           error_free/1, memo/1, filter/2, flag_values/1, pe_type/1]).

% callable/1, member/2, string/1,

:- use_module(library(lists)).
:- use_module(library(assertions)).
:- use_module(library(metaprops)).

:- license(gplv2).

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

% Built-in in CiaoPP
:- true prop (regtype)/1 + (global(prop), declaration) # "Defines a regular type.".
:- true comp (regtype)/1 + sideff(free).

regtype(Goal) :- call(Goal).

:- use_module(library(termtyping)).
:- use_module(library(nativeprops)).

:- regtype callable(T)
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

:- true prop member(X,L) # "~w is an element of ~w."-[X, L].
:- true comp member/2 + (sideff(free), bind_ins).
:- true comp member(_,L) : list(L) + eval.
:- trust success member(_,L) => list(L).
:- trust success member(X,L) : ground(L) => ground(X).

% member(X, [X|_]).
% member(X, [_Y|Xs]):- member(X, Xs).

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

:- true prop num_code/1 + regtype.

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

:- regtype atm_or_atm_list(T)
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

:- global not_further_inst(G,V)
        # "~w is not further instantiated by ~w."-[V, G].
:- true comp not_further_inst/2 + sideff(free).

not_further_inst(Goal, _) :- call(Goal).

:- global sideff(G,X) : (callable(G), member(X,[free,soft,hard]))
# "Declares that ~w is side-effect ~w, free
   (if its execution has no observable result other than its success,
   its failure, or its abortion), soft (if its execution may have other
   observable results which, however, do not affect subsequent execution,
   e.g., input/output), or hard (e.g., assert/retract)."-[G, X].

:- true comp (sideff)/2 + ((native), sideff(free)).

sideff(Goal, _) :- call(Goal).

:- global bind_ins(Goal) # "~w is binding insensitive."-[Goal].

bind_ins(Goal) :- call(Goal).

:- global error_free(Goal) # "~w is error free."-[Goal].

error_free(Goal) :- call(Goal).

:- global memo(Goal) # "~w should be memoized (not unfolded)."-[Goal].

memo(Goal) :- call(Goal).

:- global filter(_, Vars) # "~w should be filtered during
        global control)."-[Vars].

filter(Goal, _) :- call(Goal).

:- regtype flag_values/1 # "Define the valid flag values".

flag_values(atom).
flag_values(integer).
flag_values(L):- list(L,atm).

:- global pe_type(Goal) # "~w will be filtered in partial
        evaluation time according to the PE types defined in the
        assertion."-[Goal].

pe_type(Goal) :- call(Goal).

:- use_module(library(implementation_module)).
:- use_module(library(unfold_calls)).

unfoldable(compat(_, _),   basicprops).
unfoldable(inst(_, _),     basicprops).

prolog:called_by(Goal, basicprops, CM, CL) :-
    nonvar(Goal),
    implementation_module(CM:Goal, M),
    unfoldable(Goal, M),
    unfold_calls(Goal, CM, unfoldable, CL).
