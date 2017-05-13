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

:- module(nativeprops,
          [clique/1, clique_1/1, constraint/1, covered/1,
           covered/2, finite_solutions/1,
           indep/1, indep/2, instance/1,
           mshare/1, mut_exclusive/1,
           nonground/1, not_covered/1, not_mut_exclusive/1,
           possibly_fails/1,
           possibly_nondet/1, relations/2, sideff_hard/1, sideff_pure/1,
           sideff_soft/1, size/2, size/3,
           size_lb/2, size_o/2, size_ub/2, size_metric/3, size_metric/4,
           succeeds/1, steps/2, steps_lb/2, steps_o/2, steps_ub/2, tau/1,
           terminates/1, test_type/2]).

:- use_module(library(assertions)).
:- use_module(library(metaprops)).
:- use_module(library(globprops)).
:- use_module(library(basicprops)).
:- use_module(library(termtyping)).
:- use_module(library(lists)).
:- use_module(library(rtcprops)).

:- license(gplv2).

% :- doc(doinclude, indep/1).
% :- doc(doinclude, indep/2).
% --------------------------------------------------------------------------
:- doc(title, "Properties which are native to analyzers").

:- doc(author, "Francisco Bueno").
:- doc(author, "Manuel Hermenegildo").
:- doc(author, "Pedro L@'{o}pez").
:- doc(author, "Edison Mera").

:- doc(module, "@cindex{properties, native} This library contains
   a set of properties which are natively understood by the different
   program analyzers of @apl{ciaopp}.  They are used by @apl{ciaopp}
   on output and they can also be used as properties in assertions.").

%    Note that the implementations provided for the properties are the ones used
%    when run-time checks are enabled.  Run-time check for properties
%    @var{Prop} must be implemented following certain rules:
%    ** Comment: these rules are incomplete! See other documentation
%                for properties.
%
%    @begin{itemize}
%    @item For any @var{Goal}, @pred{call(Goal)} must be equivalent to:
%
%      intercept(Prop(Goal), rtcheck(_, _, _, _, _), true).
%
%    @item Remove the choicepoints if the goal does not introduce new ones.
%
%    @item Try to throw the run-time check exception as soon as the
%    property being validated has been violated.
%
%    @item All the checks must be compatible among them.
%    @end{itemize}


:- doc(usage, "@tt{:- use_module(library(assertions(nativeprops)))}

   or also as a package @tt{:- use_package(nativeprops)}.

   Note the different names of the library and the package.").

% --------------------------------------------------------------------------

:- doc(tau(Types), "~w contains a list with the type associations
   for each variable, in the form @tt{V/[T1,..,TN]}. Note that tau is used
   in object-oriented programs only"-[Types]).

% TODO: Improve documentation saying if the run-time checks of some
% TODO: properties are complete (exhaustive), incomplete, not possible
% TODO: or unimplemented --EMM.

:- prop tau(TypeInfo)
# "~w is a list of associations between variables and list of types"-[TypeInfo].

tau([]).
tau([Var/Type|R]) :-
        var(Var),
        list(Type),
        valid_type(Type),
        tau(R).

valid_type([Type]) :-
        atom(Type).
valid_type([Type|Rest]) :-
        atom(Type),
        valid_type(Rest).



:- doc(constraint(C), "~w contains a list of linear (in)equalities
   that relate variables and @tt{int} values. For example,  @tt{[A < B + 4]}
   is a constraint while @tt{[A < BC + 4]} or @tt{[A = 3.4, B >= C]} are
   not."-[C]).

:- prop constraint(C)
# "~w is a list of linear equations"-[C].

constraint([]).
constraint([Cons|Rest]) :-
        constraint_(Cons),
        constraint(Rest).

constraint_(=(Lin_Expr1, Lin_Expr2)) :-
        lin_expr(Lin_Expr1),
        lin_expr(Lin_Expr2).
constraint_(=<(Lin_Expr1, Lin_Expr2)) :-
        lin_expr(Lin_Expr1),
        lin_expr(Lin_Expr2).
constraint_(>=(Lin_Expr1, Lin_Expr2)) :-
        lin_expr(Lin_Expr1),
        lin_expr(Lin_Expr2).
constraint_(<(Lin_Expr1, Lin_Expr2)) :-
        lin_expr(Lin_Expr1),
        lin_expr(Lin_Expr2).
constraint_(>(Lin_Expr1, Lin_Expr2)) :-
        lin_expr(Lin_Expr1),
        lin_expr(Lin_Expr2).

lin_expr(PPL_Var) :-
        ppl_var(PPL_Var), !.
lin_expr(Coeff) :-
        coefficient(Coeff).
% lin_expr(+(Lin_Expr), Vars, +(New_Lin_Expr)) :-
%       lin_expr(Lin_Expr, Vars, New_Lin_Expr).
lin_expr(+(Lin_Expr)) :-
        lin_expr(Lin_Expr).
lin_expr(-(Lin_Expr)) :-
        lin_expr(Lin_Expr).
lin_expr(+(Lin_Expr1, Lin_Expr2)) :-
        lin_expr(Lin_Expr1),
        lin_expr(Lin_Expr2).
lin_expr(-(Lin_Expr1, Lin_Expr2)) :-
        lin_expr(Lin_Expr1),
        lin_expr(Lin_Expr2).
lin_expr(*(Coeff, Lin_Expr)) :-
        coefficient(Coeff),
        lin_expr(Lin_Expr).
lin_expr(*(Lin_Expr, Coeff)) :-
        coefficient(Coeff),
        lin_expr(Lin_Expr).

ppl_var(Var) :-
        var(Var).
coefficient(Coeff) :-
        ground(Coeff),
        int(Coeff).


:- doc(covered(X, Y), "All variables occuring in ~w occur also
   in ~w."-[X, Y]).

:- prop covered(X, Y) # "~w is covered by ~w."-[X, Y].

covered(X, Y) :-
    term_variables(X, VarsX),
    term_variables(Y, VarsY),
    forall(member(V, VarsX),
           ( member(VY, VarsY),
             V==VY
           )).

:- doc(mshare(X), "~w contains all @index{sharing sets}
   @cite{jacobs88,abs-int-naclp89} which specify the possible variable
   occurrences in the terms to which the variables involved in the
   clause may be bound. Sharing sets are a compact way of representing
   groundness of variables and dependencies between variables. This
   representation is however generally difficult to read for
   humans. For this reason, this information is often translated to
   @prop{ground/1}, @prop{indep/1} and @prop{indep/2} properties,
   which are easier to read."-[X]).

% :- test mshare(L) : (L = [[A], [p(A)]]) + fails.
% :- test mshare(L) : (L = [[_], [p(_)]]) + not_fails.

:- prop mshare(X) + (native(sharing(X)), no_rtcheck)
# "The sharing pattern is @tt{~w}."-[X].

mshare(L) :-
        maplist(term_variables, L, V),
        \+ not_mshare(V).

% try to find a counter-example:
not_mshare([V1|L]) :-
        member(V2, L),
        member(X1, V1),
        member(X2, V2),
        X1 == X2 -> true
    ;
        not_mshare(L).

:- doc(clique(X), "~w is a set of variables of interest, much the
   same as a sharing group but ~w represents all the sharing groups in
   the powerset of those variables. Similar to a sharing group, a clique is
   often translated to @prop{ground/1}, @prop{indep/1}, and @prop{indep/2}
   properties."-[X, X]).

:- prop clique(X) + (native(clique(X)), no_rtcheck)
# "The clique pattern is @tt{~w}."-[X].

clique(_).

:- doc(clique_1(X), "~w is a set of variables of interest, much
   the same as a sharing group but ~w represents all the sharing
   groups in the powerset of those variables but disregarding the
   singletons. Similar to a sharing group, a clique_1 is often translated
   to @prop{ground/1}, @prop{indep/1}, and @prop{indep/2} properties."-[X, X]).

:- prop clique_1(X) + (native(clique_1(X)), no_rtcheck)
# "The 1-clique pattern is @tt{~w}."-[X].

clique_1(_).

:- prop nonground(X) + native(not_ground(X))
# "@tt{~w} is not ground."-[X].

nonground(X) :- \+ ground(X).

:- doc(possibly_fails(X), "Non-failure is not ensured for any call
   of the form ~w @cite{non-failure-iclp97}. In other words,
   nothing can be ensured about non-failure nor termination of such
   calls."-[X]).

:- global possibly_fails(X) + (no_rtcheck)
# "Non-failure is not ensured for calls of the form ~w."-[X].

possibly_fails(Goal) :- call(Goal).

:- doc(covered(X), "For any call of the form ~w there is at
   least one clause whose test succeeds (i.e., all the calls of the
   form ~w are covered) @cite{non-failure-iclp97}."-[X, X]).

:- global covered(X) + rtcheck(unimplemented)
# "All the calls of the form ~w are covered."-[X].

covered(Goal) :- call(Goal).

:- doc(not_covered(X), "There is some call of the form ~w for
   which there is no clause whose test succeeds
   @cite{non-failure-iclp97}."-[X]).

:- global not_covered(X) + rtcheck(unimplemented)
# "Not all of the calls of the form ~w are covered."-[X].

not_covered(Goal) :- call(Goal).

:- doc(possibly_nondet(X), "Non-determinism is not ensured for all
   calls of the form ~w. In other words, nothing can be ensured
   about determinacy nor termination of such calls."-[X]).

:- global possibly_nondet(X) + no_rtcheck
# "Non-determinism is not ensured for calls of the form ~w."-[X].

possibly_nondet(Goal) :- call(Goal).

:- global test_type/2 # "Indicates the type of test that a predicate
        performs.  Required by the nonfailure analyisis.".

test_type(Goal, _) :- call(Goal).

:- global mut_exclusive(X) + rtcheck(unimplemented) #
"For any call of the form ~w at most one clause succeeds,
i.e., clauses are pairwise exclusive."-[X].

mut_exclusive(Goal) :- call(Goal).

:- global not_mut_exclusive(X) + rtcheck(unimplemented)
# "For some calls of the form ~w more than one clause may succeed,
i.e., clauses are not disjoint for some calls."-[X].

not_mut_exclusive(Goal) :- call(Goal).

:- prop size_lb(X, Y) + no_rtcheck
# "The minimum size of the terms to which the
   argument ~w is bound is given by the expression
   ~w. Various measures can be used to determine the size of an
   argument, e.g., list-length, term-size, term-depth, integer-value,
   etc. @cite{caslog,granularity-jsc}."-[X, Y].

size_lb(_, _).

:- prop size_ub(X, Y) + no_rtcheck
# "The maximum size of the terms to which the
   argument ~w is bound is given by the expression
   ~w. Various measures can be used to determine the size of an
   argument, e.g., list-length, term-size, term-depth, integer-value,
   etc. @cite{caslog,granularity-jsc}."-[X, Y].

size_ub(_, _).

:- prop size_o(X, Y) + no_rtcheck
# "The size of argument ~w is in the order of ~w."-[X, Y].

size_o(_, _).

:- global size_metric(_, Approx, Var, Metric) + no_rtcheck
# "~w is the metric of the variable ~w, for the
   approximation ~w. Currently, ~w can be:
   @tt{int/1}, @tt{size/1}, @tt{length/1}, @tt{depth/2}, and
   @tt{void/1}."-[Approx, Var, Metric, Metric].

size_metric(Goal, _, _, _) :- call(Goal).

:- global size_metric(_, Var, Metric) + no_rtcheck
# "~w is the metric of the variable ~w, for any
   approximation."-[Var, Metric].

size_metric(Goal, _, _) :- call(Goal).

% ------------------------------------------------------------------

:- global steps_lb(X, Y) + no_rtcheck
# "The minimum computation time (in
   resolution steps) spent by any call of the form ~w is given by
   the expression ~w @cite{low-bounds-ilps97,granularity-jsc}"-[X, Y].

steps_lb(Goal, _) :- call(Goal).

:- global steps_ub(X, Y) + no_rtcheck
# "The maximum computation time (in
   resolution steps) spent by any call of the form ~w is given by
   the expression ~w @cite{caslog,granularity-jsc}."-[X, Y].

steps_ub(Goal, _) :- call(Goal).

:- global steps(X, Y) + no_rtcheck
# "The time (in resolution steps) spent by any
   call of the form ~w is given by the expression ~w"-[X, Y].

steps(Goal, _) :- call(Goal).

:- global steps_o(X, Y) + no_rtcheck
# "~w is the complexity order of the cost of any call of the form
~w."-[X, Y].

steps_o(Goal, _) :- call(Goal).

%%%%%%%%%%%%%%%%%%%%
% Amadeo
:- true prop indep(X, Y) + native(indep([[X, Y]]))
# "~w and ~w do not have variables in common."-[X, Y].

indep(A, B) :-
        mark(A, Ground), % Ground is var if A ground
        nonvar(Ground), % If 1st argument was ground, no need to proceed
        marked(B), !,
        fail.
indep(_, _).

mark('$$Mark', no) :- !. % Mark the variable, signal variable found
mark(Atom,     _) :- atomic(Atom), !.
mark(Complex,  GR) :- mark(Complex, 1, GR).

mark(Args, Mth, GR) :-
        arg(Mth, Args, ThisArg), !,
        mark(ThisArg, GR),
        Nth is Mth+1,
        mark(Args, Nth, GR).
mark(_, _, _).

marked(Term) :-
        functor(Term, F, A),
        ( A > 0, !, marked(Term, 1)
        ; F = '$$Mark' ).

marked(Args, Mth) :-
        arg(Mth, Args, ThisArg), !,
        ( marked(ThisArg)
        ; Nth is Mth+1,
            marked(Args, Nth)
        ).

:- true prop indep(X) + native(indep(X))
# "The variables in pairs in @tt{~w} are pairwise independent."-[X].

indep([]).
indep([[X, Y]|L]) :- indep(X, Y), indep(L).
%%%%%%%%%%%%%%%%%%%%

:- global sideff_pure(X) + no_rtcheck
# "~w is pure, i.e., has no side-effects."-[X].

sideff_pure(Goal) :- call(Goal).

:- global sideff_soft(X) + no_rtcheck
# "~w has @index{soft side-effects}, i.e., those not affecting
   program execution (e.g., input/output)."-[X].

sideff_soft(Goal) :- call(Goal).

:- global sideff_hard(X) + no_rtcheck
# "~w has @index{hard side-effects}, i.e., those that might affect
   program execution (e.g., assert/retract)."-[X].

sideff_hard(Goal) :- call(Goal).

:- global finite_solutions(X) + no_rtcheck
# "Calls of the form ~w produce a
   finite number of solutions @cite{non-failure-iclp97}."-[X].

finite_solutions(Goal) :- call(Goal).

:- global relations(X, N) + rtcheck(unimplemented)
# "The goal ~w produces ~w
   solutions. In other words, ~w is the cardinality of the
   solution set of ~w."-[X, N, N, X].

relations(Goal, _) :- call(Goal).

:- global terminates(X) + no_rtcheck
# "Calls of the form ~w always terminate @cite{non-failure-iclp97}."-[X].

terminates(Goal) :- call(Goal).

% % Built-in in CiaoPP
% :- export(entry_point_name/2).
% :- prop entry_point_name/2 + no_rtcheck.
% % if you change this declaration, you have to change ciaoPP:
% :- meta_predicate entry_point_name(0, ?).
% :- impl_defined(entry_point_name/2).
% :- doc(hide, entry_point_name/2).

/*
:- prop user_error(Goal, S) #
        "Calls of the form @var{Goal} write @var{S} to standard error.".

:- meta_predicate user_error(goal, ?).
user_error(Goal, S) :-
        mktemp_in_tmp('nativeprops_XXXXXX', FileName),
        open_error(FileName, SO),
        call(Goal),
        close_error(SO),
        file_to_string(FileName, S1),
        display_string(S1),
        (
            S \== S1 ->
            send_comp_rtcheck(Goal, user_error(S), user_error(S1))
        ;
            true
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Collapsed properties, to improve performance of run-time checks. --EMM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% These properties are not well implemented:
:- prop not_fails_is_det/1
# "Collapsed property of @var{not_fails/1} and @var{is_det/1}.".

:- meta_predicate not_fails_is_det(goal).
not_fails_is_det(Goal) :-
        Solved = solved(no),
        (
            true
        ;
            arg(1, Solved, no) ->
            send_comp_rtcheck(Goal, not_fails, fails),
            fail
        ),
        Goal,
        (
            arg(1, Solved, no)
        ->
            true
        ;
            send_comp_rtcheck(Goal, is_det, non_det))
% more than one solution!
        ),
        '$setarg'(1, Solved, yes, true).

:- prop not_fails_non_det/1
# "Collapsed property of @var{not_fails/1} and @var{non_det/1}.".

:- meta_predicate not_fails_non_det(goal).
not_fails_non_det(Goal) :-
        Solved = solved(no),
        (
            true
        ;
            arg(1, Solved, no) ->
            send_comp_rtcheck(Goal, not_fails, fails),
            fail
        ;
            arg(1, Solved, one) ->
            send_comp_rtcheck(Goal, non_det, is_det),
            fail
        ),
        '$metachoice'(C0),
        Goal,
        '$metachoice'(C1),
        (
            arg(1, Solved, no) ->
            (
                C1 == C0 ->
                !,
                send_comp_rtcheck(Goal, non_det, no_choicepoints))
            ;
                '$setarg'(1, Solved, one, true)
            )
        ;
            '$setarg'(1, Solved, yes, true)
        ).
*/

:- prop size(A, X, Y) + no_rtcheck
# "~w is the size of argument ~w, for the approximation ~w."-[A, X, Y].

size(_, _, _).


:- prop size(X, Y) + no_rtcheck
# "~w is the size of argument ~w, for any approximation."-[X, Y].

size(_, _).

:- global succeeds(Prop) + no_rtcheck # "A call to ~w succeeds."-[Prop].
succeeds(Prop) :- Prop.
