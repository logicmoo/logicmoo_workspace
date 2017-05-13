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
          [nfi/2, fi/2, clique/1, clique_1/1, constraint/1, covered/1,
           covered/2, exception/1, exception/2, fails/1, finite_solutions/1,
           have_choicepoints/1, indep/1, indep/2, instance/1, is_det/1,
           linear/1, mshare/1, mut_exclusive/1, no_choicepoints/1,
           no_exception/1, no_exception/2, no_signal/1, no_signal/2, non_det/1,
           nonground/1, not_covered/1, not_fails/1, not_mut_exclusive/1,
           num_solutions/2, num_solutions_eq/2, solutions/2, possibly_fails/1,
           possibly_nondet/1, relations/2, sideff_hard/1, sideff_pure/1,
           sideff_soft/1, signal/1, signal/2, signals/2, size/2, size/3,
           size_lb/2, size_o/2, size_ub/2, size_metric/3, size_metric/4,
           succeeds/1, steps/2, steps_lb/2, steps_o/2, steps_ub/2, tau/1,
           terminates/1, test_type/2, throws/2, throw/2, nsh/2, user_output/2,
           test_throw_2/4, is_pred/2, mod_qual/1, mod_qual/2, check/1, trust/1,
           true/1, false/1 ]).

:- use_module(library(assertions)).
:- use_module(library(basicprops)).
:- use_module(library(send_check)).
:- use_module(library(static_strip_module)).
:- use_module(library(termtyping)).
:- use_module(library(lists)).
:- use_module(library(intercept)).
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

:- native tau(TypeInfo)
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

:- native constraint(C)
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

:- native covered(X, Y) # "~w is covered by ~w."-[X, Y].

covered(X, Y) :-
    term_variables(X, VarsX),
    term_variables(Y, VarsY),
    forall(member(V, VarsX),
           ( member(VY, VarsY),
             V==VY
           )).

:- native linear(X)
# "~w is bound to a term which is linear, i.e., if it contains any variables,
   such variables appear only once in the term. For example, @tt{[1,2,3]} and
   @tt{f(A,B)} are linear terms, while @tt{f(A,A)} is not."-[X].

linear(T) :-
    term_variables(T, Vars),
    maplist(occurrs_one(T), Vars).

occurrs_one(T, Var) :- occurrences_of_var(Var, T, 1).

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


:- doc(fails(X), "Calls of the form ~w fail."-[X]).

:- global fails(X) + (native)
# "Calls of the form ~w fail."-[X].

fails(Goal) :-
        Solved = solved(no),
        test_throw_2(Goal, fails, _, true),
        (
            arg(1, Solved, no) ->
            send_comp_rtcheck(Goal, fails, not_fails),
            nb_setarg(1, Solved, yes)
        ;
            true
        ).


:- doc(bug, "A missing property is succeeds (not_fails = succeeds
        or not_terminates. -- EMM").

:- doc(not_fails(X), "Calls of the form ~w produce at least
   one solution, or do not terminate @cite{non-failure-iclp97}."-[X]).
%
:- global not_fails(X) + (native)
# "All the calls of the form ~w do not fail."-[X].
% %
% :- meta_predicate not_fails( goal ).
%
% not_fails( X ) :-
%       if( X , true , throw( rtcheck( nf , fail , X  ) ) ).

not_fails(Goal) :-
        Solved = solved(no),
        (
            true
        ;
            arg(1, Solved, no) ->
            send_comp_rtcheck(Goal, not_fails, fails),
            fail
        ),
        prolog_current_choice(C0),
        test_throw_2(Goal, not_fails, _, true),
        prolog_current_choice(C1),
        ( C0 == C1 -> !
        ; nb_setarg(1, Solved, yes) ).

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

:- doc(is_det(X), "All calls of the form ~w are
   deterministic, i.e., produce at most one solution, or do not
   terminate.  In other words, if ~w succeeds, it can only
   succeed once. It can still leave choice points after its execution,
   but when backtracking into these, it can only fail or go into an
   infinite loop."-[X, X]).

:- global is_det(X)
# "All calls of the form ~w are deterministic."-[X].

is_det(Goal) :-
        Solved = solved(no),
        Goal,
        (
            arg(1, Solved, no)
        ->
            true
        ;
            send_comp_rtcheck(Goal, is_det, non_det)
            % more than one solution!
        ),
        nb_setarg(1, Solved, yes).

:- doc(non_det(X), "All calls of the form ~w are
   non-deterministic, i.e., produce several solutions."-[X]).

:- global non_det(X)
# "All calls of the form ~w are non-deterministic."-[X].

non_det(Goal) :-
        Solved = solved(no),
        (
            true
        ;
            arg(1, Solved, one) ->
            send_comp_rtcheck(Goal, non_det, is_det),
            fail
        ),
        prolog_current_choice(C0),
        Goal,
        prolog_current_choice(C1),
        (
            arg(1, Solved, no) ->
            (
                C1 == C0 ->
                !,
                send_comp_rtcheck(Goal, non_det, no_choicepoints)
            ;
                nb_setarg(1, Solved, one)
            )
        ;
            nb_setarg(1, Solved, yes)
        ).

:- global no_choicepoints(X)
# "A call to ~w does not create choicepoints."-[X].

no_choicepoints(Goal) :-
        prolog_current_choice(C0),
        Goal,
        prolog_current_choice(C1),
        ( C1 == C0 -> true
        ; send_comp_rtcheck(Goal, no_choicepoints, have_choicepoints)
        ).

:- global have_choicepoints(X)
# "A call to ~w creates choicepoints."-[X].

have_choicepoints(Goal) :-
        prolog_current_choice(C0),
        Goal,
        prolog_current_choice(C1),
        ( C1 == C0 ->
            send_comp_rtcheck(Goal, have_choicepoints, no_choicepoints)
        ; true ).

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

:- global num_solutions_eq(X, N) : callable * int
# "All the calls of the form ~w have ~w solutions."-[X, N].

num_solutions_eq(Goal, N) :-
        Sols = solutions(0),
        (
            true
        ;
            arg(1, Sols, A),
            (
                (A == done ; A == N) -> fail
            ;
                send_comp_rtcheck(Goal, num_solutions_eq(N), Sols),
                fail
            )
        ),
        prolog_current_choice(C0),
        call(Goal),
        prolog_current_choice(C1),
        arg(1, Sols, A),
        (
            A == done -> true
        ;
            N1 is A + 1,
            (
                C1 == C0 ->
                !,
                (
                    N1 == N -> true
                ;
                    send_comp_rtcheck(Goal, num_solutions_eq(N),
                        num_solutions_eq(N1))
                )
            ;
                (
                    N1 > N ->
                    send_comp_rtcheck(Goal, num_solutions_eq(N),
                        num_solutions(>(N))),
                    nb_setarg(1, Sols, done)
                ;
                    nb_setarg(1, Sols, N1)
                )
            )
        ).

:- meta_predicate num_solutions(0, 1).

:- global num_solutions(Goal, Check) : callable * callable
# "For a call to ~w, @pred{~w(X)} succeeds, where @var{X} is
   the number of solutions."-[Goal, Check].

num_solutions(Goal, Check) :-
        Sols = num_solutions(0),
        (
            true
        ;
            arg(1, Sols, N0),
            (
                call(Check, N0) -> fail
            ;
                send_comp_rtcheck(Goal, num_solutions(Check),
                    num_solutions(N0)),
                fail
            )
        ),
        prolog_current_choice(C0),
        call(Goal),
        prolog_current_choice(C1),
        arg(1, Sols, N0),
        N1 is N0 + 1,
        (
            C1 == C0 ->
            !,
            (
                call(Check, N1) -> true
            ;
                send_comp_rtcheck(Goal, num_solutions(Check), num_solutions(N0))
            )
        ;
            nb_setarg(1, Sols, N1)
        ).

:- global solutions(Goal, Sols) : callable * list
# "~w produces the solutions listed in ~w."-[Goal, Sols].

solutions(Goal, Sols) :-
        Goal = _:Sol,
        Remaining = solutions(Sols),
        (
            true
        ;
            arg(1, Remaining, Sols0),
            (
                (Sols == done ; Sols0 == []) -> fail
            ;
                append(Sols2, Sols0, Sols),
                send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2)),
                fail
            )
        ),
        prolog_current_choice(C0),
        call(Goal),
        prolog_current_choice(C1),
        arg(1, Remaining, Sols0),
        (
            Sols0 == done -> true
        ;
            [Elem|Sols1] = Sols0,
            (
                C1 == C0 ->
                !,
                (
                    Elem \= Sol ->
                    append(Curr, Sols0, Sols),
                    append(Curr, [Sol], Sols2),
                    send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2))
                ;
                    true
                )
            ;
                (
                    Elem \= Sol ->
                    append(Curr, Sols0,   Sols),
                    append(Curr, [Sol|_], Sols2),
                    send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2)),
                    nb_setarg(1, Remaining, done)
                ;
                    nb_setarg(1, Remaining, Sols1)
                )
            )
        ).

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

:- dynamic signal_db/3.

asserta_signal_check(Choice, _, E, _) :-
        asserta(signal_db(Choice, no, E)).
asserta_signal_check(Choice, Goal, _, CheckThrown) :-
        end_signal_check(Choice, Goal, CheckThrown), fail.

retract_signal_check(Choice, Goal, _, CheckThrown) :-
        end_signal_check(Choice, Goal, CheckThrown).
retract_signal_check(Choice, _, E, _) :-
        asserta(signal_db(Choice, no, E)),
        fail.

signal_prop(yes, E, signal(yes, E), signal(no,  E)).
signal_prop(no,  E, signal(no,  E), signal(yes, E)).

end_signal_check(Choice, Goal, CheckThrown) :-
        retract(signal_db(Choice, Thrown, E)),
        signal_prop(CheckThrown, E, EP, EV),
        ( Thrown = CheckThrown -> true
        ; send_comp_rtcheck(Goal, EP, EV)
        ).

emit_signal(Choice, E) :-
        retract(signal_db(Choice, _, _)),
        assertz(signal_db(Choice, yes, E)).

:- global signal(Goal)
# "Calls of the form ~w throw a signal."-[Goal].

signal(Goal) :- signal(Goal, _).

:- global signal(Goal, E)
# "A call to ~w sends a signal that unifies with ~w."-[Goal, E].

signal(Goal, E) :-
        prolog_current_choice(Choice),
        asserta_signal_check(Choice, Goal, E, yes),
        prolog_current_choice(C0),
        intercept(Goal, E, (emit_signal(Choice, E), send_signal(E))),
        prolog_current_choice(C1),
        retract_signal_check(Choice, Goal, E, yes),
        (C0 == C1 -> ! ; true).

:- global no_signal(Goal)
# "Calls of the form ~w do not send any signal."-[Goal].

no_signal(Goal) :- no_signal(Goal, _).

:- global no_signal(Goal, E)
# "Calls of the form ~w do not send the signal ~w."-[Goal, E].

no_signal(Goal, E) :-
        prolog_current_choice(Choice),
        asserta_signal_check(Choice, Goal, E, no),
        prolog_current_choice(C0),
        intercept(Goal, E, (emit_signal(Choice, E), throw(E))),
        prolog_current_choice(C1),
        retract_signal_check(Choice, Goal, E, no),
        (C0 == C1 -> ! ; true).

:- global exception(Goal)
# "Calls of the form ~w throw an exception."-[Goal].

exception(Goal) :-
        Goal,
        send_comp_rtcheck(Goal, exception, no_exception).

:- global throw/2.
throw(Goal, E) :-
        test_throw_2(Goal, throw(E), F, F\=E).

:- meta_predicate test_throw_2(0, ?, ?, 0).
test_throw_2(Goal, Prop, F, Test) :-
        catch(Goal, F,
            (
                (
                    F \= assrchk(_, _),
                    Test
                ->
                    send_comp_rtcheck(Goal, Prop, exception(F))
                ;
                    true
                ),
                throw(F)
            )).

:- global exception(Goal, E) # "Calls of the form ~w throw an
exception that unifies with ~w."-[Goal, E].
% exception(Goal, E) :- exception(throw(Goal, E)).

exception(Goal, E) :-
        test_throw_2(Goal, exception(E), F, F\=E),
        send_comp_rtcheck(Goal, exception(E), no_exception).

:- global no_exception(Goal) #
        "Calls of the form ~w do not throw any exception."-[Goal].

no_exception(Goal) :- test_throw_2(Goal, no_exception, _, true).

:- global no_exception(Goal, E) # "Calls of the form ~w do not
        throw exception ~w."-[Goal, E].

no_exception(Goal, E) :- test_throw_2(Goal, no_exception(E), F, \+ F\=E).

:- global throws(Goal, Es)
# "Calls of the form ~w can throw only the exceptions that
   unify with the terms listed in ~w."-[Goal, Es].

throws(Goal, EL) :- test_throw_2(Goal, throws(EL), F, \+ memberchk(F, EL)).

:- global signals(Goal, Es) + rtcheck(unimplemented)
# "Calls of the form ~w can generate only the signals that
   unify with the terms listed in ~w."-[Goal, Es].

signals(Goal, _E) :- call(Goal).

:- global user_output(Goal, S) #
        "Calls of the form ~w write ~w to standard output."-[Goal, S].

user_output(Goal, S) :-
    setup_call_cleanup(new_memory_file(FileName),
                       use_output_mf(Goal, S, FileName),
                       free_memory_file(FileName)).

use_output_mf(Goal, S, FileName) :-
    asserta_user_output_check(FileName, Goal, S),
    prolog_current_choice(C0),
    catch(Goal, E,
          ( end_output_check(FileName, Goal, S),
            throw(E)
          )),
    prolog_current_choice(C1),
    retract_user_output_check(FileName, Goal, S),
    ( C0 == C1
    ->!,
      output_check(FileName, Goal, S)
    ; true
    ).

asserta_user_output_check(FileName, _, _) :-
    open_memory_file(FileName, write, Stream),
    tell(Stream).
asserta_user_output_check(FileName, Goal, S) :-
    told,
    output_check(FileName, Goal, S),
    fail.

retract_user_output_check(_, _, _) :-
    told.
retract_user_output_check(FileName, _, _) :-
    open_memory_file(FileName, append, Stream),
    append(Stream),
    fail.

end_output_check(FileName, Goal, S) :-
    told,
    output_check(FileName, Goal, S).

output_check(FileName, Goal, S) :-
    memory_file_to_string(FileName, S1),
    format("~s", [S1]),
    ( S \== S1
    ->send_comp_rtcheck(Goal, user_output(S), user_output(S1))
    ; true
    ),
    !.

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

:- global instance(Prop) + no_rtcheck
# "Uses Prop as an instantiation property. Verifies that execution of
   ~w does not produce bindings for the argument variables."-[Prop].

instance(Goal) :-
    term_variables(Goal, VS),
    Goal,
    term_variables(Goal, VO),
    ( VS == VO
    ->true
    ; !,
      fail
    ).

:- global nfi(_,V) # "~w is not further instantiated."-[V].
:- true comp nfi/2 + (sideff(free), no_rtcheck).

nfi(Goal, V) :-
    copy_term(V, X),
    call(Goal),
    ( subsumes_term(V, X)
    ->true
    ; send_comp_rtcheck(Goal, nfi, fi)
    ).

:- global fi(_, V) # "~w is further instantiated."-[V].
:- true comp fi/2 + (sideff(free), no_rtcheck).

fi(Goal, V) :-
    copy_term(V, X),
    call(Goal),
    ( subsumes_term(V, X)
    ->send_comp_rtcheck(Goal, fi, nfi)
    ; true
    ).

:- global nsh/2.
nsh(Goal, Arg) :-
    check_nsh(Goal, Arg),
    call(Goal).

check_nsh(_:Goal, Arg) :-
    ( term_variables(Arg, Vars),
      Vars \= []
    ->Goal =.. [_|Args],
      ( select(Arg0, Args, Left),
        Arg0 == Arg             % TODO: this can be static
      ->term_variables(Left, GVars),
        intersection(Vars, GVars, Shared),
        ( Shared \= []
        ->send_comp_rtcheck(Goal, nsh, shared(Shared))
        ; true
        )
      ; true
      )
    ; true
    ).


% BUG: if the trace have all the ports active, we can not use ';'/2 in is_pred/2
% and some variables becomes uninstantiated. That is an SWI-Prolog bug but I
% don't have time to isolate it --EMM

:- true prop is_pred(P, N) + no_rtcheck
    # "check that ~w is a defined predicate with ~w extra arguments."-[P, N].
:- meta_predicate is_pred(:, ?).
is_pred(Pred, N) :-
    nnegint(N),
    is_pred_2(Pred, N).

is_pred_2(M:Pred, N) :-
    var(Pred), !,
    current_predicate(M:F/A),
    A >= N,
    A1 is A - N,
    functor(Pred, F, A1).
is_pred_2(M:Pred, N) :-
    functor(Pred, F, A1),
    A is A1 + N,
    current_predicate(M:F/A).

:- true prop mod_qual/1 + no_rtcheck.
mod_qual(M:V) :-
    static_strip_module(V, M, _, CM),
    current_module(CM).

:- true prop mod_qual/2 + no_rtcheck.
:- meta_predicate mod_qual(?, :).
mod_qual(M:V, T) :-
    static_strip_module(V, M, C, CM),
    current_module(CM),
    type(C, T).

:- meta_predicate check(0).
check(_).

:- meta_predicate trust(0).
trust(_).

:- meta_predicate true(0).
true(_).

:- meta_predicate false(0).
false(_).
