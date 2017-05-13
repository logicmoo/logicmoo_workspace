/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(globprops, [(global)/1, (global)/2, (declaration)/1, (declaration)/2,
                      det/1, semidet/1, nondet/1, multi/1, dupclauses/1, iso/1,
                      (deprecated)/1, num_solutions/2, num_solutions_eq/2,
                      solutions/2, is_det/1, non_det/1, fails/1, not_fails/1,
                      unknown/1, equiv/2, have_choicepoints/1, nsh/2, nfi/2,
                      fi/2, no_choicepoints/1, signal/1, signal/2, signals/2,
                      throws/2, throw/2, exception/1, exception/2, database/1,
                      user_output/2, no_exception/1, no_exception/2, eval/1,
                      no_signal/1, no_signal/2, meta_modes/1, no_meta_modes/1]).

:- use_module(library(assertions)).
:- use_module(library(intercept)).
:- use_module(library(metaprops)).
:- use_module(library(send_check)).

%    Note that the implementations provided for the properties are the ones used
%    when run-time checks are enabled.  Run-time check for properties
%    @var{Prop} must be implemented following certain rules:
%
%    - For any @var{Goal}, @pred{call(Goal)} must be equivalent to:
%
%      intercept(Prop(Goal), rtcheck(_, _, _, _, _), true).
%
%    - Remove the choicepoints if the goal does not introduce new ones.
%
%    - Try to throw the run-time check exception as soon as the property being
%      validated has been violated.
%
%    - All the checks must be compatible among them.
%
%    - They are side-effects free

:- meta_predicate equiv(0, 0).
:- global equiv(Goal1,Goal2) # "~w is equivalent to ~w."-[Goal1, Goal2].

equiv(Goal, _) :- call(Goal).

:- global det(X) + equiv(not_fails(is_det(X))).

:- global unknown/1.

unknown(Goal) :- Goal.

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

:- global semidet(X) + equiv(is_det(X)).

semidet(Goal) :- is_det(Goal).

:- global nondet/1.

nondet(Goal) :- Goal.

:- global multi(X) + equiv(not_fails(X)).

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

:- global fails(X)
# "Calls of the form ~w fail."-[X].

fails(Goal) :-
    Solved = solved(no),
    test_throw_2(Goal, fails, _, true),
    ( arg(1, Solved, no)
    ->send_comp_rtcheck(Goal, fails, not_fails),
      nb_setarg(1, Solved, yes)
    ; true
    ).

:- global not_fails(X)
# "All the calls of the form ~w do not fail."-[X].

not_fails(Goal) :-
    Solved = solved(no),
    ( true
    ; arg(1, Solved, no)
    ->send_comp_rtcheck(Goal, not_fails, fails),
      fail
    ),
    prolog_current_choice(C0),
    test_throw_2(Goal, not_fails, _, true),
    prolog_current_choice(C1),
    ( C0 == C1
    ->!
    ; nb_setarg(1, Solved, yes)
    ).

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
    ( arg(1, Solved, no)
    ->true
    ; send_comp_rtcheck(Goal, is_det, non_det)
      % more than one solution!
    ),
    nb_setarg(1, Solved, yes).

:- global non_det(X)
# "All calls of the form ~w are non-deterministic."-[X].

non_det(Goal) :-
    Solved = solved(no),
    ( true
    ; arg(1, Solved, one)
    ->send_comp_rtcheck(Goal, non_det, is_det),
      fail
    ),
    prolog_current_choice(C0),
    Goal,
    prolog_current_choice(C1),
    ( arg(1, Solved, no)
    ->( C1 == C0
      ->!,
        send_comp_rtcheck(Goal, non_det, no_choicepoints)
      ; nb_setarg(1, Solved, one)
      )
    ; nb_setarg(1, Solved, yes)
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
    ( C1 == C0
    ->send_comp_rtcheck(Goal, have_choicepoints, no_choicepoints)
    ; true
    ).

:- global num_solutions_eq(X, N) : callable * int
# "All the calls of the form ~w have ~w solutions."-[X, N].

num_solutions_eq(Goal, N) :-
    Sols = solutions(0),
    ( true
    ; arg(1, Sols, A),
      ( (A == done ; A == N)
      ->fail
      ; send_comp_rtcheck(Goal, num_solutions_eq(N), Sols),
        fail
      )
    ),
    prolog_current_choice(C0),
    call(Goal),
    prolog_current_choice(C1),
    arg(1, Sols, A),
    ( A == done
    ->true
    ; N1 is A + 1,
      ( C1 == C0
      ->!,
        ( N1 == N
        ->true
        ; send_comp_rtcheck(Goal, num_solutions_eq(N),
                            num_solutions_eq(N1))
        )
      ; ( N1 > N
        ->send_comp_rtcheck(Goal, num_solutions_eq(N),
                            num_solutions(>(N))),
          nb_setarg(1, Sols, done)
        ; nb_setarg(1, Sols, N1)
        )
      )
    ).

:- meta_predicate num_solutions(0, 1).

:- global num_solutions(Goal, Check) : callable * callable
# "For a call to ~w, @pred{~w(X)} succeeds, where @var{X} is
   the number of solutions."-[Goal, Check].

num_solutions(Goal, Check) :-
    Sols = num_solutions(0),
    ( true
    ; arg(1, Sols, N0),
      ( call(Check, N0)
      ->fail
      ; send_comp_rtcheck(
            Goal, num_solutions(Check), num_solutions(N0)),
        fail
      )
    ),
    prolog_current_choice(C0),
    call(Goal),
    prolog_current_choice(C1),
    arg(1, Sols, N0),
    N1 is N0 + 1,
    ( C1 == C0
    ->!,
      ( call(Check, N1)
      ->true
      ; send_comp_rtcheck(Goal, num_solutions(Check), num_solutions(N0))
      )
    ; nb_setarg(1, Sols, N1)
    ).

:- global solutions(Goal, Sols) : callable * list
# "~w produces the solutions listed in ~w."-[Goal, Sols].

solutions(Goal, Sols) :-
    Goal = _:Sol,
    Remaining = solutions(Sols),
    ( true
    ; arg(1, Remaining, Sols0),
      ( (Sols == done ; Sols0 == []) -> fail
      ; append(Sols2, Sols0, Sols),
        send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2)),
        fail
      )
    ),
    prolog_current_choice(C0),
    call(Goal),
    prolog_current_choice(C1),
    arg(1, Remaining, Sols0),
    ( Sols0 == done
    ->true
    ; [Elem|Sols1] = Sols0,
      ( C1 == C0
      ->!,
        ( Elem \= Sol
        ->append(Curr, Sols0, Sols),
          append(Curr, [Sol], Sols2),
          send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2))
        ; true
        )
      ; ( Elem \= Sol
        ->append(Curr, Sols0,   Sols),
          append(Curr, [Sol|_], Sols2),
          send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols2)),
          nb_setarg(1, Remaining, done)
        ; nb_setarg(1, Remaining, Sols1)
        )
      )
    ).

:- global database(X) # "A call to ~w will change the prolog database"-[X].

database(Goal) :- call(Goal).

:- global eval(Goal) # "~w is evaluable at compile-time."-[Goal].

eval(Goal) :- call(Goal).

:- global dupclauses/1 + (eval, database)
    # "States that a predicate has repeated clauses".
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

:- global signals(Goal, Es)
# "Calls of the form ~w can generate only the signals that
   unify with the terms listed in ~w."-[Goal, Es].

signals(Goal, _) :- call(Goal).

:- global meta_modes(Goal)
    # "The modes for ~w are specified in the meta_predicate declaration"-[Goal].

meta_modes(Goal) :- call(Goal).

:- global no_meta_modes(Goal)
    # "The modes for ~w are not specified in the meta_predicate declaration"-[Goal].

no_meta_modes(Goal) :- call(Goal).

:- global declaration (deprecated)/1
# "Specifies that the predicate marked with this global property has been
   deprecated, i.e., its use is not recommended any more since it will be
   deleted at a future date. Typically this is done because its functionality
   has been superseded by another predicate.".

deprecated(Goal) :- call(Goal).

:- global iso/1 # "Complies with the ISO-Prolog standard.".

iso(Goal) :- call(Goal).

:- global nfi(_,V) # "~w is not further instantiated."-[V].

nfi(Goal, V) :-
    copy_term(V, X),
    call(Goal),
    ( subsumes_term(V, X)
    ->true
    ; send_comp_rtcheck(Goal, nfi, fi)
    ).

:- global fi(_, V) # "~w is further instantiated."-[V].

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
