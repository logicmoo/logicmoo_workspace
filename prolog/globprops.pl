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

:- module(globprops,
          [(deprecated)/1,
           database/1,
           det/1,
           dupclauses/1,
           equiv/2,
           eval/1,
           exception/1,
           exception/2,
           fails/1,
           failure/1,
           fi/2,
           have_choicepoints/1,
           is_det/1,
           iso/1,
           meta_modes/1,
           multi/1,
           nfi/2,
           no_choicepoints/1,
           no_exception/1,
           no_exception/2,
           no_meta_modes/1,
           no_signal/1,
           no_signal/2,
           non_det/1,
           nondet/1,
           not_fails/1,
           nsh/2,
           num_solutions/2,
           num_solutions_eq/2,
           semidet/1,
           signal/1,
           signal/2,
           signals/2,
           solutions/2,
           throw/2,
           throws/2,
           unknown/1,
           user_output/2
          ]).

:- use_module(library(assertions)).
:- use_module(library(intercept)).
:- use_module(library(metaprops)).
:- use_module(library(typeprops)).
:- use_module(library(send_check)).

/** <module> Global Properties

    These are the properties that can be placed in the global section of an
    assertion, or in a program-point assertion

    Note that the implementations provided for the properties are executed when
    run-time checking is enabled.  Such properties should be implemented
    following certain rules:

    - For any Goal, call(Goal) must be equivalent to:
      ```
      intercept(Prop(Goal), RTCheck, true).
      ```
    - Should remove the choicepoints if the goal does not introduce new ones.

    - Should try to throw the run-time check exception as soon as the property
      being validated has been violated.

    - All the checks must be compatible among them.

    - Should be conmutative.

    - Should be side-effects free
*/

%!  equiv(:Goal1,:Goal2)
%
%   Goal1 is equivalent to Goal2

:- meta_predicate equiv(0, 0).
:- global equiv/2.

equiv(_, Goal) :- call(Goal).

%!  unknown(:Goal)
%
%   Does nothing, just provided as a stub for the meta/2 predicate

:- global unknown/1.

unknown(Goal) :- Goal.

%!  det(:Goal)
%
%   Goal has exactly one solution

:- global det(X) + equiv(not_fails(is_det(X))).

det(Goal) :-
    Solved = solved(no),
    ( true
    ; arg(1, Solved, no)
    ->send_comp_rtcheck(Goal, det, fails),
      fail
    ),
    prolog_current_choice(C1),
    Goal,
    prolog_current_choice(C2),
    ( arg(1, Solved, no)
    ->true
    ; send_comp_rtcheck(Goal, det, non_det)
    ),
    ( C1 == C2
    ->!
    ; nb_setarg(1, Solved, yes)
    ).

%!  semidet(:Goal)
%
%   Goal has zero or one solution

:- global semidet/1.

semidet(Goal) :- semidet(Goal, semidet).

%!  nondet(:Goal)
%
%   Goal is non-deterministic.

:- global nondet/1.

nondet(Goal) :- Goal.

%!  multi(:Goal)
%
%   Goal could have multiple solutions.

:- global multi/1.

multi(Goal) :- multi(Goal, multi).

multi(Goal, Prop) :-
    Solved = solved(no),
    ( true
    ; arg(1, Solved, no)
    ->send_comp_rtcheck(Goal, Prop, failure),
      fail
    ),
    prolog_current_choice(C1),
    test_throw_2(Goal, Prop, _, true),
    prolog_current_choice(C2),
    ( C1 == C2
    ->!
    ; nb_setarg(1, Solved, yes)
    ).

%!  not_fails(:Goal)
%
%   Goal does not fail

:- global not_fails(X) + equiv(multi(X)).

not_fails(Goal) :- multi(Goal, not_fails).

%!  failure(:Goal)
%
%   Goal always fail

:- global failure/1.

failure(Goal) :- failure(Goal, failure).

%!  fails(:Goal)
%
%   Equivalent to failure/1

:- global fails(X) + equiv(failure(X)).

fails(Goal) :- failure(Goal, fails).

failure(Goal, Prop) :-
    Solved = solved(no),
    test_throw_2(Goal, Prop, _, true),
    ( arg(1, Solved, no)
    ->send_comp_rtcheck(Goal, Prop, not_fails),
      nb_setarg(1, Solved, yes)
    ; true
    ).


%!  is_det(:Goal)
%
%   Goal is deterministic. Equivalent to semidet.

:- global is_det(X) + equiv(semidet(X)).

is_det(Goal) :- semidet(Goal, is_det).

semidet(Goal, Prop) :-
    Solved = solved(no),
    Goal,
    ( arg(1, Solved, no)
    ->true
    ; send_comp_rtcheck(Goal, Prop, non_det)
    ),
    nb_setarg(1, Solved, yes).

%!  non_det(:Goal)
%
%   Goal is non-deterministic

:- global non_det/1.

non_det(Goal) :-
    Solved = solved(no),
    ( true
    ; arg(1, Solved, one)
    ->send_comp_rtcheck(Goal, non_det, is_det),
      fail
    ),
    prolog_current_choice(C1),
    Goal,
    prolog_current_choice(C2),
    ( arg(1, Solved, no)
    ->( C2 == C1
      ->!,
        send_comp_rtcheck(Goal, non_det, no_choicepoints)
      ; nb_setarg(1, Solved, one)
      )
    ; nb_setarg(1, Solved, yes)
    ).

%!  no_choicepoints(:Goal)
%
%   Goal does not leave choicepoints on exit

:- global no_choicepoints/1.

no_choicepoints(Goal) :-
    prolog_current_choice(C1),
    Goal,
    prolog_current_choice(C2),
    ( C2 == C1
    ->true
    ; send_comp_rtcheck(Goal, no_choicepoints, have_choicepoints)
    ).

%!  have_choicepoints(:Goal)
%
%   Goal leave choicepoints on exit

:- global have_choicepoints/1.

have_choicepoints(Goal) :-
    prolog_current_choice(C1),
    Goal,
    prolog_current_choice(C2),
    ( C2 == C1
    ->send_comp_rtcheck(Goal, have_choicepoints, no_choicepoints)
    ; true
    ).

%!  num_solutions_eq(:Goal, Num:int)
%
%   Goal have Num solutions

:- global num_solutions_eq/2.

num_solutions_eq(N, Goal) :-
    Sols = solutions(0),
    ( true
    ; arg(1, Sols, A),
      ( ( A == done
        ; A == N
        )
      ->fail
      ; send_comp_rtcheck(Goal, num_solutions_eq(N), Sols),
        fail
      )
    ),
    prolog_current_choice(C1),
    call(Goal),
    prolog_current_choice(C2),
    arg(1, Sols, A),
    ( A == done
    ->true
    ; N1 is A+1,
      ( C2 == C1
      ->!,
        ( N1 == N
        ->true
        ; send_comp_rtcheck(Goal, num_solutions_eq(N), num_solutions_eq(N1))
        )
      ; N1 > N
      ->send_comp_rtcheck(Goal, num_solutions_eq(N), num_solutions(>(N))),
        nb_setarg(1, Sols, done)
      ; nb_setarg(1, Sols, N1)
      )
    ).

:- meta_predicate num_solutions(0, 1).
%!   num_solutions(:Goal, :Check)
%
%    If the number of solutions of Goal is N, call(Check, N) succeeds.

:- global num_solutions/2 + meta_modes.

num_solutions(Goal, Check) :-
    Sols = num_solutions(0),
    ( true
    ; arg(1, Sols, N1),
      ( call(Check, N1)
      ->fail
      ; send_comp_rtcheck(Goal, num_solutions(Check), num_solutions(N1)),
        fail
      )
    ),
    prolog_current_choice(C1),
    call(Goal),
    prolog_current_choice(C2),
    arg(1, Sols, N1),
    N2 is N1+1,
    ( C2 == C1
    ->!,
      ( call(Check, N2)
      ->true
      ; send_comp_rtcheck(Goal, num_solutions(Check), num_solutions(N1))
      )
    ; nb_setarg(1, Sols, N2)
    ).

%!   solutions(:Goal, +Sols)
%
%    Goal produces the solutions listed in Sols

:- global solutions(:, +list).

solutions(Goal, Sols) :-
    Goal = _:Sol,
    Remaining = solutions(Sols),
    ( true
    ; arg(1, Remaining, Sols1),
      ( ( Sols == done
        ; Sols1 == []
        )
      ->fail
      ; append(Sols3, Sols1, Sols),
        send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols3)),
        fail
      )
    ),
    prolog_current_choice(C1),
    call(Goal),
    prolog_current_choice(C2),
    arg(1, Remaining, Sols1),
    ( Sols1 == done
    ->true
    ; [Elem|Sols2] = Sols1,
      ( C2 == C1
      ->!,
        ( Elem \= Sol
        ->append(Curr, Sols1, Sols),
          append(Curr, [Sol], Sols3),
          send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols3))
        ; true
        )
      ; Elem \= Sol
      ->append(Curr, Sols1, Sols),
        append(Curr, [Sol|_], Sols3),
        send_comp_rtcheck(Goal, solutions(Sols), solutions(Sols3)),
        nb_setarg(1, Remaining, done)
      ; nb_setarg(1, Remaining, Sols2)
      )
    ).

%!  database(:Goal)
%
%   Goal will change the prolog database.

:- global database/1.

database(Goal) :- call(Goal).

%!  eval(:Goal)
%
%   Goal is evaluable at compile-time.

:- global eval/1.

eval(Goal) :- call(Goal).

%!  dupclauses(:Goal)
%
%   Goal is a predicate with duplicated clauses.

:- global dupclauses/1 + (eval, database).

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

asserta_signal_check(Choice, _, E, _) :- asserta(signal_db(Choice, no, E)).
asserta_signal_check(Choice, G, _, Thrown) :-
    end_signal_check(Choice, G, Thrown),
    fail.

retract_signal_check(Choice, G, _, Thrown) :- end_signal_check(Choice, G, Thrown).
retract_signal_check(Choice, _, E, _) :- asserta(signal_db(Choice, no, E)), fail.

signal_prop(yes, E, signal(E, yes), signal(E, no )).
signal_prop(no,  E, signal(E, no ), signal(E, yes)).

end_signal_check(Choice, Goal, CheckThrown) :-
    retract(signal_db(Choice, Thrown, E)),
    signal_prop(CheckThrown, E, EP, EV),
    ( Thrown = CheckThrown
    ->true
    ; send_comp_rtcheck(Goal, EP, EV)
    ).

emit_signal(Choice, E) :-
    retract(signal_db(Choice, _, _)),
    assertz(signal_db(Choice, yes, E)).

%!  signal(:Goal)
%
%   Goal sends a signal

:- global signal/1.

signal(Goal) :- signal(_, Goal).

%!  signal(:Goal, Signal)
%
%   Goal sends a signal that unifies with Signal

:- global signal/2.

signal(Signal, Goal) :-
    prolog_current_choice(Choice),
    asserta_signal_check(Choice, Goal, Signal, yes),
    prolog_current_choice(C1),
    intercept(Goal, Signal,
              ( emit_signal(Choice, Signal),
                send_signal(Signal)
              )),
    prolog_current_choice(C2),
    retract_signal_check(Choice, Goal, Signal, yes),
    ( C1 == C2
    ->!
    ; true
    ).

%!  no_signal(:Goal)
%
%   Goal don't send any signal.

:- global no_signal/1.

no_signal(Goal) :- no_signal(_, Goal).

%!  no_signal(:Goal, Signal)
%
%   Goal don't send signals that unifies with Signal

:- global no_signal/2.

no_signal(Signal, Goal) :-
    prolog_current_choice(Choice),
    asserta_signal_check(Choice, Goal, Signal, no),
    prolog_current_choice(C1),
    intercept(Goal, Signal,
              ( emit_signal(Choice, Signal),
                throw(Signal)
              )),
    prolog_current_choice(C2),
    retract_signal_check(Choice, Goal, Signal, no),
    ( C1 == C2
    ->!
    ; true
    ).

%!  exception(:Goal)
%
%   Goal throws an exception.

:- global exception/1.

exception(Goal) :- Goal, send_comp_rtcheck(Goal, exception, no_exception).

%!  throw(Exception, :Goal)
%
%   Goal throws an exception that unifies with Exception

:- global throw/2.
throw(E, Goal) :- test_throw_2(Goal, throw(E), F, F \= E).

:- meta_predicate test_throw_2(0, ?, ?, 0).
test_throw_2(Goal, Prop, F, Test) :-
    catch(Goal, F,
          ( ( F \= assrchk(_, _),
              Test
            ->send_comp_rtcheck(Goal, Prop, exception(F))
            ; true
            ),
            throw(F)
          )).

%!  exception(Exception, :Goal)
%
%   Goal throws an exception that unifies with Exception

:- global exception(E, Goal) + equiv(exception(throw(E, Goal))).

exception(E, Goal) :-
    test_throw_2(Goal, exception(E), F, F \= E),
    send_comp_rtcheck(Goal, exception(E), no_exception).

%!  no_exception(:Goal)
%
%   Goal doesn't throw any exception.

:- global no_exception/1.

no_exception(Goal) :- test_throw_2(Goal, no_exception, _, true).

%!  no_exception(Exception, :Goal)
%
%   Goal doesn't throw an exception that unifies with Exception

:- global no_exception/2.

no_exception(E, Goal) :- test_throw_2(Goal, no_exception(E), F, \+F \= E).

%!  throws(Exceptions:List, :Goal)
%
%   Goal can only throw the exceptions that unify with the elements of
%   Exceptions

:- global throws/2.

throws(EL, Goal) :- test_throw_2(Goal, throws(EL), F, \+memberchk(F, EL)).

%!  signals(Signals:List, :Goal)
%
%   Goal can generate only the signals that unify with the elements of
%   Signals
%
%   @tbd proper implementation

:- global signals/2.

signals(_, Goal) :- call(Goal).

%!  meta_modes(:Goal)
%
%   The modes for Goal are specified in the meta_predicate declaration.

:- global meta_modes/1.

meta_modes(Goal) :- call(Goal).

%!  no_meta_modes(:Goal)
%
%   The modes for ~w are not specified in the meta_predicate declaration.

:- global no_meta_modes/1.

no_meta_modes(Goal) :- call(Goal).

%!  deprecated(:Goal)
%
%  Specifies that the predicate marked with this global property has been
%  deprecated, i.e., its use is not recommended any more since it will be
%  deleted at a future date. Typically this is done because its functionality
%  has been superseded by another predicate.

:- global declaration (deprecated)/1.

deprecated(Goal) :- call(Goal).

%!  iso(:Goal)
%
%  Complies with the ISO-Prolog standard.

:- global iso/1.

iso(Goal) :- call(Goal).

%!  nfi(Term, :Goal)
%
%   On success of Goal, Term is not further instantiated.

:- global nfi/2.

nfi(V, Goal) :-
    copy_term(V, X),
    call(Goal),
    ( subsumes_term(V, X)
    ->true
    ; send_comp_rtcheck(Goal, nfi, fi)
    ).

%!  fi(Term, :Goal)
%
%   On success of Goal, Term is further instantiated.

:- global fi/2.

fi(V, Goal) :-
    copy_term(V, X),
    call(Goal),
    ( subsumes_term(V, X)
    ->send_comp_rtcheck(Goal, fi, nfi)
    ; true
    ).

%!  nsh(Term, :Goal)
%
%   On call of Goal, Goal and Term don't share variables

:- global nsh/2.
nsh(Arg, Goal) :- check_nsh(Arg, Goal), call(Goal).

 check_nsh(Arg, _:Goal) :-
    ( term_variables(Arg, Vars),
      Vars \= []
    ->Goal =.. [_|Args],
      ( select(Arg1, Args, Left),
        Arg1 == Arg
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

%!  user_output(+String, :Goal)
%
%   Goal produces String as standard output

:- global user_output/2.

user_output(S, Goal) :-
    setup_call_cleanup(new_memory_file(File),
                       use_output_mf(File, S, Goal),
                       free_memory_file(File)).

use_output_mf(File, S, Goal) :-
    asserta_user_output_check(File, S, Goal),
    prolog_current_choice(C1),
    catch(Goal, E,
          ( end_output_check(File, S, Goal),
            throw(E)
          )),
    prolog_current_choice(C2),
    retract_user_output_check(File, S, Goal),
    ( C1 == C2
    ->!,
      output_check(File, S, Goal)
    ; true
    ).

asserta_user_output_check(File, _, _) :-
    open_memory_file(File, write, Stream),
    tell(Stream).
 asserta_user_output_check(File, S, Goal) :-
    told,
    output_check(File, S, Goal),
    fail.

retract_user_output_check(_, _, _) :-
    told.
 retract_user_output_check(File, _, _) :-
    open_memory_file(File, append, Stream),
    append(Stream),
    fail.

end_output_check(File, S, Goal) :- told, output_check(File, S, Goal).

output_check(File, S, Goal) :-
    memory_file_to_string(File, S1),
    format("~s", [S1]),
    ( S \== S1
    ->send_comp_rtcheck(Goal, user_output(S), user_output(S1))
    ; true
    ),
    !.
