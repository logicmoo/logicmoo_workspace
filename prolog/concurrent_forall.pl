/*  Part of Extended Libraries for Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2020, Process Design Center, Breda, The Netherlands.
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

:- module(concurrent_forall,
          [ concurrent_forall/2,
            concurrent_forall/3
          ]).

:- use_module(library(countsols)).
:- use_module(library(thread), []).

:- meta_predicate
        handle_result(+, 0 ),
        concurrent_forall(0, 0 ),
        concurrent_forall(0, 0, 0 ).

%!  concurrent_forall(:Cond, :Action) is semidet.
%
%   Concurrent  version   of  forall/2.   This  predicate  will   prove  several
%   alternatives  of Cond  with  Action, using  multiple  threads.  The  maximum
%   number of threads  defined is the amount of cores  available.  If the number
%   of pending jobs is greater than the  number of workers, then the system will
%   wait until a job is finished before to process the next alternative, this is
%   done to avoid that the alternatives of Cond could overflow the memory.

concurrent_forall(Cond, Action) :-
    concurrent_forall(Cond, Action, true).

%!  concurrent_forall(:Cond, :Action, :Join) is semidet.
%
%   Join is called after the execution of Action in the main thread. Sometimes
%   we still need to execute a part of the code serialized.  Is equivalent to
%   forall(Cond, (Action, ignore(Join))).

concurrent_forall(Cond, Action, Join) :-
    current_prolog_flag(cpu_count, CPUCount),
    message_queue_create(Done),
    message_queue_create(Queue),
    ini_counter(0, Counter),
    SWorkers = workers(0, []),
    term_variables(Join, JoinVars), sort(JoinVars, OJoinVars),
    term_variables(Cond, CondVars), sort(CondVars, OCondVars),
    ord_subtract(OJoinVars, OCondVars, ExternVars),
    Templ =.. [v|JoinVars],
    copy_term(t(Join, Templ, ExternVars), t(Join2, Templ2, ExternVars)),
    forall(Cond,
           (   SWorkers = workers(WorkerCount, Workers),
               Counter = count(I1),
               concur(done(WorkerCount), I1, I2, Join2, Templ2, Done, cleanup(Workers, Queue), Result, [], Exitted),
               nb_setarg(1, Counter, I2),
               handle_result(Result,
                             ( subtract(Workers, Exitted, RemainingWorkers),
                               forall(member(_, RemainingWorkers),
                                      thread_send_message(Queue, done)),
                               thread:concur_cleanup(Result, RemainingWorkers, [Queue, Done])
                             )),
               inc_counter(Counter, I),
               (   WorkerCount < CPUCount
               ->  create_worker(Queue, Done, Id, []),
                   succ(WorkerCount, WorkerCount2),
                   nb_setarg(1, SWorkers, WorkerCount2),
                   nb_setarg(2, SWorkers, [Id|Workers])
               ;   true
               ),
               thread_send_message(Queue, goal(I, Action, Templ))
           )),
    SWorkers = workers(WorkerCount, Workers),
    forall(member(_, Workers),
           thread_send_message(Queue, done)),
    Counter = count(I),
    concur(wait, I, _, Join2, Templ2, Done, cleanup(Workers, Queue), Result, [], Exitted),
    subtract(Workers, Exitted, RemainingWorkers),
    thread:concur_cleanup(Result, RemainingWorkers, [Queue, Done]),
    handle_result(Result, true).

handle_result(Result, FailHandler) :-
    (   Result == true
    ->  true
    ;   Result = false
    ->  FailHandler,
        fail
    ;   Result = exception(Error)
    ->  throw(Error)
    ).

create_worker(Queue, Done, Id, Options) :-
    thread_create(worker(Queue, Done), Id,
                  [ at_exit(thread_send_message(Done, finished(Id)))
                  | Options
                  ]).

%!  worker(+WorkQueue, +DoneQueue) is det.
%
%   Process jobs from WorkQueue and send the results to DoneQueue.

worker(Queue, Done) :-
    thread_get_message(Queue, Message),
    debug(concurrent, 'Worker: received ~p', [Message]),
    (   Message = goal(Id, Goal, Vars)
    ->  (   Goal
        ->  thread_send_message(Done, done(Id, Vars)),
            worker(Queue, Done)
        )
    ;   true
    ).

%!  concur(+Wait, +N1, -N, :Join, +Vars, +Done:queue, +Cleanup,
%!         -Result, +Exitted0, -Exitted) is semidet.
%
%   Wait for completion, failure or error.
%
%   @arg Exited List of thread-ids with threads that completed
%   before all work was done.

concur(done(NW), N, N, _, _, Done, _, true, Exitted, Exitted) :-
    message_queue_property(Done, size(0 )),
    N =< NW,
    !.
concur(wait, 0,  0, _, _, _, _, true, Exitted, Exitted) :- !.
concur(Wait, N1, N, Join, Vars, Done, Cleanup, Status, Exitted1, Exitted) :-
    debug(concurrent, 'Concurrent: waiting for workers ...', []),
    catch(thread_get_message(Done, Exit), Error,
          thread:concur_abort(Error, Cleanup, Done, Exitted1)),
    debug(concurrent, 'Waiting: received ~p', [Exit]),
    (   Exit = done(Id, Bind)
    ->  debug(concurrent, 'Concurrent: Job ~p completed', [Id]),
        ignore(\+ ( Vars = Bind,
                    Join
                  )),
        N2 is N1 - 1,
        concur(Wait, N2, N, Join, Vars, Done, Cleanup, Status, Exitted1, Exitted)
    ;   Exit = finished(Thread)
    ->  thread_join(Thread, JoinStatus),
        debug(concurrent, 'Concurrent: waiter ~p joined: ~p',
              [Thread, JoinStatus]),
        (   JoinStatus == true
        ->  concur(Wait, N1, N, Join, Vars, Done, Cleanup, Status, [Thread|Exitted1], Exitted)
        ;   Status = JoinStatus,
            Exitted = [Thread|Exitted1]
        )
    ).
