# Using tabling to reason about a changing world

Intelligent agents often have to reason about a changing world.
Reasoning about a changing world is a form of _stream reasoning_. A
classical Prolog system can do little better than implementing a loop
that fetches updates from the outside world to update the world
representation, typically represented as a set of dynamic predicates.
Next, use backward reasoning rules (SLD resolution) can be used to make
conclusions about the new state of the world. This approach has several
disadvantages:

  - SLD resolution is not declarative, being subject to non-termination
    and highly order dependent performance.
  - The design is single threaded.  For example combining both urgent
    shallow reasoning and slower reasoning about the mid-term plan is
    hard to implement.
  - Keeping track of multiple tasks needs to be implemented as a state
    engine, entangling the different tasks.

## How does SWI-Prolog improve this?

SWI-Prolog has a number of extensions that help dealing with reasoning
over a changing blackboard. These extensions can be divided into several
groups.

### Tabling (SLG resolution)

__Tabling__ (SLG resolution) deals with non-termination, providing a
super-set of the Datalog semantics. It also provides _memoization_
avoiding re-computation. Tabling is provided by several Prolog
implementations. SWI-Prolog provides several extensions to the original
tabling implementations. We particularly thank Theresa Swift and David
S. Warren for their help advancing SWI-Prolog's tabling.

  - __Well Founded Semantics__ provides a clean solution to problems
    related to negation such as _Russels paradox_: _"the barber in a town
    shaves every person who does not shave himself"_.  This feature
    is shared with XSB Prolog.
  - __Incremental tabling__ maintains an _incremental dependency graph_ (IDG),
    linking dynamic predicates to (indirectly) dependent tables.  Such
    tables are _invalidated_ if the dynamic predicate changes and lazily
    re-evaluated when needed.  If an intermediate node in this dependency
    graph evaluates to the old result, dependent tables are marked as valid
    again without recomputation.  This feature is shared with XSB Prolog.
  - __Monotonic tabling__ deals efficiently with adding new clauses to
    dynamic predicates, provided the dependency is _monotomic_.  This turns
    quadratic update times into linear behavior.  It is unique to SWI-Prolog.
  - __Shared tabling__ allows multiple threads to share tables.  This
    implies they can reuse each other results.  Shared tabling is supported
    by several Prolog systems, but only SWI-Prolog combines this with
    _Well Founded Semantics_ and _incremental tabling_.


### Deal with concurrent tasks

Reactive intelligent agents have to deal with multiple streams providing
new information about the world as well as multiple concurrent actions
the agent may undertake.  There are several ways in which SWI-Prolog can deal
with this.

#### Threads

The most obvious solution is to use threads: fully concurrent Prolog
engines that run on a same shared Prolog database. With low-cost IoT
hardware such as the Raspberry-Pi having multiple cores, threads are
attractive. Unfortunately, dealing with the dependencies is complicated
and error-prone. For this reason, and notably when the threads are
heavily mutually dependent and not particularly CPU intensive, threads
may not always be the best solution. Note that popular languages such as
JavaScript and Python are essentially single threaded, switching between
tasks based on external events. SWI-Prolog has two mechanisms that
provided similar single-threaded solutions to deal with multiple inputs
and concurrent actions.

#### Engines

Prolog engines have been invented by Paul Tarau. An engine is almost the
same as a thread, but it can be in two states: _attached_ and
_detached_. In the _attached_ mode it is just a Prolog thread, while in
the _detached_ mode it represents the frozen state of the Prolog thread.
Once attached (running), it can yield control to the thread that
attached it either by producing an answer or by using engine_yield/1.

Engines allow a single OS thread to perform _cooperative multitasking_,
similarly to what JavaScript and Python do. _Cooperative multitasking_
does avoid many of the complications associated with multi threading.
Unfortunately it breaks when tasks are not cooperative and hold the CPU
for too long. Note that threads may be combined with engines.

#### Delimited continuations

Delimited continuations allows snapping _"the remainder of the current
computation"_ as a Prolog term. This term can be restarted later,
finishing the work. Delimited continuations can serve a goal similar to
engines. Whenever a task needs to block for input or simply wants to
give other tasks the opportunity to make progress, the task calls
shift/1 indicating what it is waiting for.  The toplevel control loop
uses reset/3 to start tasks or resume continuations.

A simple skeleton for the overall control loop is below. The first
clause checks for input and restarts a continuation (task) that was
waiting for this input. The second clause checks for a clause willing to
continue without waiting after it gave up the CPU to allow other tasks
to make progress and the last waits for new input. Different scheduling
policies may be realised using _priority queues_ as provided by e.g.,
library(heaps).


```
loop(Pending) :-
   get_input_nonblocking(Message),
   !,
   find_continuation(Message, Pending, Continuation, Pending2),
   reset(Continuation, Ball, NewContinuation),
   (   NewContinuation == 0
   ->  loop(Pending2)		% task completed
   ;   append(Pending2, [Ball-NewContinuation], Pending3),
       loop(Pending3)
   ).
loop(Pending) :-
   selectchk(yield-Continuation, Pending, Pending2),
   !,
   reset(Continuation, Ball, NewContinuation),
   (   NewContinuation == 0
   ->  loop(Pending2)		% task completed
   ;   append(Pending2, [Ball-NewContinuation], Pending3),
       loop(Pending3)
   ).
loop(Pending) :-
   wait_for_input,
   loop(Pending).
```

### Dealing with a changing world

Representing the world as a set of dynamic predicate poses problems. For
example, a predicate ``temperature/1`` representing the current ambient
temperature must be updated using a retractall/1 to remove the current
temperature followed by an asserta/1 to record the new temperature:

```
update_temp(Temp) :-
    retractall(temperature(_)),
    asserta(temperature(Temp)).
```

If we do the database update asynchronously with the reasoners, i.e., in
a separate thread, the reasoner may find itself in a situation where no
temperature is known. We can adjust the code to first add the new
temperature and them remove the old. The presence of two temperature
facts may upset the reasoner as well though.

What to do? If we use the _cooperative multitasking_ approach based on
engines or delimited continuations there is no problem as the main thread
will read events from the input and handle them. As nothing runs
concurrently, this works fine.

When using threads, one option is to use with_mutex/2 to make sure
database updates and reasoning are fully synchronized. This makes
threads far less useful as a slow reasoner may block updates and
withhold urgent reasoners from notifying a change to the world. A second
alternative is to use transaction/1. This allows a number of database
changes to become available to the global Prolog database _atomically_.
For example, wrapping ``update_temp/1`` in transaction/1 ensures any
thread will see exactly one temperature at any point in time.

A reasoner asking for the temperature multiple times during its
reasoning process may, despite the use of transaction/1, get different
values for the different requests. If this can pose problems, snapshot/1
comes into the picture. A goal called through snapshot/1 sees a frozen,
isolated and consistent version of the dynamic database: changes made by
other threads after the start of the snapshot are invisible and changes
made inside the snapshot are invisible to other threads.


## Summarizing

SWI-Prolog offers a large set of primitives to deal with a changing
world. Tabling allows for truly declarative reasoning with minimal
recomputation after updates to the world as well as sharing derivations
between threads. Delimited continuations and engines allow for event
driven reasoning over multiple concurrent tasks in a single thread based
on _cooperative multitasking_. Transactions and snapshots allow for
truly concurrent reasoning. Concurrent reasoning profits from
multi-core hardware and is insensitive to tasks that hold the CPU,
providing more reactive systems. SWI-Prolog allows combining the two
approaches in one process.
