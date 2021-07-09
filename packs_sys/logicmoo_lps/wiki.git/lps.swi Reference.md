#lps.swi Server#
*By <http://logicalcontracts.com>, April 5, 2019*


*This document summarizes the open source extensions to the language described in <http://lps.doc.ic.ac.uk>. The system can be tried online at <http://demo.logicalcontracts.com> - start by its Examples menu.*

[TOC]

# About Time #

LPS predicates may be timestamped with "LPS time" or "cycle time", in holds(Fluent,Time) and happens(Event,FromTime,ToTime) literals in the internal syntax representation; external syntax sometimes leaves time implicit. The next sections describe how this abstract, computable "internal timeline" maps to real worl timelines.

## Internal time ##

Internal time is written as an integer, or as a simple arithmetic expression rooted on one othe following operators: ``` + - *```. Notice that time expressions need to be ground when their literal is evaluated - no time constraint handling at the moment. Example:

	..., someFluent at T, someAction("3 cycles later") from T+3, ...


Notice the absence of ```/```, which is used for structured time (described below).


## Real time ##
```real_time(RT) at T```

System fluent that returns the real time (floating number with seconds since Jan 1, 1970) at the begin of LPS cycle T.

```real_time_beginning(RTB)```

Timeless predicate that returns the (real) time when the LPS execution began.

```maxRealTime(Seconds)```

Time limit for the LPS program to execute. If maxTime(NumCycles) is also present, the earliest will cause the program to terminate.

```observe Event(s) at 'Datetime'```

Declare the observation events for injection into the LPS program immediately after the real time moment 'Datetime' occurs. Datetime must be an atom or string, see formats in <http://www.swi-prolog.org/pldoc/doc_for?object=parse_time/3>. 

Notice that "real time" can be simulated, see below.

```lps_terminate(Cause) from T1```


System action that gracefully finishes the program. Cause is a term that can be used to transmit some final condition to the program's owner.

## Simulated real time ##
Sometimes a LPS program is designed to work very slowly - say over years as a contract executes. To test such programs, real time can be "simulated".

```simulatedRealTimeBeginning(Datetime)```

Sets the "real date and time" to be returned by ```real_time(RT)``` at the beginning of cycle 1. The argument must be an atom with a date/time expression, as specified in <http://www.swi-prolog.org/pldoc/doc_for?object=parse_time/3>

```simulatedRealTimePerCycle(Seconds)```

Determines the additional "simulated real time" added at each cycle.

So for example a program with

```
simulatedRealTimeBeginning('2014-06-01'). 
simulatedRealTimePerCycle(86400). % seconds in 1 day
```
...will have real_time(RT) at 10 as the real time (in seconds) correspnding to 0:00 of 2014-06-10.

Other utility predicates can be used with:
```
:- include(system('date_utils.pl')).
```
Please see coments in engine/system/date_utils.pl.

## Structured real time ##

\(The following functionality currently requires ```
:- include(system('date_utils.pl')).
``` )

LPS (simulation cycle) time T can be mapped to real time via the abovementioned ```real_time(RT) at T```. But most contracts refer dates rather than seconds, therefore times can be represented alternatively as ```Year/Month/Day```terms (*more specific terms down to the second are to be supported in the future*). The following fluent returns the current date:

	real_date(Y/M/D) at T

These events occurs on date boundaries:

	real_date_begin(Y/M/D) from T1 to T2
	real_date_end(Y/M/D) from T1 to T2

Based on the above, these syntactic forms are supported by the engine:

	someFluent at 2018/1/29

...means that the fluent holds at all cycle times from the beginning of that day until the very last engine cycle before 2018/1/30 arrives. Same as  ```real_date(2018/1/29) at T, someFluent at T```.

	someEvent from 2018/1/29 to 2018/6
	
... means the event starts no earlier then the first cycle for that day and happens until no later then the last cycle of June 2018. Same as ```real_date_begin(2018/1/29) to T1, true at T1, someEvent from T1 to _T2, real_date_end(RT2) from _ to _, RT2 @=< 2018/6.```

@=< is a lexical term comparison operator.

For convenience, cycle and structured times can be mixed together in binary *comparisons*, for example:

```someEvent to Finish, Finish @=< 2018/6/6```

...is equivalent to:

```someEvent to Finish, real_date(RT) at Finish, RT @=< 2018/6/6```


# Background execution #

*This section introduces background processing for LPS on SWI Prolog (barebones). SWISH/web functionaly comes next below.*

In what follows we distinguish a LPS **program** (the source code) from an **execution** of it (of which there may be several).


```background(ID)``` option

Causes the LPS interpreter to launch a new SWI Prolog thread executing the given program. When used in ```golps(File,[...,background(ID),...])``` etc. will cause this predicate to return immediately, binding ID to a token denoting the new LPS program execution thread.

```minCycleTime(Seconds)```

Add a fixed "sleep" time to the program execution at the begin of each cycle, for the sake of the server environment.

```interpreter:inject_events(+LPSengineThread,+Events,-Result)```

Inject all events into the LPS server thread, after inserting whatever observations are declared for the program. See interpreter:updateEvents/3.

If successful, ```Result = ok(InsertionCycle)```; else, e.g. if some event violates the program integrity constraints, all events are rejected and ```Result = failed(Cause)``

```interpreter: get_fluents(+LPSThreadID,+Fluents,-Cycle,-Values)```

Fetches current values for all templates in Fluents into Values. Returns also the Cycle at which the values were samples. See answer_thread_queries/1.

## Experimental LPS inter-program calling ##

The following "system actions" are provided by the LPS interpreter, and require the **dc** option.

```lps_ask(ID,Events) from T1 to T2```

Send all Events to the program execution ID, and **wait** for the events to be consumed - failing if they're not (say, because they violate ID's program constraints). Notice that this effectively hangs the LPS interpreter (for the current program) until the events are processed by ID - thus to **use with care**.

This is defined internally by the sequence of the next two actions.

```lps_ask(ID,Events,MessageID) from T1 to T2```
Send all Events to the program execution ID, succeeding immediately and returning a unique token MessageID, that can be used to find whether the events are accepted.

```lps_outcome(MessageID,Result) from T3 to T4```

Find out whether events sent by the previous action (denoted by MessageID) were accepted by the destinatary LPS program execution. lps_outcome will be delayed until the events were processed, and will return Result=ok(InsertionCycle) or Result=failure(Cause).

# LPS web servers #

The above features are made available in the LPS web server demo (currently at http://demo.logicalcontracts.com) via the following new predicates and web services. This brings LPS into new territory, and **server operation challenges**.

To minimize server load, LPS programs are obliged to declare a minimum cycle sleep time with minCycleTime(Min), currently Min >= 0.1 mS. LPS server executions will be aborted if the expected load per user (or in the whole system) is too big *(see check_user_server_usage in logicalcontracts/lc/lps_server_UI.pl)*.

## Prolog predicates ##
For use in the SWISH goal field.

### serve(-ID) ###
Executes in background the LPS program in the current editor window. ID is bound to a thread identifier, unique in the running SWI Prolog/SWISH server. A term renderer is used to display a link to the "LPS manager" page for the new LPS execution.

### servers(IDs) ###
Shows links to managers for all running LPS programs of the current user.

### servers ###
Shows all LPS program threads IDs, even after they're finished.

### kill\_all\_servers ###
Brutal cleanup, kills LPS programs of all users.

## Servers UI ##

The LPS manager page shows the current status of the LPS server program, including real time global attributes. In addition it provides links to inspect current state of all fluents, and a single form link to inject events into the program. It also has link to kill (lps_terminate) the program.

The fluent display and event injection links can be copied from the page into a browser address bar and reused:

```
http://demo.logicalcontracts.com/lps_server/fluents/ProgramID?fluents=[FluentTemplates]

http://demo.logicalcontracts.com/lps_server/events/ProgramID?events=[EventAtoms]
```
The HTTP status of the events response will be either 200 (OK) or 409 (conflict with the server program integrity constraints).

Both the fluents and events pages display also the LPS cycle time of sampling and injection, respectively.

The events web service above can also respond with fluents, sampled in the cycle prior or after event injection, by simply adding query variables ```fluents``` and (optionally) ```after```:

```http://demo.logicalcontracts.com/lps_server/events/ProgramID?events=[pickup(kant,fork1)]&fluents=[available(_)]&after=true```

The above will return the state of fluent available after the pickup event (and others in the same cycle) are processed.

## Example ##

### Basics ###

* Open <http://demo.logicalcontracts.com/example/diningPhilosophers-table.pl> ; launch a LPS server, by executing the proposed goal serve(ID)

* Follow the "Manage XXX" link and see the resulting page. Refresh it and see time gone by. Follow the fluents link.

* Back to the manager page, edit the "events to send" field to: [pickup(miguel,fork1)], and click Send, and see the "Events result" page. Then refresh it, and see how the second instance of the same event is rejected by preconditions in the server.

* Refresh the fluents page; you should see a different state.
* At some point you may get a "thread XXX does not exist", meaning that that particular program execution has finished. Go back to http://demo.logicalcontracts.com/example/diningPhilosophers-table.pl and launch another server.

### Philosophers dining remotely ###

Next we take the dining philosphers example and split it into two: a "table" with domain conditions, and a "client" with the philosophers and their layout and intentions:

* Open <http://demo.logicalcontracts.com/example/diningPhilosophers-table.pl> ; launch a LPS server, by executing the proposed goal serve(ID). Copy the LPS server thread ID (e.g. lps1) obtained from the LPS manager page, or from the end of its URL.

* Open <http://demo.logicalcontracts.com/example/diningPhilosophers-client.pl>; edit the proposed goal to carry the relevant LPS server ID obtained above, e.g.  ... + table(lps1)..., and run it. You should see a textual run of dining philosophers.

# Hibernation #

LPS programs can have their execution *suspended*: its whole state is saved as a single term, including pending goals; it finishes; and it can be resumed later, given the original program and the saved execution state.
## Saving execution state ##
A LPS program can save its state anytime by invoking the system action:

```lps_save_finish_execution(File)```

A LPS web server can also be suspended, serving the saved state term in the resulting page. This operation is available from the server manager page. The current URL is of the form:

```/lps_server/events/ID?events=[lps_terminate]&fluents=[lps_saved_state(_,_,_,_,_,_)]&after=true```

Notice that the special event and "fluent" used may have access restrictions.

## Resuming a saved execution ##
A LPS program can *resume*, or start from a previously "hibernated", saved state.

Hibernation requires that the program is still the same, or more precisely, *equivalent*. A LPS program is deemed equivalent for hibernation purposes if:

* Its internal syntax form, serialised as in the source program, is the same (modulo term variants). This is ensured by computing a SHA256 hash of the text, excluding program comments and white space.
* The engines used for saving and restoring are version compatible.

Notice that although "LPS simulation time" (cycles)  are independent of hibernation, real time isn't. For example, consider a program with a real time execution limit of 1 hour, that is suspended after just 1 minute, with 59 minutes left; it it is resumed a day later, it will start but finish immediately.

### Bundling a saved state in the program ###
The simplest way to resume an execution is to include the saved state in the program source code itself, e.g.

```lps_saved_state('0872e13090f71236da7ffb7dabd62e5a',t(1505983225.885826,454,date(2017,9,21,8,40,31.391840934,0,'UTC',-)),[],[],[available(fork1),available(fork2),available(fork3),available(fork4),available(fork5),real_time(1505983231.391349)],[]).```

When starting the program the LPS engine checks for a lps_saved_state(...) term in the source. If there is one, it uses this term to "jumpstart" the program from the cycle when it was saved.

### The restore(File) option ###

```golps(ProgramFile,[...restore(SavedStateFile)..])```

The restore option allows a program to (re)start farther ahead in its computation: if present, the SavedStateFile is used to preset goals, reactive rules, cycle number etc in the engine before executing the program.

If the saved state is not compatible with the program, execution is aborted.

# Authentication and permissions #

Some operations require the user to be authenticated: lps\_send\_email, google nlp calls, and likely others in the future. To circumvent this requirement (e.g. for development purposes), uncomment ```allow_anonymous_powerful_ops```.

Other operations are allowed only to a specific (operation-relative) user:

* lps_terminate events are allowed into a LPS server only if the client has the same user that created the server
* LPS server manager pages are accessible only to the user that created the server

See INSTALL_server.md for more about LPS user authentication predicate ```lps_user(User)``` and necessary system configuration.

# External (Prolog) fluents and actions #

Prolog predicates can be called from any LPS clause, implicitly as timeless predicates: their results are assumed repeatable and constant over time, without a relevant impact on the world outside the program.

But sometimes, when their resuls or relevant side-effects are time-dependent, it's convenient to embed them instead as time-dependent literals in LPS, so that their execution can be articulated in time and meaning with the rest of the time-dependent logic. The following LPS extensions allow it:

* If a literal ```F at T``` has no corresponding fluent declaration, but F is defined as a Prolog predicate, then F is assumed to be an "external extensional fluent".
* If a literal ```A from T1 to T2``` has no corresponding action declaration, but A is defined as a Prolog predicate, then A is assumed to be an "external basic action".

In this way the above time variables will be bound to the LPS cycles when the fluent was sampled (```T```) and the action executed (```T1,T2```).

Notes about the Prolog predicate:

* It can be defined together with the LPS program, or elsewhere in the Prolog system.
* It is always called via findall, as the LPS engine will use its different answer bindings to pursue parallel goals. Predicate failure makes the action incompatible, as if violating a precondition.
* External fluents can NOT be the fluent in a post condition (although it can be referred in its conditions).

# External (Prolog) events #

Sometimes it's useful to poll some state (or call some builtin function) in the "outside world". So declaring...

```prolog_events P, Q, ... .```

... will "inject" these events if the corresponding Prolog goals succeed. Notice that a Prolog predicate with the name and arity of P **must** exist and be accessible in the user module.

Example:

```
prolog_events random(X).
fluents dice(_Turn,_Face).
random(X) from _ to T2 initiates dice(T2,Face) if Face is round(X*6+0.5).
```

```random/1```, a Prolog system predicate, gets called at each cycle, initiating dice/2 fluent tuples over time.

## A logical explanation ##
The above could in principle be achieved instead with the following pseudo code for each event:

```observe([P],T) :- current_time(T), findall(P,P,Ps), member(P,Ps).```

... or also with the following LPS pseudo-code:

```
events P
actions P.
fluents polling. initially polling.
if polling then callprolog(P), P.
```
Again, the Prolog generated events are injected/checked/etc with observations and web events at each LPS cycle.

**NOTE**: internal observations and prolog_events are checked together against preconditions, and rejected or accepted altogether; web events are injected afterwards, and are accepted/rejected altogether but independently of the previous.

# Other misc LPS engine improvements 

* Generic replace/update post conditions, e.g. transfer(From, To, Amount) updates Old to New in balance(From, Old) if New is Old – Amount. Old/New can be (structurally similar) lists of variables.
* More flexible preconditions/constraints, referring both the current and the next states. For example, the goat cannot be left alone with the cabbage and the goat cannot be left alone with the wolf.
* Aggregate predicate: findall(X,FluentGoal,L) at T.
* Negated fluent conditions.
* Multiple actions in the same cycle are serialized wrt state change, thereby enabling accumulation/aggregation of fluent changes, unless declared unserializable.
* Macro actions starting at the same time are parallelized
* Macro actions are acceptable as the post condition event; this allows a state change to coincide with the end of the composite event, whereas the same implemented with a reactive rule would suffer from a 1 cycle lag

## If-then-else (conditionals in consequents) ##

Rule reactive bodies (consequents), as well as composite events / macro actions they call, can use an experimental if-then-else construct:

	..., (if Condition then Then else Else), ... 

The construct is roughly equivalent to

```
if_then_else(Condition,Then,Else) if Condition,Then.
if_then_else(Condition,Then,Else) if not Condition, Else.
```

Condition is evaluated using negation as (finite) failure.

TBD: further documentation

## Meta variables ##

Generic events can be used via the happens(Event,T1,T2) predicate. The following will keep an history of all last occurences of past events:

```
fluents happened/2.
happens(E,_,To) initiates happened(E,To) if not happened(E,_).
```
This will omit composite events though, for the sake of engine efficiency - as in general there will be many more composite than atomic events. To remember composite events handle them explicitly, e.g.:

```
end_of_day(Date) initiates happened(Date,Cycle) if true at Cycle.
```
NOTE: Postconditions are defined only for simple events, e.g. over a single cycle transition - even if they are defined by intensional ("composite") rules.

The following will forbid all except the two action templates shown:

```
false happens(E,_,_), not member(E,[ myAction1(_), myAction2(foo) ]).
```

Fluents can also be generalised via meta variables, wrapped in the holds(Fluent,T) predicate. The following super fluent will include all user fluent tuples:

```
superFluent(F) at T if holds(F,T), not system_fluent(F).
```

## Meta predicates ##

Similarly to ```clause(Head,Body)``` in Prolog, LPS exposes the current program via the following (timeless) meta predicates:

	reactive_rule(Antecedent,Consequent).
	l_events(Head,Body).  % composite events
	l_int(Head,Body). % intensional fluents
	initiated(Event,Fluent,Conditions). 
	terminated(Event,Fluent,Conditions).
	updated(Event,Fluent,OldSubTerm-NewSubTerm,Conditions).
	d_pre(PreConditions).
	fluents(F). events(E). actions(A). % declarations

**Note** that in the present implementation the above predicates will cause an error if the program does not contain its corresponding component.

# Editing Actions #

For convenience the following action verbs are available, to directly "edit" the designated fluents without the need to resort to an additional action definition:

* ```initiate Fluent```
* ```terminate Fluent```
* ```update Old to New in fluent(..,Old,..)```

Time arguments (from.., to..) are implicit.

To understand their meaning, consider the following example *not* using the above:

```
fluents gameOn.
actions finishGame.
finishGame terminates gameOn.
% in some reactive rule:
...., finishGame from _ to _,...
```
Instead one can simply write:

```
fluents gameOn.
% in some reactive rule:
....,terminate gameOn from _ to _,...
```

# Summary of system commands #

Documentation for command line usage is TBD. The folowing are commands to be used on SWISH's query pane. 

## Execute a LPS program in simulated time ##

These commands execute a LPS program and wait for its end (when failure or timeout happens), and only then show its full state evolution and event trace:

	go(Timeline).

Timeline is rendered graphicall, showing fluent states as blue ranges and atomic events as orange or green dots. 

	go(Timeline,[composites]).

Same, but also includes some composite events in the timeline: those that span more than 2 states, and are either completed macro actions that include atomic actions, or composite events that trigger post conditions; other composites are not shown.

	go(Timeline, [sample(ListOfFluentTemplates)]) .

The selected intensional fluents are "sampled" at each cycle, and displayed together with extensional fluents.

	go(Timeline,[stacked_fluents]).

Displays fluent states stacked, vertically, rather than aligned.

	state_diagram(Graph).

Shows the same information as a timeline, but instead as a state transitions diagram, made of common fluent states (nodes) and event/times between them (edges), abstracting from time.

	state_diagram(Graph,[abstract_numbers, non_reflexive]).

Same, but further abstracting numbers into a single constant 'n', and ommitting reflexive edges, e.g. events ans actions which do not change state.

	go.

Prints actions and events, as well as fluent states, on SWISH's output panel.

	gov.

Same, but with verbose output.

## Execute a LPS program in real time ##
These commands start the program and immediately report its progress in real time.

	serve(TwoDscene).

This is the same command already referred above, for background execution. But **if** there are ```d(Thing,DisplayProperties)`` clauses in the program, a [2D scene representation](://bitbucket.org/lpsmasters/lps_corner/src/cefe68210aff7b3c9ac40ee097753443d09b0410/swish/2dWord.md?at=master&fileviewer=file-view-default) is rendered in real time, on SWISH's output panel.

## Syntax conversion commands ##

The program in the text editor panel is assumed to be in lps.swi surface syntax. 

	dump.

Print the internal syntax representation of the program.

	dumplps.

Print the surface syntax representation; useful just for debugging the system.

	dumpjs.

Print the lps.js syntax representation of the program. Error and warning messages may be printed, if the given lps.swi program uses features unavailable on lps.js.


# File extension conventions #

LPS program filenames typically use the following extensions:

Syntax  | File extension
------------- | -------------
Surface, as supported by lps.swi | ```.pl```
Surface, as supported by lps.js | ```.lps```
Internal  | ```.lpsw```, ```.pl_P```, ```.lps_.P```

On lps.swi running over swish only ```.pl``` is used.

In addition to the above extensions, ```.lpst``` denotes the test results of a program: for a program in foo.pl, the file foo.pl_P.lpst will contain its expected behavior - state transitions, actions etc.