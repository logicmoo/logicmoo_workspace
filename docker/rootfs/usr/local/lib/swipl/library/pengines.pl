:- encoding(utf8).
/*  Part of SWI-Prolog

    Author:        Torbjörn Lager and Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014-2020, Torbjörn Lager,
                              VU University Amsterdam
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

:- module(pengines,
          [ pengine_create/1,                   % +Options
            pengine_ask/3,                      % +Pengine, :Query, +Options
            pengine_next/2,                     % +Pengine. +Options
            pengine_stop/2,                     % +Pengine. +Options
            pengine_event/2,                    % -Event, +Options
            pengine_input/2,                    % +Prompt, -Term
            pengine_output/1,                   % +Term
            pengine_respond/3,                  % +Pengine, +Input, +Options
            pengine_debug/2,                    % +Format, +Args
            pengine_self/1,                     % -Pengine
            pengine_pull_response/2,            % +Pengine, +Options
            pengine_destroy/1,                  % +Pengine
            pengine_destroy/2,                  % +Pengine, +Options
            pengine_abort/1,                    % +Pengine
            pengine_application/1,              % +Application
            current_pengine_application/1,      % ?Application
            pengine_property/2,                 % ?Pengine, ?Property
            pengine_user/1,                     % -User
            pengine_event_loop/2,               % :Closure, +Options
            pengine_rpc/2,                      % +Server, :Goal
            pengine_rpc/3                       % +Server, :Goal, +Options
          ]).

/** <module> Pengines: Web Logic Programming Made Easy

The library(pengines) provides an  infrastructure   for  creating Prolog
engines in a (remote) pengine server  and accessing these engines either
from Prolog or JavaScript.

@author Torbjörn Lager and Jan Wielemaker
*/

:- autoload(library(aggregate),[aggregate_all/3]).
:- autoload(library(apply),[maplist/2,partition/4,exclude/3,maplist/3]).
:- autoload(library(broadcast),[broadcast/1]).
:- autoload(library(charsio),[open_chars_stream/2]).
:- autoload(library(debug),[debug/1,debugging/1,debug/3,assertion/1]).
:- autoload(library(error),
	    [ must_be/2,
	      existence_error/2,
	      permission_error/3,
	      domain_error/2
	    ]).
:- autoload(library(filesex),[directory_file_path/3]).
:- autoload(library(listing),[listing/1]).
:- autoload(library(lists),[member/2,flatten/2,select/3,append/3]).
:- autoload(library(modules),[in_temporary_module/3]).
:- autoload(library(occurs),[sub_term/2]).
:- autoload(library(option),
	    [select_option/3,option/2,option/3,select_option/4]).
:- autoload(library(prolog_stack),[print_prolog_backtrace/2]).
:- autoload(library(sandbox),[safe_goal/1]).
:- autoload(library(statistics),[thread_statistics/2]).
:- autoload(library(term_to_json),[term_to_json/2]).
:- autoload(library(thread_pool),
	    [thread_pool_create/3,thread_create_in_pool/4]).
:- autoload(library(time),[alarm/4,call_with_time_limit/2]).
:- autoload(library(uri),
	    [ uri_components/2,
	      uri_query_components/2,
	      uri_data/3,
	      uri_data/4,
	      uri_encoded/3
	    ]).
:- autoload(library(http/http_client),[http_read_data/3]).
:- autoload(library(http/http_cors),[cors_enable/0,cors_enable/2]).
:- autoload(library(http/http_dispatch),
	    [http_handler/3,http_404/2,http_reply_file/3]).
:- autoload(library(http/http_open),[http_open/3]).
:- autoload(library(http/http_parameters),[http_parameters/2]).
:- autoload(library(http/http_stream),[is_cgi_stream/1]).
:- autoload(library(http/http_wrapper),[http_peer/2]).

:- use_module(library(settings),[setting/2,setting/4]).
:- use_module(library(http/http_json),
              [http_read_json_dict/2,reply_json/1]).

:- if(exists_source(library(uuid))).
:- autoload(library(uuid), [uuid/2]).
:- endif.


:- meta_predicate
    pengine_create(:),
    pengine_rpc(+, +, :),
    pengine_event_loop(1, +).

:- multifile
    write_result/3,                 % +Format, +Event, +Dict
    event_to_json/3,                % +Event, -JSON, +Format
    prepare_module/3,               % +Module, +Application, +Options
    prepare_goal/3,                 % +GoalIn, -GoalOut, +Options
    authentication_hook/3,          % +Request, +Application, -User
    not_sandboxed/2.                % +User, +App

:- predicate_options(pengine_create/1, 1,
                     [ id(-atom),
                       alias(atom),
                       application(atom),
                       destroy(boolean),
                       server(atom),
                       ask(compound),
                       template(compound),
                       chunk(integer),
                       bindings(list),
                       src_list(list),
                       src_text(any),           % text
                       src_url(atom),
                       src_predicates(list)
                     ]).
:- predicate_options(pengine_ask/3, 3,
                     [ template(any),
                       chunk(integer),
                       bindings(list)
                     ]).
:- predicate_options(pengine_next/2, 2,
                     [ chunk(integer),
                       pass_to(pengine_send/3, 3)
                     ]).
:- predicate_options(pengine_stop/2, 2,
                     [ pass_to(pengine_send/3, 3)
                     ]).
:- predicate_options(pengine_respond/3, 2,
                     [ pass_to(pengine_send/3, 3)
                     ]).
:- predicate_options(pengine_rpc/3, 3,
                     [ chunk(integer),
                       pass_to(pengine_create/1, 1)
                     ]).
:- predicate_options(pengine_send/3, 3,
                     [ delay(number)
                     ]).
:- predicate_options(pengine_event/2, 2,
                     [ listen(atom),
                       pass_to(thread_get_message/3, 3)
                     ]).
:- predicate_options(pengine_pull_response/2, 2,
                     [ pass_to(http_open/3, 3)
                     ]).
:- predicate_options(pengine_event_loop/2, 2,
                     []).                       % not yet implemented

% :- debug(pengine(transition)).
:- debug(pengine(debug)).               % handle pengine_debug in pengine_rpc/3.

goal_expansion(random_delay, Expanded) :-
    (   debugging(pengine(delay))
    ->  Expanded = do_random_delay
    ;   Expanded = true
    ).

do_random_delay :-
    Delay is random(20)/1000,
    sleep(Delay).

:- meta_predicate                       % internal meta predicates
    solve(+, ?, 0, +),
    findnsols_no_empty(+, ?, 0, -),
    pengine_event_loop(+, 1, +).

/**  pengine_create(:Options) is det.

    Creates a new pengine. Valid options are:

    * id(-ID)
      ID gets instantiated to the id of the created pengine.  ID is
      atomic.

    * alias(+Name)
      The pengine is named Name (an atom). A slave pengine (child) can
      subsequently be referred to by this name.

    * application(+Application)
      Application in which the pengine runs.  See pengine_application/1.

    * server(+URL)
      The pengine will run in (and in the Prolog context of) the pengine
      server located at URL.

    * src_list(+List_of_clauses)
      Inject a list of Prolog clauses into the pengine.

    * src_text(+Atom_or_string)
      Inject the clauses specified by a source text into the pengine.

    * src_url(+URL)
      Inject the clauses specified in the file located at URL into the
      pengine.

    * src_predicates(+List)
      Send the local predicates denoted by List to the remote pengine.
      List is a list of predicate indicators.

Remaining  options  are  passed  to  http_open/3  (meaningful  only  for
non-local pengines) and thread_create/3. Note   that for thread_create/3
only options changing the stack-sizes can be used. In particular, do not
pass the detached or alias options..

Successful creation of a pengine will return an _event term_ of the
following form:

    * create(ID, Term)
      ID is the id of the pengine that was created.
      Term is not used at the moment.

An error will be returned if the pengine could not be created:

    * error(ID, Term)
      ID is invalid, since no pengine was created.
      Term is the exception's error term.
*/


pengine_create(M:Options0) :-
    translate_local_sources(Options0, Options, M),
    (   select_option(server(BaseURL), Options, RestOptions)
    ->  remote_pengine_create(BaseURL, RestOptions)
    ;   local_pengine_create(Options)
    ).

%!  translate_local_sources(+OptionsIn, -Options, +Module) is det.
%
%   Translate  the  `src_predicates`  and  `src_list`  options  into
%   `src_text`. We need to do that   anyway for remote pengines. For
%   local pengines, we could avoid  this   step,  but  there is very
%   little point in transferring source to a local pengine anyway as
%   local pengines can access any  Prolog   predicate  that you make
%   visible to the application.
%
%   Multiple sources are concatenated  to  end   up  with  a  single
%   src_text option.

translate_local_sources(OptionsIn, Options, Module) :-
    translate_local_sources(OptionsIn, Sources, Options2, Module),
    (   Sources == []
    ->  Options = Options2
    ;   Sources = [Source]
    ->  Options = [src_text(Source)|Options2]
    ;   atomics_to_string(Sources, Source)
    ->  Options = [src_text(Source)|Options2]
    ).

translate_local_sources([], [], [], _).
translate_local_sources([H0|T], [S0|S], Options, M) :-
    nonvar(H0),
    translate_local_source(H0, S0, M),
    !,
    translate_local_sources(T, S, Options, M).
translate_local_sources([H|T0], S, [H|T], M) :-
    translate_local_sources(T0, S, T, M).

translate_local_source(src_predicates(PIs), Source, M) :-
    must_be(list, PIs),
    with_output_to(string(Source),
                   maplist(list_in_module(M), PIs)).
translate_local_source(src_list(Terms), Source, _) :-
    must_be(list, Terms),
    with_output_to(string(Source),
                   forall(member(Term, Terms),
                          format('~k .~n', [Term]))).
translate_local_source(src_text(Source), Source, _).

list_in_module(M, PI) :-
    listing(M:PI).

/**  pengine_send(+NameOrID, +Term) is det

Same as pengine_send(NameOrID, Term, []).
*/

pengine_send(Target, Event) :-
    pengine_send(Target, Event, []).


/**  pengine_send(+NameOrID, +Term, +Options) is det

Succeeds immediately and  places  Term  in   the  queue  of  the pengine
NameOrID. Options is a list of options:

   * delay(+Time)
     The actual sending is delayed by Time seconds. Time is an integer
     or a float.

Any remaining options are passed to http_open/3.
*/

pengine_send(Target, Event, Options) :-
    must_be(atom, Target),
    pengine_send2(Target, Event, Options).

pengine_send2(self, Event, Options) :-
    !,
    thread_self(Queue),
    delay_message(queue(Queue), Event, Options).
pengine_send2(Name, Event, Options) :-
    child(Name, Target),
    !,
    delay_message(pengine(Target), Event, Options).
pengine_send2(Target, Event, Options) :-
    delay_message(pengine(Target), Event, Options).

delay_message(Target, Event, Options) :-
    option(delay(Delay), Options),
    !,
    alarm(Delay,
          send_message(Target, Event, Options),
          _AlarmID,
          [remove(true)]).
delay_message(Target, Event, Options) :-
    random_delay,
    send_message(Target, Event, Options).

send_message(queue(Queue), Event, _) :-
    thread_send_message(Queue, pengine_request(Event)).
send_message(pengine(Pengine), Event, Options) :-
    (   pengine_remote(Pengine, Server)
    ->  remote_pengine_send(Server, Pengine, Event, Options)
    ;   pengine_thread(Pengine, Thread)
    ->  thread_send_message(Thread, pengine_request(Event))
    ;   existence_error(pengine, Pengine)
    ).

%!  pengine_request(-Request) is det.
%
%   To be used by a pengine to wait  for the next request. Such messages
%   are placed in the  queue  by   pengine_send/2.  Keeps  the thread in
%   normal state if an event arrives within a second. Otherwise it waits
%   for the `idle_limit` setting while   using  thread_idle/2 to minimis
%   resources.

pengine_request(Request) :-
    thread_self(Me),
    thread_get_message(Me, pengine_request(Request), [timeout(1)]),
    !.
pengine_request(Request) :-
    pengine_self(Self),
    get_pengine_application(Self, Application),
    setting(Application:idle_limit, IdleLimit0),
    IdleLimit is IdleLimit0-1,
    thread_self(Me),
    (   thread_idle(thread_get_message(Me, pengine_request(Request),
                                       [timeout(IdleLimit)]),
                    long)
    ->  true
    ;   Request = destroy
    ).


%!  pengine_reply(+Event) is det.
%!  pengine_reply(+Queue, +Event) is det.
%
%   Reply Event to the parent of the   current  Pengine or the given
%   Queue.  Such  events  are  read   by    the   other   side  with
%   pengine_event/1.
%
%   If the message cannot be sent within the `idle_limit` setting of
%   the pengine, abort the pengine.

pengine_reply(Event) :-
    pengine_parent(Queue),
    pengine_reply(Queue, Event).

pengine_reply(_Queue, _Event0) :-
    nb_current(pengine_idle_limit_exceeded, true),
    !.
pengine_reply(Queue, Event0) :-
    arg(1, Event0, ID),
    wrap_first_answer(ID, Event0, Event),
    random_delay,
    debug(pengine(event), 'Reply to ~p: ~p', [Queue, Event]),
    (   pengine_self(ID),
        \+ pengine_detached(ID, _)
    ->  get_pengine_application(ID, Application),
        setting(Application:idle_limit, IdleLimit),
        debug(pengine(reply), 'Sending ~p, timout: ~q', [Event, IdleLimit]),
        (   thread_send_message(Queue, pengine_event(ID, Event),
                                [ timeout(IdleLimit)
                                ])
        ->  true
        ;   thread_self(Me),
            debug(pengine(reply), 'pengine_reply: timeout for ~q (thread ~q)',
                  [ID, Me]),
            nb_setval(pengine_idle_limit_exceeded, true),
            thread_detach(Me),
            abort
        )
    ;   thread_send_message(Queue, pengine_event(ID, Event))
    ).

wrap_first_answer(ID, Event0, CreateEvent) :-
    wrap_first_answer_in_create_event(CreateEvent, [answer(Event0)]),
    arg(1, CreateEvent, ID),
    !,
    retract(wrap_first_answer_in_create_event(CreateEvent, [answer(Event0)])).
wrap_first_answer(_ID, Event, Event).


empty_queue :-
    pengine_parent(Queue),
    empty_queue(Queue, 0, Discarded),
    debug(pengine(abort), 'Abort: discarded ~D messages', [Discarded]).

empty_queue(Queue, C0, C) :-
    thread_get_message(Queue, _Term, [timeout(0)]),
    !,
    C1 is C0+1,
    empty_queue(Queue, C1, C).
empty_queue(_, C, C).


/** pengine_ask(+NameOrID, @Query, +Options) is det

Asks pengine NameOrID a query Query.

Options is a list of options:

    * template(+Template)
      Template is a variable (or a term containing variables) shared
      with the query. By default, the template is identical to the
      query.

    * chunk(+Integer)
      Retrieve solutions in chunks of Integer rather than one by one. 1
      means no chunking (default). Other integers indicate the maximum
      number of solutions to retrieve in one chunk.

    * bindings(+Bindings)
      Sets the global variable '$variable_names' to a list of
      `Name = Var` terms, providing access to the actual variable
      names.

Any remaining options are passed to pengine_send/3.

Note that the predicate pengine_ask/3 is deterministic, even for queries
that have more than one solution. Also,  the variables in Query will not
be bound. Instead, results will  be  returned   in  the  form  of _event
terms_.

    * success(ID, Terms, Projection, Time, More)
      ID is the id of the pengine that succeeded in solving the query.
      Terms is a list holding instantiations of `Template`.  Projection
      is a list of variable names that should be displayed. Time is
      the CPU time used to produce the results and finally, More
      is either `true` or `false`, indicating whether we can expect the
      pengine to be able to return more solutions or not, would we call
      pengine_next/2.

    * failure(ID)
      ID is the id of the pengine that failed for lack of a solutions.

    * error(ID, Term)
      ID is the id of the pengine throwing the exception.
      Term is the exception's error term.

    * output(ID, Term)
      ID is the id of a pengine running the query that called
      pengine_output/1. Term is the term that was passed in the first
      argument of pengine_output/1 when it was called.

    * prompt(ID, Term)
      ID is the id of the pengine that called pengine_input/2 and Term is
      the prompt.

Defined in terms of pengine_send/3, like so:

==
pengine_ask(ID, Query, Options) :-
    partition(pengine_ask_option, Options, AskOptions, SendOptions),
    pengine_send(ID, ask(Query, AskOptions), SendOptions).
==
*/

pengine_ask(ID, Query, Options) :-
    partition(pengine_ask_option, Options, AskOptions, SendOptions),
    pengine_send(ID, ask(Query, AskOptions), SendOptions).


pengine_ask_option(template(_)).
pengine_ask_option(chunk(_)).
pengine_ask_option(bindings(_)).
pengine_ask_option(breakpoints(_)).


/** pengine_next(+NameOrID, +Options) is det

Asks pengine NameOrID for the  next  solution   to  a  query  started by
pengine_ask/3. Defined options are:

    * chunk(+Count)
    Modify the chunk-size to Count before asking the next set of
    solutions.

Remaining  options  are  passed  to    pengine_send/3.   The  result  of
re-executing the current goal is returned  to the caller's message queue
in the form of _event terms_.

    * success(ID, Terms, Projection, Time, More)
      See pengine_ask/3.

    * failure(ID)
      ID is the id of the pengine that failed for lack of more solutions.

    * error(ID, Term)
      ID is the id of the pengine throwing the exception.
      Term is the exception's error term.

    * output(ID, Term)
      ID is the id of a pengine running the query that called
      pengine_output/1. Term is the term that was passed in the first
      argument of pengine_output/1 when it was called.

    * prompt(ID, Term)
      ID is the id of the pengine that called pengine_input/2 and Term
      is the prompt.

Defined in terms of pengine_send/3, as follows:

==
pengine_next(ID, Options) :-
    pengine_send(ID, next, Options).
==

*/

pengine_next(ID, Options) :-
    select_option(chunk(Count), Options, Options1),
    !,
    pengine_send(ID, next(Count), Options1).
pengine_next(ID, Options) :-
    pengine_send(ID, next, Options).


/** pengine_stop(+NameOrID, +Options) is det

Tells pengine NameOrID to stop looking  for   more  solutions to a query
started by pengine_ask/3. Options are passed to pengine_send/3.

Defined in terms of pengine_send/3, like so:

==
pengine_stop(ID, Options) :-
    pengine_send(ID, stop, Options).
==
*/

pengine_stop(ID, Options) :- pengine_send(ID, stop, Options).


/** pengine_abort(+NameOrID) is det

Aborts the running query. The pengine goes   back  to state `2', waiting
for new queries.

@see pengine_destroy/1.
*/

pengine_abort(Name) :-
    (   child(Name, Pengine)
    ->  true
    ;   Pengine = Name
    ),
    (   pengine_remote(Pengine, Server)
    ->  remote_pengine_abort(Server, Pengine, [])
    ;   pengine_thread(Pengine, Thread),
        debug(pengine(abort), 'Signalling thread ~p', [Thread]),
        catch(thread_signal(Thread, throw(abort_query)), _, true)
    ).


/** pengine_destroy(+NameOrID) is det.
    pengine_destroy(+NameOrID, +Options) is det.

Destroys the pengine NameOrID.  With the option force(true), the pengine
is killed using abort/0 and pengine_destroy/2 succeeds.
*/

pengine_destroy(ID) :-
    pengine_destroy(ID, []).

pengine_destroy(Name, Options) :-
    (   child(Name, ID)
    ->  true
    ;   ID = Name
    ),
    option(force(true), Options),
    !,
    (   pengine_thread(ID, Thread)
    ->  catch(thread_signal(Thread, abort),
              error(existence_error(thread, _), _), true)
    ;   true
    ).
pengine_destroy(ID, _) :-
    catch(pengine_send(ID, destroy),
          error(existence_error(pengine, ID), _),
          retractall(child(_,ID))).


/*================= pengines administration =======================
*/

%!  current_pengine(?Id, ?Parent, ?Location)
%
%   Dynamic predicate that registers our known pengines.  Id is
%   an atomic unique datatype.  Parent is the id of our parent
%   pengine.  Location is one of
%
%     - thread(ThreadId)
%     - remote(URL)

:- dynamic
    current_pengine/6,              % Id, ParentId, Thread, URL, App, Destroy
    pengine_queue/4,                % Id, Queue, TimeOut, Time
    output_queue/3,                 % Id, Queue, Time
    pengine_user/2,                 % Id, User
    pengine_data/2,                 % Id, Data
    pengine_detached/2.             % Id, Data
:- volatile
    current_pengine/6,
    pengine_queue/4,
    output_queue/3,
    pengine_user/2,
    pengine_data/2,
    pengine_detached/2.

:- thread_local
    child/2.                        % ?Name, ?Child

%!  pengine_register_local(+Id, +Thread, +Queue, +URL, +App, +Destroy) is det.
%!  pengine_register_remote(+Id, +URL, +Queue, +App, +Destroy) is det.
%!  pengine_unregister(+Id) is det.

pengine_register_local(Id, Thread, Queue, URL, Application, Destroy) :-
    asserta(current_pengine(Id, Queue, Thread, URL, Application, Destroy)).

pengine_register_remote(Id, URL, Application, Destroy) :-
    thread_self(Queue),
    asserta(current_pengine(Id, Queue, 0, URL, Application, Destroy)).

%!  pengine_unregister(+Id)
%
%   Called by the pengine thread  destruction.   If  we are a remote
%   pengine thread, our URL  equals  =http=   and  the  queue is the
%   message queue used to send events to the HTTP workers.

pengine_unregister(Id) :-
    thread_self(Me),
    (   current_pengine(Id, Queue, Me, http, _, _)
    ->  with_mutex(pengine, sync_destroy_queue_from_pengine(Id, Queue))
    ;   true
    ),
    retractall(current_pengine(Id, _, Me, _, _, _)),
    retractall(pengine_user(Id, _)),
    retractall(pengine_data(Id, _)).

pengine_unregister_remote(Id) :-
    retractall(current_pengine(Id, _Parent, 0, _, _, _)).

%!  pengine_self(-Id) is det.
%
%   True if the current thread is a pengine with Id.

pengine_self(Id) :-
    thread_self(Thread),
    current_pengine(Id, _Parent, Thread, _URL, _Application, _Destroy).

pengine_parent(Parent) :-
    nb_getval(pengine_parent, Parent).

pengine_thread(Pengine, Thread) :-
    current_pengine(Pengine, _Parent, Thread, _URL, _Application, _Destroy),
    Thread \== 0,
    !.

pengine_remote(Pengine, URL) :-
    current_pengine(Pengine, _Parent, 0, URL, _Application, _Destroy).

get_pengine_application(Pengine, Application) :-
    current_pengine(Pengine, _Parent, _, _URL, Application, _Destroy),
    !.

get_pengine_module(Pengine, Pengine).

:- if(current_predicate(uuid/2)).
pengine_uuid(Id) :-
    uuid(Id, [version(4)]).             % Version 4 is random.
:- else.
pengine_uuid(Id) :-
    (   current_prolog_flag(max_integer, Max1)
    ->  Max is Max1-1
    ;   Max is 1<<128
    ),
    random_between(0, Max, Num),
    atom_number(Id, Num).
:- endif.

%!  protect_pengine(+Id, :Goal) is semidet.
%
%   Run Goal while protecting the Pengine  Id from being destroyed. Used
%   by the HTTP  I/O  routines  to   avoid  that  the  Pengine's  module
%   disappears while I/O is in progress. We  use a pool of locks because
%   the lock may be held relatively long by output routines.
%
%   This also runs Goal if the Pengine no longer exists. This deals with
%   Pengines terminated through destroy_or_continue/1.
%
%   @bug After destroy_or_continue/1 takes the destroy route, the module
%   may drop-out at any point in time,   resulting  in a possible crash.
%   Seems the only safe way out is   to  do (de)serialization inside the
%   Pengine.

:- meta_predicate protect_pengine(+, 0).

protect_pengine(Id, Goal) :-
    term_hash(Id, Hash),
    LockN is Hash mod 64,
    atom_concat(pengine_done_, LockN, Lock),
    with_mutex(Lock,
               (   pengine_thread(Id, _)
               ->  Goal
               ;   Goal
               )).


/** pengine_application(+Application) is det.

Directive that must be used to declare a pengine application module. The
module must not be associated to any   file.  The default application is
=pengine_sandbox=.  The  example  below  creates    a   new  application
=address_book=  and  imports  the  API  defined    in  the  module  file
=adress_book_api.pl= into the application.

  ==
  :- pengine_application(address_book).
  :- use_module(address_book:adress_book_api).
  ==
*/

pengine_application(Application) :-
    throw(error(context_error(nodirective,
                             pengine_application(Application)), _)).

:- multifile
    system:term_expansion/2,
    current_application/1.

%!  current_pengine_application(?Application) is nondet.
%
%   True when Application is a currently defined application.
%
%   @see pengine_application/1

current_pengine_application(Application) :-
    current_application(Application).


% Default settings for all applications

:- setting(thread_pool_size, integer, 100,
           'Maximum number of pengines this application can run.').
:- setting(thread_pool_stacks, list(compound), [],
           'Maximum stack sizes for pengines this application can run.').
:- setting(slave_limit, integer, 3,
           'Maximum number of slave pengines a master pengine can create.').
:- setting(time_limit, number, 300,
           'Maximum time to wait for output').
:- setting(idle_limit, number, 300,
           'Pengine auto-destroys when idle for this time').
:- setting(safe_goal_limit, number, 10,
           'Maximum time to try proving safety of the goal').
:- setting(program_space, integer, 100_000_000,
           'Maximum memory used by predicates').
:- setting(allow_from, list(atom), [*],
           'IP addresses from which remotes are allowed to connect').
:- setting(deny_from, list(atom), [],
           'IP addresses from which remotes are NOT allowed to connect').
:- setting(debug_info, boolean, false,
           'Keep information to support source-level debugging').


system:term_expansion((:- pengine_application(Application)), Expanded) :-
    must_be(atom, Application),
    (   module_property(Application, file(_))
    ->  permission_error(create, pengine_application, Application)
    ;   true
    ),
    expand_term((:- setting(Application:thread_pool_size, integer,
                            setting(pengines:thread_pool_size),
                            'Maximum number of pengines this \c
                            application can run.')),
                ThreadPoolSizeSetting),
    expand_term((:- setting(Application:thread_pool_stacks, list(compound),
                            setting(pengines:thread_pool_stacks),
                            'Maximum stack sizes for pengines \c
                            this application can run.')),
                ThreadPoolStacksSetting),
    expand_term((:- setting(Application:slave_limit, integer,
                            setting(pengines:slave_limit),
                            'Maximum number of local slave pengines \c
                            a master pengine can create.')),
                SlaveLimitSetting),
    expand_term((:- setting(Application:time_limit, number,
                            setting(pengines:time_limit),
                            'Maximum time to wait for output')),
                TimeLimitSetting),
    expand_term((:- setting(Application:idle_limit, number,
                            setting(pengines:idle_limit),
                            'Pengine auto-destroys when idle for this time')),
                IdleLimitSetting),
    expand_term((:- setting(Application:safe_goal_limit, number,
                            setting(pengines:safe_goal_limit),
                            'Maximum time to try proving safety of the goal')),
                SafeGoalLimitSetting),
    expand_term((:- setting(Application:program_space, integer,
                            setting(pengines:program_space),
                            'Maximum memory used by predicates')),
                ProgramSpaceSetting),
    expand_term((:- setting(Application:allow_from, list(atom),
                            setting(pengines:allow_from),
                            'IP addresses from which remotes are allowed \c
                            to connect')),
                AllowFromSetting),
    expand_term((:- setting(Application:deny_from, list(atom),
                            setting(pengines:deny_from),
                            'IP addresses from which remotes are NOT \c
                            allowed to connect')),
                DenyFromSetting),
    expand_term((:- setting(Application:debug_info, boolean,
                            setting(pengines:debug_info),
                            'Keep information to support source-level \c
                            debugging')),
                DebugInfoSetting),
    flatten([ pengines:current_application(Application),
              ThreadPoolSizeSetting,
              ThreadPoolStacksSetting,
              SlaveLimitSetting,
              TimeLimitSetting,
              IdleLimitSetting,
              SafeGoalLimitSetting,
              ProgramSpaceSetting,
              AllowFromSetting,
              DenyFromSetting,
              DebugInfoSetting
            ], Expanded).

% Register default application

:- pengine_application(pengine_sandbox).


/** pengine_property(?Pengine, ?Property) is nondet.

True when Property is a property of   the  given Pengine. Enumerates all
pengines  that  are  known  to  the   calling  Prolog  process.  Defined
properties are:

  * self(ID)
    Identifier of the pengine.  This is the same as the first argument,
    and can be used to enumerate all known pengines.
  * alias(Name)
    Name is the alias name of the pengine, as provided through the
    `alias` option when creating the pengine.
  * thread(Thread)
    If the pengine is a local pengine, Thread is the Prolog thread
    identifier of the pengine.
  * remote(Server)
    If the pengine is remote, the URL of the server.
  * application(Application)
    Pengine runs the given application
  * module(Module)
    Temporary module used for running the Pengine.
  * destroy(Destroy)
    Destroy is =true= if the pengines is destroyed automatically
    after completing the query.
  * parent(Queue)
    Message queue to which the (local) pengine reports.
  * source(?SourceID, ?Source)
    Source is the source code with the given SourceID. May be present if
    the setting `debug_info` is present.
  * detached(?Time)
    Pengine was detached at Time.
*/


pengine_property(Id, Prop) :-
    nonvar(Id), nonvar(Prop),
    pengine_property2(Prop, Id),
    !.
pengine_property(Id, Prop) :-
    pengine_property2(Prop, Id).

pengine_property2(self(Id), Id) :-
    current_pengine(Id, _Parent, _Thread, _URL, _Application, _Destroy).
pengine_property2(module(Id), Id) :-
    current_pengine(Id, _Parent, _Thread, _URL, _Application, _Destroy).
pengine_property2(alias(Alias), Id) :-
    child(Alias, Id),
    Alias \== Id.
pengine_property2(thread(Thread), Id) :-
    current_pengine(Id, _Parent, Thread, _URL, _Application, _Destroy),
    Thread \== 0.
pengine_property2(remote(Server), Id) :-
    current_pengine(Id, _Parent, 0, Server, _Application, _Destroy).
pengine_property2(application(Application), Id) :-
    current_pengine(Id, _Parent, _Thread, _Server, Application, _Destroy).
pengine_property2(destroy(Destroy), Id) :-
    current_pengine(Id, _Parent, _Thread, _Server, _Application, Destroy).
pengine_property2(parent(Parent), Id) :-
    current_pengine(Id, Parent, _Thread, _URL, _Application, _Destroy).
pengine_property2(source(SourceID, Source), Id) :-
    pengine_data(Id, source(SourceID, Source)).
pengine_property2(detached(When), Id) :-
    pengine_detached(Id, When).

/** pengine_output(+Term) is det

Sends Term to the parent pengine or thread.
*/

pengine_output(Term) :-
    pengine_self(Me),
    pengine_reply(output(Me, Term)).


/** pengine_debug(+Format, +Args) is det

Create a message using format/3 from Format   and  Args and send this to
the    client.    The    default    JavaScript    client    will    call
=|console.log(Message)|=  if  there  is   a    console.   The  predicate
pengine_rpc/3 calls debug(pengine(debug), '~w',   [Message]).  The debug
topic pengine(debug) is enabled by default.

@see debug/1 and nodebug/1 for controlling the pengine(debug) topic
@see format/2 for format specifications
*/

pengine_debug(Format, Args) :-
    pengine_parent(Queue),
    pengine_self(Self),
    catch(safe_goal(format(atom(_), Format, Args)), E, true),
    (   var(E)
    ->  format(atom(Message), Format, Args)
    ;   message_to_string(E, Message)
    ),
    pengine_reply(Queue, debug(Self, Message)).


/*================= Local pengine =======================
*/

%!  local_pengine_create(+Options)
%
%   Creates  a  local   Pengine,   which    is   a   thread  running
%   pengine_main/2.  It maintains two predicates:
%
%     - The global dynamic predicate id/2 relates Pengines to their
%       childs.
%     - The local predicate id/2 maps named childs to their ids.

local_pengine_create(Options) :-
    thread_self(Self),
    option(application(Application), Options, pengine_sandbox),
    create(Self, Child, Options, local, Application),
    option(alias(Name), Options, Child),
    assert(child(Name, Child)).


%!  thread_pool:create_pool(+Application) is det.
%
%   On demand creation of a thread pool for a pengine application.

:- multifile thread_pool:create_pool/1.

thread_pool:create_pool(Application) :-
    current_application(Application),
    setting(Application:thread_pool_size, Size),
    setting(Application:thread_pool_stacks, Stacks),
    thread_pool_create(Application, Size, Stacks).

%!  create(+Queue, -Child, +Options, +URL, +Application) is det.
%
%   Create a new pengine thread.
%
%   @arg Queue is the queue (or thread handle) to report to
%   @arg Child is the identifier of the created pengine.
%   @arg URL is one of =local= or =http=

create(Queue, Child, Options, local, Application) :-
    !,
    pengine_child_id(Child),
    create0(Queue, Child, Options, local, Application).
create(Queue, Child, Options, URL, Application) :-
    pengine_child_id(Child),
    catch(create0(Queue, Child, Options, URL, Application),
          Error,
          create_error(Queue, Child, Error)).

pengine_child_id(Child) :-
    (   nonvar(Child)
    ->  true
    ;   pengine_uuid(Child)
    ).

create_error(Queue, Child, Error) :-
    pengine_reply(Queue, error(Child, Error)).

create0(Queue, Child, Options, URL, Application) :-
    (  current_application(Application)
    -> true
    ;  existence_error(pengine_application, Application)
    ),
    (   URL \== http                    % pengine is _not_ a child of the
                                        % HTTP server thread
    ->  aggregate_all(count, child(_,_), Count),
        setting(Application:slave_limit, Max),
        (   Count >= Max
        ->  throw(error(resource_error(max_pengines), _))
        ;   true
        )
    ;   true
    ),
    partition(pengine_create_option, Options, PengineOptions, RestOptions),
    thread_create_in_pool(
        Application,
        pengine_main(Queue, PengineOptions, Application), ChildThread,
        [ at_exit(pengine_done)
        | RestOptions
        ]),
    option(destroy(Destroy), PengineOptions, true),
    pengine_register_local(Child, ChildThread, Queue, URL, Application, Destroy),
    thread_send_message(ChildThread, pengine_registered(Child)),
    (   option(id(Id), Options)
    ->  Id = Child
    ;   true
    ).

pengine_create_option(src_text(_)).
pengine_create_option(src_url(_)).
pengine_create_option(application(_)).
pengine_create_option(destroy(_)).
pengine_create_option(ask(_)).
pengine_create_option(template(_)).
pengine_create_option(bindings(_)).
pengine_create_option(chunk(_)).
pengine_create_option(alias(_)).
pengine_create_option(user(_)).


%!  pengine_done is det.
%
%   Called from the pengine thread   `at_exit`  option. Destroys _child_
%   pengines  using  pengine_destroy/1.  Cleaning  up   the  Pengine  is
%   synchronised by the `pengine_done` mutex. See read_event/6.

:- public
    pengine_done/0.

pengine_done :-
    thread_self(Me),
    (   thread_property(Me, status(exception('$aborted'))),
        thread_detach(Me),
        pengine_self(Pengine)
    ->  catch(pengine_reply(destroy(Pengine, abort(Pengine))),
              error(_,_), true)
    ;   true
    ),
    forall(child(_Name, Child),
           pengine_destroy(Child)),
    pengine_self(Id),
    protect_pengine(Id, pengine_unregister(Id)).


%!  pengine_main(+Parent, +Options, +Application)
%
%   Run a pengine main loop. First acknowledges its creation and run
%   pengine_main_loop/1.

:- thread_local wrap_first_answer_in_create_event/2.

:- meta_predicate
    pengine_prepare_source(:, +).

pengine_main(Parent, Options, Application) :-
    fix_streams,
    thread_get_message(pengine_registered(Self)),
    nb_setval(pengine_parent, Parent),
    pengine_register_user(Options),
    set_prolog_flag(mitigate_spectre, true),
    catch(in_temporary_module(
              Self,
              pengine_prepare_source(Application, Options),
              pengine_create_and_loop(Self, Application, Options)),
          prepare_source_failed,
          pengine_terminate(Self)).

pengine_create_and_loop(Self, Application, Options) :-
    setting(Application:slave_limit, SlaveLimit),
    CreateEvent = create(Self, [slave_limit(SlaveLimit)|Extra]),
    (   option(ask(Query0), Options)
    ->  asserta(wrap_first_answer_in_create_event(CreateEvent, Extra)),
        (   string(Query0)                      % string is not callable
        ->  (   option(template(TemplateS), Options)
            ->  Ask2 = Query0-TemplateS
            ;   Ask2 = Query0
            ),
            catch(ask_to_term(Ask2, Self, Query, Template, Bindings),
                  Error, true),
            (   var(Error)
            ->  true
            ;   send_error(Error),
                throw(prepare_source_failed)
            )
        ;   Query = Query0,
            option(template(Template), Options, Query),
            option(bindings(Bindings), Options, [])
        ),
        option(chunk(Chunk), Options, 1),
        pengine_ask(Self, Query,
                    [ template(Template),
                      chunk(Chunk),
                      bindings(Bindings)
                    ])
    ;   Extra = [],
        pengine_reply(CreateEvent)
    ),
    pengine_main_loop(Self).


%!  ask_to_term(+AskSpec, +Module, -Options, OptionsTail) is det.
%
%   Translate the AskSpec into a query, template and bindings. The trick
%   is that we must parse using the  operator declarations of the source
%   and we must make sure  variable   sharing  between  query and answer
%   template are known.

ask_to_term(Ask-Template, Module, Ask1, Template1, Bindings) :-
    !,
    format(string(AskTemplate), 't((~s),(~s))', [Template, Ask]),
    term_string(t(Template1,Ask1), AskTemplate,
                [ variable_names(Bindings0),
                  module(Module)
                ]),
    phrase(template_bindings(Template1, Bindings0), Bindings).
ask_to_term(Ask, Module, Ask1, Template, Bindings1) :-
    term_string(Ask1, Ask,
                [ variable_names(Bindings),
                  module(Module)
                ]),
    exclude(anon, Bindings, Bindings1),
    dict_create(Template, swish_default_template, Bindings1).

template_bindings(Var, Bindings) -->
    { var(Var) }, !,
    (   { var_binding(Bindings, Var, Binding)
        }
    ->  [Binding]
    ;   []
    ).
template_bindings([H|T], Bindings) -->
    !,
    template_bindings(H, Bindings),
    template_bindings(T, Bindings).
template_bindings(Compoound, Bindings) -->
    { compound(Compoound), !,
      compound_name_arguments(Compoound, _, Args)
    },
    template_bindings(Args, Bindings).
template_bindings(_, _) --> [].

var_binding(Bindings, Var, Binding) :-
    member(Binding, Bindings),
    arg(2, Binding, V),
    V == Var, !.

%!  fix_streams is det.
%
%   If we are a pengine that is   created  from a web server thread,
%   the current output points to a CGI stream.

fix_streams :-
    fix_stream(current_output).

fix_stream(Name) :-
    is_cgi_stream(Name),
    !,
    debug(pengine(stream), '~w is a CGI stream!', [Name]),
    set_stream(user_output, alias(Name)).
fix_stream(_).

%!  pengine_prepare_source(:Application, +Options) is det.
%
%   Load the source into the pengine's module.
%
%   @throws =prepare_source_failed= if it failed to prepare the
%           sources.

pengine_prepare_source(Module:Application, Options) :-
    setting(Application:program_space, SpaceLimit),
    set_module(Module:program_space(SpaceLimit)),
    delete_import_module(Module, user),
    add_import_module(Module, Application, start),
    catch(prep_module(Module, Application, Options), Error, true),
    (   var(Error)
    ->  true
    ;   send_error(Error),
        throw(prepare_source_failed)
    ).

prep_module(Module, Application, Options) :-
    maplist(copy_flag(Module, Application), [var_prefix]),
    forall(prepare_module(Module, Application, Options), true),
    setup_call_cleanup(
        '$set_source_module'(OldModule, Module),
        maplist(process_create_option(Module), Options),
        '$set_source_module'(OldModule)).

copy_flag(Module, Application, Flag) :-
    current_prolog_flag(Application:Flag, Value),
    !,
    set_prolog_flag(Module:Flag, Value).
copy_flag(_, _, _).

process_create_option(Application, src_text(Text)) :-
    !,
    pengine_src_text(Text, Application).
process_create_option(Application, src_url(URL)) :-
    !,
    pengine_src_url(URL, Application).
process_create_option(_, _).


%!  prepare_module(+Module, +Application, +Options) is semidet.
%
%   Hook, called to initialize  the   temporary  private module that
%   provides the working context of a pengine. This hook is executed
%   by the pengine's thread.  Preparing the source consists of three
%   steps:
%
%     1. Add Application as (first) default import module for Module
%     2. Call this hook
%     3. Compile the source provided by the the `src_text` and
%        `src_url` options
%
%   @arg    Module is a new temporary module (see
%           in_temporary_module/3) that may be (further) prepared
%           by this hook.
%   @arg    Application (also a module) associated to the pengine.
%   @arg    Options is passed from the environment and should
%           (currently) be ignored.


pengine_main_loop(ID) :-
    catch(guarded_main_loop(ID), abort_query, pengine_aborted(ID)).

pengine_aborted(ID) :-
    thread_self(Self),
    debug(pengine(abort), 'Aborting ~p (thread ~p)', [ID, Self]),
    empty_queue,
    destroy_or_continue(abort(ID)).


%!  guarded_main_loop(+Pengine) is det.
%
%   Executes state `2' of  the  pengine,   where  it  waits  for two
%   events:
%
%     - destroy
%     Terminate the pengine
%     - ask(:Goal, +Options)
%     Solve Goal.

guarded_main_loop(ID) :-
    pengine_request(Request),
    (   Request = destroy
    ->  debug(pengine(transition), '~q: 2 = ~q => 1', [ID, destroy]),
        pengine_terminate(ID)
    ;   Request = ask(Goal, Options)
    ->  debug(pengine(transition), '~q: 2 = ~q => 3', [ID, ask(Goal)]),
        ask(ID, Goal, Options)
    ;   debug(pengine(transition), '~q: 2 = ~q => 2', [ID, protocol_error]),
        pengine_reply(error(ID, error(protocol_error, _))),
        guarded_main_loop(ID)
    ).


pengine_terminate(ID) :-
    pengine_reply(destroy(ID)),
    thread_self(Me),            % Make the thread silently disappear
    thread_detach(Me).


%!  solve(+Chunk, +Template, :Goal, +ID) is det.
%
%   Solve Goal. Note that because we can ask for a new goal in state
%   `6', we must provide for an ancesteral cut (prolog_cut_to/1). We
%   need to be sure to  have  a   choice  point  before  we can call
%   prolog_current_choice/1. This is the reason   why this predicate
%   has two clauses.

solve(Chunk, Template, Goal, ID) :-
    prolog_current_choice(Choice),
    State = count(Chunk),
    statistics(cputime, Epoch),
    Time = time(Epoch),
    nb_current('$variable_names', Bindings),
    filter_template(Template, Bindings, Template2),
    '$current_typein_module'(CurrTypeIn),
    (   '$set_typein_module'(ID),
        call_cleanup(catch(findnsols_no_empty(State, Template2,
                                              set_projection(Goal, Bindings),
                                              Result),
                           Error, true),
                     query_done(Det, CurrTypeIn)),
        arg(1, Time, T0),
        statistics(cputime, T1),
        CPUTime is T1-T0,
        (   var(Error)
        ->  projection(Projection),
            (   var(Det)
            ->  pengine_reply(success(ID, Result, Projection,
                                      CPUTime, true)),
                more_solutions(ID, Choice, State, Time)
            ;   !,                      % commit
                destroy_or_continue(success(ID, Result, Projection,
                                            CPUTime, false))
            )
        ;   !,                          % commit
            (   Error == abort_query
            ->  throw(Error)
            ;   destroy_or_continue(error(ID, Error))
            )
        )
    ;   !,                              % commit
        arg(1, Time, T0),
        statistics(cputime, T1),
        CPUTime is T1-T0,
        destroy_or_continue(failure(ID, CPUTime))
    ).
solve(_, _, _, _).                      % leave a choice point

query_done(true, CurrTypeIn) :-
    '$set_typein_module'(CurrTypeIn).


%!  set_projection(:Goal, +Bindings)
%
%   findnsols/4 copies its goal  and   template  to  avoid instantiation
%   thereof when it stops after finding   N solutions. Using this helper
%   we can a renamed version of Bindings that we can set.

set_projection(Goal, Bindings) :-
    b_setval('$variable_names', Bindings),
    call(Goal).

projection(Projection) :-
    nb_current('$variable_names', Bindings),
    !,
    maplist(var_name, Bindings, Projection).
projection([]).

%!  filter_template(+Template0, +Bindings, -Template) is det.
%
%   Establish the final template. This is   there  because hooks such as
%   goal_expansion/2 and the SWISH query  hooks   can  modify the set of
%   bindings.
%
%   @bug Projection and template handling is pretty messy.

filter_template(Template0, Bindings, Template) :-
    is_dict(Template0, swish_default_template),
    !,
    dict_create(Template, swish_default_template, Bindings).
filter_template(Template, _Bindings, Template).

findnsols_no_empty(N, Template, Goal, List) :-
    findnsols(N, Template, Goal, List),
    List \== [].

destroy_or_continue(Event) :-
    arg(1, Event, ID),
    (   pengine_property(ID, destroy(true))
    ->  thread_self(Me),
        thread_detach(Me),
        pengine_reply(destroy(ID, Event))
    ;   pengine_reply(Event),
        guarded_main_loop(ID)
    ).

%!  more_solutions(+Pengine, +Choice, +State, +Time)
%
%   Called after a solution was found while  there can be more. This
%   is state `6' of the state machine. It processes these events:
%
%     * stop
%     Go back via state `7' to state `2' (guarded_main_loop/1)
%     * next
%     Fail.  This causes solve/3 to backtrack on the goal asked,
%     providing at most the current `chunk` solutions.
%     * next(Count)
%     As `next`, but sets the new chunk-size to Count.
%     * ask(Goal, Options)
%     Ask another goal.  Note that we must commit the choice point
%     of the previous goal asked for.

more_solutions(ID, Choice, State, Time) :-
    pengine_request(Event),
    more_solutions(Event, ID, Choice, State, Time).

more_solutions(stop, ID, _Choice, _State, _Time) :-
    !,
    debug(pengine(transition), '~q: 6 = ~q => 7', [ID, stop]),
    destroy_or_continue(stop(ID)).
more_solutions(next, ID, _Choice, _State, Time) :-
    !,
    debug(pengine(transition), '~q: 6 = ~q => 3', [ID, next]),
    statistics(cputime, T0),
    nb_setarg(1, Time, T0),
    fail.
more_solutions(next(Count), ID, _Choice, State, Time) :-
    Count > 0,
    !,
    debug(pengine(transition), '~q: 6 = ~q => 3', [ID, next(Count)]),
    nb_setarg(1, State, Count),
    statistics(cputime, T0),
    nb_setarg(1, Time, T0),
    fail.
more_solutions(ask(Goal, Options), ID, Choice, _State, _Time) :-
    !,
    debug(pengine(transition), '~q: 6 = ~q => 3', [ID, ask(Goal)]),
    prolog_cut_to(Choice),
    ask(ID, Goal, Options).
more_solutions(destroy, ID, _Choice, _State, _Time) :-
    !,
    debug(pengine(transition), '~q: 6 = ~q => 1', [ID, destroy]),
    pengine_terminate(ID).
more_solutions(Event, ID, Choice, State, Time) :-
    debug(pengine(transition), '~q: 6 = ~q => 6', [ID, protocol_error(Event)]),
    pengine_reply(error(ID, error(protocol_error, _))),
    more_solutions(ID, Choice, State, Time).

%!  ask(+Pengine, :Goal, +Options)
%
%   Migrate from state `2' to `3'.  This predicate validates that it
%   is safe to call Goal using safe_goal/1 and then calls solve/3 to
%   prove the goal. It takes care of the chunk(N) option.

ask(ID, Goal, Options) :-
    catch(prepare_goal(ID, Goal, Goal1, Options), Error, true),
    !,
    (   var(Error)
    ->  option(template(Template), Options, Goal),
        option(chunk(N), Options, 1),
        solve(N, Template, Goal1, ID)
    ;   pengine_reply(error(ID, Error)),
        guarded_main_loop(ID)
    ).

%!  prepare_goal(+Pengine, +GoalIn, -GoalOut, +Options) is det.
%
%   Prepare GoalIn for execution in Pengine.   This  implies we must
%   perform goal expansion and, if the   system  is sandboxed, check
%   the sandbox.
%
%   Note that expand_goal(Module:GoalIn, GoalOut) is  what we'd like
%   to write, but this does not work correctly if the user wishes to
%   expand `X:Y` while interpreting `X` not   as the module in which
%   to run `Y`. This happens in the  CQL package. Possibly we should
%   disallow this reinterpretation?

prepare_goal(ID, Goal0, Module:Goal, Options) :-
    option(bindings(Bindings), Options, []),
    b_setval('$variable_names', Bindings),
    (   prepare_goal(Goal0, Goal1, Options)
    ->  true
    ;   Goal1 = Goal0
    ),
    get_pengine_module(ID, Module),
    setup_call_cleanup(
        '$set_source_module'(Old, Module),
        expand_goal(Goal1, Goal),
        '$set_source_module'(_, Old)),
    (   pengine_not_sandboxed(ID)
    ->  true
    ;   get_pengine_application(ID, App),
        setting(App:safe_goal_limit, Limit),
        catch(call_with_time_limit(
                  Limit,
                  safe_goal(Module:Goal)), E, true)
    ->  (   var(E)
        ->  true
        ;   E = time_limit_exceeded
        ->  throw(error(sandbox(time_limit_exceeded, Limit),_))
        ;   throw(E)
        )
    ).


%%  prepare_goal(+Goal0, -Goal1, +Options) is semidet.
%
%   Pre-preparation hook for running Goal0. The hook runs in the context
%   of the pengine. Goal is the raw   goal  given to _ask_. The returned
%   Goal1 is subject  to  goal   expansion  (expand_goal/2)  and sandbox
%   validation (safe_goal/1) prior to  execution.   If  this goal fails,
%   Goal0 is used for further processing.
%
%   @arg Options provides the options as given to _ask_


%%  pengine_not_sandboxed(+Pengine) is semidet.
%
%   True when pengine does not operate in sandboxed mode. This implies a
%   user must be  registered  by   authentication_hook/3  and  the  hook
%   pengines:not_sandboxed(User, Application) must succeed.

pengine_not_sandboxed(ID) :-
    pengine_user(ID, User),
    pengine_property(ID, application(App)),
    not_sandboxed(User, App),
    !.

%%  not_sandboxed(+User, +Application) is semidet.
%
%   This hook is called to see whether the Pengine must be executed in a
%   protected environment. It is only called after authentication_hook/3
%   has confirmed the authentity  of  the   current  user.  If this hook
%   succeeds, both loading the code and  executing the query is executed
%   without enforcing sandbox security.  Typically, one should:
%
%     1. Provide a safe user authentication hook.
%     2. Enable HTTPS in the server or put it behind an HTTPS proxy and
%        ensure that the network between the proxy and the pengine
%        server can be trusted.


/** pengine_pull_response(+Pengine, +Options) is det

Pulls a response (an event term) from the  slave Pengine if Pengine is a
remote process, else does nothing at all.
*/

pengine_pull_response(Pengine, Options) :-
    pengine_remote(Pengine, Server),
    !,
    remote_pengine_pull_response(Server, Pengine, Options).
pengine_pull_response(_ID, _Options).


/** pengine_input(+Prompt, -Term) is det

Sends Prompt to the master (parent) pengine and waits for input. Note that Prompt may be
any term, compound as well as atomic.
*/

pengine_input(Prompt, Term) :-
    pengine_self(Self),
    pengine_parent(Parent),
    pengine_reply(Parent, prompt(Self, Prompt)),
    pengine_request(Request),
    (   Request = input(Input)
    ->  Term = Input
    ;   Request == destroy
    ->  abort
    ;   throw(error(protocol_error,_))
    ).


/** pengine_respond(+Pengine, +Input, +Options) is det

Sends a response in the form of the term Input to a slave (child) pengine
that has prompted its master (parent) for input.

Defined in terms of pengine_send/3, as follows:

==
pengine_respond(Pengine, Input, Options) :-
    pengine_send(Pengine, input(Input), Options).
==

*/

pengine_respond(Pengine, Input, Options) :-
    pengine_send(Pengine, input(Input), Options).


%!  send_error(+Error) is det.
%
%   Send an error to my parent.   Remove non-readable blobs from the
%   error term first using replace_blobs/2. If  the error contains a
%   stack-trace, this is resolved to a string before sending.

send_error(error(Formal, context(prolog_stack(Frames), Message))) :-
    is_list(Frames),
    !,
    with_output_to(string(Stack),
                   print_prolog_backtrace(current_output, Frames)),
    pengine_self(Self),
    replace_blobs(Formal, Formal1),
    replace_blobs(Message, Message1),
    pengine_reply(error(Self, error(Formal1,
                                    context(prolog_stack(Stack), Message1)))).
send_error(Error) :-
    pengine_self(Self),
    replace_blobs(Error, Error1),
    pengine_reply(error(Self, Error1)).

%!  replace_blobs(Term0, Term) is det.
%
%   Copy Term0 to Term, replacing non-text   blobs. This is required
%   for error messages that may hold   streams  and other handles to
%   non-readable objects.

replace_blobs(Blob, Atom) :-
    blob(Blob, Type), Type \== text,
    !,
    format(atom(Atom), '~p', [Blob]).
replace_blobs(Term0, Term) :-
    compound(Term0),
    !,
    compound_name_arguments(Term0, Name, Args0),
    maplist(replace_blobs, Args0, Args),
    compound_name_arguments(Term, Name, Args).
replace_blobs(Term, Term).


/*================= Remote pengines =======================
*/


remote_pengine_create(BaseURL, Options) :-
    partition(pengine_create_option, Options, PengineOptions0, RestOptions),
        (       option(ask(Query), PengineOptions0),
                \+ option(template(_Template), PengineOptions0)
        ->      PengineOptions = [template(Query)|PengineOptions0]
        ;       PengineOptions = PengineOptions0
        ),
    options_to_dict(PengineOptions, PostData),
    remote_post_rec(BaseURL, create, PostData, Reply, RestOptions),
    arg(1, Reply, ID),
    (   option(id(ID2), Options)
    ->  ID = ID2
    ;   true
    ),
    option(alias(Name), Options, ID),
    assert(child(Name, ID)),
    (   (   functor(Reply, create, _)   % actually created
        ;   functor(Reply, output, _)   % compiler messages
        )
    ->  option(application(Application), PengineOptions, pengine_sandbox),
        option(destroy(Destroy), PengineOptions, true),
        pengine_register_remote(ID, BaseURL, Application, Destroy)
    ;   true
    ),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

options_to_dict(Options, Dict) :-
    select_option(ask(Ask), Options, Options1),
    select_option(template(Template), Options1, Options2),
    !,
    no_numbered_var_in(Ask+Template),
    findall(AskString-TemplateString,
            ask_template_to_strings(Ask, Template, AskString, TemplateString),
            [ AskString-TemplateString ]),
    options_to_dict(Options2, Dict0),
    Dict = Dict0.put(_{ask:AskString,template:TemplateString}).
options_to_dict(Options, Dict) :-
    maplist(prolog_option, Options, Options1),
    dict_create(Dict, _, Options1).

no_numbered_var_in(Term) :-
    sub_term(Sub, Term),
    subsumes_term('$VAR'(_), Sub),
    !,
    domain_error(numbered_vars_free_term, Term).
no_numbered_var_in(_).

ask_template_to_strings(Ask, Template, AskString, TemplateString) :-
    numbervars(Ask+Template, 0, _),
    WOpts = [ numbervars(true), ignore_ops(true), quoted(true) ],
    format(string(AskTemplate), '~W\n~W', [ Ask, WOpts,
                                            Template, WOpts
                                          ]),
    split_string(AskTemplate, "\n", "", [AskString, TemplateString]).

prolog_option(Option0, Option) :-
    create_option_type(Option0, term),
    !,
    Option0 =.. [Name,Value],
    format(string(String), '~k', [Value]),
    Option =.. [Name,String].
prolog_option(Option, Option).

create_option_type(ask(_),         term).
create_option_type(template(_),    term).
create_option_type(application(_), atom).

remote_pengine_send(BaseURL, ID, Event, Options) :-
    remote_send_rec(BaseURL, send, ID, [event=Event], Reply, Options),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

remote_pengine_pull_response(BaseURL, ID, Options) :-
    remote_send_rec(BaseURL, pull_response, ID, [], Reply, Options),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

remote_pengine_abort(BaseURL, ID, Options) :-
    remote_send_rec(BaseURL, abort, ID, [], Reply, Options),
    thread_self(Queue),
    pengine_reply(Queue, Reply).

%!  remote_send_rec(+Server, +Action, +ID, +Params, -Reply, +Options)
%
%   Issue a GET request on Server and   unify Reply with the replied
%   term.

remote_send_rec(Server, Action, ID, [event=Event], Reply, Options) :-
    !,
    server_url(Server, Action, [id=ID], URL),
    http_open(URL, Stream,              % putting this in setup_call_cleanup/3
              [ post(prolog(Event))     % makes it impossible to interrupt.
              | Options
              ]),
    call_cleanup(
        read_prolog_reply(Stream, Reply),
        close(Stream)).
remote_send_rec(Server, Action, ID, Params, Reply, Options) :-
    server_url(Server, Action, [id=ID|Params], URL),
    http_open(URL, Stream, Options),
    call_cleanup(
        read_prolog_reply(Stream, Reply),
        close(Stream)).

remote_post_rec(Server, Action, Data, Reply, Options) :-
    server_url(Server, Action, [], URL),
    probe(Action, URL),
    http_open(URL, Stream,
              [ post(json(Data))
              | Options
              ]),
    call_cleanup(
        read_prolog_reply(Stream, Reply),
        close(Stream)).

%!  probe(+Action, +URL) is det.
%
%   Probe the target. This is a  good   idea  before posting a large
%   document and be faced with an authentication challenge. Possibly
%   we should make this an option for simpler scenarios.

probe(create, URL) :-
    !,
    http_open(URL, Stream, [method(options)]),
    close(Stream).
probe(_, _).

read_prolog_reply(In, Reply) :-
    set_stream(In, encoding(utf8)),
    read(In, Reply0),
    rebind_cycles(Reply0, Reply).

rebind_cycles(@(Reply, Bindings), Reply) :-
    is_list(Bindings),
    !,
    maplist(bind, Bindings).
rebind_cycles(Reply, Reply).

bind(Var = Value) :-
    Var = Value.

server_url(Server, Action, Params, URL) :-
    uri_components(Server, Components0),
    uri_query_components(Query, Params),
    uri_data(path, Components0, Path0),
    atom_concat('pengine/', Action, PAction),
    directory_file_path(Path0, PAction, Path),
    uri_data(path, Components0, Path, Components),
    uri_data(search, Components, Query),
    uri_components(URL, Components).


/** pengine_event(?EventTerm) is det.
    pengine_event(?EventTerm, +Options) is det.

Examines the pengine's event queue  and   if  necessary blocks execution
until a term that unifies to Term  arrives   in  the queue. After a term
from the queue has been unified to Term,   the  term is deleted from the
queue.

   Valid options are:

   * timeout(+Time)
     Time is a float or integer and specifies the maximum time to wait
     in seconds. If no event has arrived before the time is up EventTerm
     is bound to the atom =timeout=.
   * listen(+Id)
     Only listen to events from the pengine identified by Id.
*/

pengine_event(Event) :-
    pengine_event(Event, []).

pengine_event(Event, Options) :-
    thread_self(Self),
    option(listen(Id), Options, _),
    (   thread_get_message(Self, pengine_event(Id, Event), Options)
    ->  true
    ;   Event = timeout
    ),
    update_remote_destroy(Event).

update_remote_destroy(Event) :-
    destroy_event(Event),
    arg(1, Event, Id),
    pengine_remote(Id, _Server),
    !,
    pengine_unregister_remote(Id).
update_remote_destroy(_).

destroy_event(destroy(_)).
destroy_event(destroy(_,_)).
destroy_event(create(_,Features)) :-
    memberchk(answer(Answer), Features),
    !,
    nonvar(Answer),
    destroy_event(Answer).


/** pengine_event_loop(:Closure, +Options) is det

Starts an event loop accepting event terms   sent to the current pengine
or thread. For each such  event   E,  calls  ignore(call(Closure, E)). A
closure thus acts as a _handler_  for   the  event. Some events are also
treated specially:

   * create(ID, Term)
     The ID is placed in a list of active pengines.

   * destroy(ID)
     The ID is removed from the list of active pengines. When the last
     pengine ID is removed, the loop terminates.

   * output(ID, Term)
     The predicate pengine_pull_response/2 is called.

Valid options are:

   * autoforward(+To)
     Forwards received event terms to slaves. To is either =all=,
     =all_but_sender= or a Prolog list of NameOrIDs. [not yet
     implemented]

*/

pengine_event_loop(Closure, Options) :-
    child(_,_),
    !,
    pengine_event(Event),
    (   option(autoforward(all), Options) % TODO: Implement all_but_sender and list of IDs
    ->  forall(child(_,ID), pengine_send(ID, Event))
    ;   true
    ),
    pengine_event_loop(Event, Closure, Options).
pengine_event_loop(_, _).

:- meta_predicate
    pengine_process_event(+, 1, -, +).

pengine_event_loop(Event, Closure, Options) :-
    pengine_process_event(Event, Closure, Continue, Options),
    (   Continue == true
    ->  pengine_event_loop(Closure, Options)
    ;   true
    ).

pengine_process_event(create(ID, T), Closure, Continue, Options) :-
    debug(pengine(transition), '~q: 1 = /~q => 2', [ID, create(T)]),
    (   select(answer(First), T, T1)
    ->  ignore(call(Closure, create(ID, T1))),
        pengine_process_event(First, Closure, Continue, Options)
    ;   ignore(call(Closure, create(ID, T))),
        Continue = true
    ).
pengine_process_event(output(ID, Msg), Closure, true, _Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 4', [ID, output(Msg)]),
    ignore(call(Closure, output(ID, Msg))),
    pengine_pull_response(ID, []).
pengine_process_event(debug(ID, Msg), Closure, true, _Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 4', [ID, debug(Msg)]),
    ignore(call(Closure, debug(ID, Msg))),
    pengine_pull_response(ID, []).
pengine_process_event(prompt(ID, Term), Closure, true, _Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 5', [ID, prompt(Term)]),
    ignore(call(Closure, prompt(ID, Term))).
pengine_process_event(success(ID, Sol, _Proj, _Time, More), Closure, true, _) :-
    debug(pengine(transition), '~q: 3 = /~q => 6/2', [ID, success(Sol, More)]),
    ignore(call(Closure, success(ID, Sol, More))).
pengine_process_event(failure(ID, _Time), Closure, true, _Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 2', [ID, failure]),
    ignore(call(Closure, failure(ID))).
pengine_process_event(error(ID, Error), Closure, Continue, _Options) :-
    debug(pengine(transition), '~q: 3 = /~q => 2', [ID, error(Error)]),
    (   call(Closure, error(ID, Error))
    ->  Continue = true
    ;   forall(child(_,Child), pengine_destroy(Child)),
        throw(Error)
    ).
pengine_process_event(stop(ID), Closure, true, _Options) :-
    debug(pengine(transition), '~q: 7 = /~q => 2', [ID, stop]),
    ignore(call(Closure, stop(ID))).
pengine_process_event(destroy(ID, Event), Closure, Continue, Options) :-
    pengine_process_event(Event, Closure, _, Options),
    pengine_process_event(destroy(ID), Closure, Continue, Options).
pengine_process_event(destroy(ID), Closure, true, _Options) :-
    retractall(child(_,ID)),
    debug(pengine(transition), '~q: 1 = /~q => 0', [ID, destroy]),
    ignore(call(Closure, destroy(ID))).


/** pengine_rpc(+URL, +Query) is nondet.
    pengine_rpc(+URL, +Query, +Options) is nondet.

Semantically equivalent to the sequence below,  except that the query is
executed in (and in the Prolog context   of) the pengine server referred
to by URL, rather than locally.

  ==
    copy_term_nat(Query, Copy),  % attributes are not copied to the server
    call(Copy),			 % executed on server at URL
    Query = Copy.
  ==

Valid options are:

    - chunk(+Integer)
      Can be used to reduce the number of network roundtrips being made.
      See pengine_ask/3.
    - timeout(+Time)
      Wait at most Time seconds for the next event from the server.
      The default is defined by the setting `pengines:time_limit`.

Remaining  options  (except   the   server    option)   are   passed  to
pengine_create/1.
*/

pengine_rpc(URL, Query) :-
    pengine_rpc(URL, Query, []).

pengine_rpc(URL, Query, M:Options0) :-
    translate_local_sources(Options0, Options1, M),
    (  option(timeout(_), Options1)
    -> Options = Options1
    ;  setting(time_limit, Limit),
       Options = [timeout(Limit)|Options1]
    ),
    term_variables(Query, Vars),
    Template =.. [v|Vars],
    State = destroy(true),              % modified by process_event/4
    setup_call_catcher_cleanup(
        pengine_create([ ask(Query),
                         template(Template),
                         server(URL),
                         id(Id)
                       | Options
                       ]),
        wait_event(Template, State, [listen(Id)|Options]),
        Why,
        pengine_destroy_and_wait(State, Id, Why)).

pengine_destroy_and_wait(destroy(true), Id, Why) :-
    !,
    debug(pengine(rpc), 'Destroying RPC because of ~p', [Why]),
    pengine_destroy(Id),
    wait_destroy(Id, 10).
pengine_destroy_and_wait(_, _, Why) :-
    debug(pengine(rpc), 'Not destroying RPC (~p)', [Why]).

wait_destroy(Id, _) :-
    \+ child(_, Id),
    !.
wait_destroy(Id, N) :-
    pengine_event(Event, [listen(Id),timeout(10)]),
    !,
    (   destroy_event(Event)
    ->  retractall(child(_,Id))
    ;   succ(N1, N)
    ->  wait_destroy(Id, N1)
    ;   debug(pengine(rpc), 'RPC did not answer to destroy ~p', [Id]),
        pengine_unregister_remote(Id),
        retractall(child(_,Id))
    ).

wait_event(Template, State, Options) :-
    pengine_event(Event, Options),
    debug(pengine(event), 'Received ~p', [Event]),
    process_event(Event, Template, State, Options).

process_event(create(_ID, Features), Template, State, Options) :-
    memberchk(answer(First), Features),
    process_event(First, Template, State, Options).
process_event(error(_ID, Error), _Template, _, _Options) :-
    throw(Error).
process_event(failure(_ID, _Time), _Template, _, _Options) :-
    fail.
process_event(prompt(ID, Prompt), Template, State, Options) :-
    pengine_rpc_prompt(ID, Prompt, Reply),
    pengine_send(ID, input(Reply)),
    wait_event(Template, State, Options).
process_event(output(ID, Term), Template, State, Options) :-
    pengine_rpc_output(ID, Term),
    pengine_pull_response(ID, Options),
    wait_event(Template, State, Options).
process_event(debug(ID, Message), Template, State, Options) :-
    debug(pengine(debug), '~w', [Message]),
    pengine_pull_response(ID, Options),
    wait_event(Template, State, Options).
process_event(success(_ID, Solutions, _Proj, _Time, false),
              Template, _, _Options) :-
    !,
    member(Template, Solutions).
process_event(success(ID, Solutions, _Proj, _Time, true),
              Template, State, Options) :-
    (   member(Template, Solutions)
    ;   pengine_next(ID, Options),
        wait_event(Template, State, Options)
    ).
process_event(destroy(ID, Event), Template, State, Options) :-
    !,
    retractall(child(_,ID)),
    nb_setarg(1, State, false),
    debug(pengine(destroy), 'State: ~p~n', [State]),
    process_event(Event, Template, State, Options).
% compatibility with older versions of the protocol.
process_event(success(ID, Solutions, Time, More),
              Template, State, Options) :-
    process_event(success(ID, Solutions, _Proj, Time, More),
                  Template, State, Options).


pengine_rpc_prompt(ID, Prompt, Term) :-
    prompt(ID, Prompt, Term0),
    !,
    Term = Term0.
pengine_rpc_prompt(_ID, Prompt, Term) :-
    setup_call_cleanup(
        prompt(Old, Prompt),
        read(Term),
        prompt(_, Old)).

pengine_rpc_output(ID, Term) :-
    output(ID, Term),
    !.
pengine_rpc_output(_ID, Term) :-
    print(Term).

%%  prompt(+ID, +Prompt, -Term) is semidet.
%
%   Hook to handle pengine_input/2 from the remote pengine. If the hooks
%   fails, pengine_rpc/3 calls read/1 using the current prompt.

:- multifile prompt/3.

%%  output(+ID, +Term) is semidet.
%
%   Hook to handle pengine_output/1 from the remote pengine. If the hook
%   fails, it calls print/1 on Term.

:- multifile output/2.


/*================= HTTP handlers =======================
*/

%   Declare  HTTP  locations  we  serve  and   how.  Note  that  we  use
%   time_limit(inifinite) because pengines have their  own timeout. Also
%   note that we use spawn. This  is   needed  because we can easily get
%   many clients waiting for  some  action   on  a  pengine to complete.
%   Without spawning, we would quickly exhaust   the  worker pool of the
%   HTTP server.
%
%   FIXME: probably we should wait for a   short time for the pengine on
%   the default worker thread. Only if  that   time  has expired, we can
%   call http_spawn/2 to continue waiting on   a  new thread. That would
%   improve the performance and reduce the usage of threads.

:- http_handler(root(pengine),               http_404([]),
                [ id(pengines) ]).
:- http_handler(root(pengine/create),        http_pengine_create,
                [ time_limit(infinite), spawn([]) ]).
:- http_handler(root(pengine/send),          http_pengine_send,
                [ time_limit(infinite), spawn([]) ]).
:- http_handler(root(pengine/pull_response), http_pengine_pull_response,
                [ time_limit(infinite), spawn([]) ]).
:- http_handler(root(pengine/abort),         http_pengine_abort,         []).
:- http_handler(root(pengine/detach),        http_pengine_detach,        []).
:- http_handler(root(pengine/list),          http_pengine_list,          []).
:- http_handler(root(pengine/ping),          http_pengine_ping,          []).
:- http_handler(root(pengine/destroy_all),   http_pengine_destroy_all,   []).

:- http_handler(root(pengine/'pengines.js'),
                http_reply_file(library('http/web/js/pengines.js'), []), []).
:- http_handler(root(pengine/'plterm.css'),
                http_reply_file(library('http/web/css/plterm.css'), []), []).


%%  http_pengine_create(+Request)
%
%   HTTP POST handler  for  =/pengine/create=.   This  API  accepts  the
%   pengine  creation  parameters  both  as  =application/json=  and  as
%   =www-form-encoded=.  Accepted parameters:
%
%     | **Parameter** | **Default**       | **Comment**                |
%     |---------------|-------------------|----------------------------|
%     | format        | `prolog`          | Output format              |
%     | application   | `pengine_sandbox` | Pengine application        |
%     | chunk         | 1                 | Chunk-size for results     |
%     | solutions     | chunked           | If `all`, emit all results |
%     | ask           | -                 | The query                  |
%     | template      | -                 | Output template            |
%     | src_text      | ""                | Program                    |
%     | src_url       | -                 | Program to download        |
%     | disposition   | -                 | Download location          |
%
%     Note that solutions=all internally  uses   chunking  to obtain the
%     results from the pengine, but the results are combined in a single
%     HTTP reply. This is currently only  implemented by the CSV backend
%     that is part of SWISH for   downloading unbounded result sets with
%     limited memory resources.

http_pengine_create(Request) :-
    reply_options(Request, [post]),
    !.
http_pengine_create(Request) :-
    memberchk(content_type(CT), Request),
    sub_atom(CT, 0, _, _, 'application/json'),
    !,
    http_read_json_dict(Request, Dict),
    dict_atom_option(format, Dict, Format, prolog),
    dict_atom_option(application, Dict, Application, pengine_sandbox),
    http_pengine_create(Request, Application, Format, Dict).
http_pengine_create(Request) :-
    Optional = [optional(true)],
    OptString = [string|Optional],
    Form = [ format(Format, [default(prolog)]),
             application(Application, [default(pengine_sandbox)]),
             chunk(_, [integer, default(1)]),
             solutions(_, [oneof([all,chunked]), default(chunked)]),
             ask(_, OptString),
             template(_, OptString),
             src_text(_, OptString),
             disposition(_, OptString),
             src_url(_, Optional)
           ],
    http_parameters(Request, Form),
    form_dict(Form, Dict),
    http_pengine_create(Request, Application, Format, Dict).

dict_atom_option(Key, Dict, Atom, Default) :-
    (   get_dict(Key, Dict, String)
    ->  atom_string(Atom, String)
    ;   Atom = Default
    ).

form_dict(Form, Dict) :-
    form_values(Form, Pairs),
    dict_pairs(Dict, _, Pairs).

form_values([], []).
form_values([H|T], Pairs) :-
    arg(1, H, Value),
    nonvar(Value),
    !,
    functor(H, Name, _),
    Pairs = [Name-Value|PairsT],
    form_values(T, PairsT).
form_values([_|T], Pairs) :-
    form_values(T, Pairs).

%!  http_pengine_create(+Request, +Application, +Format, +OptionsDict)


http_pengine_create(Request, Application, Format, Dict) :-
    current_application(Application),
    !,
    allowed(Request, Application),
    authenticate(Request, Application, UserOptions),
    dict_to_options(Dict, Application, CreateOptions0),
    append(UserOptions, CreateOptions0, CreateOptions),
    pengine_uuid(Pengine),
    message_queue_create(Queue, [max_size(25)]),
    setting(Application:time_limit, TimeLimit),
    get_time(Now),
    asserta(pengine_queue(Pengine, Queue, TimeLimit, Now)),
    broadcast(pengine(create(Pengine, Application, CreateOptions))),
    create(Queue, Pengine, CreateOptions, http, Application),
    create_wait_and_output_result(Pengine, Queue, Format,
                                  TimeLimit, Dict),
    gc_abandoned_queues.
http_pengine_create(_Request, Application, Format, _Dict) :-
    Error = existence_error(pengine_application, Application),
    pengine_uuid(ID),
    output_result(Format, error(ID, error(Error, _))).


dict_to_options(Dict, Application, CreateOptions) :-
    dict_pairs(Dict, _, Pairs),
    pairs_create_options(Pairs, Application, CreateOptions).

pairs_create_options([], _, []) :- !.
pairs_create_options([N-V0|T0], App, [Opt|T]) :-
    Opt =.. [N,V],
    pengine_create_option(Opt), N \== user,
    !,
    (   create_option_type(Opt, atom)
    ->  atom_string(V, V0)               % term creation must be done if
    ;   V = V0                           % we created the source and know
    ),                                   % the operators.
    pairs_create_options(T0, App, T).
pairs_create_options([_|T0], App, T) :-
    pairs_create_options(T0, App, T).

%!  wait_and_output_result(+Pengine, +Queue,
%!                         +Format, +TimeLimit) is det.
%
%   Wait for the Pengine's Queue and if  there is a message, send it
%   to the requester using  output_result/1.   If  Pengine  does not
%   answer within the time specified   by  the setting =time_limit=,
%   Pengine is aborted and the  result is error(time_limit_exceeded,
%   _).

wait_and_output_result(Pengine, Queue, Format, TimeLimit) :-
    (   catch(thread_get_message(Queue, pengine_event(_, Event),
                                 [ timeout(TimeLimit)
                                 ]),
              Error, true)
    ->  (   var(Error)
        ->  debug(pengine(wait), 'Got ~q from ~q', [Event, Queue]),
            ignore(destroy_queue_from_http(Pengine, Event, Queue)),
            protect_pengine(Pengine, output_result(Format, Event))
        ;   output_result(Format, died(Pengine))
        )
    ;   time_limit_exceeded(Pengine, Format)
    ).

%!  create_wait_and_output_result(+Pengine, +Queue, +Format,
%!                                +TimeLimit, +Dict) is det.
%
%   Intercepts  the  `solutions=all'  case    used  for  downloading
%   results. Dict may contain a  `disposition`   key  to  denote the
%   download location.

create_wait_and_output_result(Pengine, Queue, Format, TimeLimit, Dict) :-
    get_dict(solutions, Dict, all),
    !,
    between(1, infinite, Page),
    (   catch(thread_get_message(Queue, pengine_event(_, Event),
                                 [ timeout(TimeLimit)
                                 ]),
              Error, true)
    ->  (   var(Error)
        ->  debug(pengine(wait), 'Page ~D: got ~q from ~q', [Page, Event, Queue]),
            (   destroy_queue_from_http(Pengine, Event, Queue)
            ->  !,
                protect_pengine(Pengine,
                                output_result(Format, page(Page, Event), Dict))
            ;   is_more_event(Event)
            ->  pengine_thread(Pengine, Thread),
                thread_send_message(Thread, pengine_request(next)),
                protect_pengine(Pengine,
                                output_result(Format, page(Page, Event), Dict)),
                fail
            ;   !,
                protect_pengine(Pengine,
                                output_result(Format, page(Page, Event), Dict))
            )
        ;   !, output_result(Format, died(Pengine))
        )
    ;   !, time_limit_exceeded(Pengine, Format)
    ),
    !.
create_wait_and_output_result(Pengine, Queue, Format, TimeLimit, _Dict) :-
    wait_and_output_result(Pengine, Queue, Format, TimeLimit).

is_more_event(success(_Id, _Answers, _Projection, _Time, true)).
is_more_event(create(_, Options)) :-
    memberchk(answer(Event), Options),
    is_more_event(Event).



%!  time_limit_exceeded(+Pengine, +Format)
%
%   The Pengine did not reply within its time limit. Send a reply to the
%   client in the requested format and interrupt the Pengine.
%
%   @bug Ideally, if the Pengine has `destroy` set to `false`, we should
%   get the Pengine back to its main   loop.  Unfortunately we only have
%   normal exceptions that may be  caught   by  the  Pengine and `abort`
%   which cannot be caught and thus destroys the Pengine.

time_limit_exceeded(Pengine, Format) :-
    call_cleanup(
        pengine_destroy(Pengine, [force(true)]),
        output_result(Format,
                      destroy(Pengine,
                              error(Pengine, time_limit_exceeded)))).


%!  destroy_queue_from_http(+Pengine, +Event, +Queue) is semidet.
%
%   Consider destroying the output queue   for Pengine after sending
%   Event back to the HTTP client. We can destroy the queue if
%
%     - The pengine already died (output_queue/3 is present) and
%       the queue is empty.
%     - This is a final (destroy) event.
%
%   @tbd    If the client did not request all output, the queue will
%           not be destroyed.  We need some timeout and GC for that.

destroy_queue_from_http(ID, _, Queue) :-
    output_queue(ID, Queue, _),
    !,
    destroy_queue_if_empty(Queue).
destroy_queue_from_http(ID, Event, Queue) :-
    debug(pengine(destroy), 'DESTROY? ~p', [Event]),
    is_destroy_event(Event),
    !,
    message_queue_property(Queue, size(Waiting)),
    debug(pengine(destroy), 'Destroy ~p (waiting ~D)', [Queue, Waiting]),
    with_mutex(pengine, sync_destroy_queue_from_http(ID, Queue)).

is_destroy_event(destroy(_)).
is_destroy_event(destroy(_,_)).
is_destroy_event(create(_, Options)) :-
    memberchk(answer(Event), Options),
    is_destroy_event(Event).

destroy_queue_if_empty(Queue) :-
    thread_peek_message(Queue, _),
    !.
destroy_queue_if_empty(Queue) :-
    retractall(output_queue(_, Queue, _)),
    message_queue_destroy(Queue).

%!  gc_abandoned_queues
%
%   Check whether there are queues  that   have  been abadoned. This
%   happens if the stream contains output events and not all of them
%   are read by the client.

:- dynamic
    last_gc/1.

gc_abandoned_queues :-
    consider_queue_gc,
    !,
    get_time(Now),
    (   output_queue(_, Queue, Time),
        Now-Time > 15*60,
        retract(output_queue(_, Queue, Time)),
        message_queue_destroy(Queue),
        fail
    ;   retractall(last_gc(_)),
        asserta(last_gc(Now))
    ).
gc_abandoned_queues.

consider_queue_gc :-
    predicate_property(output_queue(_,_,_), number_of_clauses(N)),
    N > 100,
    (   last_gc(Time),
        get_time(Now),
        Now-Time > 5*60
    ->  true
    ;   \+ last_gc(_)
    ).

%!  sync_destroy_queue_from_http(+Pengine, +Queue) is det.
%!  sync_delay_destroy_queue(+Pengine, +Queue) is det.
%
%   Handle destruction of the message queue connecting the HTTP side
%   to the pengine. We cannot delete the queue when the pengine dies
%   because the queue may contain output  events. Termination of the
%   pengine and finishing the  HTTP  exchange   may  happen  in both
%   orders. This means we need handle this using synchronization.
%
%     * sync_destroy_queue_from_pengine(+Pengine, +Queue)
%     Called (indirectly) from pengine_done/1 if the pengine's
%     thread dies.
%     * sync_destroy_queue_from_http(+Pengine, +Queue)
%     Called from destroy_queue/3, from wait_and_output_result/4,
%     i.e., from the HTTP side.

:- dynamic output_queue_destroyed/1.

sync_destroy_queue_from_http(ID, Queue) :-
    (   output_queue(ID, Queue, _)
    ->  destroy_queue_if_empty(Queue)
    ;   thread_peek_message(Queue, pengine_event(_, output(_,_)))
    ->  debug(pengine(destroy), 'Delay destruction of ~p because of output',
              [Queue]),
        get_time(Now),
        asserta(output_queue(ID, Queue, Now))
    ;   message_queue_destroy(Queue),
        asserta(output_queue_destroyed(Queue))
    ).

%!  sync_destroy_queue_from_pengine(+Pengine, +Queue)
%
%   Called  from  pengine_unregister/1  when    the  pengine  thread
%   terminates. It is called while the mutex `pengine` held.

sync_destroy_queue_from_pengine(ID, Queue) :-
    (   retract(output_queue_destroyed(Queue))
    ->  true
    ;   get_time(Now),
        asserta(output_queue(ID, Queue, Now))
    ),
    retractall(pengine_queue(ID, Queue, _, _)).


http_pengine_send(Request) :-
    reply_options(Request, [get,post]),
    !.
http_pengine_send(Request) :-
    http_parameters(Request,
                    [ id(ID, [ type(atom) ]),
                      event(EventString, [optional(true)]),
                      format(Format, [default(prolog)])
                    ]),
    catch(read_event(ID, Request, Format, EventString, Event),
          Error,
          true),
    (   var(Error)
    ->  debug(pengine(event), 'HTTP send: ~p', [Event]),
        (   pengine_thread(ID, Thread)
        ->  pengine_queue(ID, Queue, TimeLimit, _),
            random_delay,
            broadcast(pengine(send(ID, Event))),
            thread_send_message(Thread, pengine_request(Event)),
            wait_and_output_result(ID, Queue, Format, TimeLimit)
        ;   atom(ID)
        ->  pengine_died(Format, ID)
        ;   http_404([], Request)
        )
    ;   Error = error(existence_error(pengine, ID), _)
    ->  pengine_died(Format, ID)
    ;   output_result(Format, error(ID, Error))
    ).

pengine_died(Format, Pengine) :-
    output_result(Format, error(Pengine,
                                error(existence_error(pengine, Pengine),_))).


%!  read_event(+Pengine, +Request, +Format, +EventString, -Event) is det
%
%   Read an event on behalve of Pengine.  Note that the pengine's module
%   should not be  deleted  while  we   are  reading  using  its  syntax
%   (module). This is ensured using the `pengine_done` mutex.
%
%   @see pengine_done/0.

read_event(Pengine, Request, Format, EventString, Event) :-
    protect_pengine(
        Pengine,
        ( get_pengine_module(Pengine, Module),
          read_event_2(Request, EventString, Module, Event0, Bindings)
        )),
    !,
    fix_bindings(Format, Event0, Bindings, Event).
read_event(Pengine, Request, _Format, _EventString, _Event) :-
    debug(pengine(event), 'Pengine ~q vanished', [Pengine]),
    discard_post_data(Request),
    existence_error(pengine, Pengine).


%%  read_event_(+Request, +EventString, +Module, -Event, -Bindings)
%
%   Read the sent event. The event is a   Prolog  term that is either in
%   the =event= parameter or as a posted document.

read_event_2(_Request, EventString, Module, Event, Bindings) :-
    nonvar(EventString),
    !,
    term_string(Event, EventString,
                [ variable_names(Bindings),
                  module(Module)
                ]).
read_event_2(Request, _EventString, Module, Event, Bindings) :-
    option(method(post), Request),
    http_read_data(Request,     Event,
                   [ content_type('application/x-prolog'),
                     module(Module),
                     variable_names(Bindings)
                   ]).

%%  discard_post_data(+Request) is det.
%
%   If this is a POST request, discard the posted data.

discard_post_data(Request) :-
    option(method(post), Request),
    !,
    setup_call_cleanup(
        open_null_stream(NULL),
        http_read_data(Request, _, [to(stream(NULL))]),
        close(NULL)).
discard_post_data(_).

%!  fix_bindings(+Format, +EventIn, +Bindings, -Event) is det.
%
%   Generate the template for json(-s) Format  from the variables in
%   the asked Goal. Variables starting  with an underscore, followed
%   by an capital letter are ignored from the template.

fix_bindings(Format,
             ask(Goal, Options0), Bindings,
             ask(Goal, NewOptions)) :-
    json_lang(Format),
    !,
    exclude(anon, Bindings, NamedBindings),
    template(NamedBindings, Template, Options0, Options1),
    select_option(chunk(Paging), Options1, Options2, 1),
    NewOptions = [ template(Template),
                   chunk(Paging),
                   bindings(NamedBindings)
                 | Options2
                 ].
fix_bindings(_, Command, _, Command).

template(_, Template, Options0, Options) :-
    select_option(template(Template), Options0, Options),
    !.
template(Bindings, Template, Options, Options) :-
    dict_create(Template, swish_default_template, Bindings).

anon(Name=_) :-
    sub_atom(Name, 0, _, _, '_'),
    sub_atom(Name, 1, 1, _, Next),
    char_type(Next, prolog_var_start).

var_name(Name=_, Name).


%!  json_lang(+Format) is semidet.
%
%   True if Format is a JSON variation.

json_lang(json) :- !.
json_lang(Format) :-
    sub_atom(Format, 0, _, _, 'json-').

%!  http_pengine_pull_response(+Request)
%
%   HTTP handler for /pengine/pull_response.  Pulls possible pending
%   messages from the pengine.

http_pengine_pull_response(Request) :-
    reply_options(Request, [get]),
    !.
http_pengine_pull_response(Request) :-
    http_parameters(Request,
            [   id(ID, []),
                format(Format, [default(prolog)])
            ]),
    reattach(ID),
    (   (   pengine_queue(ID, Queue, TimeLimit, _)
        ->  true
        ;   output_queue(ID, Queue, _),
            TimeLimit = 0
        )
    ->  wait_and_output_result(ID, Queue, Format, TimeLimit)
    ;   http_404([], Request)
    ).

%!  http_pengine_abort(+Request)
%
%   HTTP handler for /pengine/abort. Note that  abort may be sent at
%   any time and the reply may  be   handled  by a pull_response. In
%   that case, our  pengine  has  already   died  before  we  get to
%   wait_and_output_result/4.

http_pengine_abort(Request) :-
    reply_options(Request, [get,post]),
    !.
http_pengine_abort(Request) :-
    http_parameters(Request,
            [   id(ID, [])
            ]),
    (   pengine_thread(ID, _Thread)
    ->  broadcast(pengine(abort(ID))),
        abort_pending_output(ID),
        pengine_abort(ID),
        reply_json(true)
    ;   http_404([], Request)
    ).

%!  http_pengine_detach(+Request)
%
%   Detach a Pengine while keeping it running.  This has the following
%   consequences:
%
%     - `/destroy_all` including the id of this pengine is ignored.
%     - Output from the pengine is stored in the queue without
%       waiting for the queue to drain.
%     - The Pengine becomes available through `/list`

http_pengine_detach(Request) :-
    reply_options(Request, [post]),
    !.
http_pengine_detach(Request) :-
    http_parameters(Request,
                    [ id(ID, [])
                    ]),
    http_read_json_dict(Request, ClientData),
    (   pengine_property(ID, application(Application)),
        allowed(Request, Application),
        authenticate(Request, Application, _UserOptions)
    ->  broadcast(pengine(detach(ID))),
        get_time(Now),
        assertz(pengine_detached(ID, ClientData.put(time, Now))),
        pengine_queue(ID, Queue, _TimeLimit, _Now),
        message_queue_set(Queue, max_size(1000)),
        pengine_reply(Queue, detached(ID)),
        reply_json(true)
    ;   http_404([], Request)
    ).

:- if(\+current_predicate(message_queue_set/2)).
message_queue_set(_,_).
:- endif.

reattach(ID) :-
    (   retract(pengine_detached(ID, _Data)),
        pengine_queue(ID, Queue, _TimeLimit, _Now)
    ->  message_queue_set(Queue, max_size(25))
    ;   true
    ).


%!  http_pengine_destroy_all(+Request)
%
%   Destroy a list of pengines. Normally   called  by pengines.js if the
%   browser window is closed.

http_pengine_destroy_all(Request) :-
    reply_options(Request, [get,post]),
    !.
http_pengine_destroy_all(Request) :-
    http_parameters(Request,
                    [ ids(IDsAtom, [])
                    ]),
    atomic_list_concat(IDs, ',', IDsAtom),
    forall(( member(ID, IDs),
             \+ pengine_detached(ID, _)
           ),
           pengine_destroy(ID, [force(true)])),
    reply_json("ok").

%!  http_pengine_ping(+Request)
%
%   HTTP handler for /pengine/ping.  If   the  requested  Pengine is
%   alive and event status(Pengine, Stats) is created, where `Stats`
%   is the return of thread_statistics/2.

http_pengine_ping(Request) :-
    reply_options(Request, [get]),
    !.
http_pengine_ping(Request) :-
    http_parameters(Request,
                    [ id(Pengine, []),
                      format(Format, [default(prolog)])
                    ]),
    (   pengine_thread(Pengine, Thread),
        Error = error(_,_),
        catch(thread_statistics(Thread, Stats), Error, fail)
    ->  output_result(Format, ping(Pengine, Stats))
    ;   output_result(Format, died(Pengine))
    ).

%!  http_pengine_list(+Request)
%
%   HTTP  handler  for  `/pengine/list`,   providing  information  about
%   running Pengines.
%
%   @tbd Only list detached Pengines associated to the logged in user.

http_pengine_list(Request) :-
    reply_options(Request, [get]),
    !.
http_pengine_list(Request) :-
    http_parameters(Request,
                    [ status(Status, [default(detached), oneof([detached])]),
                      application(Application, [default(pengine_sandbox)])
                    ]),
    allowed(Request, Application),
    authenticate(Request, Application, _UserOptions),
    findall(Term, listed_pengine(Application, Status, Term), Terms),
    reply_json(json{pengines: Terms}).

listed_pengine(Application, detached, State) :-
    State = pengine{id:Id,
                    detached:Time,
                    queued:Queued,
                    stats:Stats},

    pengine_property(Id, application(Application)),
    pengine_property(Id, detached(Time)),
    pengine_queue(Id, Queue, _TimeLimit, _Now),
    message_queue_property(Queue, size(Queued)),
    (   pengine_thread(Id, Thread),
        catch(thread_statistics(Thread, Stats), _, fail)
    ->  true
    ;   Stats = thread{status:died}
    ).


%!  output_result(+Format, +EventTerm) is det.
%!  output_result(+Format, +EventTerm, +OptionsDict) is det.
%
%   Formulate an HTTP response from a pengine event term. Format is
%   one of =prolog=, =json= or =json-s=.

:- dynamic
    pengine_replying/2.             % +Pengine, +Thread

output_result(Format, Event) :-
    arg(1, Event, Pengine),
    thread_self(Thread),
    cors_enable,            % contingent on http:cors setting
    disable_client_cache,
    setup_call_cleanup(
        asserta(pengine_replying(Pengine, Thread), Ref),
        catch(output_result(Format, Event, _{}),
              pengine_abort_output,
              true),
        erase(Ref)).

output_result(Lang, Event, Dict) :-
    write_result(Lang, Event, Dict),
    !.
output_result(prolog, Event, _) :-
    !,
    format('Content-type: text/x-prolog; charset=UTF-8~n~n'),
    write_term(Event,
               [ quoted(true),
                 ignore_ops(true),
                 fullstop(true),
                 blobs(portray),
                 portray_goal(portray_blob),
                 nl(true)
               ]).
output_result(Lang, Event, _) :-
    json_lang(Lang),
    !,
    (   event_term_to_json_data(Event, JSON, Lang)
    ->  reply_json(JSON)
    ;   assertion(event_term_to_json_data(Event, _, Lang))
    ).
output_result(Lang, _Event, _) :-    % FIXME: allow for non-JSON format
    domain_error(pengine_format, Lang).

%!  portray_blob(+Blob, +Options) is det.
%
%   Portray non-text blobs that may  appear   in  output  terms. Not
%   really sure about that. Basically such  terms need to be avoided
%   as they are meaningless outside the process. The generated error
%   is hard to debug though, so now we send them as `'$BLOB'(Type)`.
%   Future versions may include more info, depending on `Type`.

:- public portray_blob/2.               % called from write-term
portray_blob(Blob, _Options) :-
    blob(Blob, Type),
    writeq('$BLOB'(Type)).

%!  abort_pending_output(+Pengine) is det.
%
%   If we get an abort, it is possible that output is being produced
%   for the client.  This predicate aborts these threads.

abort_pending_output(Pengine) :-
    forall(pengine_replying(Pengine, Thread),
           abort_output_thread(Thread)).

abort_output_thread(Thread) :-
    catch(thread_signal(Thread, throw(pengine_abort_output)),
          error(existence_error(thread, _), _),
          true).

%!  write_result(+Lang, +Event, +Dict) is semidet.
%
%   Hook that allows for different output formats. The core Pengines
%   library supports `prolog` and various   JSON  dialects. The hook
%   event_to_json/3 can be used to refine   the  JSON dialects. This
%   hook must be used if  a   completely  different output format is
%   desired.

%!  disable_client_cache
%
%   Make sure the client will not cache our page.
%
%   @see http://stackoverflow.com/questions/49547/making-sure-a-web-page-is-not-cached-across-all-browsers

disable_client_cache :-
    format('Cache-Control: no-cache, no-store, must-revalidate\r\n\c
            Pragma: no-cache\r\n\c
            Expires: 0\r\n').

event_term_to_json_data(Event, JSON, Lang) :-
    event_to_json(Event, JSON, Lang),
    !.
event_term_to_json_data(success(ID, Bindings0, Projection, Time, More),
                        json{event:success, id:ID, time:Time,
                             data:Bindings, more:More, projection:Projection},
                        json) :-
    !,
    term_to_json(Bindings0, Bindings).
event_term_to_json_data(destroy(ID, Event),
                        json{event:destroy, id:ID, data:JSON},
                        Style) :-
    !,
    event_term_to_json_data(Event, JSON, Style).
event_term_to_json_data(create(ID, Features0), JSON, Style) :-
    !,
    (   select(answer(First0), Features0, Features1)
    ->  event_term_to_json_data(First0, First, Style),
        Features = [answer(First)|Features1]
    ;   Features = Features0
    ),
    dict_create(JSON, json, [event(create), id(ID)|Features]).
event_term_to_json_data(destroy(ID, Event),
                        json{event:destroy, id:ID, data:JSON}, Style) :-
    !,
    event_term_to_json_data(Event, JSON, Style).
event_term_to_json_data(error(ID, ErrorTerm), Error, _Style) :-
    !,
    Error0 = json{event:error, id:ID, data:Message},
    add_error_details(ErrorTerm, Error0, Error),
    message_to_string(ErrorTerm, Message).
event_term_to_json_data(failure(ID, Time),
                        json{event:failure, id:ID, time:Time}, _) :-
    !.
event_term_to_json_data(EventTerm, json{event:F, id:ID}, _) :-
    functor(EventTerm, F, 1),
    !,
    arg(1, EventTerm, ID).
event_term_to_json_data(EventTerm, json{event:F, id:ID, data:JSON}, _) :-
    functor(EventTerm, F, 2),
    arg(1, EventTerm, ID),
    arg(2, EventTerm, Data),
    term_to_json(Data, JSON).

:- public add_error_details/3.

%%  add_error_details(+Error, +JSON0, -JSON)
%
%   Add format error code and  location   information  to an error. Also
%   used by pengines_io.pl.

add_error_details(Error, JSON0, JSON) :-
    add_error_code(Error, JSON0, JSON1),
    add_error_location(Error, JSON1, JSON).

%%  add_error_code(+Error, +JSON0, -JSON) is det.
%
%   Add a =code= field to JSON0 of Error is an ISO error term. The error
%   code is the functor name of  the   formal  part  of the error, e.g.,
%   =syntax_error=,  =type_error=,  etc.   Some    errors   carry   more
%   information:
%
%     - existence_error(Type, Obj)
%     {arg1:Type, arg2:Obj}, where Obj is stringified of it is not
%     atomic.

add_error_code(error(existence_error(Type, Obj), _), Error0, Error) :-
    atom(Type),
    !,
    to_atomic(Obj, Value),
    Error = Error0.put(_{code:existence_error, arg1:Type, arg2:Value}).
add_error_code(error(Formal, _), Error0, Error) :-
    callable(Formal),
    !,
    functor(Formal, Code, _),
    Error = Error0.put(code, Code).
add_error_code(_, Error, Error).

% What to do with large integers?
to_atomic(Obj, Atomic) :- atom(Obj),   !, Atomic = Obj.
to_atomic(Obj, Atomic) :- number(Obj), !, Atomic = Obj.
to_atomic(Obj, Atomic) :- string(Obj), !, Atomic = Obj.
to_atomic(Obj, Atomic) :- term_string(Obj, Atomic).


%%  add_error_location(+Error, +JSON0, -JSON) is det.
%
%   Add a =location= property if the  error   can  be  associated with a
%   source location. The location is an   object  with properties =file=
%   and =line= and, if available, the character location in the line.

add_error_location(error(_, file(Path, Line, -1, _CharNo)), Term0, Term) :-
    atom(Path), integer(Line),
    !,
    Term = Term0.put(_{location:_{file:Path, line:Line}}).
add_error_location(error(_, file(Path, Line, Ch, _CharNo)), Term0, Term) :-
    atom(Path), integer(Line), integer(Ch),
    !,
    Term = Term0.put(_{location:_{file:Path, line:Line, ch:Ch}}).
add_error_location(_, Term, Term).


%!  event_to_json(+Event, -JSONTerm, +Lang) is semidet.
%
%   Hook that translates a Pengine event  structure into a term suitable
%   for reply_json/1, according to the language specification Lang. This
%   can be used to massage general Prolog terms, notably associated with
%   `success(ID, Bindings, Projection,  Time,   More)`  and  `output(ID,
%   Term)` into a format suitable for processing at the client side.

%:- multifile pengines:event_to_json/3.


                 /*******************************
                 *        ACCESS CONTROL        *
                 *******************************/

%!  allowed(+Request, +Application) is det.
%
%   Check whether the peer is allowed to connect.  Returns a
%   =forbidden= header if contact is not allowed.

allowed(Request, Application) :-
    setting(Application:allow_from, Allow),
    match_peer(Request, Allow),
    setting(Application:deny_from, Deny),
    \+ match_peer(Request, Deny),
    !.
allowed(Request, _Application) :-
    memberchk(request_uri(Here), Request),
    throw(http_reply(forbidden(Here))).

match_peer(_, Allowed) :-
    memberchk(*, Allowed),
    !.
match_peer(_, []) :- !, fail.
match_peer(Request, Allowed) :-
    http_peer(Request, Peer),
    debug(pengine(allow), 'Peer: ~q, Allow: ~q', [Peer, Allowed]),
    (   memberchk(Peer, Allowed)
    ->  true
    ;   member(Pattern, Allowed),
        match_peer_pattern(Pattern, Peer)
    ).

match_peer_pattern(Pattern, Peer) :-
    ip_term(Pattern, IP),
    ip_term(Peer, IP),
    !.

ip_term(Peer, Pattern) :-
    split_string(Peer, ".", "", PartStrings),
    ip_pattern(PartStrings, Pattern).

ip_pattern([], []).
ip_pattern([*], _) :- !.
ip_pattern([S|T0], [N|T]) :-
    number_string(N, S),
    ip_pattern(T0, T).


%%  authenticate(+Request, +Application, -UserOptions:list) is det.
%
%   Call authentication_hook/3, returning either `[user(User)]`, `[]` or
%   an exception.

authenticate(Request, Application, UserOptions) :-
    authentication_hook(Request, Application, User),
    !,
    must_be(ground, User),
    UserOptions = [user(User)].
authenticate(_, _, []).

%%  authentication_hook(+Request, +Application, -User) is semidet.
%
%   This hook is called  from  the   =/pengine/create=  HTTP  handler to
%   discover whether the server is accessed   by  an authorized user. It
%   can react in three ways:
%
%     - Succeed, binding User to a ground term.  The authentity of the
%       user is available through pengine_user/1.
%     - Fail.  The =/create= succeeds, but the pengine is not associated
%       with a user.
%     - Throw an exception to prevent creation of the pengine.  Two
%       meaningful exceptions are:
%         - throw(http_reply(authorise(basic(Realm))))
%         Start a normal HTTP login challenge (reply 401)
%         - throw(http_reply(forbidden(Path))))
%         Reject the request using a 403 repply.
%
%   @see http_authenticate/3 can be used to implement this hook using
%        default HTTP authentication data.

pengine_register_user(Options) :-
    option(user(User), Options),
    !,
    pengine_self(Me),
    asserta(pengine_user(Me, User)).
pengine_register_user(_).


%%  pengine_user(-User) is semidet.
%
%   True when the pengine was create by  an HTTP request that authorized
%   User.
%
%   @see authentication_hook/3 can be used to extract authorization from
%        the HTTP header.

pengine_user(User) :-
    pengine_self(Me),
    pengine_user(Me, User).

%!  reply_options(+Request, +Methods) is semidet.
%
%   Reply the HTTP OPTIONS request

reply_options(Request, Allowed) :-
    option(method(options), Request),
    !,
    cors_enable(Request,
                [ methods(Allowed)
                ]),
    format('Content-type: text/plain\r\n'),
    format('~n').                   % empty body


                 /*******************************
                 *        COMPILE SOURCE        *
                 *******************************/

/** pengine_src_text(+SrcText, +Module) is det

Asserts the clauses defined in SrcText in   the  private database of the
current Pengine. This  predicate  processes   the  `src_text'  option of
pengine_create/1.
*/

pengine_src_text(Src, Module) :-
    pengine_self(Self),
    format(atom(ID), 'pengine://~w/src', [Self]),
    extra_load_options(Self, Options),
    setup_call_cleanup(
        open_chars_stream(Src, Stream),
        load_files(Module:ID,
                   [ stream(Stream),
                     module(Module),
                     silent(true)
                   | Options
                   ]),
        close(Stream)),
    keep_source(Self, ID, Src).

system:'#file'(File, _Line) :-
    prolog_load_context(stream, Stream),
    set_stream(Stream, file_name(File)),
    set_stream(Stream, record_position(false)),
    set_stream(Stream, record_position(true)).

%%   pengine_src_url(+URL, +Module) is det
%
%    Asserts the clauses defined in URL in   the private database of the
%    current Pengine. This predicate processes   the `src_url' option of
%    pengine_create/1.
%
%    @tbd: make a sensible guess at the encoding.

pengine_src_url(URL, Module) :-
    pengine_self(Self),
    uri_encoded(path, URL, Path),
    format(atom(ID), 'pengine://~w/url/~w', [Self, Path]),
    extra_load_options(Self, Options),
    (   get_pengine_application(Self, Application),
        setting(Application:debug_info, false)
    ->  setup_call_cleanup(
            http_open(URL, Stream, []),
            ( set_stream(Stream, encoding(utf8)),
              load_files(Module:ID,
                         [ stream(Stream),
                           module(Module)
                         | Options
                         ])
            ),
            close(Stream))
    ;   setup_call_cleanup(
            http_open(URL, TempStream, []),
            ( set_stream(TempStream, encoding(utf8)),
              read_string(TempStream, _, Src)
            ),
            close(TempStream)),
        setup_call_cleanup(
            open_chars_stream(Src, Stream),
            load_files(Module:ID,
                       [ stream(Stream),
                         module(Module)
                       | Options
                       ]),
            close(Stream)),
        keep_source(Self, ID, Src)
    ).


extra_load_options(Pengine, Options) :-
    pengine_not_sandboxed(Pengine),
    !,
    Options = [].
extra_load_options(_, [sandboxed(true)]).


keep_source(Pengine, ID, SrcText) :-
    get_pengine_application(Pengine, Application),
    setting(Application:debug_info, true),
    !,
    to_string(SrcText, SrcString),
    assertz(pengine_data(Pengine, source(ID, SrcString))).
keep_source(_, _, _).

to_string(String, String) :-
    string(String),
    !.
to_string(Atom, String) :-
    atom_string(Atom, String),
    !.

		 /*******************************
		 *            SANDBOX		*
		 *******************************/

:- multifile
    sandbox:safe_primitive/1.

sandbox:safe_primitive(pengines:pengine_input(_, _)).
sandbox:safe_primitive(pengines:pengine_output(_)).
sandbox:safe_primitive(pengines:pengine_debug(_,_)).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

prolog:error_message(sandbox(time_limit_exceeded, Limit)) -->
    [ 'Could not prove safety of your goal within ~f seconds.'-[Limit], nl,
      'This is normally caused by an insufficiently instantiated'-[], nl,
      'meta-call (e.g., call(Var)) for which it is too expensive to'-[], nl,
      'find all possible instantations of Var.'-[]
    ].
