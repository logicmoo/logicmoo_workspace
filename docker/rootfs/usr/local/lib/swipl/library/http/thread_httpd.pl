/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2020, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

:- module(thread_httpd,
          [ http_current_server/2,      % ?:Goal, ?Port
            http_server_property/2,     % ?Port, ?Property
            http_server/2,              % :Goal, +Options
            http_workers/2,             % +Port, ?WorkerCount
            http_add_worker/2,          % +Port, +Options
            http_current_worker/2,      % ?Port, ?ThreadID
            http_stop_server/2,         % +Port, +Options
            http_spawn/2,               % :Goal, +Options

            http_requeue/1,             % +Request
            http_close_connection/1,    % +Request
            http_enough_workers/3       % +Queue, +Why, +Peer
          ]).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(socket)).
:- use_module(library(thread_pool)).
:- use_module(library(gensym)).
:- use_module(http_wrapper).
:- use_module(http_path).

:- autoload(library(uri), [uri_resolve/3]).

:- predicate_options(http_server/2, 2,
                     [ port(any),
                       unix_socket(atom),
                       entry_page(atom),
                       tcp_socket(any),
                       workers(positive_integer),
                       timeout(number),
                       keep_alive_timeout(number),
                       silent(boolean),
                       ssl(list(any)),  % if http/http_ssl_plugin is loaded
                       pass_to(system:thread_create/3, 3)
                     ]).
:- predicate_options(http_spawn/2, 2,
                     [ pool(atom),
                       pass_to(system:thread_create/3, 3),
                       pass_to(thread_pool:thread_create_in_pool/4, 4)
                     ]).
:- predicate_options(http_add_worker/2, 2,
                     [ timeout(number),
                       keep_alive_timeout(number),
                       max_idle_time(number),
                       pass_to(system:thread_create/3, 3)
                     ]).

/** <module> Threaded HTTP server

Most   code   doesn't   need  to   use  this   directly;  instead   use
library(http/http_server),  which  combines   this  library  with   the
typical HTTP libraries that most servers need.

This library defines the HTTP server  frontend of choice for SWI-Prolog.
It is based on the multi-threading   capabilities of SWI-Prolog and thus
exploits multiple cores  to  serve   requests  concurrently.  The server
scales well and can cooperate with   library(thread_pool) to control the
number of concurrent requests of a given   type.  For example, it can be
configured to handle 200 file download requests concurrently, 2 requests
that potentially uses a lot of memory and   8 requests that use a lot of
CPU resources.

On   Unix   systems,    this    library     can    be    combined   with
library(http/http_unix_daemon) to realise a proper  Unix service process
that creates a web server at  port   80,  runs under a specific account,
optionally detaches from the controlling terminal, etc.

Combined with library(http/http_ssl_plugin) from the   SSL package, this
library   can   be   used   to    create     an    HTTPS   server.   See
<plbase>/doc/packages/examples/ssl/https for an example   server using a
self-signed SSL certificate.
*/

:- meta_predicate
    http_server(1, :),
    http_current_server(1, ?),
    http_spawn(0, +).

:- dynamic
    current_server/6,       % Port, Goal, Thread, Queue, Scheme, StartTime
    queue_worker/2,         % Queue, ThreadID
    queue_options/2.        % Queue, Options

:- multifile
    make_socket_hook/3,
    accept_hook/2,
    close_hook/1,
    open_client_hook/6,
    http:create_pool/1,
    http:schedule_workers/1.

:- meta_predicate
    thread_repeat_wait(0).

%!  http_server(:Goal, :Options) is det.
%
%   Create a server at Port that calls Goal for each parsed request.
%   Options provide a list of options. Defined options are
%
%     * port(?Address)
%     Port to bind to.  Address is either a port or a term
%     Host:Port. The port may be a variable, causing the system
%     to select a free port.  See tcp_bind/2.
%
%     * unix_socket(+Path)
%     Instead of binding to a TCP port, bind to a _Unix Domain
%     Socket_ at Path.
%
%     * entry_page(+URI)
%     Affects the message printed while the server is started.
%     Interpreted as a URI relative to the server root.
%
%     * tcp_socket(+Socket)
%     If provided, use this socket instead of the creating one and
%     binding it to an address.  The socket must be bound to an
%     address.
%
%     * workers(+Count)
%     Determine the number of worker threads.  Default is 5.  This
%     is fine for small scale usage.  Public servers typically need
%     a higher number.
%
%     * timeout(+Seconds)
%     Maximum time of inactivity trying to read the request after a
%     connection has been opened.  Default is 60 seconds.  See
%     set_stream/1 using the _timeout_ option.
%
%     * keep_alive_timeout(+Seconds)
%     Time to keep `Keep alive' connections alive.  Default is
%     2 seconds.
%
%     * stack_limit(+Bytes)
%     Stack limit to use for the workers.  The default is inherited
%     from the `main` thread.
%     If you need to control resource usage you may consider the
%     `spawn` option of http_handler/3 and library(thread_pool).
%
%     * silent(Bool)
%     If `true` (default `false`), do not print an informational
%     message that the server was started.
%
%   A  typical  initialization  for  an    HTTP   server  that  uses
%   http_dispatch/1 to relay requests to predicates is:
%
%     ==
%     :- use_module(library(http/thread_httpd)).
%     :- use_module(library(http/http_dispatch)).
%
%     start_server(Port) :-
%         http_server(http_dispatch, [port(Port)]).
%     ==
%
%   Note that multiple servers  can  coexist   in  the  same  Prolog
%   process. A notable application of this is   to have both an HTTP
%   and HTTPS server, where the HTTP   server redirects to the HTTPS
%   server for handling sensitive requests.

http_server(Goal, M:Options0) :-
    server_address(Address, Options0),
    !,
    make_socket(Address, M:Options0, Options),
    create_workers(Options),
    create_server(Goal, Address, Options),
    (   option(silent(true), Options0)
    ->  true
    ;   print_message(informational,
                      httpd_started_server(Address, Options0))
    ).
http_server(_Goal, _:Options0) :-
    existence_error(server_address, Options0).

server_address(Address, Options) :-
    (   option(port(Port), Options)
    ->  Address = Port
    ;   option(unix_socket(Path), Options)
    ->  Address = unix_socket(Path)
    ).

address_port(_IFace:Port, Port) :- !.
address_port(unix_socket(Path), Path) :- !.
address_port(Address, Address) :- !.

tcp_address(Port) :-
    var(Port),
    !.
tcp_address(Port) :-
    integer(Port),
    !.
tcp_address(_Iface:_Port).

%!  make_socket(+Address, :OptionsIn, -OptionsOut) is det.
%
%   Create the HTTP server socket and  worker pool queue. OptionsOut
%   is quaranteed to hold the option queue(QueueId).
%
%   @arg   OptionsIn   is   qualified   to     allow   passing   the
%   module-sensitive ssl option argument.

make_socket(Address, M:Options0, Options) :-
    tcp_address(Address),
    make_socket_hook(Address, M:Options0, Options),
    !.
make_socket(Address, _:Options0, Options) :-
    option(tcp_socket(_), Options0),
    !,
    make_addr_atom('httpd', Address, Queue),
    Options = [ queue(Queue)
              | Options0
              ].
make_socket(Address, _:Options0, Options) :-
    tcp_address(Address),
    !,
    tcp_socket(Socket),
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Address),
    tcp_listen(Socket, 64),
    make_addr_atom('httpd', Address, Queue),
    Options = [ queue(Queue),
                tcp_socket(Socket)
              | Options0
              ].
:- if(current_predicate(unix_domain_socket/1)).
make_socket(Address, _:Options0, Options) :-
    Address = unix_socket(Path),
    !,
    unix_domain_socket(Socket),
    tcp_bind(Socket, Path),
    tcp_listen(Socket, 64),
    make_addr_atom('httpd', Address, Queue),
    Options = [ queue(Queue),
                tcp_socket(Socket)
              | Options0
              ].
:- endif.

%!  make_addr_atom(+Scheme, +Address, -Atom) is det.
%
%   Create an atom that identifies  the   server's  queue and thread
%   resources.

make_addr_atom(Scheme, Address, Atom) :-
    phrase(address_parts(Address), Parts),
    atomic_list_concat([Scheme,@|Parts], Atom).

address_parts(Atomic) -->
    { atomic(Atomic) },
    !,
    [Atomic].
address_parts(Host:Port) -->
    !,
    address_parts(Host), [:], address_parts(Port).
address_parts(ip(A,B,C,D)) -->
    !,
    [ A, '.', B, '.', C, '.', D ].
address_parts(unix_socket(Path)) -->
    [Path].


%!  create_server(:Goal, +Address, +Options) is det.
%
%   Create the main server thread that runs accept_server/2 to
%   listen to new requests.

create_server(Goal, Address, Options) :-
    get_time(StartTime),
    memberchk(queue(Queue), Options),
    scheme(Scheme, Options),
    autoload_https(Scheme),
    address_port(Address, Port),
    make_addr_atom(Scheme, Port, Alias),
    thread_self(Initiator),
    thread_create(accept_server(Goal, Initiator, Options), _,
                  [ alias(Alias)
                  ]),
    thread_get_message(server_started),
    assert(current_server(Port, Goal, Alias, Queue, Scheme, StartTime)).

scheme(Scheme, Options) :-
    option(scheme(Scheme), Options),
    !.
scheme(Scheme, Options) :-
    (   option(ssl(_), Options)
    ;   option(ssl_instance(_), Options)
    ),
    !,
    Scheme = https.
scheme(http, _).

autoload_https(https) :-
    \+ clause(accept_hook(_Goal, _Options), _),
    exists_source(library(http/http_ssl_plugin)),
    !,
    use_module(library(http/http_ssl_plugin)).
autoload_https(_).

%!  http_current_server(:Goal, ?Port) is nondet.
%
%   True if Goal is the goal of a server at Port.
%
%   @deprecated Use http_server_property(Port, goal(Goal))

http_current_server(Goal, Port) :-
    current_server(Port, Goal, _, _, _, _).


%!  http_server_property(?Port, ?Property) is nondet.
%
%   True if Property is a property of the HTTP server running at
%   Port.  Defined properties are:
%
%       * goal(:Goal)
%       Goal used to start the server. This is often
%       http_dispatch/1.
%       * scheme(-Scheme)
%       Scheme is one of `http` or `https`.
%       * start_time(?Time)
%       Time-stamp when the server was created.

http_server_property(_:Port, Property) :-
    integer(Port),
    !,
    server_property(Property, Port).
http_server_property(Port, Property) :-
    server_property(Property, Port).

server_property(goal(Goal), Port) :-
    current_server(Port, Goal, _, _, _, _).
server_property(scheme(Scheme), Port) :-
    current_server(Port, _, _, _, Scheme, _).
server_property(start_time(Time), Port) :-
    current_server(Port, _, _, _, _, Time).


%!  http_workers(+Port, -Workers) is det.
%!  http_workers(+Port, +Workers:int) is det.
%
%   Query or set the number of workers  for the server at this port.
%   The number of workers is dynamically   modified. Setting it to 1
%   (one) can be used to profile the worker using tprofile/1.

http_workers(Port, Workers) :-
    must_be(ground, Port),
    current_server(Port, _, _, Queue, _, _),
    !,
    (   integer(Workers)
    ->  resize_pool(Queue, Workers)
    ;   findall(W, queue_worker(Queue, W), WorkerIDs),
        length(WorkerIDs, Workers)
    ).
http_workers(Port, _) :-
    existence_error(http_server, Port).


%!  http_add_worker(+Port, +Options) is det.
%
%   Add a new worker to  the  HTTP   server  for  port Port. Options
%   overrule the default queue  options.   The  following additional
%   options are processed:
%
%     - max_idle_time(+Seconds)
%     The created worker will automatically terminate if there is
%     no new work within Seconds.

http_add_worker(Port, Options) :-
    must_be(ground, Port),
    current_server(Port, _, _, Queue, _, _),
    !,
    queue_options(Queue, QueueOptions),
    merge_options(Options, QueueOptions, WorkerOptions),
    atom_concat(Queue, '_', AliasBase),
    create_workers(1, 1, Queue, AliasBase, WorkerOptions).
http_add_worker(Port, _) :-
    existence_error(http_server, Port).


%!  http_current_worker(?Port, ?ThreadID) is nondet.
%
%   True if ThreadID is the identifier   of  a Prolog thread serving
%   Port. This predicate is  motivated  to   allow  for  the  use of
%   arbitrary interaction with the worker thread for development and
%   statistics.

http_current_worker(Port, ThreadID) :-
    current_server(Port, _, _, Queue, _, _),
    queue_worker(Queue, ThreadID).


%!  accept_server(:Goal, +Initiator, +Options)
%
%   The goal of a small server-thread accepting new requests and
%   posting them to the queue of workers.

accept_server(Goal, Initiator, Options) :-
    catch(accept_server2(Goal, Initiator, Options), http_stop, true),
    thread_self(Thread),
    retract(current_server(_Port, _, Thread, _Queue, _Scheme, _StartTime)),
    close_server_socket(Options).

accept_server2(Goal, Initiator, Options) :-
    thread_send_message(Initiator, server_started),
    repeat,
      (   catch(accept_server3(Goal, Options), E, true)
      ->  (   var(E)
          ->  fail
          ;   accept_rethrow_error(E)
          ->  throw(E)
          ;   print_message(error, E),
              fail
          )
      ;   print_message(error,      % internal error
                        goal_failed(accept_server3(Goal, Options))),
          fail
      ).

accept_server3(Goal, Options) :-
    accept_hook(Goal, Options),
    !.
accept_server3(Goal, Options) :-
    memberchk(tcp_socket(Socket), Options),
    memberchk(queue(Queue), Options),
    debug(http(connection), 'Waiting for connection', []),
    tcp_accept(Socket, Client, Peer),
    debug(http(connection), 'New HTTP connection from ~p', [Peer]),
    thread_send_message(Queue, tcp_client(Client, Goal, Peer)),
    http_enough_workers(Queue, accept, Peer).

accept_rethrow_error(http_stop).
accept_rethrow_error('$aborted').


%!  close_server_socket(+Options)
%
%   Close the server socket.

close_server_socket(Options) :-
    close_hook(Options),
    !.
close_server_socket(Options) :-
    memberchk(tcp_socket(Socket), Options),
    !,
    tcp_close_socket(Socket).


%!  http_stop_server(+Port, +Options)
%
%   Stop the indicated  HTTP  server   gracefully.  First  stops all
%   workers, then stops the server.
%
%   @tbd    Realise non-graceful stop

http_stop_server(Host:Port, Options) :-         % e.g., localhost:4000
    ground(Host),
    !,
    http_stop_server(Port, Options).
http_stop_server(Port, _Options) :-
    http_workers(Port, 0),                  % checks Port is ground
    current_server(Port, _, Thread, Queue, _Scheme, _Start),
    retractall(queue_options(Queue, _)),
    thread_signal(Thread, throw(http_stop)),
    catch(connect(localhost:Port), _, true),
    thread_join(Thread, _),
    message_queue_destroy(Queue).

connect(Address) :-
    setup_call_cleanup(
        tcp_socket(Socket),
        tcp_connect(Socket, Address),
        tcp_close_socket(Socket)).

%!  http_enough_workers(+Queue, +Why, +Peer) is det.
%
%   Check that we have enough workers in our queue. If not, call the
%   hook http:schedule_workers/1 to extend  the   worker  pool. This
%   predicate can be used by accept_hook/2.

http_enough_workers(Queue, _Why, _Peer) :-
    message_queue_property(Queue, waiting(_0)),
    !,
    debug(http(scheduler), '~D waiting for work; ok', [_0]).
http_enough_workers(Queue, Why, Peer) :-
    message_queue_property(Queue, size(Size)),
    (   enough(Size, Why)
    ->  debug(http(scheduler), '~D in queue; ok', [Size])
    ;   current_server(Port, _, _, Queue, _, _),
        Data = _{ port:Port,
                  reason:Why,
                  peer:Peer,
                  waiting:Size,
                  queue:Queue
                },
        debug(http(scheduler), 'Asking to reschedule: ~p', [Data]),
        catch(http:schedule_workers(Data),
              Error,
              print_message(error, Error))
    ->  true
    ;   true
    ).

enough(0, _).
enough(1, keep_alive).                  % I will be ready myself


%!  http:schedule_workers(+Data:dict) is semidet.
%
%   Hook called if a  new  connection   or  a  keep-alive connection
%   cannot be scheduled _immediately_ to a worker. Dict contains the
%   following keys:
%
%     - port:Port
%     Port number that identifies the server.
%     - reason:Reason
%     One of =accept= for a new connection or =keep_alive= if a
%     worker tries to reschedule itself.
%     - peer:Peer
%     Identify the other end of the connection
%     - waiting:Size
%     Number of messages waiting in the queue.
%     - queue:Queue
%     Message queue used to dispatch accepted messages.
%
%   Note that, when called with `reason:accept`,   we  are called in
%   the time critical main accept loop.   An  implementation of this
%   hook shall typically send  the  event   to  thread  dedicated to
%   dynamic worker-pool management.
%
%   @see    http_add_worker/2 may be used to create (temporary) extra
%           workers.


                 /*******************************
                 *    WORKER QUEUE OPERATIONS   *
                 *******************************/

%!  create_workers(+Options)
%
%   Create the pool of HTTP worker-threads. Each worker has the
%   alias http_worker_N.

create_workers(Options) :-
    option(workers(N), Options, 5),
    option(queue(Queue), Options),
    catch(message_queue_create(Queue), _, true),
    atom_concat(Queue, '_', AliasBase),
    create_workers(1, N, Queue, AliasBase, Options),
    assert(queue_options(Queue, Options)).

create_workers(I, N, _, _, _) :-
    I > N,
    !.
create_workers(I, N, Queue, AliasBase, Options) :-
    gensym(AliasBase, Alias),
    thread_create(http_worker(Options), Id,
                  [ alias(Alias)
                  | Options
                  ]),
    assertz(queue_worker(Queue, Id)),
    I2 is I + 1,
    create_workers(I2, N, Queue, AliasBase, Options).


%!  resize_pool(+Queue, +Workers) is det.
%
%   Create or destroy workers. If workers   are  destroyed, the call
%   waits until the desired number of waiters is reached.

resize_pool(Queue, Size) :-
    findall(W, queue_worker(Queue, W), Workers),
    length(Workers, Now),
    (   Now < Size
    ->  queue_options(Queue, Options),
        atom_concat(Queue, '_', AliasBase),
        I0 is Now+1,
        create_workers(I0, Size, Queue, AliasBase, Options)
    ;   Now == Size
    ->  true
    ;   Now > Size
    ->  Excess is Now - Size,
        thread_self(Me),
        forall(between(1, Excess, _), thread_send_message(Queue, quit(Me))),
        forall(between(1, Excess, _), thread_get_message(quitted(_)))
    ).


%!  http_worker(+Options)
%
%   Run HTTP worker main loop. Workers   simply  wait until they are
%   passed an accepted socket to process  a client.
%
%   If the message quit(Sender) is read   from the queue, the worker
%   stops.

http_worker(Options) :-
    debug(http(scheduler), 'New worker', []),
    prolog_listen(this_thread_exit, done_worker),
    option(queue(Queue), Options),
    option(max_idle_time(MaxIdle), Options, infinite),
    thread_repeat_wait(get_work(Queue, Message, MaxIdle)),
      debug(http(worker), 'Waiting for a job ...', []),
      debug(http(worker), 'Got job ~p', [Message]),
      (   Message = quit(Sender)
      ->  !,
          thread_self(Self),
          thread_detach(Self),
          (   Sender == idle
          ->  true
          ;   retract(queue_worker(Queue, Self)),
              thread_send_message(Sender, quitted(Self))
          )
      ;   open_client(Message, Queue, Goal, In, Out,
                      Options, ClientOptions),
          (   catch(http_process(Goal, In, Out, ClientOptions),
                    Error, true)
          ->  true
          ;   Error = goal_failed(http_process/4)
          ),
          (   var(Error)
          ->  fail
          ;   current_message_level(Error, Level),
              print_message(Level, Error),
              memberchk(peer(Peer), ClientOptions),
              close_connection(Peer, In, Out),
              fail
          )
      ).

get_work(Queue, Message, infinite) :-
    !,
    thread_get_message(Queue, Message).
get_work(Queue, Message, MaxIdle) :-
    (   thread_get_message(Queue, Message, [timeout(MaxIdle)])
    ->  true
    ;   Message = quit(idle)
    ).


%!  open_client(+Message, +Queue, -Goal, -In, -Out,
%!              +Options, -ClientOptions) is semidet.
%
%   Opens the connection to the client in a worker from the message
%   sent to the queue by accept_server/2.

open_client(requeue(In, Out, Goal, ClOpts),
            _, Goal, In, Out, Opts, ClOpts) :-
    !,
    memberchk(peer(Peer), ClOpts),
    option(keep_alive_timeout(KeepAliveTMO), Opts, 2),
    check_keep_alive_connection(In, KeepAliveTMO, Peer, In, Out).
open_client(Message, Queue, Goal, In, Out, Opts,
            [ pool(client(Queue, Goal, In, Out)),
              timeout(Timeout)
            | Options
            ]) :-
    catch(open_client(Message, Goal, In, Out, Options, Opts),
          E, report_error(E)),
    option(timeout(Timeout), Opts, 60),
    (   debugging(http(connection))
    ->  memberchk(peer(Peer), Options),
        debug(http(connection), 'Opened connection from ~p', [Peer])
    ;   true
    ).


%!  open_client(+Message, +Goal, -In, -Out,
%!              -ClientOptions, +Options) is det.

open_client(Message, Goal, In, Out, ClientOptions, Options) :-
    open_client_hook(Message, Goal, In, Out, ClientOptions, Options),
    !.
open_client(tcp_client(Socket, Goal, Peer), Goal, In, Out,
            [ peer(Peer),
              protocol(http)
            ], _) :-
    tcp_open_socket(Socket, In, Out).

report_error(E) :-
    print_message(error, E),
    fail.


%!  check_keep_alive_connection(+In, +TimeOut, +Peer, +In, +Out) is semidet.
%
%   Wait for the client for at most  TimeOut seconds. Succeed if the
%   client starts a new request within   this  time. Otherwise close
%   the connection and fail.

check_keep_alive_connection(In, TMO, Peer, In, Out) :-
    stream_property(In, timeout(Old)),
    set_stream(In, timeout(TMO)),
    debug(http(keep_alive), 'Waiting for keep-alive ...', []),
    catch(peek_code(In, Code), E, true),
    (   var(E),                     % no exception
        Code \== -1                 % no end-of-file
    ->  set_stream(In, timeout(Old)),
        debug(http(keep_alive), '\tre-using keep-alive connection', [])
    ;   (   Code == -1
        ->  debug(http(keep_alive), '\tRemote closed keep-alive connection', [])
        ;   debug(http(keep_alive), '\tTimeout on keep-alive connection', [])
        ),
        close_connection(Peer, In, Out),
        fail
    ).


%!  done_worker
%
%   Called when worker is terminated  due   to  http_workers/2  or a
%   (debugging) exception. In  the   latter  case, recreate_worker/2
%   creates a new worker.

done_worker :-
    thread_self(Self),
    thread_detach(Self),
    retract(queue_worker(Queue, Self)),
    thread_property(Self, status(Status)),
    !,
    (   catch(recreate_worker(Status, Queue), _, fail)
    ->  print_message(informational,
                      httpd_restarted_worker(Self))
    ;   done_status_message_level(Status, Level),
        print_message(Level,
                      httpd_stopped_worker(Self, Status))
    ).
done_worker :-                                  % received quit(Sender)
    thread_self(Self),
    thread_property(Self, status(Status)),
    done_status_message_level(Status, Level),
    print_message(Level,
                  httpd_stopped_worker(Self, Status)).

done_status_message_level(true, silent) :- !.
done_status_message_level(exception('$aborted'), silent) :- !.
done_status_message_level(_, informational).


%!  recreate_worker(+Status, +Queue) is semidet.
%
%   Deal with the possibility  that   threads  are,  during development,
%   killed with abort/0. We recreate the worker to avoid that eventually
%   we run out of workers. If  we  are   aborted  due  to a halt/0 call,
%   thread_create/3 will raise a permission error.
%
%   The first clause deals with the possibility  that we cannot write to
%   `user_error`. This is possible when Prolog   is started as a service
%   using some service managers. Would be  nice   if  we  could write an
%   error, but where?

recreate_worker(exception(error(io_error(write,user_error),_)), _Queue) :-
    halt(2).
recreate_worker(exception(Error), Queue) :-
    recreate_on_error(Error),
    queue_options(Queue, Options),
    atom_concat(Queue, '_', AliasBase),
    create_workers(1, 1, Queue, AliasBase, Options).

recreate_on_error('$aborted').
recreate_on_error(time_limit_exceeded).

%!  thread_httpd:message_level(+Exception, -Level)
%
%   Determine the message stream used  for   exceptions  that  may occur
%   during server_loop/5. Being multifile, clauses can   be added by the
%   application to refine error handling.   See  also message_hook/3 for
%   further programming error handling.

:- multifile
    message_level/2.

message_level(error(io_error(read, _), _),               silent).
message_level(error(socket_error(epipe,_), _),           silent).
message_level(error(http_write_short(_Obj,_Written), _), silent).
message_level(error(timeout_error(read, _), _),          informational).
message_level(keep_alive_timeout,                        silent).

current_message_level(Term, Level) :-
    (   message_level(Term, Level)
    ->  true
    ;   Level = error
    ).


%!  http_requeue(+Header)
%
%   Re-queue a connection to  the  worker   pool.  This  deals  with
%   processing additional requests on keep-alive connections.

http_requeue(Header) :-
    requeue_header(Header, ClientOptions),
    memberchk(pool(client(Queue, Goal, In, Out)), ClientOptions),
    memberchk(peer(Peer), ClientOptions),
    http_enough_workers(Queue, keep_alive, Peer),
    thread_send_message(Queue, requeue(In, Out, Goal, ClientOptions)),
    !.
http_requeue(Header) :-
    debug(http(error), 'Re-queue failed: ~p', [Header]),
    fail.

requeue_header([], []).
requeue_header([H|T0], [H|T]) :-
    requeue_keep(H),
    !,
    requeue_header(T0, T).
requeue_header([_|T0], T) :-
    requeue_header(T0, T).

requeue_keep(pool(_)).
requeue_keep(peer(_)).
requeue_keep(protocol(_)).


%!  http_process(Message, Queue, +Options)
%
%   Handle a single client message on the given stream.

http_process(Goal, In, Out, Options) :-
    debug(http(server), 'Running server goal ~p on ~p -> ~p',
          [Goal, In, Out]),
    option(timeout(TMO), Options, 60),
    set_stream(In, timeout(TMO)),
    set_stream(Out, timeout(TMO)),
    http_wrapper(Goal, In, Out, Connection,
                 [ request(Request)
                 | Options
                 ]),
    next(Connection, Request).

next(Connection, Request) :-
    next_(Connection, Request), !.
next(Connection, Request) :-
    print_message(warning, goal_failed(next(Connection,Request))).

next_(switch_protocol(SwitchGoal, _SwitchOptions), Request) :-
    !,
    memberchk(pool(client(_Queue, _Goal, In, Out)), Request),
    (   catch(call(SwitchGoal, In, Out), E,
              (   print_message(error, E),
                  fail))
    ->  true
    ;   http_close_connection(Request)
    ).
next_(spawned(ThreadId), _) :-
    !,
    debug(http(spawn), 'Handler spawned to thread ~w', [ThreadId]).
next_(Connection, Request) :-
    downcase_atom(Connection, 'keep-alive'),
    http_requeue(Request),
    !.
next_(_, Request) :-
    http_close_connection(Request).


%!  http_close_connection(+Request)
%
%   Close connection associated to Request.  See also http_requeue/1.

http_close_connection(Request) :-
    memberchk(pool(client(_Queue, _Goal, In, Out)), Request),
    memberchk(peer(Peer), Request),
    close_connection(Peer, In, Out).

%!  close_connection(+Peer, +In, +Out)
%
%   Closes the connection from the server to the client.  Errors are
%   currently silently ignored.

close_connection(Peer, In, Out) :-
    debug(http(connection), 'Closing connection from ~p', [Peer]),
    catch(close(In, [force(true)]), _, true),
    catch(close(Out, [force(true)]), _, true).

%!  http_spawn(:Goal, +Options) is det.
%
%   Continue this connection on a  new   thread.  A handler may call
%   http_spawn/2 to start a new thread that continues processing the
%   current request using Goal. The original   thread returns to the
%   worker pool for processing new requests.   Options are passed to
%   thread_create/3, except for:
%
%       * pool(+Pool)
%       Interfaces to library(thread_pool), starting the thread
%       on the given pool.
%
%   If a pool does not exist, this predicate calls the multifile
%   hook http:create_pool/1 to create it. If this predicate succeeds
%   the operation is retried.

http_spawn(Goal, Options) :-
    select_option(pool(Pool), Options, ThreadOptions),
    !,
    current_output(CGI),
    catch(thread_create_in_pool(Pool,
                                wrap_spawned(CGI, Goal), Id,
                                [ detached(true)
                                | ThreadOptions
                                ]),
          Error,
          true),
    (   var(Error)
    ->  http_spawned(Id)
    ;   Error = error(resource_error(threads_in_pool(_)), _)
    ->  throw(http_reply(busy))
    ;   Error = error(existence_error(thread_pool, Pool), _),
        create_pool(Pool)
    ->  http_spawn(Goal, Options)
    ;   throw(Error)
    ).
http_spawn(Goal, Options) :-
    current_output(CGI),
    thread_create(wrap_spawned(CGI, Goal), Id,
                  [ detached(true)
                  | Options
                  ]),
    http_spawned(Id).

wrap_spawned(CGI, Goal) :-
    set_output(CGI),
    http_wrap_spawned(Goal, Request, Connection),
    next(Connection, Request).

%!  create_pool(+Pool)
%
%   Lazy  creation  of  worker-pools  for   the  HTTP  server.  This
%   predicate calls the hook http:create_pool/1.   If the hook fails
%   it creates a default pool of size   10. This should suffice most
%   typical usecases. Note that we  get   a  permission error if the
%   pool is already created.  We can ignore this.

create_pool(Pool) :-
    E = error(permission_error(create, thread_pool, Pool), _),
    catch(http:create_pool(Pool), E, true).
create_pool(Pool) :-
    print_message(informational, httpd(created_pool(Pool))),
    thread_pool_create(Pool, 10, []).


		 /*******************************
		 *         WAIT POLICIES	*
		 *******************************/

:- meta_predicate
    thread_repeat_wait(0).

%!  thread_repeat_wait(:Goal) is multi.
%
%   Acts as `repeat,  thread_idle(Goal)`,  choosing   whether  to  use a
%   `long` or `short` idle time based on the average firing rate.

thread_repeat_wait(Goal) :-
    new_rate_mma(5, 1000, State),
    repeat,
      update_rate_mma(State, MMA),
      long(MMA, IsLong),
      (   IsLong == brief
      ->  call(Goal)
      ;   thread_idle(Goal, IsLong)
      ).

long(MMA, brief) :-
    MMA < 0.05,
    !.
long(MMA, short) :-
    MMA < 1,
    !.
long(_, long).

%!  new_rate_mma(+N, +Resolution, -State) is det.
%!  update_rate_mma(!State, -MMA) is det.
%
%   Implement _Modified Moving  Average_  computing   the  average  time
%   between requests as an exponential moving averate with alpha=1/N.
%
%   @arg Resolution is the time resolution  in 1/Resolution seconds. All
%   storage is done in integers to avoid  the need for stack freezing in
%   nb_setarg/3.
%
%   @see https://en.wikipedia.org/wiki/Moving_average

new_rate_mma(N, Resolution, rstate(Base, 0, MaxI, Resolution, N, 0)) :-
    current_prolog_flag(max_tagged_integer, MaxI),
    get_time(Base).

update_rate_mma(State, MMAr) :-
    State = rstate(Base, Last, MaxI, Resolution, N, MMA0),
    get_time(Now),
    Stamp is round((Now-Base)*Resolution),
    (   Stamp > MaxI
    ->  nb_setarg(1, State, Now),
        nb_setarg(2, State, 0)
    ;   true
    ),
    Diff is Stamp-Last,
    nb_setarg(2, State, Stamp),
    MMA is round(((N-1)*MMA0+Diff)/N),
    nb_setarg(6, State, MMA),
    MMAr is MMA/float(Resolution).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(httpd_started_server(Port, Options)) -->
    [ 'Started server at '-[] ],
    http_root(Port, Options).
prolog:message(httpd_stopped_worker(Self, Status)) -->
    [ 'Stopped worker ~p: ~p'-[Self, Status] ].
prolog:message(httpd_restarted_worker(Self)) -->
    [ 'Replaced aborted worker ~p'-[Self] ].
prolog:message(httpd(created_pool(Pool))) -->
    [ 'Created thread-pool ~p of size 10'-[Pool], nl,
      'Create this pool at startup-time or define the hook ', nl,
      'http:create_pool/1 to avoid this message and create a ', nl,
      'pool that fits the usage-profile.'
    ].

http_root(Address, Options) -->
    { landing_page(Address, URI, Options) },
    [ '~w'-[URI] ].

landing_page(Host:Port, URI, Options) :-
    !,
    must_be(atom, Host),
    must_be(integer, Port),
    http_server_property(Port, scheme(Scheme)),
    (   default_port(Scheme, Port)
    ->  format(atom(Base), '~w://~w', [Scheme, Host])
    ;   format(atom(Base), '~w://~w:~w', [Scheme, Host, Port])
    ),
    entry_page(Base, URI, Options).
landing_page(unix_socket(Path), URI, _Options) :-
    !,
    format(string(URI), 'Unix domain socket "~w"', [Path]).
landing_page(Port, URI, Options) :-
    landing_page(localhost:Port, URI, Options).

default_port(http, 80).
default_port(https, 443).

entry_page(Base, URI, Options) :-
    option(entry_page(Entry), Options),
    !,
    uri_resolve(Entry, Base, URI).
entry_page(Base, URI, _) :-
    http_absolute_location(root(.), Entry, []),
    uri_resolve(Entry, Base, URI).
