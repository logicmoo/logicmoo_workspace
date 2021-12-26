/*  Part of SWI-Prolog

    Author:        Hongxin Liang and Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(stomp,
          [ stomp_connection/5,    % +Address, +Host, +Headers,
                                   % :Callback, -Connection
            stomp_connection/6,    % +Address, +Host, +Headers,
                                   % :Callback, -Connection, +Options
            stomp_connection_property/2, % ?Connection, ?Property
            stomp_destroy_connection/1, % +Connection
            stomp_connect/1,       % +Connection
            stomp_connect/2,       % +Connection, +Options
            stomp_teardown/1,      % +Connection
            stomp_reconnect/1,	   % +Connection
            stomp_send/4,          % +Connection, +Destination, +Headers, +Body
            stomp_send_json/4,     % +Connection, +Destination, +Headers, +JSON
            stomp_subscribe/4,     % +Connection, +Destination, +Id, +Headers
            stomp_unsubscribe/2,   % +Connection, +Id
            stomp_ack/2,           % +Connection, +MsgHeaders
            stomp_nack/2,          % +Connection, +MsgHeaders
            stomp_ack/3,           % +Connection, +MessageId, +Headers
            stomp_nack/3,          % +Connection, +MessageId, +Headers
            stomp_transaction/2,   % +Connection, :Goal
            stomp_disconnect/2,    % +Connection, +Headers
                                   % Low level predicates
            stomp_begin/2,         % +Connection, +Transaction
            stomp_commit/2,        % +Connection, +Transaction
            stomp_abort/2,         % +Connection, +Transaction
            stomp_setup/2          % +Connection, +Options
          ]).

/** <module> STOMP client.

This module provides a STOMP  (Simple   (or  Streaming)  Text Orientated
Messaging Protocol) client. This client  is   based  on  work by Hongxin
Liang. The current version is a major rewrite, both changing the API and
the low-level STOMP frame (de)serialization.

The predicate stomp_connection/5 is used to   register a connection. The
connection is established by  stomp_connect/1,   which  is lazily called
from any of the predicates that send   a STOMP frame. After establishing
the connection two threads are created.   One  receives STOMP frames and
the other manages and watches the _heart beat_.

## Threading	{#stomp-threading}

Upon receiving a frame the   callback registered with stomp_connection/5
is called in  the  context  of   the  receiving  thread.  More demanding
applications may decide to send incomming frames to a SWI-Prolog message
queue and have one  or  more   _worker  threads_  processing  the input.
Alternatively, frames may be  inspected  by   the  receiving  thread and
either processed immediately or be dispatched   to either new or running
threads.  The best scenario depends on the message rate, processing time
and concurrency of the Prolog application.

All message sending predicates of this library are _thread safe_. If two
threads send a frame to the  same   connection  the library ensures that
both frames are properly serialized.

## Reconnecting	{#stomp-reconnecting}

By default this library tries to establish   the connection and the user
gets notified by  means  of  a   `disconnected`  pseudo  frame  that the
connection is lost. Using the Options argument of stomp_connection/6 the
system can be configured to try and keep connecting if the server is not
available and reconnect if  the  connection   is  lost.  See the pong.pl
example.

@author Hongxin Liang and Jan Wielemaker
@license BSD-2
@see http://stomp.github.io/index.html
@see https://github.com/jasonrbriggs/stomp.py
@tbd TSL support
*/

:- meta_predicate
    stomp_connection(+, +, +, 4, -),
    stomp_connection(+, +, +, 4, -, +),
    stomp_transaction(+, 0).

:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(gensym)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/json)).
:- use_module(library(readutil)).
:- use_module(library(socket)).
:- use_module(library(uuid)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(time)).

:- dynamic
    connection_property/3.

%!  stomp_connection(+Address, +Host, +Headers, :Callback,
%!                   -Connection) is det.
%!  stomp_connection(+Address, +Host, +Headers, :Callback,
%!                   -Connection, +Options) is det.
%
%   Create a connection reference. The connection is   not set up yet by
%   this predicate. Callback is called on  any received frame except for
%   _heart beat_ frames as below.
%
%   ```
%   call(Callback, Command, Connection, Header, Body)
%   ```
%
%   Where command is one of  the  commands   below.  `Header`  is a dict
%   holding the STOMP frame header, where  all values are strings except
%   for the `'content-length'` key value which is passed as an integer.
%
%   Body  is  a   string   or,   if   the    data   is   of   the   type
%   ``application/json``, a dict.
%
%     - connected
%       A connection was established.  Connection and Header are valid.
%     - disconnected
%       The connection was lost.  Only Connection is valid.
%     - message
%       A message arrived.  All three arguments are valid.  Body is
%       a dict if the ``content-type`` of the message is
%       ``application/json`` and a string otherwise.
%     - heartbeat
%       A heartbeat was received.  Only Connection is valid.  This
%       callback is also called for each newline that follows a frame.
%       These newlines can be a heartbeat, but can also be additional
%       newlines that follow a frame.
%     - heartbeat_timeout
%       No heartbeat was received.  Only Connection is valid.
%     - error
%       An error happened.  All three arguments are valid and handled
%       as `message`.
%
%   Note that stomp_teardown/1 causes the receiving and heartbeat thread
%   to be signalled with abort/0.
%
%   Options processed:
%
%     - reconnect(+Bool)
%       Try to reestablish the connection to the server if the
%       connection is lost.  Default is `false`
%     - connect_timeout(+Seconds)
%       Maximum time to try and reestablish a connection.  The
%       default is `600` (10 minutes).
%     - json_options(+Options)
%       Options passed to json_read_dict/3 to translate
%       `application/json` content to Prolog.  Default is `[]`.
%
%   @arg Address is a valid address   for tcp_connect/3. Normally a term
%   Host:Port, e.g., `localhost:32772`.
%   @arg Host is a path denoting the STOMP host.  Often just `/`.
%   @arg Headers is a dict with STOMP headers used for the ``CONNECT``
%   request.
%   @arg Connection is an opaque ground term that identifies the
%   connection.

stomp_connection(Address, Host, Headers, Callback, Connection) :-
    stomp_connection(Address, Host, Headers, Callback, Connection, []).

stomp_connection(Address, Host, Headers, Callback, Connection, Options) :-
    option(reconnect(Reconnect), Options, false),
    option(connect_timeout(Timeout), Options, 600),
    option(json_options(JSONOptions), Options, []),
    valid_address(Address),
    must_be(atom, Host),
    must_be(dict, Headers),
    must_be(callable, Callback),
    uuid(Connection),
    retractall(connection_property(Connection, _, _)),
    update_connection_mapping(
        Connection,
        _{ address: Address,
           callback: Callback,
           host: Host,
           headers: Headers,
           reconnect: Reconnect,
           connect_timeout: Timeout,
           json_options: JSONOptions
         }).

valid_address(Host:Port) :-
    !,
    must_be(atom, Host),
    must_be(integer, Port).
valid_address(Address) :-
    type_error(stom_address, Address).

connection_property(address).
connection_property(callback).
connection_property(host).
connection_property(headers).
connection_property(reconnect).
connection_property(connect_timeout).

%!  stomp_connection_property(?Connection, ?Property) is nondet.
%
%   True when Property, is a property  of Connection. Defined properties
%   are:
%
%     - address(Address)
%     - callback(Callback)
%     - host(Host)
%     - headers(Headers)
%     - reconnect(Bool)
%     - connect_timeout(Seconds)
%       All the above properties result from the stomp_connection/6
%       registration.
%     - receiver_thread_id(Thread)
%     - stream(Stream)
%     - heartbeat_thread_id(Thread)
%     - received_heartbeat(TimeStamp)
%       These describe an active STOMP connection.

stomp_connection_property(Connection, Property) :-
    var(Property),
    !,
    connection_property(Connection, Name, Value),
    Property =.. [Name,Value].
stomp_connection_property(Connection, Property) :-
    must_be(compound, Property),
    Property =.. [Name,Value],
    query_connection_property(Connection, Name, Value).

%!  stomp_destroy_connection(+Connection)
%
%   Destroy a connection. If it is active, first use stomp_teardown/1 to
%   disconnect.

stomp_destroy_connection(Connection) :-
    must_be(ground, Connection),
    (   query_connection_property(Connection, address, _)
    ->  stomp_teardown(Connection),
        retractall(connection_property(Connection, _, _))
    ;   existence_error(stomp_connection, Connection)
    ).

%!  stomp_setup(+Connection, +Options) is det.
%
%   Set up the actual socket connection and start receiving thread. This
%   is a no-op if the connection has   already been created. The Options
%   processed are below. Other options are passed to tcp_connect/3.
%
%     - timeout(+Seconds)
%       If tcp_connect/3 fails, retry until the timeout is reached.
%       If Seconds is `inf` or `infinite`, keep retrying forever.

stomp_setup(Connection, Options) :-
    stomp_setup(Connection, _New, Options).

stomp_setup(Connection, false, _) :-
    query_connection_property(Connection, stream, _Stream),
    !.
stomp_setup(Connection, New, Options) :-
    with_mutex(stomp, stomp_setup_guarded(Connection, New, Options)).

stomp_setup_guarded(Connection, false, _) :-
    query_connection_property(Connection, stream, _Stream),
    !.
stomp_setup_guarded(Connection, true, Options) :-
    query_connection_property(Connection, address, Address),
    connect(Connection, Address, Stream, Options),
    set_stream(Stream, encoding(utf8)),
    gensym(stomp_receive, Alias),
    thread_create(receive(Connection, Stream), ReceiverThreadId, [alias(Alias)]),
    debug(stomp(connection), 'Handling input on thread ~p', [ReceiverThreadId]),
    update_connection_mapping(Connection,
                              _{ receiver_thread_id: ReceiverThreadId,
                                 stream:Stream
                               }).

%!  connect(+Connection, +Address, -Stream, +Options) is det.
%
%   Connect to Address. If the option timeout(Sec) is present, retry the
%   connection until the timeout is reached.

connect(Connection, Address, Stream, Options) :-
    stomp_deadline(Connection, Deadline, Options),
    connect_with_deadline(Connection, Address, Stream, Deadline, Options).

connect_with_deadline(_Connection, Address, Stream, once, Options) :-
    !,
    tcp_connect(Address, Stream, Options).
connect_with_deadline(Connection, Address, Stream, Deadline, Options) :-
    number(Deadline),
    !,
    between(0, infinite, Retry),
      get_time(Now),
      Timeout is Deadline-Now,
      (   Now > 0
      ->  (   catch(call_with_time_limit(
                        Timeout,
                        tcp_connect(Address, Stream, Options)),
                    Error,
                    true)
          ->  (   var(Error)
              ->  !
              ;   (   debugging(stomp(connection))
                  ->  print_message(warning, Error)
                  ;   true
                  ),
                  wait_retry(Connection, Error, Retry, Deadline)
              )
          ;   wait_retry(Connection, failed, Retry, Deadline)
          )
      ;   throw(stomp_error(connect, Connection, timeout))
      ).
connect_with_deadline(Connection, Address, Stream, Deadline, Options) :-
    between(0, infinite, Retry),
      Error = error(Formal, _),
      (   catch(tcp_connect(Address, Stream, Options),
                Error,
                true)
      ->  (   var(Formal)
          ->  !
          ;   (   debugging(stomp(connection))
              ->  print_message(warning, Error)
              ;   true
              ),
              wait_retry(Connection, Formal, Retry, Deadline)
          )
      ;   wait_retry(Connection, failed, Retry, Deadline)
      ).

wait_retry(Connection, Why, _Retry, _Deadline) :-
    Why = error(stomp_error(connect, Connection, error(_)), _),
    !,
    throw(Why).
wait_retry(Connection, _Why, Retry, Deadline) :-
    Wait0 is min(10, 0.1 * (1<<Retry)),
    (   number(Deadline)
    ->  get_time(Now),
        Wait is min(Deadline-Now, Wait0)
    ;   Wait = Wait0
    ),
    (   Wait > 0
    ->  sleep(Wait),
        fail
    ;   throw(error(stomp_error(connect, Connection, timeout), _))
    ).


%!  stomp_teardown(+Connection) is semidet.
%
%   Tear down the socket connection, stop receiving thread and heartbeat
%   thread (if applicable).  The  registration   of  the  connection  as
%   created by stomp_connection/5 is preserved and the connection may be
%   reconnected using stomp_connect/1.

stomp_teardown(Connection) :-
    terminate_helper(Connection, receiver_thread_id),
    terminate_helper(Connection, heartbeat_thread_id),
    forall(query_connection_property(Connection, stream, Stream),
           close(Stream, [force(true)])),
    debug(stomp(connection), 'retract connection mapping for ~p', [Connection]),
    reset_connection_properties(Connection).

terminate_helper(Connection, Helper) :-
    retract(connection_property(Connection, Helper, Thread)),
    \+ thread_self(Thread),
    catch(thread_signal(Thread, abort), error(_,_), fail),
    !,
    thread_join(Thread, _Status).
terminate_helper(_, _).

reset_connection_properties(Connection) :-
    findall(P,
            (   query_connection_property(Connection, P, _),
                \+ connection_property(P)
            ), Ps),
    forall(member(P, Ps),
           retractall(connection_property(Connection, P, _))).

%!  stomp_reconnect(+Connection) is det.
%
%   Teardown the connection and try to reconnect.

stomp_reconnect(Connection) :-
    stomp_teardown(Connection),
    stomp_connect(Connection).

%!  stomp_connect(+Connection) is det.
%!  stomp_connect(+Connection, +Options) is det.
%
%   Setup the connection. First ensures a  socket connection and if this
%   is new send a ``CONNECT``  frame.   Protocol  version  and heartbeat
%   negotiation will be  handled.  ``STOMP``  frame   is  not  used  for
%   backward compatibility.
%
%   This predicate waits for the connection handshake to have completed.
%   Currently it waits for a maximum   of  10 seconds after establishing
%   the socket for the server reply.
%
%   Calling this on an established connection has no effect.
%
%   @see http://stomp.github.io/stomp-specification-1.2.html#CONNECT_or_STOMP_Frame).
%   @error stomp_error(connect, Connection, Detail) if no connection
%   could be established.

stomp_connect(Connection) :-
    stomp_connect(Connection, []).

stomp_connect(Connection, Options) :-
    update_reconnect_property(Connection),
    stomp_deadline(Connection, Deadline, Options),
    stomp_deadline_connect(Connection, Deadline, Options).

update_reconnect_property(Connection) :-
    query_connection_property(Connection, reconnect, disconnected),
    !,
    update_connection_property(Connection, reconnect, true).
update_reconnect_property(_).

stomp_deadline_connect(Connection, Deadline, Options) :-
    between(0, infinite, Retry),
      stomp_setup(Connection, New, [deadline(Deadline)|Options]),
      (   New == true
      ->  Error = error(Formal, _),
          catch(connect_handshake(Connection), Error, true),
          (   var(Formal)
          ->  !
          ;   stomp_teardown(Connection),
              wait_retry(Connection, Error, Retry, Deadline)
          )
      ;   query_connection_property(Connection, connected, _)
      ->  true
      ;   wait_connected(Connection)
      ->  true
      ;   stomp_teardown(Connection),
          wait_retry(Connection, failed, Retry, Deadline)
      ).

connect_handshake(Connection) :-
    query_connection_property(Connection, headers, Headers),
    query_connection_property(Connection, host, Host),
    send_frame(Connection,
               connect,
               Headers.put(_{ 'accept-version':'1.0,1.1,1.2',
                              host:Host
                            })),
    (   Heartbeat = Headers.get('heart-beat')
    ->  update_connection_property(Connection, 'heart-beat', Heartbeat)
    ;   true
    ),
    thread_self(Self),
    update_connection_property(Connection, waiting_thread, Self),
    (   thread_get_message(Self, stomp(connected(Connection, Status)),
                           [timeout(10)])
    ->  (   Status == true
        ->  get_time(Now),
            update_connection_property(Connection, connected, Now)
        ;   throw(error(stomp_error(connect, Connection, Status), _))
        )
    ;   throw(error(stomp_error(connect, Connection, timeout), _))
    ).

wait_connected(Connection) :-
    thread_wait(query_connection_property(Connection, connected, _),
                [ timeout(10),
                  wait_preds([connection_property/3])
                ]),
    !.
wait_connected(Connection) :-
    throw(error(stomp_error(connect, Connection, timeout), _)).

%!  stomp_deadline(+Connection, -Deadline, +Options) is det.
%
%   True when there is a connection timeout and Deadline is the deadline
%   for establishing a connection.  Deadline is one of
%
%     - Number
%       The deadline as a time stamp
%     - infinite
%       Keep trying
%     - once
%       Try to connect once.

stomp_deadline(_Connection, Deadline, Options) :-
    option(deadline(Deadline), Options),
    !.
stomp_deadline(Connection, Deadline, Options) :-
    (   option(timeout(Time), Options)
    ;   query_connection_property(Connection, connect_timeout, Time)
    ),
    !,
    (   number(Time)
    ->  get_time(Now),
        Deadline is Now+Time
    ;   must_be(oneof([inf,infinite]), Time),
        Deadline = infinite
    ).
stomp_deadline(_, once, _).


%!  stomp_send(+Connection, +Destination, +Headers, +Body) is det.
%
%   Send  a  ``SEND``  frame.  If   ``content-type``  is  not  provided,
%   ``text/plain`` will be used. ``content-length``   will  be filled in
%   automatically.
%
%   @see http://stomp.github.io/stomp-specification-1.2.html#SEND

stomp_send(Connection, Destination, Headers, Body) :-
    add_transaction(Headers, Headers1),
    send_frame(Connection, send, Headers1.put(destination, Destination), Body).

%!  stomp_send_json(+Connection, +Destination, +Headers, +JSON) is det.
%
%   Send a ``SEND`` frame. ``JSON`` can be either a JSON term or a dict.
%   ``content-type`` is filled in  automatically as ``application/json``
%   and ``content-length`` will be filled in automatically as well.
%
%   @see http://stomp.github.io/stomp-specification-1.2.html#SEND

stomp_send_json(Connection, Destination, Headers, JSON) :-
    add_transaction(Headers, Headers1),
    atom_json_term(Body, JSON,
                   [ as(string),
                     width(0)           % No layout for speed
                   ]),
    send_frame(Connection, send,
               Headers1.put(_{ destination:Destination,
                               'content-type':'application/json'
                             }),
               Body).

%!  stomp_subscribe(+Connection, +Destination, +Id, +Headers) is det.
%
%   Send a ``SUBSCRIBE`` frame.
%
%   @see http://stomp.github.io/stomp-specification-1.2.html#SUBSCRIBE

stomp_subscribe(Connection, Destination, Id, Headers) :-
    send_frame(Connection, subscribe,
               Headers.put(_{destination:Destination, id:Id})).

%!  stomp_unsubscribe(+Connection, +Id) is det.
%
%   Send an ``UNSUBSCRIBE`` frame.
%
%   @see http://stomp.github.io/stomp-specification-1.2.html#UNSUBSCRIBE

stomp_unsubscribe(Connection, Id) :-
    send_frame(Connection, unsubscribe, _{id:Id}).

%!  stomp_ack(+Connection, +MessageId, +Headers) is det.
%
%   Send an ``ACK`` frame. See stomp_ack/2 for simply passing the header
%   received with the message we acknowledge.
%
%   @see http://stomp.github.io/stomp-specification-1.2.html#ACK

stomp_ack(Connection, MessageId, Headers) :-
    send_frame(Connection, ack, Headers.put('message-id', MessageId)).

%!  stomp_nack(+Connection, +MessageId, +Headers) is det.
%
%   Send a ``NACK`` frame.  See  stomp_nack/2   for  simply  passing the
%   header received with the message we acknowledge.
%
%   @see http://stomp.github.io/stomp-specification-1.2.html#NACK

stomp_nack(Connection, MessageId, Headers) :-
    send_frame(Connection, nack, Headers.put('message-id', MessageId)).

%!  stomp_ack(+Connection, +MsgHeader) is det.
%!  stomp_nack(+Connection, +MsgHeader) is det.
%
%   Reply with an ACK or NACK based on the received message header. On a
%   STOMP 1.1 request we get an `ack` field in the header and reply with
%   an  `id`.  For  STOMP  1.2  we   reply  with  the  `message-id`  and
%   `subscription` that we received with the message.

stomp_ack(Connection, Header) :-
    stomp_ack_nack(Connection, ack, Header).

stomp_nack(Connection, Header) :-
    stomp_ack_nack(Connection, nack, Header).

stomp_ack_nack(Connection, Type, Header) :-
    (   Id = Header.get(ack)
    ->  send_frame(Connection, Type, _{id: Id})
    ;   Pass = _{'message-id':_, subscription:_},
        Pass :< Header
    ->  send_frame(Connection, Type, Pass)
    ).


%!  stomp_begin(+Connection, +Transaction) is det.
%
%   Send a ``BEGIN`` frame.
%   @see http://stomp.github.io/stomp-specification-1.2.html#BEGIN

stomp_begin(Connection, Transaction) :-
    send_frame(Connection, begin, _{transaction:Transaction}).

%!  stomp_commit(+Connection, +Transaction) is det.
%
%   Send a ``COMMIT`` frame.
%
%   @see http://stomp.github.io/stomp-specification-1.2.html#COMMIT

stomp_commit(Connection, Transaction) :-
    send_frame(Connection, commit, _{transaction:Transaction}).

%!  stomp_abort(+Connection, +Transaction) is det.
%
%   Send a ``ABORT`` frame.
%
%   @see http://stomp.github.io/stomp-specification-1.2.html#ABORT

stomp_abort(Connection, Transaction) :-
    send_frame(Connection, abort, _{transaction:Transaction}).

%!  stomp_transaction(+Connection, :Goal) is semidet.
%
%   Run Goal as  once/1,  tagging  all   ``SEND``  messages  inside  the
%   transaction with the transaction id.  If   Goal  fails  or raises an
%   exception the transaction is aborted.   Failure  or exceptions cause
%   the transaction to be aborted using   stomp_abort/2, after which the
%   result is forwarded.

stomp_transaction(Connection, Goal) :-
    uuid(TransactionID),
    stomp_transaction(Connection, Goal, TransactionID).

stomp_transaction(Connection, Goal, TransactionID) :-
    stomp_begin(Connection, TransactionID),
    (   catch(once(Goal), Error, true)
    ->  (   var(Error)
        ->  stomp_commit(Connection, TransactionID)
        ;   stomp_abort(Connection, TransactionID),
            throw(Error)
        )
    ;   stomp_abort(Connection, TransactionID),
        fail
    ).

in_transaction(TransactionID) :-
    prolog_current_frame(Frame),
    prolog_frame_attribute(
        Frame, parent_goal,
        stomp_transaction(_Connection, _Goal, TransactionID)).

add_transaction(Headers, Headers1) :-
    in_transaction(TransactionID),
    !,
    Headers1 = Headers.put(transaction, TransactionID).
add_transaction(Headers, Headers).


%!  stomp_disconnect(+Connection, +Headers) is det.
%
%   Send a ``DISCONNECT`` frame. If the   connection has the `reconnect`
%   property set to `true`, this  will   be  reset  to `disconnected` to
%   avoid  reconnecting.  A  subsequent    stomp_connect/2   resets  the
%   reconnect status.
%
%   @see http://stomp.github.io/stomp-specification-1.2.html#DISCONNECT

stomp_disconnect(Connection, Headers) :-
    (   query_connection_property(Connection, reconnect, true)
    ->  update_connection_property(Connection, reconnect, disconnected)
    ;   true
    ),
    send_frame(Connection, disconnect, Headers).

%!  send_frame(+Connection, +Command, +Headers) is det.
%!  send_frame(+Connection, +Command, +Headers, +Body) is det.
%
%   Send a frame. If no connection is  established connect first. If the
%   sending results in an I/O error, disconnect, reconnect and try again
%   if the `reconnect` propertys is set.

send_frame(Connection, Command, Headers) :-
    send_frame(Connection, Command, Headers, "").

send_frame(Connection, Command, Headers, Body) :-
    Error = error(Formal,_),
    catch(send_frame_guarded(Connection, Command, Headers, Body),
          Error,
          true),
    (   var(Formal)
    ->  true
    ;   query_connection_property(Connection, reconnect, true)
    ->  notify(Connection, disconnected),
        stomp_teardown(Connection),
        debug(stomp(connection), 'Sending thread reconnects', []),
        send_frame(Connection, Command, Headers, Body)
    ;   notify(Connection, disconnected),
        throw(Error)
    ).

send_frame_guarded(Connection, Command, Headers, Body) :-
    has_body(Command),
    !,
    connection_stream(Connection, Stream),
    default_content_type('text/plain', Headers, Headers1),
    body_bytes(Body, ContentLength),
    Headers2 = Headers1.put('content-length', ContentLength),
    with_output_to(Stream,
                   ( send_command(Stream, Command),
                     send_header(Stream, Headers2),
                     format(Stream, '~w\u0000\n', [Body]),
                     flush_output(Stream))).
send_frame_guarded(Connection, heartbeat, _Headers, _Body) :-
    !,
    connection_stream(Connection, Stream),
    nl(Stream),
    flush_output(Stream).
send_frame_guarded(Connection, Command, Headers, _Body) :-
    connection_stream(Connection, Stream),
    with_output_to(Stream,
                   ( send_command(Stream, Command),
                     send_header(Stream, Headers),
                     format(Stream, '\u0000\n', []),
                     flush_output(Stream))).

%!  connection_stream(+Connection, -Stream)

connection_stream(Connection, Stream) :-
    query_connection_property(Connection, stream, Stream),
    !.
connection_stream(Connection, Stream) :-
    stomp_connect(Connection),
    query_connection_property(Connection, stream, Stream).

send_command(Stream, Command) :-
    string_upper(Command, Upper),
    format(Stream, '~w\n', [Upper]).

send_header(Stream, Headers) :-
    dict_pairs(Headers, _, Pairs),
    maplist(send_header_line(Stream), Pairs),
    nl(Stream).

send_header_line(Stream, Name-Value) :-
    (   number(Value)
    ->  format(Stream, '~w:~w\n', [Name,Value])
    ;   escape_value(Value, String),
        format(Stream, '~w:~w\n', [Name,String])
    ).

escape_value(Value, String) :-
    split_string(Value, "\n:\\", "", [_]),
    !,
    String = Value.
escape_value(Value, String) :-
    string_codes(Value, Codes),
    phrase(escape(Codes), Encoded),
    string_codes(String, Encoded).

escape([]) --> [].
escape([H|T]) --> esc(H), escape(T).

esc(0'\r) --> !, "\\r".
esc(0'\n) --> !, "\\n".
esc(0':)  --> !, "\\c".
esc(0'\\) --> !, "\\\\".
esc(C)    --> [C].

default_content_type(ContentType, Header0, Header) :-
    (   get_dict('content-type', Header0, _)
    ->  Header = Header0
    ;   put_dict('content-type', Header0, ContentType, Header)
    ).

body_bytes(String, Bytes) :-
    setup_call_cleanup(
        open_null_stream(Out),
        ( write(Out, String),
          byte_count(Out, Bytes)
        ),
        close(Out)).


		 /*******************************
		 *        INCOMING DATA		*
		 *******************************/

%!  read_frame(+Connection, +Stream, -Frame) is det.
%
%   Read a frame from a STOMP stream.   On end-of-file, Frame is unified
%   with the atom `end_of_file`. Otherwise  it   is  a  dict holding the
%   `cmd`, `headers` (another dict) and `body` (a string)

read_frame(Connection, Stream, Frame) :-
    read_command(Stream, Command),
    (   Command == end_of_file
    ->  Frame = end_of_file
    ;   Command == heartbeat
    ->  Frame = stomp_frame{cmd:heartbeat}
    ;   read_header(Stream, Header),
        (   has_body(Command)
        ->  read_content(Connection, Stream, Header, Content),
            Frame = stomp_frame{cmd:Command, headers:Header, body:Content}
        ;   Frame = stomp_frame{cmd:Command, headers:Header}
        )
    ).

has_body(send).
has_body(message).
has_body(error).

read_command(Stream, Command) :-
    read_line_to_string(Stream, String),
    debug(stomp(command), 'Got line ~p', [String]),
    (   String == end_of_file
    ->  Command = end_of_file
    ;   String == ""
    ->  Command = heartbeat
    ;   string_lower(String, Lwr),
        atom_string(Command, Lwr)
    ).

read_header(Stream, Header) :-
    read_header_lines(Stream, Pairs, []),
    dict_pairs(Header, stomp_header, Pairs).

read_header_lines(Stream, Header, Seen) :-
    read_line_to_string(Stream, Line),
    (   Line == ""
    ->  Header = []
    ;   sub_string(Line, Before, _, After, ":")
    ->  sub_atom(Line, 0, Before, _, Name),
        (   memberchk(Name, Seen)
        ->  read_header_lines(Stream, Header, Seen)
        ;   sub_string(Line, _, After, 0, Value0),
            convert_value(Name, Value0, Value),
            Header = [Name-Value|More],
            read_header_lines(Stream, More, [Name|Seen])
        )
    ).

convert_value('content-length', String, Bytes) :-
    !,
    number_string(Bytes, String).
convert_value(_, String, Value) :-
    unescape_header_value(String, Value).

unescape_header_value(String, Value) :-
    sub_atom(String, _, _, _, "\\"),
    !,
    string_codes(String, Codes),
    phrase(unescape(Plain), Codes),
    string_codes(Value, Plain).
unescape_header_value(String, String).

unescape([H|T]) --> "\\", !, unesc(H), unescape(T).
unescape([H|T]) --> [H], !, unescape(T).
unescape([]) --> [].

unesc(0'\r) --> "r", !.
unesc(0'\n) --> "n", !.
unesc(0':)  --> "c", !.
unesc(0'\\) --> "\\", !.
unesc(_) --> [C], { syntax_error(invalid_stomp_escape(C)) }.

%!  read_content(+Connection, +Stream, +Header, -Content) is det.
%
%   Read the body. Note that the body   may  be followed by an arbitrary
%   number of newlines. We leave them in place to avoid blocking.

read_content(Connection, Stream, Header, Content) :-
    _{ 'content-length':Bytes,
       'content-type':Type
     } :< Header,
    setup_call_cleanup(
        stream_range_open(Stream, DataStream, [size(Bytes)]),
        read_content(Connection, Type, DataStream, Header, Content),
        close(DataStream)).

read_content(Connection, "application/json", Stream, _Header, Content) :-
    !,
    query_connection_property(Connection, json_options, Options),
    json_read_dict(Stream, Content, Options).
read_content(_Connection, _Type, Stream, _Header, Content) :-
    read_string(Stream, _, Content).


%!  receive(+Connection, +Stream) is det.
%
%   Read and process incoming messages from Stream.

receive(Connection, Stream) :-
    E = error(Formal, _),
    catch(read_frame(Connection, Stream, Frame), E, true),
    !,
    (   var(Formal)
    ->  debug(stomp(receive), 'received frame ~p', [Frame]),
        (   Frame == end_of_file
        ->  receive_done(Connection, end_of_file)
        ;   process_frame(Connection, Frame),
            receive(Connection, Stream)
        )
    ;   receive_done(Connection, E)
    ).
receive(Connection, _Stream) :-
    receive_done(Connection, "failed to read frame").

%!  receive_done(+Connection, +Why)
%
%   The receiver thread needs to  close   the  connection due to reading
%   end-of-file, an I/O error or failure to parse a frame. If connection
%   is configured to be restarted this   thread  will try to reestablish
%   the connection. After reestablishing the   connection  this receiver
%   thread terminates.

receive_done(Connection, Why) :-
    (   Why = error(_,_)
    ->  print_message(warning, Why)
    ;   true
    ),
    notify(Connection, disconnected),
    stomp_teardown(Connection),
    (   query_connection_property(Connection, reconnect, true)
    ->  debug(stomp(connection), 'Receiver thread reconnects (~p)', [Why]),
        stomp_connect(Connection)
    ;   debug(stomp(connection), 'Receiver thread terminates (~p)', [Why])
    ),
    thread_self(Me),
    thread_detach(Me).

%!  process_frame(+Connection, +Frame) is det.
%
%   Process an incoming frame.

process_frame(Connection, Frame) :-
    Frame.cmd = heartbeat, !,
    get_time(Now),
    debug(stomp(heartbeat), 'received heartbeat at ~w', [Now]),
    update_connection_property(Connection, received_heartbeat, Now),
    notify(Connection, heartbeat, _{}, "").
process_frame(Connection, Frame) :-
    _{cmd:FrameType, headers:Headers, body:Body} :< Frame,
    !,
    process_connect(FrameType, Connection, Frame),
    notify(Connection, FrameType, Headers, Body).
process_frame(Connection, Frame) :-
    _{cmd:FrameType, headers:Headers} :< Frame,
    process_connect(FrameType, Connection, Frame),
    notify(Connection, FrameType, Headers).

process_connect(connected, Connection, Frame) :-
    retract(connection_property(Connection, waiting_thread, Waiting)),
    !,
    thread_send_message(Waiting, stomp(connected(Connection, true))),
    start_heartbeat_if_required(Connection, Frame.headers).
process_connect(error, Connection, Frame) :-
    retract(connection_property(Connection, waiting_thread, Waiting)),
    !,
    thread_send_message(
        Waiting,
        stomp(connected(Connection, error(Frame.body)))).
process_connect(_, _, _).

start_heartbeat_if_required(Connection, Headers) :-
    (   query_connection_property(Connection, 'heart-beat', CHB),
        SHB = Headers.get('heart-beat')
    ->  start_heartbeat(Connection, CHB, SHB)
    ;   true
    ).

start_heartbeat(Connection, CHB, SHB) :-
    extract_heartbeats(CHB, CX, CY),
    extract_heartbeats(SHB, SX, SY),
    calculate_heartbeats(CX-CY, SX-SY, X-Y),
    \+ (X =:= 0, Y =:= 0),
    !,
    debug(stomp(heartbeat), 'calculated heartbeats are ~p,~p', [X, Y]),
    SendSleep is X / 1000,
    ReceiveSleep is Y / 1000 + 2,
    (   X =:= 0
    ->  SleepTime = ReceiveSleep
    ;   (   Y =:= 0
        ->  SleepTime = SendSleep
        ;   SleepTime is gcd(X, Y) / 2000
        )
    ),
    get_time(Now),
    gensym(stomp_heartbeat, Alias),
    debug(stomp(heartbeat), 'Creating heartbeat thread (~p ~p ~p)',
          [SendSleep, ReceiveSleep, SleepTime]),
    thread_create(heartbeat_loop(Connection, SendSleep, ReceiveSleep,
                                 SleepTime, Now, Now),
                  HeartbeatThreadId, [alias(Alias)]),
    update_connection_mapping(Connection,
                              _{ heartbeat_thread_id:HeartbeatThreadId,
                                 received_heartbeat:Now
                               }).
start_heartbeat(_, _, _).

extract_heartbeats(Heartbeat, X, Y) :-
    split_string(Heartbeat, ",", " ", [XS, YS]),
    number_string(X, XS),
    number_string(Y, YS).

calculate_heartbeats(CX-CY, SX-SY, X-Y) :-
    (   CX =\= 0, SY =\= 0
    ->  X is max(CX, floor(SY))
    ;   X = 0
    ),
    (   CY =\= 0, SX =\= 0
    ->  Y is max(CY, floor(SX))
    ;   Y = 0
    ).

heartbeat_loop(Connection, SendSleep, ReceiveSleep, SleepTime,
               SendTime, ReceiveTime) :-
    sleep(SleepTime),
    get_time(Now),
    (   Now - SendTime > SendSleep
    ->  SendTime1 = Now,
        debug(stomp(heartbeat), 'sending a heartbeat message at ~p', [Now]),
        send_frame(Connection, heartbeat, _{})
    ;   SendTime1 = SendTime
    ),
    (   Now - ReceiveTime > ReceiveSleep
    ->  ReceiveTime1 = Now,
        check_receive_heartbeat(Connection, Now, ReceiveSleep)
    ;   ReceiveTime1 = ReceiveTime
    ),
    heartbeat_loop(Connection, SendSleep, ReceiveSleep, SleepTime,
                   SendTime1, ReceiveTime1).

check_receive_heartbeat(Connection, Now, ReceiveSleep) :-
    query_connection_property(Connection, received_heartbeat, ReceivedHeartbeat),
    DiffReceive is Now - ReceivedHeartbeat,
    (   DiffReceive > ReceiveSleep
    ->  debug(stomp(heartbeat),
              'Heartbeat timeout: diff_receive=~p, time=~p, lastrec=~p',
              [DiffReceive, Now, ReceivedHeartbeat]),
        notify(Connection, heartbeat_timeout)
    ;   true
    ).

%!  notify(+Connection, +FrameType) is det.
%!  notify(+Connection, +FrameType, +Header) is det.
%!  notify(+Connection, +FrameType, +Header, +Body) is det.
%
%   Call the callback using  FrameType.

notify(Connection, FrameType) :-
    notify(Connection, FrameType, stomp_header{cmd:FrameType}, "").

notify(Connection, FrameType, Header) :-
    notify(Connection, FrameType, Header, "").

notify(Connection, FrameType, Header, Body) :-
    query_connection_property(Connection, callback, Callback),
    Error = error(Formal,_),
    (   catch_with_backtrace(
            call(Callback, FrameType, Connection, Header, Body),
            Error, true)
    ->  (   var(Formal)
        ->  true
        ;   print_message(warning, Error)
        )
    ;   true
    ).

update_connection_mapping(Connection, Dict) :-
    dict_pairs(Dict, _, Pairs),
    maplist(update_connection_property(Connection), Pairs).

update_connection_property(Connection, Name-Value) :-
    update_connection_property(Connection, Name, Value).

update_connection_property(Connection, Name, Value) :-
    transaction(update_connection_property_(Connection, Name, Value)).

update_connection_property_(Connection, Name, Value) :-
    retractall(connection_property(Connection, Name, _)),
    asserta(connection_property(Connection, Name, Value)).

query_connection_property(Connection, Name, Value) :-
    (   nonvar(Name),
        nonvar(Connection)
    ->  connection_property(Connection, Name, Value),
        !
    ;   connection_property(Connection, Name, Value)
    ).


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(stomp_error(connect, Connection, error(Message))) -->
    { connection_property(Connection, address, Address) },
    [ 'STOMPL: Failed to connect to ~p: ~p'-[Address, Message] ].
prolog:error_message(stomp_error(connect, Connection, Detail)) -->
    { connection_property(Connection, address, Address) },
    [ 'STOMPL: Failed to connect to ~p: ~p'-[Address, Detail] ].
