/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2020, Jeffrey Rosenwald
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

:- module(tipc,
          [ tipc_socket/2,              % -Socket  +Type
            tipc_close_socket/1,        % +Socket
            tipc_setopt/2,              % +Socket, +Option
            tipc_bind/3,                % +Socket, +Address +Scope
            tipc_listen/2,              % +Socket, +BackLog
            tipc_accept/3,              % +Master, -Slave, -PeerName
            tipc_open_socket/3,         % +Socket, -Read, -Write
            tipc_get_name/2,            % +Socket, -Address
            tipc_get_peer_name/2,       % +Socket, -Address
            tipc_connect/2,             % +Socket, +Address
            tipc_receive/4,             % +Socket, -Data, -Sender, +Options
            tipc_send/4,                % +Socket, +Data, +Receiver, +Options
%           tipc_subscribe/5, % +Socket, +Address, +Timeout, +Filter,
%           +Usr_handle

%           tipc_receive_subscr_event/2, % +Socket, -Event
            tipc_canonical_address/2,   % -Address, +port_id/2
            tipc_service_probe/1,       % ?Address
            tipc_service_probe/2,       % ?Address, ?PortId
            tipc_service_port_monitor/2, % +Address, :Goal
            tipc_service_port_monitor/3, % +Address, :Goal, +Timeout
            tipc_service_exists/1,      % +Address
            tipc_service_exists/2,       % +Address +Timeout
            tipc_initialize/0            %
          ]).
:- autoload(library(apply),[maplist/3]).
:- autoload(library(lists),[member/2]).

:- use_foreign_library(foreign(tipc)).

:- multifile tipc_stack_initialize/0.

/** <module> TIPC Sockets

Transparent Inter-Process Communication  (TIPC)   provides  a  flexible,
reliable, fault-tolerant, high-speed,  and   low-overhead  framework for
inter-process  communication  between  federations   of  trusted  peers,
operating as a unit. It was developed  by   Ericsson  AB,  as a means to
provide for communications between Common  Control Systems processes and
Network  Element  peers  in  telephone    switching  systems,  sometimes
operating at arm's  length  on  different   line  cards  or  mainframes.
Delegation of responsibility in  this  way   is  one  of the fundamental
precepts of the Erlang programming system,   also developed at Ericsson.
TIPC represents a more generalized version of the same behavioral design
pattern. For an overview, please see: tipc_overview.md.

__Errors__

The TIPC module uses the error   handling functions from library(socket)
and therefore all the functions below may throw error(socket_error(Code,
Message)) where `Code` is the  lowercase   version  of the C-macro error
code and `Message` is an atom describing   the error in a human friendly
format, depending on the current  locale.   See  the  socket library for
details.

@author Jeffrey Rosenwald (JeffRose@acm.org)
@see    <http://tipc.sf.net>, <http://www.erlang.org>
@compat Linux only
*/

%!   tipc_socket(-SocketId, +SocketType) is det.
%
%    Creates  a  TIPC-domain  socket  of    the  type  specified  by
%    SocketType, and unifies it to an  identifier, SocketId.
%
%    @param SocketType is one of  the   following  atoms:
%
%    * rdm - unnumbered, reliable datagram service,
%    * dgram - unnumbered, unreliable datagram service,
%    * seqpacket - numbered, reliable datagram service, and
%    * stream - reliable, connection-oriented byte-stream
%      service

%!   tipc_close_socket(+SocketId) is det.
%
%    Closes the indicated socket, making SocketId invalid. In stream
%    applications, sockets are closed by closing both stream handles
%    returned by tipc_open_socket/3.  There  are   two  cases  where
%    tipc_close_socket/1   is   used   because     there    are   no
%    stream-handles:
%
%     * After tipc_accept/3, the server does  a fork/1 to handle the
%     client in a sub-process. In this   case the accepted socket is
%     not longer needed from the main   server and must be discarded
%     using tipc_close_socket/1.
%
%     *  If,  after  discovering   the    connecting   client   with
%     tipc_accept/3,  the  server  does  not   want  to  accept  the
%     connection, it should discard the  accepted socket immediately
%     using tipc_close_socket/1.
%
%    @param SocketId the socket identifier returned by tipc_socket/2
%    or tipc_accept/3.

%!  tipc_subscribe(+SocketId, +NameSeqAddress, +Timeout, +Filter, +UserHandle) is det.
%
%    Subscribes to events related to a publisher that is bound to
%    the multi-cast address specified in NameSeqAddress.
%
%    Timeout  specifies  the  duration  of    the   subscription  in
%    milliseconds. Specifying a  Timeout  of   -1,  provides  for  a
%    subscription of infinite duration. Filter  specifies the events
%    of interest to the  subscriber.  It   is  an  integer bit field
%    where:
%
%    * (1 is X /\ 1) -> "notify port availability",
%    * (2 is X /\ 2) -> "notify service availability", and
%    * (4 is X /\ 4) -> "notify subscription cancellation".
%
%    UserHandle is an eight-byte, user-defined buffer that is passed
%    transparently to the subscriber  with   every  notification. An
%    application that uses this service must   be  prepared to parse
%    event notifications received from  the   TIPC  topology server.
%    tipc_receive_subscr_event/4, is provided for this purpose.
%
%    It is possible to have  multiple   subscriptions  active on the
%    same socket any one time, making periodic subscription renewal
%    easy.
%
%    _|This predicate is valid only on connections to the topology
%    server: name(1,1,0).|_
%
%    _|Please note that this predicate should be considered private.
%    It has been removed from the Prolog public API. Its use in user
%    programs is strongly discouraged. See the "tipc_service"
%    predicates for equivalent functionality.|_
%
%    @param SocketId the socket  identifier   that  was  provided by
%    tipc_socket/2,  and  is  connected  to   the  topology  server,
%    name(1,1,0), via tipc_connect/2.
%
%    @param NameSeqAddress the name_seq/3 address  of the service to
%    be monitored.
%
%    @param PortId the port-id of the socket   that  is bound to the
%    service
%
%    @param Timeout an integer that specifies   the  duration of the
%    subscription in milliseconds. A  duration   of  -1, specifies a
%    subscription of infinite duration.  @param   Filter  the  event
%    filter bit map.
%
%    @param UserHandle an eight-byte code that is passed
%    transparently to the user with each event notification.

%!   tipc_open_socket(+SocketId, -InStream, -OutStream) is det.
%
%    Opens two SWI-Prolog I/O-streams, one to   deal with input from
%    the socket and one with output   to  the socket. If tipc_bind/3
%    has been called on the socket, OutStream is useless and will
%    not be created. After closing both InStream and OutStream, the
%    socket itself is discarded.

%!   tipc_bind(+Socket, +Address, +ScopingOption) is det.
%
%    Associates/disassociates a socket with the name/3 or name_seq/3
%    address specified in Address. It also registers/unregisters it in
%    the  topology  server  name  table.   This  makes  the  address
%    visible/invisible to the rest of the   network according to the
%    scope specified in ScopingOption. ScopingOption   is a grounded
%    term that is one of:
%
%     $ scope(Scope) : where Scope is  one of: =zone=, =cluster=, or
%     =node=. Servers may bind to more   than  one address by making
%     successive calls to tipc_bind/3, one for  each address that it
%     wishes to advertise. The server will   receive traffic for all
%     of them. A server may, for  example, register one address with
%     node scope, another with cluster scope,  and a third with zone
%     scope. A client may then limit   the scope of its transmission
%     by specifying the appropriate address.
%
%     $ no_scope(Scope) : where Scope is as defined above. An
%     application may target a specific address for removal from its
%     collection of addresses by specifying the address and its
%     scope. The scoping option, =|no_scope(all)|=, may be used to
%     unbind the socket from all of its registered addresses. This
%     feature allows an application to gracefully exit from service.
%     Because the socket remains open, the application may continue
%     to service current transactions to completion. TIPC however,
%     will not schedule any new work for the server instance. If no
%     other servers are available, the work will be rejected or
%     dropped according to the socket options specified by the
%     client.
%
%    Connection-oriented, byte-stream services are  implemented with
%    this predicate combined with   tipc_listen/2 and tipc_accept/3.
%    Connectionless, datagram services  may   be  implemented  using
%    tipc_receive/4.
%
%    Note that clients do not  need  to   bind  to  any address. Its
%    port-id is sufficient for this role.   And server sockets (e.g.
%    those that are bound to name/3   or  name_seq/3, addresses) may
%    not act as clients. That is, they may not originate connections
%    from the socket using tipc_connect/2. Servers however, may
%    originate datagrams from bound sockets using tipc_send/4.
%    Please see the TIPC programmers's guide for other restrictions.

%!   tipc_listen(+Socket,+Backlog) is det.
%
%    Listens for incoming requests for connections. Backlog
%    indicates how many pending connection requests are allowed.
%    Pending requests are requests that are not yet acknowledged
%    using tipc_accept/3. If the indicated number is exceeded, the
%    requesting client will be signalled that the service is
%    currently not available. A suggested default value is 5.

%!   tipc_accept(+Socket, -Slave, -Peer) is det.
%
%    Blocks on a server socket  and   waits  for connection requests
%    from clients. On success,  it  creates   a  new  socket for the
%    client and binds the identifier to Slave.  Peer is bound to the
%    TIPC address, port_id/2, of the client.

%!   tipc_connect(+Socket, +TIPC_address) is det.
%
%    Provides a connection-oriented, client-interface   to connect a
%    socket to a given TIPC_address.   After  successful completion,
%    tipc_open_socket/3 may be used  to   create  I/O-Streams to the
%    remote socket.

%!   tipc_get_name(+Socket, -TIPC_address) is det.
%
%    Unifies TIPC_address with the port-id assigned to the socket.

%!   tipc_get_peer_name(+Socket, -TIPC_address) is det.
%
%    Unifies TIPC_address with the port-id assigned to the socket
%    that this socket is connected to.

%!   tipc_setopt(+Socket,+Option) is det.
%
%    Sets options on the socket. Defined options are:
%
%    $ importance(+Priority) :
%    Allow sockets to assign a priority   to their traffic. Priority
%    is one of : =low= (default), =medium=, =high=, or =critical=.
%
%    $ src_droppable(+Boolean) :
%    Allow TIPC to silently discard packets in congested situations,
%    rather than queuing them for later transmission.
%
%    $ dest_droppable(+Boolean) :
%    Allow TIPC to silently discard packets in congested situations,
%    rather than returning them to the sender as undeliverable.
%
%    $ conn_timeout(+Seconds) :
%    Specifies the time interval that tipc_connect/2 will use before
%    abandoning a connection attempt. Default: 8.000 sec.

%!   tipc_receive(+Socket, -Data, -From, +OptionList) is det.
%
%    Waits  for,  and  returns  the  next  datagram.  Like  its  UDP
%    counterpart, the data are returned as   a  Prolog string object
%    (see string_codes/2). From is  an   address  structure of the
%    form port_id/2, indicating the sender of the message.
%
%     Defined options are:
%
%      * as(+Type)
%      Defines the returned term-type. Type is one of atom, codes or
%      string (default).
%
%      * nonblock
%      Poll the socket and return immediately. If a message is
%      present, it is returned. If not, then an exception,
%      error(socket_error(eagain, Message), _), will be thrown. Users
%      are cautioned not to "spin" unnecessarily on non-blocking
%      receives as they may prevent the system from servicing other
%      background activities such as XPCE event dispatching.
%
%    The typical sequence to receive a connectionless TIPC datagram is:
%
%    ==
%    receive :-
%            tipc_socket(S, dgram),
%            tipc_bind(S, name(18888, 10, 0), scope(zone)),
%            repeat,
%                tipc_receive(Socket, Data, From, [as(atom)]),
%                format('Got ~q from ~q~n', [Data, From]),
%                Data == quit,
%            !, tipc_close_socket(S).
%    ==
%

%!   tipc_send(+Socket, +Data, +To, +Options) is det.
%
%    sends a TIPC datagram to one or more destinations. Like its UDP
%    counterpart, Data is a string, atom  or code-list providing the
%    data to be sent.  To  is   a  name/3,  name_seq/3, or port_id/2
%    address structure. See tipc_overview.txt, for more information
%    on TIPC Address Structures. Options is currently unused.
%
%    A simple example to send a connectionless TIPC datagram is:
%
%    ==
%    send(Message) :-
%            tipc_socket(S, dgram),
%            tipc_send(S, Message, name(18888, 10,0), []),
%            tipc_close_socket(S).
%    ==
%
%    Messages are delivered silently unless  some form of congestion
%    was encountered and the   =|dest_droppable(false)|=  option was
%    issued on the sender's socket. In  this case, the send succeeds
%    but a notification in the form of  an empty message is returned
%    to the sender from  the  receiver,   indicating  some  kind  of
%    delivery failure. The port-id of the   receiver  is returned in
%    congestion conditions. A =|port_id(0,0)|=, is   returned if the
%    destination address was invalid. Senders   and receivers should
%    beware of this possibility.
%
%

%!  tipc_event(+Data, -Event, -Residue) is det.
%
%   Parses event notifications received from   the  topology server
%   into Prolog structures. This  predicate   has  been  permanently
%   removed.
%

%!  tipc_receive_subscr_event(+Socket, -Event) is semidet.
%
%   Receives and parses event notifications received from the
%   TIPC Topology Server.
%
%   _|Please note that this predicate is considered private. Its use
%   in user programs is strongly discouraged. See the
%   tipc-service predicates for alternatives.|_
%
%   @param Socket is a TIPC socket that has been previously
%   connected to the topology server using tipc_connect/2, and for
%   which a subscription has been sent using tipc_subscribe/5,
%   above.
%
%   @param Event is the structure: =|tipc_event(-Action, -Subscr,
%   -Found, -Port_id)|=. On subscription timeout, the
%   atom, =subscr_timeout= is returned. Subscr is the name-sequence
%   address of the original subscription.
%
%   Action is one of:
%
%     $ published :
%     The socket specified by Port_id has been bound to
%     the name_seq/3 address specified in Found.
%
%     $ withdrawn :
%     The socket specified by Port_id has been
%     unbound from the name_seq/3 address specified in Found. See
%     the no_scope/1 option of tipc_bind/3.

:- multifile
    prolog:message/3.

prolog:message(error(socket_error(_Code, Message), _)) -->
    [ 'Socket error: ~w'-[Message] ].

%!  tipc_canonical_address(-CanonicalAddress, +PortId) is det.
%
%   Translates a port_id/2 address into canonical TIPC form:
%
%   * tipc_address(Zone, Cluster, Node, Reference)
%
%   It is provided for debugging an printing purposes only. The
%   canonical address is not used for any other purpose.

integerAsU32(In, Out) :-
    nonvar(In),
    (   In < 0
    ->  Out is In + 0x100000000
    ;   Out is In
    ).
integerAsU32(In, Out) :-
    nonvar(Out),
    (   Out > 0x7fffffff
    ->  In is Out - 0x100000000
    ;   In is Out
    ).

tipc_canonical_address(tipc_address(Z,C,N, Ref1), port_id(Ref, Node)) :-
    integerAsU32(Ref, Ref1),
    integerAsU32(Node, X),
    Z is (X >> 24) /\ 0xFF,
    C is (X >> 12) /\ 0xFFF,
    N is X /\ 0xFFF.

user:portray(port_id(Ref, Node)) :-
    tipc_canonical_address(tipc_address(Z,C,N, Ref1), port_id(Ref, Node)),
    format('port_id(''<~w.~w.~w:~w>'')', [Z,C,N, Ref1]).

%!  tipc_service_exists(+Address, +Timeout) is semidet.
%!  tipc_service_exists(+Address) is semidet.
%
%   Interrogates the TIPC topology server to see if a service is
%   available at an advertised Address.
%
%   @param Address is one of:   =|name(Type,  Instance, Domain)|= or
%   =|name_seq(Type,  Lower,  Upper)|=.   A    name/3,   address  is
%   translated to a name_seq/3, following, where Lower and Upper are
%   assigned the value of Instance. Domain is unused and must be
%   zero. A =|name_seq(Type, Lower, Upper)|= is a multi-cast
%   address. This predicate succeeds if there is at least one
%   service that would answer according to multi-cast addressing
%   rules.
%
%   @param Timeout is optional. It is a non-negative real number
%   that specifies the amount of time in seconds to block and wait
%   for a service to become available. Fractions of a second are
%   also permissible.
%

tipc_address(name(T, I, 0), name_seq(T, I, I)).
tipc_address(name_seq(T, L, U), name_seq(T, L, U)).
tipc_address(mcast(T, L, U), name_seq(T, L, U)).
%
%

tipc_service_exists(Address) :-
    tipc_service_exists(Address, 0.0).

tipc_service_exists(Address, Timeout) :-
    tipc_address(Address, NameSeq),
    !,
    ITime is integer(Timeout * 1000),
    try_finally(tipc_socket(S, seqpacket),
                tipc_close_socket(S)),
    tipc_connect(S, name(1,1,0)),   % connect to the topology server
    tipc_subscribe(S, NameSeq, ITime, 2, "swipl"),
    repeat,
        tipc_receive_subscr_event(S, Data),
        (   Data == subscr_timeout
        ->  !, fail
        ;   Data = tipc_event(published, NameSeq, _FoundSeq, _Port_id)
        ),
    !.

%!  tipc_service_probe(?Address) is nondet.
%!  tipc_service_probe(?Address, ?PortId) is nondet.
%   Allows a user to discover the instance ranges and/or port-ids
%   for a particular service.
%
%   @param Address is a name_seq/3 address. The address type must be
%   grounded.
%
%   @param PortId is  unified  with  the   port-id  for  a  specific
%   name_sequence address.
%

try_finally(Setup, Cleanup) :-
    setup_call_cleanup(Setup, ( Solution = yes ; Solution = no ), Cleanup),
    Solution = yes.

tipc_service_probe(Address) :-
    tipc_address(Address, name_seq(Type, Lower, Upper)),
    integer(Type),
    NameSeq = name_seq(Type, Lower, Upper),
    try_finally(tipc_socket(S, seqpacket), tipc_close_socket(S)),
    tipc_connect(S, name(1,1,0)),   % connect to the topology server
    tipc_subscribe(S, name_seq(Type, 0, 4294967295), 0, 2, "swipl"),  % look for everything
    sp_collect(S, Members),
    !,
    member([NameSeq, _], Members).

tipc_service_probe(Address, PortId) :-
    tipc_address(Address, name_seq(Type, Lower, Upper)),
    integer(Type),
    NameSeq = name_seq(Type, Lower, Upper),
    try_finally(tipc_socket(S, seqpacket), tipc_close_socket(S)),
    tipc_connect(S, name(1,1,0)),   % connect to the topology server
    tipc_subscribe(S, name_seq(Type, 0, 4294967295), 0, 1, "swipl"),  % look for everything
    sp_collect(S, Members),
    !,
    member([NameSeq, PortId], Members).

sp_collect(S, Members) :-
    findall([NameSeq, PortId],
        ( repeat,
          tipc_receive_subscr_event(S, Data),
          (   Data == subscr_timeout
              ->  !, fail
              ;   Data = tipc_event(published, _Service, NameSeq, PortId)
          )
        ), Members).

%!  tipc_service_port_monitor(+Addresses, :Goal) is det.
%!  tipc_service_port_monitor(+Addresses, :Goal, ?Timeout) is det.
%
%   Monitors a collection of worker threads that are bound to a list
%   of Addresses. A single port monitor may be used to provide
%   surveillance over workers that are providing a number of
%   different services. For a given address type, discontiguous
%   port ranges may be specified, but overlapping port ranges may
%   not. Goal for example, may simply choose to broadcast the
%   notification, thus delegating the notification event handling to
%   others.
%
%   @param Addresses is a list of name/3 or name_seq/3 addresses
%   for the services to be monitored.
%
%   @param Goal is a predicate that will be called when a
%   worker's publication status changes. The Goal
%   is called exactly once per event with its the last argument
%   unified with the structure:
%
%   $ published(-NameSeq, -PortId) : when the worker binds
%   its socket to the address.
%
%   $ withdrawn(-NameSeq, -PortId) : when the worker
%   unbinds its socket from the address.
%
%   @param Timeout is optional.  It  is   one  of:
%
%   $ Timeout : a non-negative real number that specifies the
%   number of seconds that surveillance is to be continued.
%
%   $ infinite : causes the monitor to run forever in the current
%   thread (e.g. never returns).
%
%   $ detached(-ThreadId) : causes the monitor to run
%   forever as a separate thread. ThreadId is unified with the
%   thread identifier of the monitor thread. This is useful when the
%   monitor is required to provide continuous surveillance, while
%   operating in the background.
%

spm_dispatch(_Goal, subscr_timeout) :- !.
spm_dispatch(Goal, tipc_event(Action, _Subscr, NameSeq, PortId)) :-
    Event =.. [Action, NameSeq, PortId],
    once(call(Goal, Event)), fail.

:- meta_predicate
    tipc_service_port_monitor(+, 1),
    tipc_service_port_monitor(+, 1, +).

tipc_service_port_monitor(Address, Goal) :-
    tipc_service_port_monitor(Address, Goal, 0.0),
    !.

tipc_service_port_monitor(Address, Goal, detached(ThreadId)) :-
    !,
    thread_create(tipc_service_port_monitor(Address, Goal, infinite),
                  ThreadId, [detached(true)]).
tipc_service_port_monitor(Address, Goal, infinite) :-
    tipc_service_port_monitor(Address, Goal, -0.001),
    !.

tipc_service_port_monitor(Addresses, Goal, Timeout) :-
    maplist(tipc_address,Addresses, NameSeqs),
    !,
    ITime is integer(Timeout * 1000),
    try_finally(tipc_socket(S, seqpacket), tipc_close_socket(S)),
    tipc_connect(S, name(1,1,0)),   % connect to the topology server
    forall(member(NameSeq, NameSeqs),
           tipc_subscribe(S, NameSeq, ITime, 1, "swipl")),
    repeat,
        tipc_receive_subscr_event(S, Data),
        spm_dispatch(Goal, Data),
    !.

%%     tipc_initialize is semidet.
%
%      causes the TIPC service and the TIPC stack to be initialized
%      and made ready for service. An application must call this
%      predicate as part of its initialization prior to any use of
%      TIPC predicates. _|Please note the change of the API.|_

tipc_initialize :-
    with_mutex(tipc_mutex,
               forall(tipc_stack_initialize, true)).

tipc_stack_initialize :-
    tipc_service_exists(name(1,1,0)).
