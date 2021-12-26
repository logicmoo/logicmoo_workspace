/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald and Jan Wielemaker
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2013, Jeffrey Rosenwald
		   2018-2020, CWI Amsterdam
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

:- module(udp_broadcast,
          [ udp_broadcast_initialize/2,         % +IPAddress, +Options
            udp_broadcast_close/1,		% +Scope

            udp_peer_add/2,                     % +Scope, +IP
            udp_peer_del/2,                     % +Scope, ?IP
            udp_peer/2                          % +Scope, -IP
          ]).
:- autoload(library(apply),[maplist/2,maplist/3]).
:- autoload(library(backcomp),[thread_at_exit/1]).
:- autoload(library(broadcast),
	    [broadcast_request/1,broadcast/1,listening/3,listen/3]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(error),
	    [must_be/2,syntax_error/1,domain_error/2,existence_error/2]).
:- autoload(library(option),[option/3]).
:- autoload(library(socket),
	    [ tcp_close_socket/1,
	      udp_socket/1,
	      tcp_bind/2,
	      tcp_getopt/2,
	      tcp_setopt/2,
	      udp_receive/4,
	      udp_send/4
	    ]).


% :- debug(udp(broadcast)).

/** <module> A UDP broadcast proxy

SWI-Prolog's broadcast library provides a  means   that  may  be used to
facilitate publish and subscribe communication regimes between anonymous
members of a community of interest.  The   members  of the community are
however, necessarily limited to a  single   instance  of Prolog. The UDP
broadcast library removes that restriction.   With  this library loaded,
any member on your local IP subnetwork that also has this library loaded
may hear and respond to your broadcasts.

This library support three styles of networking as described below. Each
of these networks have their own   advantages  and disadvantages. Please
study the literature to understand the consequences.

  $ broadcast :
  Broadcast messages are sent to the LAN subnet. The broadcast
  implementation uses two UDP ports: a public to address the whole
  group and a private one to address a specific node.  Broadcasting
  is generally a good choice if the subnet is small and traffic is
  low.

  $ unicast :
  Unicast sends copies of packages to known peers.  Unicast networks
  can easily be routed.  The unicast version uses a single UDP port
  per node.  Unicast is generally a good choice for a small party,
  in particular if the peers are in different networks.

  $ multicast :
  Multicast is like broadcast, but it can be configured to
  work accross networks and may work more efficiently on VLAN networks.
  Like the broadcast setup, two UDP ports are used.  Multicasting can
  in general deliver the most efficient LAN and WAN networks, but
  requires properly configured routing between the peers.

After initialization and, in the case   of  a _unicast_ network managing
the  set  of  peers,   communication    happens   through   broadcast/1,
broadcast_request/1 and listen/1,2,3.

A broadcast/1 or broadcast_request/1 of the   shape  udp(Scope, Term) or
udp(Scope, Term, TimeOut) is forwarded over the UDP network to all peers
that joined the same `Scope`.  To   prevent  the  potential for feedback
loops, only the plain `Term`  is   broadcasted  locally.  The timeout is
optional. It specifies the amount to time  to wait for replies to arrive
in response to a  broadcast_request/1.  The   default  period  is  0.250
seconds. The timeout is ignored for broadcasts.

An example of three separate processes   cooperating in the same _scope_
called `peers`:

==
Process A:

   ?- listen(number(X), between(1, 5, X)).
   true.

   ?-

Process B:

   ?- listen(number(X), between(7, 9, X)).
   true.

   ?-

Process C:

   ?- findall(X, broadcast_request(udp(peers, number(X))), Xs).
   Xs = [1, 2, 3, 4, 5, 7, 8, 9].

   ?-
==

It is also  possible  to  carry  on   a  private  dialog  with  a single
responder. To do this, you supply a   compound of the form, Term:PortId,
to a UDP scoped broadcast/1 or  broadcast_request/1, where PortId is the
ip-address and port-id of  the  intended   listener.  If  you  supply an
unbound variable, PortId, to broadcast_request, it  will be unified with
the address of the listener  that  responds   to  Term.  You  may send a
directed broadcast to a specific member by simply providing this address
in a similarly structured compound  to   a  UDP  scoped broadcast/1. The
message is sent via unicast to that member   only by way of the member's
broadcast listener. It is received by  the   listener  just as any other
broadcast would be. The listener does not know the difference.

For example, in order to discover who responded with a particular value:

==
Host B Process 1:

   ?- listen(number(X), between(1, 5, X)).
   true.

   ?-

Host A Process 1:


   ?- listen(number(X), between(7, 9, X)).
   true.

   ?-

Host A Process 2:

   ?- listen(number(X), between(1, 5, X)).
   true.

   ?- bagof(X, broadcast_request(udp(peers,number(X):From,1)), Xs).
   From = ip(192, 168, 1, 103):34855,
   Xs = [7, 8, 9] ;
   From = ip(192, 168, 1, 103):56331,
   Xs = [1, 2, 3, 4, 5] ;
   From = ip(192, 168, 1, 104):3217,
   Xs = [1, 2, 3, 4, 5].
==

All incomming trafic is handled  by  a   single  thread  with  the alias
`udp_inbound_proxy`. This thread also performs  the internal dispatching
using broadcast/1 and broadcast_request/1. Future   versions may provide
for handling these requests in separate threads.


## Caveats {#udp-broadcase-caveats}

While the implementation is mostly transparent, there are some important
and subtle differences that must be taken into consideration:

    * UDP broadcast requires an initialization step in order to
    launch the broadcast listener proxy. See
    udp_broadcast_initialize/2.

    * Prolog's broadcast_request/1 is nondet. It sends the request,
    then evaluates the replies synchronously, backtracking as needed
    until a satisfactory reply is received. The remaining potential
    replies are not evaluated.  With UDP, all peers will send all
    answers to the query.  The receiver may however stop listening.

    * A UDP broadcast/1 is completely asynchronous.

    * A  UDP broadcast_request/1 is partially synchronous. A
    broadcast_request/1 is sent, then the sender balks for a period of
    time (default: 250 ms) while the replies are collected. Any reply
    that is received after this period is silently discarded. A
    optional second argument is provided so that a sender may specify
    more (or less) time for replies.

    * Replies are presented to the user as a choice point on arrival,
    until the broadcast request timer finally expires. This
    allows traffic to propagate through the system faster and provides
    the requestor with the opportunity to terminate a broadcast request
    early if desired, by simply cutting choice points.

    * Please beware that broadcast request transactions remain active
    and resources consumed until broadcast_request finally fails on
    backtracking, an uncaught exception occurs, or until choice points
    are cut. Failure to properly manage this will likely result in
    chronic exhaustion of UDP sockets.

    * If a listener is connected to a generator that always succeeds
    (e.g. a random number generator), then the broadcast request will
    never terminate and trouble is bound to ensue.

    * broadcast_request/1 with =|udp_subnet|= scope is _not_ reentrant.
    If a listener performs a broadcast_request/1 with UDP scope
    recursively, then disaster looms certain. This caveat does not apply
    to a UDP scoped broadcast/1, which can safely be performed from a
    listener context.

    * UDP broadcast's capacity is not infinite. While it can tolerate
    substantial bursts of activity, it is designed for short bursts of
    small messages. Unlike TIPC, UDP is unreliable and has no QOS
    protections. Congestion is likely to cause trouble in the form of
    non-Byzantine failure. That is, late, lost (e.g. infinitely late),
    or duplicate datagrams. Caveat emptor.

    * A UDP broadcast_request/1 term that is grounded is considered to
    be a broadcast only. No replies are collected unless the there is at
    least one unbound variable to unify.

    * A UDP broadcast/1 always succeeds, even if there are no
    listeners.

    * A UDP broadcast_request/1 that receives no replies will fail.

    * Replies may be coming from many different places in the network
    (or none at all). No ordering of replies is implied.

    * Prolog terms are sent to others after first converting them to
    atoms using term_string/3.  Serialization does not deal with cycles,
    attributes or sharing.   The hook udp_term_string_hook/3 may be
    defined to change the message serialization and support different
    message formats and/or encryption.

    * The broadcast model is based on anonymity and a presumption of
    trust--a perfect recipe for compromise. UDP is an Internet protocol.
    A UDP broadcast listener exposes a public port, which is
    static and shared by all listeners, and a private port, which is
    semi-static and unique to the listener instance. Both can be seen
    from off-cluster nodes and networks. Usage of this module exposes
    the node and consequently, the cluster to significant security
    risks. So have a care when designing your application. You must talk
    only to those who share and contribute to your concerns using a
    carefully prescribed protocol.

    * UDP broadcast categorically and silently ignores all message
    traffic originating from or terminating on nodes that are not
    members of the local subnet. This security measure only keeps honest
    people honest!

@author    Jeffrey Rosenwald (JeffRose@acm.org), Jan Wielemaker
@license   BSD-2
@see       tipc.pl
*/

:- multifile
    udp_term_string_hook/3,                     % +Scope, ?Term, ?String
    udp_unicast_join_hook/3,                    % +Scope, +From, +Data
    black_list/1.                               % +Term

:- meta_predicate
    safely(0),
    safely_det(0).

safely(Predicate) :-
    Err = error(_,_),
    catch(Predicate, Err,
          print_message_fail(Err)).

safely_det(Predicate) :-
    Err = error(_,_),
    catch(Predicate, Err,
          print_message_fail(Err)),
    !.
safely_det(_).

print_message_fail(Term) :-
    print_message(error, Term),
    fail.

udp_broadcast_address(IPAddress, Subnet, BroadcastAddress) :-
    IPAddress = ip(A1, A2, A3, A4),
    Subnet = ip(S1, S2, S3, S4),
    BroadcastAddress = ip(B1, B2, B3, B4),

    B1 is A1 \/ (S1 xor 255),
    B2 is A2 \/ (S2 xor 255),
    B3 is A3 \/ (S3 xor 255),
    B4 is A4 \/ (S4 xor 255).

%!  udp_broadcast_service(?Scope, ?Address) is nondet.
%
%   provides the UDP broadcast address for   a  given Scope. At present,
%   only one scope is supported, =|udp_subnet|=.

%!  udp_scope(?ScopeName, ?ScopeDef)

:- dynamic
    udp_scope/2,
    udp_scope_peer/2.
:- volatile
    udp_scope/2,
    udp_scope_peer/2.
%
%  Here's a UDP proxy to Prolog's broadcast library
%
%  A sender may extend a broadcast  to  a   subnet  of  a UDP network by
%  specifying a =|udp_subnet|= scoping qualifier   in his/her broadcast.
%  The qualifier has the effect of  selecting the appropriate multi-cast
%  address for the transmission. Thus,  the   sender  of the message has
%  control over the scope of his/her traffic on a per-message basis.
%
%  All in-scope listeners receive the   broadcast and simply rebroadcast
%  the message locally. All broadcast replies, if any, are sent directly
%  to the sender via the port-id that   was received with the broadcast.
%
%  Each listener exposes two UDP ports,  a   shared  public port that is
%  bound to a well-known port number and   a  private port that uniquely
%  indentifies the listener. Broadcasts are received  on the public port
%  and replies are  sent  on  the   private  port.  Directed  broadcasts
%  (unicasts) are received on the private port   and replies are sent on
%  the private port.

%  Thread 1 listens for directed traffic on the private port.
%

:- dynamic
    udp_private_socket/3,                       % Port, Socket, FileNo
    udp_public_socket/4,                        % Scope, Port, Socket, FileNo
    udp_closed/1.				% Scope

udp_inbound_proxy(Master) :-
    thread_at_exit(inbound_proxy_died),
    make_private_socket,
    thread_send_message(Master, udp_inbound_ready),
    udp_inbound_proxy_loop.

udp_inbound_proxy_loop :-
    forall(udp_scope(Scope, ScopeData),
           make_public_socket(ScopeData, Scope)),
    retractall(udp_closed(_)),
    findall(FileNo, udp_socket_file_no(FileNo), FileNos),
    catch(dispatch_inbound(FileNos),
          E, dispatch_exception(E)),
    udp_inbound_proxy_loop.

dispatch_exception(E) :-
    E = error(_,_),
    !,
    print_message(warning, E).
dispatch_exception(_).


%!  make_private_socket is det.
%
%   Create our private socket. This socket is used for messages that are
%   directed to me. Note that we only  need this for broadcast networks.
%   If we use a unicast network we use   our public port to contact this
%   specific server.

make_private_socket :-
    udp_private_socket(_Port, S, _F),
    !,
    (   (   udp_scope(Scope, broadcast(_,_,_))
        ;   udp_scope(Scope, multicast(_,_))
        ),
        \+ udp_closed(Scope)
    ->  true
    ;   tcp_close_socket(S),
        retractall(udp_private_socket(_,_,_))
    ).
make_private_socket :-
    udp_scope(_, broadcast(_,_,_)),
    !,
    udp_socket(S),
    tcp_bind(S, Port),
    tcp_getopt(S, file_no(F)),
    tcp_setopt(S, broadcast),
    assertz(udp_private_socket(Port, S, F)).
make_private_socket :-
    udp_scope(_, multicast(_,_)),
    !,
    udp_socket(S),
    tcp_bind(S, Port),
    tcp_getopt(S, file_no(F)),
    assertz(udp_private_socket(Port, S, F)).
make_private_socket.

%!  make_public_socket(+ScopeData, +Scope)
%
%   Create the public port Scope.

make_public_socket(_, Scope) :-
    udp_public_socket(Scope, _Port, S, _),
    !,
    (   udp_closed(Scope)
    ->  tcp_close_socket(S),
        retractall(udp_public_socket(Scope, _, _, _))
    ;   true
    ).
make_public_socket(broadcast(_SubNet, _Broadcast, Port), Scope) :-
    udp_socket(S),
    tcp_setopt(S, reuseaddr),
    tcp_bind(S, Port),
    tcp_getopt(S, file_no(F)),
    assertz(udp_public_socket(Scope, Port, S, F)).
make_public_socket(multicast(Group, Port), Scope) :-
    udp_socket(S),
    tcp_setopt(S, reuseaddr),
    tcp_bind(S, Port),
    tcp_setopt(S, ip_add_membership(Group)),
    tcp_getopt(S, file_no(F)),
    assertz(udp_public_socket(Scope, Port, S, F)).
make_public_socket(unicast(Port), Scope) :-
    udp_socket(S),
    tcp_bind(S, Port),
    tcp_getopt(S, file_no(F)),
    assertz(udp_public_socket(Scope, Port, S, F)).

udp_socket_file_no(FileNo) :-
    udp_private_socket(_,_,FileNo).
udp_socket_file_no(FileNo) :-
    udp_public_socket(_,_,_,FileNo).

%!  dispatch_inbound(+FileNos)
%
%   Dispatch inbound traffic. This loop   uses  wait_for_input/3 to wait
%   for one or more UDP sockets and   dispatches  the requests using the
%   internal broadcast service. For an  incomming broadcast _request_ we
%   send the reply only to the  requester   and  therefore we must use a
%   socket that is not in broadcast mode.

dispatch_inbound(FileNos) :-
    debug(udp(broadcast), 'Waiting for ~p', [FileNos]),
    wait_for_input(FileNos, Ready, infinite),
    debug(udp(broadcast), 'Ready: ~p', [Ready]),
    maplist(dispatch_ready, Ready),
    dispatch_inbound(FileNos).

dispatch_ready(FileNo) :-
    udp_private_socket(_Port, Private, FileNo),
    !,
    udp_receive(Private, Data, From, [max_message_size(65535)]),
    debug(udp(broadcast), 'Inbound on private port', []),
    (   in_scope(Scope, From),
        udp_term_string(Scope, Term, Data) % only accept valid data
    ->  ld_dispatch(Private, Term, From, Scope)
    ;   true
    ).
dispatch_ready(FileNo) :-
    udp_public_socket(Scope, _PublicPort, Public, FileNo),
    !,
    udp_receive(Public, Data, From, [max_message_size(65535)]),
    debug(udp(broadcast), 'Inbound on public port from ~p for scope ~p',
          [From, Scope]),
    (   in_scope(Scope, From),
        udp_term_string(Scope, Term, Data) % only accept valid data
    ->  (   udp_scope(Scope, unicast(_))
        ->  ld_dispatch(Public, Term, From, Scope)
        ;   udp_private_socket(_PrivatePort, Private, _FileNo),
            ld_dispatch(Private, Term, From, Scope)
        )
    ;   udp_scope(Scope, unicast(_)),
        udp_term_string(Scope, Term, Data),
        unicast_out_of_scope_request(Scope, From, Term)
    ->  true
    ;   true
    ).

in_scope(Scope, Address) :-
    udp_scope(Scope, ScopeData),
    in_scope(ScopeData, Scope, Address),
    !.
in_scope(Scope, From) :-
    debug(udp(broadcast), 'Out-of-scope ~p datagram from ~p',
          [Scope, From]),
    fail.

in_scope(broadcast(Subnet, Broadcast, _PublicPort), _Scope, IP:_FromPort) :-
    udp_broadcast_address(IP, Subnet, Broadcast).
in_scope(multicast(_Group, _Port), _Scope, _From).
in_scope(unicast(_PublicPort), Scope, IP:_) :-
    udp_peer(Scope, IP:_).


%!  ld_dispatch(+PrivateSocket, +Term, +From, +Scope)
%
%   Locally dispatch Term received from From. If it concerns a broadcast
%   request, send the replies to PrivateSocket   to  From. The multifile
%   hook black_list/1 can be used to ignore certain messages.

ld_dispatch(_S, Term, From, _Scope) :-
    debug(udp(broadcast), 'ld_dispatch(~p) from ~p', [Term, From]),
    fail.
ld_dispatch(_S, Term, _From, _Scope) :-
    blacklisted(Term), !.
ld_dispatch(S, request(Key, Term), From, Scope) :-
    !,
    forall(safely(broadcast_request(Term)),
           safely((udp_term_string(Scope, reply(Key,Term), Message),
                   udp_send(S, Message, From, [])))).
ld_dispatch(_S, send(Term), _From, _Scope) :-
    !,
    safely_det(broadcast(Term)).
ld_dispatch(_S, reply(Key, Term), From, _Scope) :-
    (   reply_queue(Key, Queue)
    ->  safely(thread_send_message(Queue, Term:From))
    ;   true
    ).

blacklisted(send(Term))      :- black_list(Term).
blacklisted(request(_,Term)) :- black_list(Term).
blacklisted(reply(_,Term))   :- black_list(Term).


%!  reload_udp_proxy
%
%   Update the UDP relaying proxy service.   The proxy consists of three
%   forwarding mechanisms:
%
%     - Listen on our _scope_.  If any messages are received, hand them
%       to udp_broadcast/3 to be broadcasted to _scope_ or sent to a
%       specific recipient.
%     - Listen on the _scope_ public port. Incomming messages are
%       relayed to the internal broadcast mechanism and replies are sent
%       to from our private socket.
%     - Listen on our private port and reply using the same port.

reload_udp_proxy :-
    reload_outbound_proxy,
    reload_inbound_proxy.

reload_outbound_proxy :-
    listening(udp_broadcast, udp(_,_), _),
    !.
reload_outbound_proxy :-
    listen(udp_broadcast, udp(Scope,Message),
           udp_broadcast(Message, Scope, 0.25)),
    listen(udp_broadcast, udp(Scope,Message,Timeout),
           udp_broadcast(Message, Scope, Timeout)),
    listen(udp_broadcast, udp_subnet(Message),  % backward compatibility
           udp_broadcast(Message, subnet, 0.25)),
    listen(udp_broadcast, udp_subnet(Message,Timeout),
           udp_broadcast(Message, subnet, Timeout)).

reload_inbound_proxy :-
    catch(thread_signal(udp_inbound_proxy, throw(udp_reload)),
          error(existence_error(thread, _),_),
          fail),
    !.
reload_inbound_proxy :-
    thread_self(Me),
    thread_create(udp_inbound_proxy(Me), _,
                  [ alias(udp_inbound_proxy),
                    detached(true)
                  ]),
    thread_get_message(Me, udp_inbound_ready, [timeout(10)]).

inbound_proxy_died :-
    thread_self(Self),
    thread_property(Self, status(Status)),
    (   catch(recreate_proxy(Status), _, fail)
    ->  print_message(informational,
                      httpd_restarted_worker(Self))
    ;   done_status_message_level(Status, Level),
        print_message(Level,
                      httpd_stopped_worker(Self, Status))
    ).

recreate_proxy(exception(Error)) :-
    recreate_on_error(Error),
    reload_inbound_proxy.

recreate_on_error('$aborted').
recreate_on_error(time_limit_exceeded).

done_status_message_level(true, silent) :- !.
done_status_message_level(exception('$aborted'), silent) :- !.
done_status_message_level(_, informational).


%!  udp_broadcast_close(+Scope)
%
%   Close a UDP broadcast scope.

udp_broadcast_close(Scope) :-
    udp_scope(Scope, _ScopeData),
    !,
    assert(udp_closed(Scope)),
    reload_udp_proxy.
udp_broadcast_close(_).


%!  udp_broadcast(+What, +Scope, +TimeOut)
%
%   Send a broadcast request to my UDP peers in Scope. What is either of
%   the shape `Term:Address` to send Term to a specific address or query
%   the address from which term is answered or it is a plain `Term`.
%
%   If `Term` is  nonground,  it  is   considered  is  a  _request_ (see
%   broadcast_request/1) and the predicate  succeeds   for  each  answer
%   received within TimeOut seconds. If Term is ground it is considered
%   an asynchronous broadcast and udp_broadcast/3 is deterministic.

udp_broadcast(Term:To, Scope, _Timeout) :-
    ground(Term), ground(To),           % broadcast to single listener
    !,
    udp_basic_broadcast(send(Term), Scope, single(To)).
udp_broadcast(Term, Scope, _Timeout) :-
    ground(Term),                       % broadcast to all listeners
    !,
    udp_basic_broadcast(send(Term), Scope, broadcast).
udp_broadcast(Term:To, Scope, Timeout) :-
    ground(To),                         % request to single listener
    !,
    setup_call_cleanup(
        request_queue(Id, Queue),
        ( udp_basic_broadcast(request(Id, Term), Scope, single(To)),
          udp_br_collect_replies(Queue, Timeout, Term:To)
        ),
        destroy_request_queue(Queue)).
udp_broadcast(Term:From, Scope, Timeout) :-
    !,                                  % request to all listeners, collect sender
    setup_call_cleanup(
        request_queue(Id, Queue),
        ( udp_basic_broadcast(request(Id, Term), Scope, broadcast),
          udp_br_collect_replies(Queue, Timeout, Term:From)
        ),
        destroy_request_queue(Queue)).
udp_broadcast(Term, Scope, Timeout) :-  % request to all listeners
    udp_broadcast(Term:_, Scope, Timeout).

:- dynamic
    reply_queue/2.

request_queue(Id, Queue) :-
    Id is random(1<<63),
    message_queue_create(Queue),
    asserta(reply_queue(Id, Queue)).

destroy_request_queue(Queue) :-         % leave queue to GC
    retractall(reply_queue(_, Queue)).


%!  udp_basic_broadcast(+Term, +Dest) is multi.
%
%   Create a UDP private socket and use it   to send Term to Address. If
%   Address is our broadcast address, set the socket in broadcast mode.
%
%   This predicate succeeds with a choice   point. Committing the choice
%   point closes S.
%
%   @arg Dest is one of single(Target) or `broadcast`.

udp_basic_broadcast(Term, Scope, Dest) :-
    debug(udp(broadcast), 'UDP proxy outbound ~p to ~p', [Term, Dest]),
    udp_term_string(Scope, Term, String),
    udp_send_message(Dest, String, Scope).

udp_send_message(single(Address), String, Scope) :-
    (   udp_scope(Scope, unicast(_))
    ->  udp_public_socket(Scope, _Port, S, _)
    ;   udp_private_socket(_Port, S, _F)
    ),
    safely(udp_send(S, String, Address, [])).
udp_send_message(broadcast, String, Scope) :-
    (   udp_scope(Scope, unicast(_))
    ->  udp_public_socket(Scope, _Port, S, _),
        forall(udp_peer(Scope, Address),
               ( debug(udp(broadcast), 'Unicast to ~p', [Address]),
                 safely(udp_send(S, String, Address, []))))
    ;   udp_scope(Scope, broadcast(_SubNet, Broadcast, Port))
    ->  udp_private_socket(_PrivatePort, S, _F),
        udp_send(S, String, Broadcast:Port, [])
    ;   udp_scope(Scope, multicast(Group, Port))
    ->  udp_private_socket(_PrivatePort, S, _F),
        udp_send(S, String, Group:Port, [])
    ).

% ! udp_br_collect_replies(+Queue, +TimeOut, -TermAndFrom) is nondet.
%
%   Collect replies on Socket for  TimeOut   seconds.  Succeed  for each
%   received message.

udp_br_collect_replies(Queue, Timeout, Reply) :-
    get_time(Start),
    Deadline is Start+Timeout,
    repeat,
       (   thread_get_message(Queue, Reply,
                              [ deadline(Deadline)
                              ])
       ->  true
       ;   !,
           fail
       ).

%!  udp_broadcast_initialize(+IPAddress, +Options) is semidet.
%
%   Initialized UDP broadcast bridge. IPAddress is the IP address on the
%   network we want to broadcast on.  IP addresses are terms ip(A,B,C,D)
%   or an atom or string of the format =|A.B.C.D|=.   Options processed:
%
%     - scope(+ScopeName)
%     Name of the scope.  Default is `subnet`.
%     - subnet_mask(+SubNet)
%     Subnet to broadcast on.  This uses the same syntax as IPAddress.
%     Default classifies the network as class A, B or C depending on
%     the the first octet and applies the default mask.
%     - port(+Port)
%     Public port to use.  Default is 20005.
%     - method(+Method)
%     Method to send a message to multiple peers.  One of
%       - broadcast
%       Use UDP broadcast messages to the LAN.  This is the
%       default
%       - multicast
%       Use UDP multicast messages.  This can be used on WAN networks,
%       provided the intermediate routers understand multicast.
%       - unicast
%       Send the messages individually to all registered peers.
%
%   For compatibility reasons Options may be the subnet mask.

udp_broadcast_initialize(IP, Options) :-
    with_mutex(udp_broadcast,
               udp_broadcast_initialize_sync(IP, Options)).

udp_broadcast_initialize_sync(IP, Options) :-
    nonvar(Options),
    Options = ip(_,_,_,_),
    !,
    udp_broadcast_initialize(IP, [subnet_mask(Options)]).
udp_broadcast_initialize_sync(IP, Options) :-
    to_ip4(IP, IPAddress),
    option(method(Method), Options, broadcast),
    must_be(oneof([broadcast, multicast, unicast]), Method),
    udp_broadcast_initialize_sync(Method, IPAddress, Options),
    reload_udp_proxy.

udp_broadcast_initialize_sync(broadcast, IPAddress, Options) :-
    option(subnet_mask(Subnet), Options, _),
    mk_subnet(Subnet, IPAddress, Subnet4),
    option(port(Port), Options, 20005),
    option(scope(Scope), Options, subnet),

    udp_broadcast_address(IPAddress, Subnet4, Broadcast),
    udp_broadcast_close(Scope),
    assertz(udp_scope(Scope, broadcast(Subnet4, Broadcast, Port))).
udp_broadcast_initialize_sync(unicast, _IPAddress, Options) :-
    option(port(Port), Options, 20005),
    option(scope(Scope), Options, subnet),
    udp_broadcast_close(Scope),
    assertz(udp_scope(Scope, unicast(Port))).
udp_broadcast_initialize_sync(multicast, IPAddress, Options) :-
    option(port(Port), Options, 20005),
    option(scope(Scope), Options, subnet),
    udp_broadcast_close(Scope),
    multicast_address(IPAddress),
    assertz(udp_scope(Scope, multicast(IPAddress, Port))).

to_ip4(Atomic, ip(A,B,C,D)) :-
    atomic(Atomic),
    !,
    (   split_string(Atomic, ".", "", Strings),
        maplist(number_string, [A,B,C,D], Strings)
    ->  true
    ;   syntax_error(illegal_ip_address)
    ).
to_ip4(IP, IP).

mk_subnet(Var, IP, Subnet) :-
    var(Var),
    !,
    (   default_subnet(IP, Subnet)
    ->  true
    ;   domain_error(ip_with_subnet, IP)
    ).
mk_subnet(Subnet, _, Subnet4) :-
    to_ip4(Subnet, Subnet4).

%!  default_subnet(+IP, -NetWork)
%
%   Determine the default network address from an IP address. This
%   classifies the network as class A, B or C.
%
%   @see https://docs.oracle.com/cd/E19504-01/802-5753/planning3-78185/index.html

default_subnet(ip(A,_,_,_), ip(A,0,0,0)) :-
    between(0,127, A), !.
default_subnet(ip(A,B,_,_), ip(A,B,0,0)) :-
    between(128,191, A), !.
default_subnet(ip(A,B,C,_), ip(A,B,C,0)) :-
    between(192,223, A), !.

multicast_address(ip(A,_,_,_)) :-
    between(224, 239, A),
    !.
multicast_address(IP) :-
    domain_error(multicast_network, IP).


		 /*******************************
		 *          UNICAST PEERS	*
		 *******************************/

%!  udp_peer_add(+Scope, +Address) is det.
%!  udp_peer_del(+Scope, ?Address) is det.
%!  udp_peer(?Scope, ?Address) is nondet.
%
%   Manage and query the set  of  known   peers  for  a unicast network.
%   Address is either a term  IP:Port  or   a  plain  IP address. In the
%   latter case the default port registered with the scope is used.
%
%   @arg Address has canonical form ip(A,B,C,D):Port.

udp_peer_add(Scope, Address) :-
    must_be(ground, Address),
    peer_address(Address, Scope, Canonical),
    (   udp_scope_peer(Scope, Canonical)
    ->  true
    ;   assertz(udp_scope_peer(Scope, Canonical))
    ).

udp_peer_del(Scope, Address) :-
    peer_address(Address, Scope, Canonical),
    retractall(udp_scope_peer(Scope, Canonical)).

udp_peer(Scope, IPAddress) :-
    udp_scope_peer(Scope, IPAddress).

peer_address(IP:Port, _Scope, IPAddress:Port) :-
    !,
    to_ip4(IP, IPAddress).
peer_address(IP, Scope, IPAddress:Port) :-
    (   udp_scope(Scope, unicast(Port))
    ->  true
    ;   existence_error(udp_scope, Scope)
    ),
    to_ip4(IP, IPAddress).



		 /*******************************
		 *             HOOKS		*
		 *******************************/

%!  udp_term_string_hook(+Scope, +Term, -String) is det.
%!  udp_term_string_hook(+Scope, -Term, +String) is semidet.
%
%   Hook  for  serializing  the  message    Term.   The  default  writes
%   =|%prolog\n|=, followed by the Prolog term  in quoted notation while
%   ignoring operators. This hook may use alternative serialization such
%   as fast_term_serialized/2, use  library(ssl)   to  realise encrypted
%   messages, etc.
%
%   @arg Scope is the scope for which the message is broadcasted.  This
%   can be used to use different serialization for different scopes.
%   @arg Term encapsulates the term broadcasted by the application as
%   follows:
%
%     - send(ApplTerm)
%       Is sent by broadcast(udp(Scope, ApplTerm))
%     - request(Id,ApplTerm)
%       Is sent by broadcast_request/1, where Id is a unique large
%       (64 bit) integer.
%     - reply(Id,ApplTerm)
%       Is sent to reply on a broadcast_request/1 request that has
%       been received.  Arguments are the same as above.
%
%   @throws The hook may throw udp(invalid_message) to stop processing
%   the message.

%!  udp_term_string(+Scope, +Term, -String) is det.
%!  udp_term_string(+Scope, -Term, +String) is semidet.
%
%   Serialize an arbitrary Prolog  term  as   a  string.  The  string is
%   prefixed by a magic key to ensure   we only accept messages that are
%   meant for us.
%
%   In mode (+,-), Term is written with the options ignore_ops(true) and
%   quoted(true).
%
%   This predicate first calls  udp_term_string_hook/3.

udp_term_string(Scope, Term, String) :-
    catch(udp_term_string_hook(Scope, Term, String), udp(Error), true),
    !,
    (   var(Error)
    ->  true
    ;   Error == invalid_message
    ->  fail
    ;   throw(udp(Error))
    ).
udp_term_string(_Scope, Term, String) :-
    (   var(String)
    ->  format(string(String), '%-prolog-\n~W',
               [ Term,
                 [ ignore_ops(true),
                   quoted(true)
                 ]
               ])
    ;   sub_string(String, 0, _, _, '%-prolog-\n'),
        term_string(Term, String,
                    [ syntax_errors(quiet)
                    ])
    ).

%!  unicast_out_of_scope_request(+Scope, +From, +Data) is semidet.

%!  udp_unicast_join_hook(+Scope, +From, +Data) is semidet.
%
%   This multifile hook is called if an   UDP package is received on the
%   port of the unicast network identified by  Scope. From is the origin
%   IP and port and Data is  the   message  data that is deserialized as
%   defined for the scope (see udp_term_string/3).
%
%   This hook is intended to initiate a  new node joining the network of
%   peers. We could in theory also  omit   the  in-scope  test and use a
%   normal broadcast to join. Using a different channal however provides
%   a basic level of security. A   possibe  implementation is below. The
%   first fragment is a hook  added  to   the  server,  the  second is a
%   predicate added to a client and the   last  initiates the request in
%   the client. The excanged term (join(X)) can   be  used to exchange a
%   welcome handshake.
%
%
%   ```
%   :- multifile udp_broadcast:udp_unicast_join_hook/3.
%   udp_broadcast:udp_unicast_join_hook(Scope, From, join(welcome)) :-
%       udp_peer_add(Scope, From),
%   ```
%
%   ```
%   join_request(Scope, Address, Reply) :-
%       udp_peer_add(Scope, Address),
%       broadcast_request(udp(Scope, join(X))).
%   ```
%
%   ```
%   ?- join_request(myscope, "1.2.3.4":10001, Reply).
%   Reply = welcome.
%   ```

unicast_out_of_scope_request(Scope, From, send(Term)) :-
    udp_unicast_join_hook(Scope, From, Term).
unicast_out_of_scope_request(Scope, From, request(Key, Term)) :-
    udp_unicast_join_hook(Scope, From, Term),
    udp_public_socket(Scope, _Port, Socket, _FileNo),
    safely((udp_term_string(Scope, reply(Key,Term), Message),
            udp_send(Socket, Message, From, []))).
