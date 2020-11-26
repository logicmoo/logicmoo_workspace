/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald, Jan Wielemaker
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2019, Jeffrey Rosenwald
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

:- module(paxos,
          [ paxos_get/1,                        % ?Term
            paxos_get/2,                        % +Key, -Value
            paxos_get/3,                        % +Key, -Value, +Options
            paxos_set/1,                        % ?Term
            paxos_set/2,                        % +Key, +Value
            paxos_set/3,                        % +Key, +Value, +Options
            paxos_on_change/2,                  % ?Term, +Goal
            paxos_on_change/3,                  % ?Key, ?Value, +Goal

            paxos_initialize/1,			% +Options

            paxos_admin_key/2,                  % ?Name, ?Key
            paxos_property/1,                   % ?Property
            paxos_quorum_ask/4,                 % ?Templ, +Msg, -Result, +Options
                                                % Hook support
            paxos_replicate_key/3               % +Nodes, ?Key, +Options
          ]).
:- autoload(library(apply),[partition/4,maplist/3]).
:- autoload(library(broadcast),
	    [ listen/3,
	      broadcast_request/1,
	      broadcast/1,
	      unlisten/1,
	      listen/2,
	      unlisten/2
	    ]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(error),
	    [permission_error/3,resource_error/1,must_be/2]).
:- autoload(library(lists),[select/3,nth1/3,max_list/2,member/2]).
:- autoload(library(option),[option/2,option/3]).
:- autoload(library(solution_sequences),[call_nth/2]).
:- use_module(library(settings),[setting/4,setting/2]).

/** <module> A Replicated Data Store

This module provides a replicated data store that is coordinated using a
variation on Lamport's Paxos concensus protocol.  The original method is
described in his paper entitled, "The   Part-time Parliament", which was
published in 1998. The algorithm is   tolerant of non-Byzantine failure.
That is late or lost delivery or   reply,  but not senseless delivery or
reply. The present algorithm takes advantage  of the convenience offered
by multicast to the quorum's membership,   who  can remain anonymous and
who can come and go as they  please without effecting Liveness or Safety
properties.

Paxos' quorum is a set of one or more attentive members, whose processes
respond to queries within some known time limit (< 20ms), which includes
roundtrip delivery delay. This property is   easy  to satisfy given that
every coordinator is necessarily a member of   the quorum as well, and a
quorum of one is  permitted.  An   inattentive  member  (e.g.  one whose
actions are late or lost) is deemed to be "not-present" for the purposes
of the present transaction and consistency   cannot  be assured for that
member. As long as there is at least one attentive member of the quorum,
then persistence of the database is assured.

Each member maintains a ledger  of   terms  along with information about
when  they  were   originally   recorded.    The   member's   ledger  is
deterministic. That is to say  that  there   can  only  be one entry per
functor/arity combination. No member will  accept   a  new term proposal
that has a line number that is equal-to   or  lower-than the one that is
already recorded in the ledger.

Paxos is a three-phase protocol:

   1: A coordinator first prepares the quorum for a new proposal by
   broadcasting a proposed term. The quorum responds by returning the
   last known line number for that functor/arity combination that is
   recorded in their respective ledgers.

   2: The coordinator selects the highest line number it receives,
   increments it by one, and then asks the quorum to finally accept the
   new term with the new line number. The quorum checks their respective
   ledgers once again and if there is still no other ledger entry for
   that functor/arity combination that is equal-to or higher than the
   specified line, then each member records the term in the ledger at
   the specified line. The member indicates consent by returning the
   specified line number back to the coordinator. If consent is withheld
   by a member, then the member returns a =nack= instead. The
   coordinator requires unanimous consent. If it isn't achieved then the
   proposal fails and the coordinator must start over from the
   beginning.

   3: Finally, the coordinator concludes the successful negotiation by
   broadcasting the agreement to the quorum in the form of a
   paxos_changed(Key,Value) event. This is the only event that
   should be of interest to user programs.

For practical reasons, we rely  on   the  partially synchronous behavior
(e.g. limited upper time bound for  replies) of broadcast_request/1 over
TIPC to ensure Progress. Perhaps more importantly,   we rely on the fact
that the TIPC broadcast listener state  machine guarantees the atomicity
of broadcast_request/1 at the process level, thus obviating the need for
external mutual exclusion mechanisms.

_|Note that this algorithm does not guarantee the rightness of the value
proposed. It only guarantees that if   successful, the value proposed is
identical for all attentive members of the quorum.|_

@author    Jeffrey Rosenwald (JeffRose@acm.org)
@license   BSD-2
@see       tipc_broadcast.pl, udp_broadcast.pl
*/

:- meta_predicate
    paxos_on_change(?, 0),
    paxos_on_change(?, ?, 0).

:- multifile
    paxos_message_hook/3,               % +PaxOS, +TimeOut, -Message
    paxos_ledger_hook/5.                % +Op, ?Key, ?Gen, ?Value, ?Status

:- setting(max_sets, nonneg, 20,
           "Max Retries to get to an agreement").
:- setting(max_gets, nonneg, 5,
           "Max Retries to get a value from the forum").
:- setting(response_timeout, float, 0.020,
           "Max time to wait for a response").
:- setting(replication_rate, number, 1000,
           "Number of keys replicated per second").
:- setting(death_half_life, number, 10,
           "Half-time for failure score").
:- setting(death_score, number, 100,
           "Consider a node dead if cummulative failure \c
            score exceeds this number").


%!  paxos_initialize(+Options) is det.
%
%   Initialize this Prolog process as a   paxos node. The initialization
%   requires an initialized and configured TIPC,  UDP or other broadcast
%   protocol. Calling this initialization may be  omitted, in which case
%   the equivant of paxos_initialize([]) is executed   lazily as part of
%   the first paxos operation.  Defined options:
%
%     - node(?NodeID)
%     When instantiated, this node rejoins the network with the given
%     node id. A fixed node idea should be used if the node is
%     configured for persistency and causes the new node to receive
%     updates for keys that have been created or modified since the
%     node left the network.  If NodeID is a variable it is unified
%     with the discovered NodeID.
%
%     NodeID must be a small non-negative integer as these identifiers
%     are used in bitmaps.

:- dynamic  paxos_initialized/0.
:- volatile paxos_initialized/0.

paxos_initialize(_Options) :-
    paxos_initialized,
    !.
paxos_initialize(Options) :-
    with_mutex(paxos, paxos_initialize_sync(Options)).

paxos_initialize_sync(_Options) :-
    paxos_initialized,
    !.
paxos_initialize_sync(Options) :-
    at_halt(paxos_leave),
    listen(paxos, paxos(X), paxos_message(X)),
    paxos_assign_node(Options),
    start_replicator,
    asserta(paxos_initialized).

paxos_initialize :-
    paxos_initialize([]).


		 /*******************************
		 *            ADMIN		*
		 *******************************/

%!  paxos_get_admin(+Name, -Value) is semidet.
%!  paxos_set_admin(+Name, +Value) is semidet.
%
%   Set administrative keys. We use a wrapper  such that we can hide the
%   key identity.

paxos_admin_key(quorum, '$paxos_quorum').
paxos_admin_key(dead,   '$paxos_dead_nodes').

paxos_get_admin(Name, Value) :-
    paxos_admin_key(Name, Key),
    paxos_get(Key, Value).

paxos_set_admin(Name, Value) :-
    paxos_admin_key(Name, Key),
    paxos_set(Key, Value).

paxos_set_admin_bg(Name, Value) :-
    thread_create(ignore(paxos_set_admin(Name, Value)), _,
                  [ detached(true)
                  ]).


		 /*******************************
		 *           NODE DATA		*
		 *******************************/

%!  node(?NodeId).
%!  quorum(?Bitmap).
%!  dead(?Bitmap).
%!  failed(?Bitmap).
%!  failed(?NodeId, ?LastTried, ?Score).
%
%   Track our identity as well as as  the   status  of  our peers in the
%   network. NodeId is a small integer. Multiple NodeIds are combined in
%   a Bitmap.
%
%     - node/1 is our identity.
%     - quorum/1 is the set of members of the quorum
%     - failed/1 is the set of members for which the last message was
%       not confirmed.
%     - failed/3 tracks individual failed nodes. If accumulates failures
%       until the node is marked _dead_.
%     - dead/1 is the set of members that is considered dead.

:- dynamic
    node/1,                             % NodeID
    quorum/1,                           % Bitmap
    failed/1,                           % Bitmap
    failed/3,                           % NodeID, LastTried, Score
    leaving/0,                          % Node is leaving
    dead/1,                             % Bitmap
    salt/1.                             % Unique key
:- volatile
    node/1,
    quorum/1,
    failed/1,
    failed/3,
    leaving/0,
    dead/1,
    salt/1.

%!  paxos_assign_node(+Options) is det.
%
%   Assign a node for this  paxos  instance.   If  node  is  given as an
%   option, this is the node id that   is used. Otherwise the network is
%   analysed and the system selects a new node.

paxos_assign_node(Options) :-
    (   option(node(Node), Options)
    ->  node(Node)
    ;   node(_)
    ),                                          % already done
    !.
paxos_assign_node(Options) :-
    between(1, 20, Retry),
    option(node(Node), Options, Node),
    (   node(_)
    ->  permission_error(set, paxos_node, Node)
    ;   true
    ),
    retractall(dead(_)),
    retractall(quorum(_)),
    retractall(failed(_)),
    retractall(failed(_,_,_)),
    retractall(leaving),
    Salt is random(1<<63),
    asserta(salt(Salt)),
    paxos_message(node(N,Q,D):From, 0.25, NodeQuery),
    findall(t(N,Q,D,From),
            broadcast_request(NodeQuery),
            Network),
    select(t(self,0,Salt,Me), Network, AllNodeStatus),
    partition(starting, AllNodeStatus, Starting, Running),
    nth_starting(Starting, Salt, Offset),
    retractall(salt(_)),
    debug(paxos(node), 'Me@~p; starting: ~p; running: ~p',
          [Me, Starting, Running]),
    arg_union(2, Running, Quorum),
    arg_union(3, Running, Dead),
    (   var(Node)
    ->  (   call_nth(( between(0, 1000, Node),
                       \+ memberchk(t(Node,_,_,_), Running),
                       Dead /\ (1<<Node) =:= 0),
                     Offset)
        ->  debug(paxos(node), 'Assigning myself node ~d', [Node])
        ;   resource_error(paxos_nodes)
        )
    ;   memberchk(t(Node,_,_,_), Running)
    ->  permission_error(set, paxos_node, Node)
    ;   Rejoin = true
    ),
    asserta(node(Node)),
    (   claim_node(Node, Me)
    ->  !,
        asserta(dead(Dead)),
        set_quorum(Node, Quorum),
        (   Rejoin == true
        ->  paxos_rejoin
        ;   true
        )
    ;   debug(paxos(node), 'Node ~p already claimed; retrying (~p)',
              [Node, Retry]),
        retractall(node(Node)),
        fail
    ).

starting(t(self,_Quorum,_Salt,_Address)).

nth_starting(Starting, Salt, N) :-
    maplist(arg(3), Starting, Salts),
    sort([Salt|Salts], Sorted),
    nth1(N, Sorted, Salt),
    !.

claim_node(Node, Me) :-
    paxos_message(claim_node(Node, Ok):From, 0.25, NodeQuery),
    forall((   broadcast_request(NodeQuery),
               From \== Me,
               debug(paxos(node), 'Claim ~p ~p: ~p', [Node, From, Ok])
           ),
           Ok == true).

set_quorum(Node, Quorum0) :-
    Quorum is Quorum0 \/ (1<<Node),
    debug(paxos(node), 'Adding ~d to quorum (now 0x~16r)', [Node, Quorum]),
    asserta(quorum(Quorum)),
    paxos_set_admin(quorum, Quorum).


%!  paxos_rejoin
%
%   Re-join the network.  Tasks:
%
%     - Remove myself from the dead list if I'm on there
%     - Tell the replicators we lost everything.

paxos_rejoin :-
    node(Node),
    repeat,
        (   paxos_get_admin(dead, Dead0)
        ->  Dead is Dead0 /\ \(1<<Node),
            (   Dead == Dead0
            ->  true
            ;   paxos_set_admin(dead, Dead)
            )
        ;   true
        ),
    !.

%!  paxos_leave is det.
%!  paxos_leave(+Node) is det.
%
%   Leave the network.  The  predicate   paxos_leave/0  is  called  from
%   at_halt/1 to ensure the node is  deleted   as  the process dies. The
%   paxos_leave/1 version is called  to  discard   other  nodes  if they
%   repeatedly did not respond to queries.

paxos_leave :-
    node(Node),
    !,
    asserta(leaving),
    paxos_leave(Node),
    Set is 1<<Node,
    paxos_message(forget(Set), -, Forget),
    broadcast(Forget),
    unlisten(paxos),
    retractall(leaving).
paxos_leave.

paxos_leave(Node) :-
    !,
    paxos_update_set(quorum, del(Node)),
    paxos_update_set(dead,   add(Node)).
paxos_leave(_).

paxos_update_set(Set, How) :-
    repeat,
      Term =.. [Set,Value],
      call(Term),
      (   How = add(Node)
      ->  NewValue is Value \/  (1<<Node)
      ;   How = del(Node)
      ->  NewValue is Value /\ \(1<<Node)
      ),
      (   Value == NewValue
      ->  true
      ;   paxos_set_admin(Set, NewValue)
      ->  true
      ;   leaving
      ),
    !.

		 /*******************************
		 *          NODE STATUS		*
		 *******************************/

%!  update_failed(+Action, +Quorum, +Alive) is det.
%
%   We just sent the Quorum a  message  and   got  a  reply from the set
%   Alive.
%
%   @arg is one of `set`, `get` or `replicate` and indicates the
%   intended action.

update_failed(Action, Quorum, Alive) :-
    Failed is Quorum /\ \Alive,
    alive(Alive),
    consider_dead(Failed),
    (   failed(Failed)
    ->  true
    ;   (   clause(failed(_Old), true, Ref)
        ->  asserta(failed(Failed)),
            erase(Ref),
            debug(paxos(node), 'Updated failed quorum to 0x~16r', [Failed])
        ;   asserta(failed(Failed))
        ),
        (   Action == set
        ->  start_replicator
        ;   true
        )
    ).

consider_dead(0) :-
    !.
consider_dead(Failed) :-
    Node is lsb(Failed),
    consider_dead1(Node),
    Rest is Failed /\ \(1<<Node),
    consider_dead(Rest).

consider_dead1(Node) :-
    clause(failed(Node, Last, Score), true, Ref),
    !,
    setting(death_half_life, HalfLife),
    setting(death_score, DeathScore),
    get_time(Now),
    Passed is Now-Last,
    NewScore is Score*(2**(-Passed/HalfLife)) + 10,
    asserta(failed(Node, Now, NewScore)),
    erase(Ref),
    (   NewScore < DeathScore
    ->  debug(paxos(node), 'Consider node ~d dead', [Node]),
        paxos_leave(Node)
    ;   true
    ).
consider_dead1(Node) :-
    get_time(Now),
    asserta(failed(Node, Now, 10)).

alive(Bitmap) :-
    (   clause(failed(Node, _Last, _Score), true, Ref),
        Bitmap /\ (1<<Node) =\= 0,
        erase(Ref),
        fail
    ;   true
    ).


%!  life_quorum(-Quorum, -LifeQuorum) is det.
%
%   Find the Quorum and the living nodes   from  the Quorum. This is the
%   set for which we wait.  If  the   LifeQuorum  is  not  a majority we
%   address the whole Quorum.
%
%   @tbd At some point in time we must remove a node from the quorum.

life_quorum(Quorum, LifeQuorum) :-
    quorum(Quorum),
    (   failed(Failed),
        Failed \== 0,
        LifeQuorum is Quorum /\ \Failed,
        majority(LifeQuorum, Quorum)
    ->  true
    ;   LifeQuorum = Quorum
    ).


		 /*******************************
		 *        NETWORK STATUS	*
		 *******************************/

:- paxos_admin_key(quorum, Key),
   listen(paxos_changed(Key, Quorum),
          update_quorum(Quorum)).
:- paxos_admin_key(dead, Key),
   listen(paxos_changed(Key, Death),
          update_dead(Death)).

update_quorum(Proposed) :-
    debug(paxos(node), 'Received quorum proposal 0x~16r', [Proposed]),
    quorum(Proposed),
    !.
update_quorum(Proposed) :-
    leaving,
    !,
    update(quorum(Proposed)).
update_quorum(Proposed) :-
    node(Node),
    Proposed /\ (1<<Node) =\= 0,
    !,
    update(quorum(Proposed)).
update_quorum(Proposed) :-
    node(Node),
    NewQuorum is Proposed \/ (1<<Node),
    update(quorum(NewQuorum)),
    debug(paxos(node), 'I''m not in the quorum! Proposing 0x~16r', [NewQuorum]),
    paxos_set_admin_bg(quorum, NewQuorum).

update_dead(Proposed) :-
    debug(paxos(node), 'Received dead proposal 0x~16r', [Proposed]),
    dead(Proposed),
    !.
update_dead(Proposed) :-
    leaving,
    !,
    update(dead(Proposed)).
update_dead(Proposed) :-
    node(Node),
    Proposed /\ (1<<Node) =:= 0,
    !,
    update(dead(Proposed)).
update_dead(Proposed) :-
    node(Node),
    NewDead is Proposed /\ \(1<<Node),
    update(dead(NewDead)),
    paxos_set_admin_bg(dead, NewDead).

update(Clause) :-
    functor(Clause, Name, Arity),
    functor(Generic, Name, Arity),
    (   clause(Generic, true, Ref)
    ->  asserta(Clause),
        erase(Ref)
    ;   asserta(Clause)
    ).

%!  paxos_property(?Property)
%
%   True if Property is  a  current   property  for  the  paxos network.
%   Defined properties are:
%
%     - node(?NodeID)
%     - quorum(?NodeBitmap)
%     - failed(?NodeBitmap)

paxos_property(node(NodeID)) :-
    node(NodeID).
paxos_property(quorum(Quorum)) :-
    quorum(Quorum).
paxos_property(failed(Nodes)) :-
    failed(Nodes).


		 /*******************************
		 *         INBOUND EVENTS	*
		 *******************************/

%!  paxos_message(?Message)
%
%   Handle inbound actions from our peers.   Defines  values for Message
%   are:
%
%     - prepare(+Key,-Node,-Gen,+Value)
%     A request message to set Key to Value. Returns the current
%     generation at which we have a value or `0` for Gen and the
%     our node id for Node.
%     - accept(+Key,-Node,+Gen,-GenA,+Value)
%     A request message to set Key to Value if Gen is newer than
%     the generation we have for Key.  In that case GenA is Gen.
%     Otherwise we reject using GenA = `nack`.
%     - changed(+Key,+Gen,+Value,+Acceptors)
%     The leader got enough accepts for setting Key to Value at Gen.
%     Acceptors is the set of nodes that accepted this value.
%     - learn(+Key,-Node,+Gen,-GenA,+Value)
%     Request message peforming phase one for replication to learner
%     nodes.
%     - learned(+Key,+Gen,+Value,+Acceptors)
%     Phase two of the replication. Confirm the newly learned knowledge.
%     - retrieve(+Key,-Node,-Gen,-Value)
%     A request message to retrieve our value for Key.  Also provides
%     our node id and the generation.
%     - forget(+Nodes)
%     Forget the existence of Nodes.
%     - node(-Node,-Quorum,-Dead)
%     Get my view about the network.  Node is the (integer) node id of
%     this node, Quorum is the idea of the quorum and Dead is the idea
%     about non-responsive nodes.
%
%   @tbd: originally the changed was  handled  by   a  get  and when not
%   successful with a new set, named   _paxos_audit_. I don't really see
%   why we need this.

paxos_message(prepare(Key,Node,Gen,Value)) :-
    node(Node),
    (   ledger(Key, Gen, _)
    ->  true
    ;   Gen = 0,
        ledger_create(Key, Gen, Value)
    ),
    debug(paxos, 'Prepared ~p-~p@~d', [Key,Value,Gen]).
paxos_message(accept(Key,Node,Gen,GenA,Value)) :-
    node(Node),
    (   ledger_update(Key, Gen, Value)
    ->  debug(paxos, 'Accepted ~p-~p@~d', [Key,Value,Gen]),
        GenA = Gen
    ;   debug(paxos, 'Rejected ~p-~p@~d', [Key,Value,Gen]),
        GenA = nack
    ).
paxos_message(changed(Key,Gen,Value,Acceptors)) :-
    debug(paxos, 'Changed ~p-~p@~d for ~p', [Key, Value, Gen, Acceptors]),
    ledger_update_holders(Key,Gen,Acceptors),
    broadcast(paxos_changed(Key,Value)).
paxos_message(learn(Key,Node,Gen,GenA,Value)) :-
    node(Node),
    debug(paxos, 'Learn ~p-~p@~p?', [Key, Value, Gen]),
    (   ledger_learn(Key,Gen,Value)
    ->  debug(paxos, 'Learned ~p-~p@~d', [Key,Value,Gen]),
        GenA = Gen
    ;   debug(paxos, 'Rejected ~p@~d', [Key, Gen]),
        GenA = nack
    ).
paxos_message(learned(Key,Gen,_Value,Acceptors)) :-
    ledger_update_holders(Key,Gen,Acceptors).
paxos_message(retrieve(Key,Node,K,Value)) :-
    node(Node),
    debug(paxos, 'Retrieving ~p', [Key]),
    ledger(Key,K,Value),
    debug(paxos, 'Retrieved ~p-~p@~d', [Key,Value,K]),
    !.
paxos_message(forget(Nodes)) :-
    ledger_forget(Nodes).
paxos_message(node(Node,Quorum,Dead)) :-
    (   node(Node),
        quorum(Quorum),
        dead(Dead)
    ->  true
    ;   salt(Salt),
        Node = self,
        Quorum = 0,
        Dead = Salt
    ).
paxos_message(claim_node(Node, Ok)) :-
    (   node(Node)
    ->  Ok = false
    ;   Ok = true
    ).
paxos_message(ask(Node, Message)) :-
    node(Node),
    broadcast_request(Message).


		 /*******************************
		 *     KEY-VALUE OPERATIONS	*
		 *******************************/

%%  paxos_set(+Term) is semidet.
%
%   Equivalent to paxos_key(Term,Key), pasox_set(Key,Term).   I.e., Term
%   is a ground compound term and its   key is the name/arity pair. This
%   version provides compatibility with older versions of this library.

%%  paxos_set(+Key, +Value) is semidet.
%%  paxos_set(+Key, +Value, +Options) is semidet.
%
%   negotiates to have Key-Value recorded in the  ledger for each of the
%   quorum's members. This predicate succeeds  if the quorum unanimously
%   accepts the proposed term. If no such   entry  exists in the Paxon's
%   ledger, then one is silently  created.   paxos_set/1  will retry the
%   transaction several times (default: 20)   before failing. Failure is
%   rare and is usually the result of a collision of two or more writers
%   writing to the same term at precisely  the same time. On failure, it
%   may be useful to wait some random period of time, and then retry the
%   transaction. By specifying a retry count   of zero, paxos_set/2 will
%   succeed iff the first ballot succeeds.
%
%   On   success,   paxos_set/1   will   also     broadcast   the   term
%   paxos_changed(Key,Value), to the quorum.
%
%   Options processed:
%
%     - retry(Retries)
%     is a non-negative integer specifying the number of retries that
%     will be performed before a set is abandoned.  Defaults to the
%     _setting_ `max_sets` (20).
%     - timeout(+Seconds)
%     Max time to wait for the forum to reply.  Defaults to the
%     _setting_ `response_timeout` (0.020, 20ms).
%
%   @arg Term is a compound  that   may  have  unbound variables.
%   @tbd If the Value is already current, should we simply do nothing?

paxos_set(Term) :-
    paxos_key(Term, Key),
    paxos_set(Key, Term, []).

paxos_set(Key, Value) :-
    paxos_set(Key, Value, []).

paxos_set(Key, Value, Options) :-
    must_be(ground, Key-Value),
    paxos_initialize,
    option(retry(Retries), Options, Retries),
    option(timeout(TMO), Options, TMO),
    apply_default(Retries, max_sets),
    apply_default(TMO, response_timeout),
    paxos_message(prepare(Key,Np,Rp,Value), TMO, Prepare),
    between(0, Retries, _),
      life_quorum(Quorum, Alive),
      Alive \== 0,
      debug(paxos, 'Set: ~p -> ~p', [Key, Value]),
      collect(Quorum, false, Np, Rp, Prepare, Rps, PrepNodes),
      debug(paxos, 'Set: quorum: 0x~16r, prepared by 0x~16r, gens ~p',
            [Quorum, PrepNodes, Rps]),
      majority(PrepNodes, Quorum),
      max_list(Rps, K),
      succ(K, K1),
      paxos_message(accept(Key,Na,K1,Ra,Value), TMO, Accept),
      collect(Alive, Ra == nack, Na, Ra, Accept, Ras, AcceptNodes),
      majority(AcceptNodes, Quorum),
      intersecting(PrepNodes, AcceptNodes),
      c_element(Ras, K, K1),
      broadcast(paxos(log(Key,Value,AcceptNodes,K1))),
      paxos_message(changed(Key,K1,Value,AcceptNodes), -, Changed),
      broadcast(Changed),
      update_failed(set, Quorum, AcceptNodes),
    !.

apply_default(Var, Setting) :-
    var(Var),
    !,
    setting(Setting, Var).
apply_default(_, _).

majority(SubSet, Set) :-
    popcount(SubSet) >= (popcount(Set)+2)//2.

intersecting(Set1, Set2) :-
    Set1 /\ Set2 =\= 0.


%!  collect(+Quorum, :Stop, ?Node, ?Template, ?Message,
%!          -Result, -NodeSet) is semidet.
%
%   Perform a broadcast request using Message.   Node and Template share
%   with Message and extract the replying node and the result value from
%   Message. Result is the list of  instantiations for Template received
%   and NodeSet is the set (bitmask) of   Node values that replies, i.e.
%   |NodeSet| is length(Result). The transfer stops   if  all members of
%   the set Quorum responded or the configured timeout passed.

collect(Quorum, Stop, Node, Template, Message, Result, NodeSet) :-
    State = state(0),
    L0 = [dummy|_],
    Answers = list(L0),
    (   broadcast_request(Message),
        debug(paxos(request), 'broadcast_request: ~p', [Message]),
        (   Stop
        ->  !,
            fail
        ;   true
        ),
        duplicate_term(Template, Copy),
        NewLastCell = [Copy|_],
        arg(1, Answers, LastCell),
        nb_linkarg(2, LastCell, NewLastCell),
        nb_linkarg(1, Answers, NewLastCell),
        arg(1, State, Replied0),
        Replied is Replied0 \/ (1<<Node),
        nb_setarg(1, State, Replied),
        Quorum /\ Replied =:= Quorum
    ->  true
    ;   true
    ),
    arg(1, State, NodeSet),
    arg(1, Answers, [_]),               % close the answer list
    L0 = [_|Result].

%!  paxos_quorum_ask(?Template, +Message, -Result, +Options)
%
%   Ask the paxos forum for their opinion.  This predicate is not really
%   part  of  the  paxos  protocol,  but    reuses  notably  the  quorum
%   maintenance mechanism of this library for   asking  questions to the
%   quorum (cluster). Message is the message to   be  asked. Result is a
%   list of copies of Template from the quorum. Options:
%
%     - timeout(+Seconds)
%       Max time to wait for a reply. Default is the setting
%       `response_timeout`.
%     - node(?Node)
%       Can be used to include the replying node into Template.
%     - quorum(?Quorum)
%       Set/query the interviewed quorum as a bitmask

paxos_quorum_ask(Template, Message, Result, Options) :-
    option(timeout(TMO), Options, TMO),
    option(node(Node), Options, _),
    option(quorum(Quorum), Options, Quorum),
    apply_default(TMO, response_timeout),
    (   var(Quorum)
    ->  life_quorum(Quorum, _Alive)
    ;   true
    ),
    paxos_message(ask(Node, Message), TMO, BroadcastMessage),
    collect(Quorum, false, Node, Template, BroadcastMessage, Result, _PrepNodes).

%!  paxos_get(?Term) is semidet.
%
%   Equivalent to paxos_key(Term,Key), pasox_get(Key,Term).   I.e., Term
%   is a compound term and its key  is the name/arity pair. This version
%   provides compatibility with older versions of this library.

%!  paxos_get(+Key, +Value) is semidet.
%!  paxos_get(+Key, +Value, +Options) is semidet.
%
%   unifies Term with the entry retrieved from the Paxon's ledger. If no
%   such entry exists in the member's local   cache,  then the quorum is
%   asked to provide a value,  which   is  verified  for consistency. An
%   implied paxos_set/1 follows. This predicate  succeeds if a term
%   with the same functor and arity exists   in  the Paxon's ledger, and
%   fails otherwise.
%
%   Options processed:
%
%     - retry(Retries)
%     is a non-negative integer specifying the number of retries that
%     will be performed before a set is abandoned.  Defaults to the
%     _setting_ `max_gets` (5).
%     - timeout(+Seconds)
%     Max time to wait for the forum to reply.  Defaults to the
%     _setting_ `response_timeout` (0.020, 20ms).
%
%   @arg Term is a compound. Any unbound variables are unified with
%   those provided in the ledger entry.

paxos_get(Term) :-
    paxos_key(Term, Key),
    paxos_get(Key, Term, []).
paxos_get(Key, Value) :-
    paxos_get(Key, Value, []).

paxos_get(Key, Value, _) :-
    ledger(Key, _Line, Value),
    !.
paxos_get(Key, Value, Options) :-
    paxos_initialize,
    option(retry(Retries), Options, Retries),
    option(timeout(TMO), Options, TMO),
    apply_default(Retries, max_gets),
    apply_default(TMO, response_timeout),
    Msg = Line-Value,
    paxos_message(retrieve(Key,Nr,Line,Value), TMO, Retrieve),
    node(Node),
    between(0, Retries, _),
      life_quorum(Quorum, Alive),
      QuorumA is Alive /\ \(1<<Node),
      collect(QuorumA, false, Nr, Msg, Retrieve, Terms, RetrievedNodes),
      debug(paxos, 'Retrieved: ~p from 0x~16r', [Terms, RetrievedNodes]),
      highest_vote(Terms, _Line-MajorityValue, Count),
      debug(paxos, 'Best: ~p with ~d votes', [MajorityValue, Count]),
      Count >= (popcount(QuorumA)+2)//2,
      debug(paxos, 'Retrieve: accept ~p', [MajorityValue]),
      update_failed(get, Quorum, RetrievedNodes),
      paxos_set(Key, MajorityValue),    % Is this needed?
    !.

highest_vote(Terms, Term, Count) :-
    msort(Terms, Sorted),
    count_votes(Sorted, Counted),
    sort(1, >, Counted, [Count-Term|_]).

count_votes([], []).
count_votes([H|T0], [N-H|T]) :-
    count_same(H, T0, 1, N, R),
    count_votes(R, T).

count_same(H, [Hc|T0], C0, C, R) :-
    H == Hc,
    !,
    C1 is C0+1,
    count_same(H, T0, C1, C, R).
count_same(_, R, C, C, R).

%!  paxos_key(+Term, -Key) is det.
%
%   Compatibility to allow for paxos_get/1, paxos_set/1, etc. The key of
%   a compound term is a term `'$c'(Name,Arity)`.   Note  that we do not
%   use `Name/Arity` and `X/Y` is  naturally   used  to organize keys as
%   hierachical _paths_.

paxos_key(Compound, '$c'(Name,Arity)) :-
    compound(Compound), !,
    compound_name_arity(Compound, Name, Arity).
paxos_key(Compound, _) :-
    must_be(compound, Compound).


		 /*******************************
		 *          REPLICATION		*
		 *******************************/

%!  start_replicator
%
%   Start or signal the replicator thread  that there may be outstanding
%   replication work.  This is the case if
%
%     - The union of _quorum_ and _learners_ was extended, and thus
%       all data may need to be replicated to the new members.
%     - A paxos_set/3 was not fully acknowledged.

start_replicator :-
    catch(thread_send_message(paxos_replicator, run),
          error(existence_error(_,_),_),
          fail),
    !.
start_replicator :-
    catch(thread_create(replicator, _,
                        [ alias(paxos_replicator),
                          detached(true)
                        ]),
          error(permission_error(_,_,_),_),
          true).

replicator :-
    setting(replication_rate, ReplRate),
    ReplSleep is 1/ReplRate,
    node(Node),
    debug(paxos(replicate), 'Starting replicator', []),
    State = state(idle),
    repeat,
      quorum(Quorum),
      dead(Dead),
      LifeQuorum is Quorum /\ \Dead,
      (   LifeQuorum /\ \(1<<Node) =:= 0
      ->  debug(paxos(replicate),
                'Me: ~d, Quorum: 0x~16r, Dead: 0x~16r: I''m alone, waiting ...',
                [Node, Quorum, Dead]),
          thread_get_message(_)
      ;   (   paxos_replicate_key(LifeQuorum, Key, [])
          ->  replicated(State, key(Key)),
              thread_self(Me),
              thread_get_message(Me, _, [timeout(ReplSleep)])
          ;   replicated(State, idle),
              thread_get_message(_)
          )
      ),
      fail.

replicated(State, key(_Key)) :-
    arg(1, State, idle),
    !,
    debug(paxos(replicate), 'Start replicating ...', []),
    nb_setarg(1, State, 1).
replicated(State, key(_Key)) :-
    !,
    arg(1, State, C0),
    C is C0+1,
    nb_setarg(1, State, C).
replicated(State, idle) :-
    arg(1, State, idle),
    !.
replicated(State, idle) :-
    arg(1, State, Count),
    debug(paxos(replicate), 'Replicated ~D keys', [Count]),
    nb_setarg(1, State, idle).


%!  paxos_replicate_key(+Nodes:bitmap, ?Key, +Options) is det.
%
%   Replicate a Key to Nodes.  If Key is unbound, a random key is
%   selected.
%
%     - timeout(+Seconds)
%     Max time to wait for the forum to reply.  Defaults to the
%     _setting_ `response_timeout` (0.020, 20ms).

paxos_replicate_key(Nodes, Key, Options) :-
    replication_key(Nodes, Key),
    option(timeout(TMO), Options, TMO),
    apply_default(TMO, response_timeout),
    ledger_current(Key, Gen, Value, Holders),
    paxos_message(learn(Key,Na,Gen,Ga,Value), TMO, Learn),
    collect(Nodes, Ga == nack, Na, Ga, Learn, _Gas, LearnedNodes),
    NewHolders is Holders \/ LearnedNodes,
    paxos_message(learned(Key,Gen,Value,NewHolders), -, Learned),
    broadcast(Learned),
    update_failed(replicate, Nodes, LearnedNodes).

replication_key(_Nodes, Key) :-
    ground(Key),
    !.
replication_key(Nodes, Key) :-
    (   Nth is 1+random(popcount(Nodes))
    ;   Nth = 1
    ),
    call_nth(needs_replicate(Nodes, Key), Nth),
    !.

needs_replicate(Nodes, Key) :-
    ledger_current(Key, _Gen, _Value, Holders),
    Nodes /\ \Holders =\= 0,
    \+ paxos_admin_key(_, Key).


		 /*******************************
		 *      KEY CHANGE EVENTS	*
		 *******************************/

%!  paxos_on_change(?Term, :Goal) is det.
%!  paxos_on_change(?Key, ?Value, :Goal) is det.
%
%   Executes the specified Goal  when   Key  changes.  paxos_on_change/2
%   listens for paxos_changed(Key,Value) notifications   for  Key, which
%   are emitted as the result   of  successful paxos_set/3 transactions.
%   When one is received for Key, then   Goal  is executed in a separate
%   thread of execution.
%
%   @arg Term is a compound, identical to that used for
%   paxos_get/1.
%   @arg Goal is one of:
%     - a callable atom or term, or
%     - the atom =ignore=, which causes monitoring for Term to be
%       discontinued.

paxos_on_change(Term, Goal) :-
    paxos_key(Term, Key),
    paxos_on_change(Key, Term, Goal).

paxos_on_change(Key, Value, Goal) :-
    Goal = _:Plain,
    must_be(callable, Plain),
    (   Plain == ignore
    ->  unlisten(paxos_user, paxos_changed(Key,Value))
    ;   listen(paxos_user, paxos_changed(Key,Value),
               key_changed(Key, Value, Goal)),
        paxos_initialize
    ).

key_changed(_Key, _Value, Goal) :-
    E = error(_,_),
    catch(thread_create(Goal, _, [detached(true)]),
          E, key_error(E)).

key_error(error(permission_error(create, thread, _), _)) :-
    !.
key_error(E) :-
    print_message(error, E).


		 /*******************************
		 *            HOOKS		*
		 *******************************/

%!  node(-Node) is det.
%
%   Get the node ID for this paxos node.

%!  quorum(-Quorum) is det.
%
%   Get the current quorum as a bitmask

%!  paxos_message(+PaxOS, +TimeOut, -BroadcastMessage) is det.
%
%   Transform a basic PaxOS message in   a  message for the broadcasting
%   service. This predicate is hooked   by paxos_message_hook/3 with the
%   same signature.
%
%   @arg TimeOut is one of `-` or a time in seconds.

paxos_message(Paxos:From, TMO, Message) :-
    paxos_message_raw(paxos(Paxos):From, TMO, Message).
paxos_message(Paxos, TMO, Message) :-
    paxos_message_raw(paxos(Paxos), TMO, Message).

paxos_message_raw(Message, TMO, WireMessage) :-
    paxos_message_hook(Message, TMO, WireMessage),
    !.
paxos_message_raw(Message, TMO, WireMessage) :-
    throw(error(mode_error(det, fail,
                           paxos:paxos_message_hook(Message, TMO, WireMessage)), _)).


		 /*******************************
		 *           STORAGE		*
		 *******************************/

%!  paxos_ledger_hook(+Action, ?Key, ?Gen, ?Value, ?Holders)
%
%   Hook called for all operations on the ledger.  Defined actions are:
%
%     - current
%       Enumerate our ledger content.
%     - get
%       Get a single value from our ledger.
%     - create
%       Create a new key in our ledger.
%     - accept
%       Accept a new newly proposed value for a key.  Failure causes
%       the library to send a _NACK_ message.
%     - set
%       Final acceptance of Ken@Gen, providing the holders that accepted
%       the new value.
%     - learn
%       Accept new keys in a new node or node that has been offline for
%       some time.

:- dynamic
    paxons_ledger/4.                    % Key, Gen, Value, Holders

%!  ledger_current(?Key, ?Gen, ?Value, ?Holders) is nondet.
%
%   True when Key is a known key in my ledger.

ledger_current(Key, Gen, Value, Holders) :-
    paxos_ledger_hook(current, Key, Gen, Value, Holders).
ledger_current(Key, Gen, Value, Holders) :-
    paxons_ledger(Key, Gen, Value, Holders),
    valid(Holders).


%!  ledger(+Key, -Gen, -Value) is semidet.
%
%   True if the ledger has Value associated  with Key at generation Gen.
%   Note that if the value is  not   yet  acknowledged  by the leader we
%   should not use it.

ledger(Key, Gen, Value) :-
    paxos_ledger_hook(get, Key, Gen, Value0, Holders),
    !,
    valid(Holders),
    Value = Value0.
ledger(Key, Gen, Value) :-
    paxons_ledger(Key, Gen, Value0, Holders),
    valid(Holders),
    !,
    Value = Value0.

%!  ledger_create(+Key, +Gen, +Value) is det.
%
%   Create a new Key-Value pair  at   generation  Gen.  This is executed
%   during the preparation phase.

ledger_create(Key, Gen, Value) :-
    paxos_ledger_hook(create, Key, Gen, Value, -),
    !.
ledger_create(Key, Gen, Value) :-
    get_time(Now),
    asserta(paxons_ledger(Key, Gen, Value, created(Now))).

%!  ledger_update(+Key, +Gen, +Value) is semidet.
%
%   Update Key to Value if the  current   generation  is older than Gen.
%   This reflects the accept phase of the protocol.

ledger_update(Key, Gen, Value) :-
    paxos_ledger_hook(accept, Key, Gen, Value, -),
    !.
ledger_update(Key, Gen, Value) :-
    paxons_ledger(Key, Gen0, _Value, _Holders),
    !,
    Gen > Gen0,
    get_time(Now),
    asserta(paxons_ledger(Key, Gen, Value, accepted(Now))),
    (   Gen0 == 0
    ->  retractall(paxons_ledger(Key, Gen0, _, _))
    ;   true
    ).

%!  ledger_update_holders(+Key, +Gen, +Holders) is det.
%
%   The leader acknowledged that Key@Gen represents a valid new

ledger_update_holders(Key, Gen, Holders) :-
    paxos_ledger_hook(set, Key, Gen, _, Holders),
    !.
ledger_update_holders(Key, Gen, Holders) :-
    clause(paxons_ledger(Key, Gen, Value, Holders0), true, Ref),
    !,
    (   Holders0 == Holders
    ->  true
    ;   asserta(paxons_ledger(Key, Gen, Value, Holders)),
        erase(Ref)
    ),
    clean_key(Holders0, Key, Gen).

clean_key(Holders, _Key, _Gen) :-
    valid(Holders),
    !.
clean_key(_, Key, Gen) :-
    (   clause(paxons_ledger(Key, Gen0, _Value, _Holders0), true, Ref),
        Gen0 < Gen,
        erase(Ref),
        fail
    ;   true
    ).


%!  ledger_learn(+Key,+Gen,+Value) is semidet.
%
%   We received a learn event.

ledger_learn(Key,Gen,Value) :-
    paxos_ledger_hook(learn, Key, Gen, Value, -),
    !.
ledger_learn(Key,Gen,Value) :-
    paxons_ledger(Key, Gen0, Value0, _Holders),
    !,
    (   Gen == Gen0,
        Value == Value0
    ->  true
    ;   Gen > Gen0
    ->  get_time(Now),
        asserta(paxons_ledger(Key, Gen, Value, learned(Now)))
    ).
ledger_learn(Key,Gen,Value) :-
    get_time(Now),
    asserta(paxons_ledger(Key, Gen, Value, learned(Now))).

%!  ledger_forget(+Nodes) is det.
%
%   Remove Nodes from all ledgers.  This is executed in a background
%   thread.

ledger_forget(Nodes) :-
    catch(thread_create(ledger_forget_threaded(Nodes), _,
                        [ detached(true)
                        ]),
          error(permission_error(create, thread, _), _),
          true).

ledger_forget_threaded(Nodes) :-
    debug(paxos(node), 'Forgetting 0x~16r', [Nodes]),
    forall(ledger_current(Key, Gen, _Value, Holders),
           ledger_forget(Nodes, Key, Gen, Holders)),
    debug(paxos(node), 'Forgotten 0x~16r', [Nodes]).

ledger_forget(Nodes, Key, Gen, Holders) :-
    NewHolders is Holders /\ \Nodes,
    (   NewHolders \== Holders,
        ledger_update_holders(Key, Gen, NewHolders)
    ->  true
    ;   true
    ).

valid(Holders) :-
    integer(Holders).


		 /*******************************
		 *             UTIL		*
		 *******************************/

%!  c_element(+NewList, +Old, -Value)
%
%   A Muller c-element is a logic block  used in asynchronous logic. Its
%   output assumes the value of its  input   iff  all  of its inputs are
%   identical. Otherwise, the output retains its original value.

c_element([New | More], _Old, New) :-
    forall(member(N, More), N == New),
    !.
c_element(_List, Old, Old).

%!  arg_union(+Arg, +ListOfTerms, -Set) is det.
%
%   Get all the nth args from ListOfTerms  and   do  a  set union on the
%   result.

arg_union(Arg, NodeStatusList, Set) :-
    maplist(arg(Arg), NodeStatusList, Sets),
    list_union(Sets, Set).

list_union(Sets, Set) :-
    list_union(Sets, 0, Set).

list_union([], Set, Set).
list_union([H|T], Set0, Set) :-
    Set1 is Set0 \/ H,
    list_union(T, Set1, Set).
