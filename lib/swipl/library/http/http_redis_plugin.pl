/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, SWI-Prolog Solutions b.v.
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

:- module(http_redis_plugin, []).
:- use_module(library(http/http_session)).
:- autoload(library(apply), [maplist/3]).
:- autoload(library(error), [must_be/2]).
:- autoload(library(lists), [member/2]).
:- autoload(library(redis), [redis/3]).
:- autoload(library(broadcast), [broadcast/1]).
:- use_module(library(debug), [debug/3]).

/** <module> Hook session management to use Redis

This module acts as  a   plugin  for library(http/http_session), storing
session information on a Redis server. This has several consequences:

  - The Prolog server may be restarted without loosing session data.
    This is notably useful when long session timeouts are used.
  - Multiple Prolog servers can act as a cluster while session
    management is used.
  - Associating Prolog data with sessions is relatively slow.  The
    assert/retract is replaced by managing a Redis list.  Data in
    this list is matched sequentially, where each term needs to be
    parsed before it can be matched.
  - Associated data is currently limited to __ground terms__.

The   library   is   activated   by   loading    it   in   addition   to
library(http/http_session)  and  using    http_set_session_options/1  to
configure the Redis database as below. The redis_server/2 predicate from
library(redis) can be used  to  specify   the  parameters  for the redis
server  such as host, port or authentication.

```
:- http_set_session_options(
       [ redis_db(default),
         redis_prefix('swipl:http:session')
       ]).
```

## Redis key usage

All  Redis  keys  reside  under  a    prefix  specified  by  the  option
redis_prefix(Prefix), which defaults to  `'swipl:http:session'`. Here we
find:

  - An ordered set at <prefix>:expire that contains the session ids,
    ordered by the time the session expires.  Session enumeration and
    garbage collection is based on this set.
  - A hash at <prefix>:session:<id> which contains the _peer_ address,
    the _last used_ time and optionally session specific settings.
  - If there is session _data_, a list at <prefix>:data:<id> of Prolog
    terms, represented as strings that contain the session data.
*/

:- multifile
    http_session:hooked/0,
    http_session:hook/1,
    http_session:session_option/2.

http_session:session_option(redis_db, atom).
http_session:session_option(redis_prefix, atom).

http_session:hooked :-
    http_session:session_setting(redis_db(_)).

%http_session:hook(assert_session(SessionID, Peer)).
%http_session:hook(set_session_option(SessionId, Setting)).
%http_session:hook(get_session_option(SessionId, Setting)).
%http_session:hook(active_session(SessionID, Peer, LastUsed)).
%http_session:hook(set_last_used(SessionID, Now, TimeOut)).
%http_session:hook(asserta(session_data(SessionId, Data))).
%http_session:hook(assertz(session_data(SessionId, Data))).
%http_session:hook(retract(session_data(SessionId, Data))).
%http_session:hook(retractall(session_data(SessionId, Data))).
%http_session:hook(session_data(SessionId, Data)).
%http_session:hook(current_session(SessionID, Data)).
%http_session:hook(close_session(?SessionID)).
%http_session:hook(gc_sessions).

http_session:hook(assert_session(SessionID, Peer)) :-
    session_db(SessionID, DB, Key),
    http_session:session_setting(timeout(Timeout)),
    peer_string(Peer, PeerS),
    get_time(Now),
    redis(DB, hset(Key,
                   peer, PeerS,
                   last_used, Now)),
    expire(SessionID, Timeout).
http_session:hook(set_session_option(SessionID, Setting)) :-
    session_db(SessionID, DB, Key),
    Setting =.. [Name,Value],
    redis(DB, hset(Key, Name, Value as prolog)),
    (   Setting = timeout(Timeout)
    ->  expire(SessionID, Timeout)
    ;   true
    ).
http_session:hook(get_session_option(SessionID, Setting)) :-
    session_db(SessionID, DB, Key),
    Setting =.. [Name,Value],
    redis(DB, hget(Key, Name), Value).
http_session:hook(active_session(SessionID, Peer, LastUsed)) :-
    session_db(SessionID, DB, Key),
    redis(DB, hget(Key, peer), PeerS),
    peer_string(Peer, PeerS),
    redis(DB, hget(Key, last_used), LastUsed as number).
http_session:hook(set_last_used(SessionID, Now, Timeout)) :-
    session_db(SessionID, DB, Key),
    redis(DB, hset(Key,
                   last_used, Now)),
    Expire is Now+Timeout,
    expire(SessionID, Expire).
http_session:hook(asserta(session_data(SessionID, Data))) :-
    must_be(ground, Data),
    session_data_db(SessionID, DB, Key),
    redis(DB, lpush(Key, Data as prolog)).
http_session:hook(assertz(session_data(SessionID, Data))) :-
    must_be(ground, Data),
    session_data_db(SessionID, DB, Key),
    redis(DB, rpush(Key, Data as prolog)).
http_session:hook(retract(session_data(SessionID, Data))) :-
    session_data_db(SessionID, DB, Key),
    redis_get_list(DB, Key, 10, List),
    member(Data, List),
    redis(DB, lrem(Key, 1, Data as prolog)).
http_session:hook(retractall(session_data(SessionID, Data))) :-
    forall(http_session:hook(retract(session_data(SessionID, Data))),
           true).
http_session:hook(session_data(SessionID, Data)) :-
    session_data_db(SessionID, DB, Key),
    redis_get_list(DB, Key, 10, List),
    member(Data, List).
http_session:hook(current_session(SessionID, Data)) :-
    session_db(SessionID, DB, Key),
    redis(DB, hget(Key, last_used), Time as number),
    get_time(Now),
    Idle is Now - Time,
    (   http_session:session_setting(SessionID, timeout(TMO)),
        TMO > 0
    ->  Idle =< TMO
    ;   true
    ),
    (   Data = peer(Peer),
        redis(DB, hget(Key, peer), PeerS),
        peer_string(Peer, PeerS)
    ;   Data = idle(Idle)
    ;   non_reserved_property(Data),
        http_session:hook(session_data(SessionID, Data))
    ).
http_session:hook(close_session(SessionID)) :-
    gc_session(SessionID).
http_session:hook(gc_sessions) :-
    gc_sessions.

non_reserved_property(P) :-
    var(P),
    !.
non_reserved_property(peer(_)) :- !, fail.
non_reserved_property(idle(_)) :- !, fail.
non_reserved_property(_).


		 /*******************************
		 *      SCHEDULE TIMEOUT	*
		 *******************************/

expire(SessionID, Timeout) :-
    get_time(Now),
    Time is Now+Timeout,
    session_expire_db(DB, Key),
    redis(DB, zadd(Key, Time, SessionID)).

gc_sessions :-
    session_expire_db(DB, Key),
    get_time(Now),
    redis(DB, zrangebyscore(Key, "-inf", Now), TimedOut as atom),
    forall(member(SessionID, TimedOut),
           gc_session(SessionID)).

gc_session(SessionID) :-
    debug(http_session(gc), 'GC session ~p', [SessionID]),
    session_db(SessionID, DB, SessionKey),
    session_expire_db(DB, TMOKey),
    redis(DB, zrem(TMOKey, SessionID)),
    redis(DB, hget(SessionKey, peer), PeerS),
    peer_string(Peer, PeerS),
    broadcast(http_session(end(SessionID, Peer))),
    redis(DB, del(SessionKey)),
    session_data_db(SessionID, DB, DataKey),
    redis(DB, del(DataKey)).


		 /*******************************
		  *
		 *             UTIL		*
		 *******************************/

peer_string(ip(A,B,C,D), String) :-
    nonvar(String),
    !,
    split_string(String, ".", "", List),
    maplist(number_string, [A,B,C,D], List).
peer_string(ip(A,B,C,D), String) :-
    atomics_to_string([A,B,C,D], ".", String).

session_db(SessionID, DB, Key) :-
    nonvar(SessionID),
    !,
    http_session:session_setting(redis_db(DB)),
    key_prefix(Prefix),
    atomics_to_string([Prefix,session,SessionID], :, Key).
session_db(SessionID, DB, Key) :-
    session_expire_db(DB, TMOKey),
    redis_zscan(DB, TMOKey, Pairs, []),
    member(SessionIDS-_Timeout, Pairs),
    atom_string(SessionID, SessionIDS),
    key_prefix(Prefix),
    atomics_to_string([Prefix,session,SessionID], :, Key).

session_expire_db(DB, Key) :-
    http_session:session_setting(redis_db(DB)),
    key_prefix(Prefix),
    atomics_to_string([Prefix,expire], :, Key).

session_data_db(SessionID, DB, Key) :-
    http_session:session_setting(redis_db(DB)),
    key_prefix(Prefix),
    atomics_to_string([Prefix,data,SessionID], :, Key).

key_prefix(Prefix) :-
    http_session:session_setting(redis_prefix(Prefix)),
    !.
key_prefix('swipl:http:sessions').
