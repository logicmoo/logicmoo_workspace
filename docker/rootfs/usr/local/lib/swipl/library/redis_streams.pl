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

:- module(redis_streams,
          [ xstream_set/3,              % +Redis, +Key, +Option
            xadd/4,                     % +Redis, +Key, ?Id, +Data:dict
            xlisten/3,                  % +Redis, +Streams, +Options
            xlisten_group/5,            % +Redis, +Group, +Consumer,
                                        % +Streams, +Options
            xconsumer_stop/1            % +Leave
          ]).
:- use_module(library(redis)).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(broadcast)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(debug)).

:- meta_predicate
    xlisten(+, +, 5, 5, +).

:- multifile
    xhook/2.                            % +Stream, +Event

:- predicate_options(xlisten/3, 3,
                     [ count(nonneg),
                       start(one_of([$,0])),
                       starts(list)
                     ]).
:- predicate_options(xlisten_group/5, 5,
                     [ block(number),
                       max_deliveries(nonneg),
                       max_claim(nonneg)
                     ]).


/** <module> Using Redis streams

A Redis stream is a set of   messages consisting of key-value pairs that
are identified by a  time  and   sequence  number.  Streams are powerful
objects that can roughly be used for three purposes:

  - Maintain and query a log of events, i.e., a _timeline_.

  - Provide an alternative to Redis' publish/subscribe API that ensures
    messages get delivered by all clients even if they are offline at
    the moment an event is published.

  - Distribute messages over a group of clients.  This mode assigns
    messages to clients in a round-robin fashion.  Clients confirm
    a specific message is handled.  Living clients can inspect the
    stream for possibly dead clients and migrate the pending messages
    to other clients.

This library abstracts the latter two scenarios. The main predicates are

  - xadd/4 to add to a stream
  - xlisten/3 to read and broadcast messages from a stream
  - xlisten_group/5 to act as a _consumer_ in a consumer group.

@see https://redis.io/topics/streams-intro
*/

:- dynamic
    xstream_option/3.

%!  xstream_set(+Redis, +Key, +Option)
%
%   Set an option on for Key on Redis.  Currently supports:
%
%     - maxlen(+Count)
%       Make xadd/4 add a ``MAXLEN ~`` Count option to the ``XADD``
%       command, capping the length of the stream.  See also
%       [Redis as a message brokering system](#redis-brokering)

xstream_set(Redis, KeyS, Option) :-
    must_be(atom, Redis),
    atom_string(Key, KeyS),
    valid_option(Option),
    functor(Option, Name, Arity),
    functor(Gen, Name, Arity),
    retractall(xstream_option(Redis, Key, Gen)),
    asserta(xstream_option(Redis, Key, Option)).

valid_option(Option) :-
    stream_option(Option),
    !.
valid_option(Option) :-
    domain_error(redis_stream_option, Option).

stream_option(maxlen(X)) :- must_be(integer, X).


%!  xadd(+Redis, +Key, ?Id, +Data:dict) is det.
%
%   Add a message to a the stream Key on Redis. The length of the stream
%   can be capped using the xstream_set/3 option maxlen(Count). If Id is
%   unbound, generating the id is left to   the server and Id is unified
%   with the returned id. The returned id  is a string consisting of the
%   time stamp in milliseconds and a sequence number. See Redis docs for
%   details.

xadd(DB, Key, Id, Dict) :-
    redis_array_dict(Array, _, Dict),
    (   var(Id)
    ->  IdIn = '*'
    ;   IdIn = Id
    ),
    (   xstream_option(DB, Key, maxlen(MaxLen))
    ->  Command =.. [xadd, Key, maxlen, ~, MaxLen, IdIn|Array]
    ;   Command =.. [xadd, Key, IdIn|Array]
    ),
    redis(DB, Command, Reply),
    return_id(Id, Reply).

return_id(Id, Reply) :-
    var(Id),
    !,
    Id = Time-Seq,
    split_string(Reply, "-", "", [TimeS,SeqS]),
    number_string(Time, TimeS),
    number_string(Seq, SeqS).
return_id(_, _).


		 /*******************************
		 *           SUBSCRIBE		*
		 *******************************/

%!  xlisten(+Redis, +Streams, +Options).
%
%   Listen using ``XREAD`` on one or more   Streams on the server Redis.
%   For each message that arrives,  call   broadcast/1,  where Data is a
%   dict representing the message.
%
%       broadcast(redis(Redis, Stream, Id, Data))
%
%   Options:
%
%     - count(+Count)
%       Process at most Count messages per stream for each request.
%     - start(+Start)
%       Normally either `0` to start get all messages from the epoch
%       or `$` to get messages starting with the last.  Default is `$`.
%     - starts(+List)
%       May be used as an alternative to the start/1 option to specify
%       the start for each stream. This may be used to restart listening
%       if the application remembers the last processed id.
%
%   Note that this predicate does  __not   terminate__.  It  is normally
%   executed in a thread. The  following   call  listens  to the streams
%   `key1`   and   `key2`   on   the   default   Redis   server.   Using
%   reconnect(true), the client will try to re-establish a connection if
%   the collection got lost.
%
%
%   ```
%   ?- redis_connect(default, C, [reconnect(true)]),
%      thread_create(xlisten(C, [key1, key2], [start($)]),
%                    _, [detached(true)]).
%   ```
%
%   @arg Redis is either a Redis server name (see redis_server/3) or
%   an open connection.  If it is a server name, a new connection is
%   opened that is closed if xlisten/3 completes.
%   @see redis_subscribe/2 implements the classical   pub/sub  system of
%   Redis that does not have any memory.

xlisten(Redis, Streams, Options) :-
    xlisten(Redis, Streams, xbroadcast, xidle, Options).

xbroadcast(Redis, Stream, Id, Dict, _Options) :-
    redis_id(Redis, RedisId),
    catch(broadcast(redis(RedisId, Stream, Id, Dict)), Error,
          print_message(error, Error)).

redis_id(redis(Id, _, _), Id) :-
    !.
redis_id(Id, Id).

xidle(_Redis, _Streams, _Starts, _NewStarts, _Options).

%!  xlisten(+Redis, +Streams, +OnBroadCast, +OnIdle, +Options).
%
%   Generalized version of xlisten/3 that is provided two callbacks: one
%   to handle a message and one after each time the underlying ``XREAD``
%   or ``XREADGROUP`` has returned and the messages are processed. These
%   callbacks are called as follows:
%
%       call(OnBroadCast, +Redis, +Stream, +MessageId, +Dict)
%       call(OnIdle, +Redis, +Streams, +Starts, +NewStarts, +Options)
%
%   Both callbacks __must__ succeeds  and  not   leave  any  open choice
%   points.  Failure or exception causes xlisten/5 to stop.

xlisten(Redis, Streams, OnBroadcast, OnIdle, Options) :-
    atom(Redis),
    !,
    setup_call_cleanup(
        redis_connect(Redis, Conn, [reconnect(true)|Options]),
        xlisten_(Conn, Streams, OnBroadcast, OnIdle, Options),
        redis_disconnect(Conn)).
xlisten(Redis, Streams, OnBroadcast, OnIdle, Options) :-
    xlisten_(Redis, Streams, OnBroadcast, OnIdle, Options).

xlisten_(Redis, Streams, OnBroadcast, OnIdle, Options) :-
    xread_command(Streams, Starts0, CommandTempl, Options),
    listen_loop(Redis, Starts0, CommandTempl,
                OnBroadcast, OnIdle, Streams, Options).

xread_command(Streams, Starts0, Starts-Command, Options) :-
    option(group(Group-Consumer), Options),
    !,
    xread_command_args(Streams, Starts0, Starts, CmdArgs, Options),
    Command =.. [xreadgroup, group, Group, Consumer | CmdArgs].
xread_command(Streams, Starts0, Starts-Command, Options) :-
    xread_command_args(Streams, Starts0, Starts, CmdArgs, Options),
    Command =.. [xread|CmdArgs].

xread_command_args(Streams, Starts0, Starts, CmdArgs, Options) :-
    option(count(Count), Options),
    !,
    option(block(Block), Options, 0),
    BlockMS is integer(Block*1000),
    start_templ(Streams, Starts0, Starts, StreamArgs, Options),
    CmdArgs = [count, Count, block, BlockMS, streams | StreamArgs].
xread_command_args(Streams, Starts0, Starts, CmdArgs, Options) :-
    option(block(Block), Options, 0),
    BlockMS is integer(Block*1000),
    start_templ(Streams, Starts0, Starts, StreamArgs, Options),
    CmdArgs = [block, BlockMS, streams | StreamArgs].

start_templ(Streams, Starts0, Starts, StreamArgs, Options) :-
    option(starts(Starts0), Options),
    !,
    length(Streams, Len),
    length(Starts, Len),
    length(Starts0, LenS),
    (   LenS =:= Len
    ->  true
    ;   domain_error(xread_starts, Starts0)
    ),
    append(Streams, Starts, StreamArgs).
start_templ(Streams, Starts0, Starts, StreamArgs, Options) :-
    option(start(Start), Options, $),
    !,
    length(Streams, Len),
    length(Starts, Len),
    length(Starts0, Len),
    maplist(=(Start), Starts0),
    append(Streams, Starts, StreamArgs).

listen_loop(Redis, Starts, CommandTempl, OnBroadcast, OnIdle, Streams, Options) :-
    copy_term(CommandTempl, Starts-Command),
    (   redis(Redis, Command, Reply),
        Reply \== nil
    ->  dispatch_streams(Reply, Redis, Streams, Starts, NewStarts,
                         OnBroadcast, OnIdle, Options)
    ;   NewStarts = Starts
    ),
    call(OnIdle, Redis, Streams, Starts, NewStarts, Options),
    listen_loop(Redis, NewStarts, CommandTempl,
                OnBroadcast, OnIdle, Streams, Options).

dispatch_streams([], _, _, Starts, NewStarts, _, _, _) :-
    maplist(copy_start, Starts, NewStarts).
dispatch_streams([Tuple|T], Redis, Streams, Starts, NewStarts,
                 OnBroadcast, OnIdle, Options) :-
    stream_tuple(Tuple, StreamS, []),
    atom_string(Stream, StreamS),
    !,                                  % xreadgroup: no more old pending stuff
    set_start(Stream, _Start, >, Streams, Starts, NewStarts),
    dispatch_streams(T, Redis, Streams, Starts, NewStarts,
                     OnBroadcast, OnIdle, Options).
dispatch_streams([Tuple|T], Redis, Streams, Starts, NewStarts,
                 OnBroadcast, OnIdle, Options) :-
    stream_tuple(Tuple, StreamS, Messages),
    atom_string(Stream, StreamS),
    set_start(Stream, Start, NewStart, Streams, Starts, NewStarts),
    dispatch_messages(Messages, Stream, Redis, Start, NewStart,
                      OnBroadcast, Options),
    dispatch_streams(T, Redis, Streams, Starts, NewStarts,
                     OnBroadcast, OnIdle, Options).

stream_tuple(Stream-Messages, Stream, Messages) :- !.
stream_tuple([Stream,Messages], Stream, Messages).

set_start(Stream, Old, New, [Stream|_], [Old|_], [New|_]) :-
    !.
set_start(Stream, Old, New, [_|Streams], [_|OldStarts], [_|NewStarts]) :-
    set_start(Stream, Old, New, Streams, OldStarts, NewStarts).

copy_start(Old, New) :-
    (   var(New)
    ->  Old = New
    ;   true
    ).

%!  dispatch_messages(+Messages, +Stream, +Redis, +Start0, -Start) is det

dispatch_messages([], _, _, Start, Start, _, _).
dispatch_messages([[Start,Data]|T], Stream, Redis, Start0, NewStart,
                  OnBroadcast, Options) :-
    dispatch_message(Data, Stream, Redis, Start, OnBroadcast, Options),
    join_starts(Start0, Start, Start1),
    dispatch_messages(T, Stream, Redis, Start1, NewStart, OnBroadcast, Options).

dispatch_message(Data, Stream, Redis, Id, OnBroadcast, Options) :-
    (   Data == nil                     % when does this happen?
    ->  Dict = redis{}
    ;   redis_array_dict(Data, redis, Dict)
    ),
    call(OnBroadcast, Redis, Stream, Id, Dict, Options).

join_starts(>, _Start, >) :-
    !.
join_starts(_Start0, Start, Start).

		 /*******************************
		 *           CONSUMERS		*
		 *******************************/

%!  xlisten_group(+Redis, +Group, +Consumer, +Streams, +Options)
%
%   Listen as Consumer to Group. This is  similar to xlisten/3, with the
%   following differences:
%
%     - Instead of using broadcast/1, broadcast_request/1 is used and
%       the message is only considered processed if broadcast_request/1
%       succeeds.  If the message is handled with success, an ``XACK``
%       is sent to the server.
%
%   Options processed:
%
%     - block(+Seconds)
%       Causes ``XREADGROUP`` to return with timeout when no messages
%       arrive within Seconds.  On a timeout, xidle_group/5 is called
%       which will try to handle messages to other consumers pending
%       longer than Seconds. Choosing the time depends on the
%       application.  Notably:
%         - Using a time shorter than the required processing time
%           will make the job migrate from consumer to consumer until
%           max_deliveries(Count) is exceeded.  Note that the original
%           receiver does not notice that the job is claimed and thus
%           multiple consumers may ultimately answer the message.
%         - Using a too long time causes an unnecessarily long delay
%           if a node fails.
%     - max_deliveries(+Count)
%       Re-deliver (using ``XCLAIM``) a message max Count times.
%       Exceeding this calls xhook/2.  Default Count is `3`.
%     - max_claim(+Count)
%       Do not claim more than Count messages during a single idle
%       action.  Default is `10`.

xlisten_group(Redis, Group, Consumer, Streams, Options) :-
    catch(xlisten(Redis, Streams, xbroadcast_group, xidle_group,
                  [ group(Group-Consumer),
                    start(0)
                  | Options
                  ]),
          redis(stop(Leave)),
          true),
    (   Leave == true
    ->  xleave_group(Redis, Group, Consumer, Streams)
    ;   true
    ).

xbroadcast_group(Connection, Stream, Id, Dict, Options) :-
    option(group(Group-Consumer), Options),
    redis_id(Connection, RedisId),
    (   catch(broadcast_request(redis_consume(Stream, Dict,
                                              _{redis:RedisId,
                                                message:Id,
                                                group:Group,
                                                consumer:Consumer})),
              Error, xbroadcast_error(Error, Connection, Stream, Group, Id))
    ->  redis(Connection, xack(Stream, Group, Id))
    ;   true
    ).

xbroadcast_error(redis(stop(Unregister)), Connection, Stream, Group, Id) :-
    !,
    redis(Connection, xack(Stream, Group, Id), _),
    throw(redis(stop(Unregister))).
xbroadcast_error(Error, _Connection, _Stream, _Group, _Id) :-
    print_message(error, Error),
    fail.

%!  xidle_group(+Redis, +Streams, +Starts, +NewStarts, +Options) is det.
%
%   Called after ``XREADGROUP`` returns and   the  returned messages (if
%   any) have been processed. If `Start   == NewStarts` no messages have
%   been processed, indicating a timeout.
%
%   This implementation looks for idle messages   on  other consumer and
%   will try to claim them.
%
%   @tbd: max time to work on other consumers messages?

xidle_group(Redis, Streams, Starts, Starts, Options) :- % Idle time
    !,
    option(group(Group-_Consumer), Options),
    claim(Streams, Redis, Group, 0, _Claimed, Options).
xidle_group(_Redis, _Streams, _Starts, _NewStarts, _Options).

claim([], _, _, Claimed, Claimed, _).
claim([Stream|ST], Redis, Group, Claimed0, Claimed, Options) :-
    claim_for_stream(Redis, Stream, Group, Claimed0, Claimed1, Options),
    claim(ST, Redis, Group, Claimed1, Claimed, Options).

claim_for_stream(Redis, Stream, Group, Claimed0, Claimed, Options) :-
    check_limit_claimed(Claimed0, Options),
    redis(Redis, xpending(Stream, Group), [Count,_Oldest,_Newest, Cons]),
    Count > 0,
    !,
    debug(redis(claim), '~D pending messages on ~p for ~p (Consumers = ~p)',
          [Count, Stream, Group, Cons]),
    claim_for_stream_for_consumers(Cons, Redis, Stream, Group,
                                   Claimed0, Claimed, Options).
claim_for_stream(_Redis, _Stream, _Group, Claimed, Claimed, _Options).

claim_for_stream_for_consumers([], _Redis, _Stream, _Group,
                               Claimed, Claimed, _Options).
claim_for_stream_for_consumers([C|T], Redis, Stream, Group,
                               Claimed0, Claimed, Options) :-
    claim_for_stream_for_consumer(Redis, Stream, Group, C,
                                  Claimed0, Claimed1, Options),
    claim_for_stream_for_consumers(T, Redis, Stream, Group,
                                   Claimed1, Claimed, Options).

claim_for_stream_for_consumer(Redis, Stream, Group, [Consumer,_Pending],
                              Claimed0, Claimed, Options) :-
    redis(Redis, xpending(Stream, Group, -, +, 10, Consumer), Reply),
    claim_messages(Reply, Redis, Stream, Group, Claimed0, Claimed, Options).

claim_messages([], _Redis, _Stream, _Group, Claimed, Claimed, _Options).
claim_messages([H|T], Redis, Stream, Group, Claimed0, Claimed, Options) :-
    debug(redis(claim), 'Found pending ~p', [H]),
    claim_message(H, Redis, Stream, Group, Claimed0, Claimed1, Options),
    claim_messages(T, Redis, Stream, Group, Claimed1, Claimed, Options).

claim_message([Id,Consumer,Idle,Delivered], Redis, Stream, Group,
              Claimed0, Claimed, Options) :-
    option(block(Block), Options),
    BlockMS is integer(Block*1000),
    Idle > BlockMS,
    check_limit_deliveries(Redis, Stream, Delivered, Id, Options),
    check_limit_claimed(Claimed0, Options),
    option(group(Group-Me), Options),
    debug(redis(claim), '~p: trying to claim ~p from ~p (idle ~D)',
          [Me, Id, Consumer, Idle]),
    redis(Redis, xclaim(Stream, Group, Me, BlockMS, Id), ClaimedMsgs),
    !,
    Claimed is Claimed0 + 1,
    debug(redis(claimed), '~p: claimed ~p', [Me, ClaimedMsgs]),
    dispatch_claimed(ClaimedMsgs, Redis, Stream, Options).
claim_message(_Msg, _Redis, _Stream, _Group, Claimed, Claimed, _Options).

dispatch_claimed([], _Redis, _Stream, _Options).
dispatch_claimed([[MsgId,MsgArray]|Msgs], Redis, Stream, Options) :-
    redis_array_dict(MsgArray, _, Dict),
    xbroadcast_group(Redis, Stream, MsgId, Dict, Options),
    dispatch_claimed(Msgs, Redis, Stream, Options).


%!  check_limit_deliveries(+Redis, +Stream, +Delivered, +Id, +Options)
%
%   If a message gets delivered to several   nodes and none of the nodes
%   is able to process it, we should stop  trying to do so at some point
%   because the failure is  most  likely   due  to  persistent error and
%   piling up such messages will harm the cluster.

check_limit_deliveries(_Redis, _Stream, Delivered, _Id, Options) :-
    option(max_deliveries(Max), Options, 3),
    Delivered =< Max,
    !.
check_limit_deliveries(Redis, Stream, Delivered, Id, Options) :-
    option(group(Group-_Me), Options),
    (   xhook(Stream, delivery_failed(Id,Group,Delivered))
    ->  true
    ;   print_message(warning, redis(delivery_failed(Id,Group,Delivered))),
        redis(Redis, xack(Stream, Group, Id))
    ),
    fail.

check_limit_claimed(Claimed0, Options) :-
    option(max_claim(Max), Options, 10),
    Claimed0 < Max.


%!  xleave_group(+Redis, +Group, +Consumer, +Streams) is det.
%
%   Remove Consumer from Group.
%
%   @tbd ``XGROUP DELCONSUMER`` only takes a single stream.  Why?

xleave_group(Redis, Group, Consumer, [Stream|_]) :-
    redis(Redis, xgroup(delconsumer, Stream, Group, Consumer), _).

%!  xconsumer_stop(+Leave)
%
%   May be called from a consumer listener   to  stop the consumer. This
%   predicate throws the exception redis(stop(Leave)),   which is caught
%   by xlisten_group/5.

xconsumer_stop(Leave) :-
    throw(redis(stop(Leave))).


		 /*******************************
		 *             HOOKS		*
		 *******************************/

%!  xhook(+Stream, +Event)
%
%   This multifile predicate is called on certain stream events. Defined
%   events are:
%
%     - delivery_failed(Id,Group,Delivered)
%       A message was delivered more than specified by max_deliveries/1
%       of xlisten_group/5.  Id is the message id, Group the group and
%       Delivered the current delivery count.  If the hooks fails, the
%       message is acknowledged using ``XACK``.  From [introduction
%       to streams](https://redis.io/topics/streams-intro):
%
%       > "So once the deliveries counter reaches a given large number
%       > that you chose, it is probably wiser to put such messages in
%       > another stream and send a notification to the system
%       > administrator. This is basically the way that Redis streams
%       > implement the concept of the dead letter."


		 /*******************************
		 *            MESSAGES		*
		 *******************************/

:- multifile prolog:message//1.

prolog:message(redis(Message)) -->
    [ 'REDIS: '-[] ],
    redis_message(Message).

redis_message(delivery_failed(Id,Group,Delivered)) -->
    [ 'Failed to deliver ~p to group ~p (tried ~D times)'-
      [Id, Group, Delivered]
    ].
