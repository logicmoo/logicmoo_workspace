/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2018, University of Amsterdam
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

:- module(stream_pool,
          [ add_stream_to_pool/2,       % +Stream, :Goal
            delete_stream_from_pool/1,  % +Stream
            close_stream_pool/0,
            dispatch_stream_pool/1,     % +TimeOut
            stream_pool_main_loop/0
          ]).
:- autoload(library(debug),[debug/3]).

:- meta_predicate
    add_stream_to_pool(+, 0).

:- volatile
    pool/2.                         % sockets don't survive a saved-state
:- dynamic
    pool/2.                         % Stream, Action

/** <module> Input multiplexing

This libary allows a single thread to  monitor multiple streams and call
a goal if input is available on a stream.

@bug Note that if the processing   predicate blocks other input channals
are not processed. This may happen, for example, if a read/2 call blocks
due to incomplete input.
*/

%!   add_stream_to_pool(+Stream :Goal)
%
%    Call Goal whenever there is input on Stream.

add_stream_to_pool(Stream, Action) :-
    strip_module(Action, Module, Plain),
    register_stream(Stream, Module:Plain).

register_stream(Stream, Goal) :-
    assert(pool(Stream, Goal)).

%!  delete_stream_from_pool(+Stream)
%
%   Retract stream from the pool

delete_stream_from_pool(Stream) :-
    retractall(pool(Stream, _)).

%!  close_stream_pool
%
%   Close all streams in the   pool. This causes stream_pool_main_loop/0
%   to terminate.

close_stream_pool :-
    forall(retract(pool(Stream, _)),
           close(Stream, [force(true)])).

%!  dispatch_stream_pool(+TimeOut)
%
%   Wait for input on one or more streams   and handle that. Wait for at
%   most TimeOut seconds (0 means infinite).

dispatch_stream_pool(Timeout) :-
    findall(S, pool(S, _), Pool),
    debug(tcp, 'Select ~p ...', [Pool]),
    catch(wait_for_input(Pool, Ready, Timeout), E, true),
    debug(tcp, '    --> ~p (E=~p)', [Ready, E]),
    (   var(E)
    ->  actions(Ready)
    ;   E = error(existence_error(stream, Stream), _)
    ->  delete_stream_from_pool(Stream)
    ).

actions([]).
actions([H|T]) :-
    action(H),
    actions(T).

action(Stream) :-
    pool(Stream, Action),
    (   catch(Action, E, true)
    ->  (   var(E)
        ->  true
        ;   print_message(error, E)
        )
    ;   print_message(warning,
                      goal_failed(Action, stream_pool))
    ).

%!  stream_pool_main_loop
%
%   Keep handling input from the streams in the pool until they have all
%   died away.

stream_pool_main_loop :-
    pool(_, _),
    !,
    (   current_prolog_flag(windows, true)
    ->  dispatch_stream_pool(1)     % so we can break out easily
    ;   dispatch_stream_pool(0)
    ),
    stream_pool_main_loop.
stream_pool_main_loop.

