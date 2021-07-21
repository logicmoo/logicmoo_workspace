/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2016, University of Amsterdam
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

:- module(http_error,
          [
          ]).
:- use_module(library(prolog_stack)).
:- use_module(library(settings)).
:- use_module(library(broadcast)).
:- use_module(library(debug)).

/** <module> Decorate uncaught HTTP exceptions with stack-trace

This module decorates uncaught exceptions of the   user code with a full
stack-trace and sends error reports to the Prolog console. The behaviour
can be controlled by

  - nodebug(http(error))
    After disabling the http(error) debug channal, errors are only sent
    to the client.  See nodebug/1 and debug/1.
  - set_setting(http:client_backtrace, false)
    Stop sending stack traces to the client. Note that sending the stack
    trace to the client simplifies debugging, it also provides clues to
    hackers on how to compromise your site. The more information you
    give them, the easier it is to break into your server!  See
    set_setting/2 and set_setting_default/2.
*/

:- setting(http:client_backtrace, boolean, true,
           'Make backtrace visible to the client').


                 /*******************************
                 *     LOG ERRORS TO STDERR     *
                 *******************************/

:- debug(http(error)).

:- listen(http(Message),
          http_listen(Message)).

:- dynamic
    saved_request/2.

http_listen(_) :-
    \+ debugging(http(error)),
    !.
http_listen(request_start(Id, Request)) :-
    !,
    asserta(saved_request(Id, Request)).
http_listen(request_finished(Id, Code, Status, _CPU, _Bytes)) :-
    retract(saved_request(Id, Request)),
    !,
    Code >= 400,
    memberchk(path(Path), Request),
    memberchk(method(Method), Request),
    upcase_atom(Method, UMethod),
    reply_status(Status, Reply),
    debug(http(error),
          '~w ~w: [~w] ~w', [UMethod, Path, Code, Reply]).

reply_status(Status, Reply) :-
    map_exception(Status, Reply),
    !.
reply_status(Status, Message) :-
    message_to_string(Status, Message).

map_exception(http_reply(bytes(ContentType,Bytes),_), bytes(ContentType,L)) :-
    string_length(Bytes, L).        % also does lists
map_exception(http_reply(Reply), Reply).
map_exception(http_reply(Reply, _), Reply).
map_exception(error(existence_error(http_location, Location), _Stack),
              error(404, Location)).


                 /*******************************
                 *     DECORATE STACK TRACES    *
                 *******************************/

:- dynamic prolog_stack:stack_guard/1.
:- multifile prolog_stack:stack_guard/1.

prolog_stack:stack_guard(httpd_wrapper:wrapper/5).
prolog_stack:stack_guard(httpd_wrapper:handler_with_output_to/5).

