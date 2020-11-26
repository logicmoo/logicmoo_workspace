/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2017, University of Amsterdam
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

:- module(http_exception,
          [ map_exception_to_http_status/4,     % +Exception, -Reply,
                                                % -HdrExtra, -Context
            in_or_exclude_backtrace/2           % +Error, -CleanedError
          ]).

/** <module> Map Prolog exceptions to HTTP errors

This module maps exceptions from various parts  of the HTTP libraries as
well as exceptions from user  handler   predicates  into meaningful HTTP
error codes such as 4XX and 5XX  codes. For example, existence errors on
http locations are mapped to 404 while out-of-stack is mapped to 503.

This library provides two hooks:

  - http:map_exception_to_http_status_hook/4 can be used to map
    additional exceptions to any HTTP status code.
  - http:bad_request_error/2 can be extended to map exceptions into 400
    bad request responses.

@see    http_header.pl, http_wrapper.pl
*/

:- multifile
    http:bad_request_error/2,                 % Formal, Context
    http:map_exception_to_http_status_hook/4. % Exception, Reply, HdrExtra, Ctx

%!  map_exception_to_http_status(+Exception, -Reply, -HdrExtra, -Context)
%
%   Map certain exceptions to HTTP  status codes. The http(not_modified)
%   provides backward compatibility to http_reply(not_modified).

map_exception_to_http_status(Exception, Reply, HdrExtra, Context) :-
    http:map_exception_to_http_status_hook(Exception, Reply, HdrExtra, Context),
    !.
map_exception_to_http_status(http(not_modified),
              not_modified,
              [connection('Keep-Alive')],
              []) :- !.
map_exception_to_http_status(http_reply(Reply),
              Reply,
              [connection(Close)],
              []) :-
    !,
    keep_alive(Reply, Close).
map_exception_to_http_status(http_reply(Reply, HdrExtra0),
              Reply,
              HdrExtra,
              Context) :-
    !,
    map_exception_to_http_status(http_reply(Reply, HdrExtra0, []),
                                 Reply,
                                 HdrExtra,
                                 Context).

map_exception_to_http_status(http_reply(Reply, HdrExtra0, Context),
              Reply,
              HdrExtra,
              Context):-
    !,
    (   memberchk(connection(_), HdrExtra0)
    ->  HdrExtra = HdrExtra0
    ;   HdrExtra = [connection(Close)|HdrExtra0],
        keep_alive(Reply, Close)
    ).
map_exception_to_http_status(error(existence_error(http_location, Location), _),
              not_found(Location),
              [connection(close)],
              []) :- !.
map_exception_to_http_status(error(permission_error(http_method, Method, Location), _),
              method_not_allowed(Method, Location),
              [connection(close)],
              []) :- !.
map_exception_to_http_status(error(permission_error(_, http_location, Location), _),
              forbidden(Location),
              [connection(close)],
              []) :- !.
map_exception_to_http_status(error(threads_in_pool(_Pool), _),
              busy,
              [connection(close)],
              []) :- !.
map_exception_to_http_status(E,
              resource_error(E),
              [connection(close)],
              []) :-
    is_resource_error(E),
    !.
map_exception_to_http_status(E,
              bad_request(E2),
              [connection(close)],
              []) :-
    bad_request_exception(E),
    !,
    discard_stack_trace(E, E2).
map_exception_to_http_status(E,
              server_error(E),
              [connection(close)],
              []).

is_resource_error(error(resource_error(_), _)).

bad_request_exception(error(Error, Context)) :-
    nonvar(Error),
    bad_request_error(Error, ContextGeneral),
    (   var(ContextGeneral)
    ->  true
    ;   Context = context(_Stack, ContextInstance)
    ->  subsumes_term(ContextGeneral, ContextInstance)
    ),
    !.

bad_request_error(Error, Context) :-
    http:bad_request_error(Error, Context).
bad_request_error(Error, Context) :-
    default_bad_request_error(Error, Context).

default_bad_request_error(domain_error(http_request, _), _).
default_bad_request_error(existence_error(http_parameter, _), _).
default_bad_request_error(type_error(_, _), http_parameter(_)).
default_bad_request_error(syntax_error(http_request_line(_)), _).
default_bad_request_error(syntax_error(http_request(_)), _).
default_bad_request_error(syntax_error(_), in_http_request).

discard_stack_trace(error(Formal, context(_,Msg)),
                    error(Formal, context(_,Msg))).

%!  in_or_exclude_backtrace(+ErrorIn, -ErrorOut)
%
%   Remove  the  stacktrace  from  the   exception,  unless  setting
%   `http:client_backtrace` is `true`.

in_or_exclude_backtrace(Error, Error) :-
    current_setting(http:client_backtrace),
    setting(http:client_backtrace, true),
    !.
in_or_exclude_backtrace(Error0, Error) :-
    discard_stack_trace(Error0, Error),
    !.
in_or_exclude_backtrace(Exception, Exception).


%!  http:bad_request_error(+Formal, -ContextTemplate) is semidet.
%
%   If  an  exception  of  the   term  error(Formal,  context(Stack,
%   Context)) is caught and  subsumes_term(ContextTemplate, Context)
%   is true, translate the exception into  an HTTP 400 exception. If
%   the exception contains a stack-trace, this  is stripped from the
%   response.
%
%   The idea behind this hook  is   that  applications can raise 400
%   responses by
%
%     - Throwing a specific (error) exception and adding a rule
%       to this predicate to interpret this as 400.
%     - Define rules for prolog:error_message//1 to formulate
%       an appropriate message.


%!  keep_alive(+Reply) is semidet.
%!  keep_alive(+Reply, -Connection) is det.
%
%   If true for Reply, the default is to keep the connection open.

keep_alive(Reply, Connection) :-
    (   keep_alive(Reply)
    ->  Connection = 'Keep-Alive'
    ;   Connection = close
    ).

keep_alive(not_modified).
keep_alive(bytes(_Type, _Bytes)).
keep_alive(file(_Type, _File)).
keep_alive(tmp_file(_Type, _File)).
keep_alive(stream(_In, _Len)).
keep_alive(cgi_stream(_In, _Len)).
keep_alive(switching_protocols(_Goal, _)).


                 /*******************************
                 *          IDE SUPPORT         *
                 *******************************/

% See library('trace/exceptions')

:- multifile
    prolog:general_exception/2.

prolog:general_exception(http_reply(_), http_reply(_)).
prolog:general_exception(http_reply(_,_), http_reply(_,_)).
