/*  Part of the SWI-Prolog HTTP package

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2014, University of Amsterdam,
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

:- module(http_cors,
          [ cors_enable/0,
            cors_enable/2                       % +Requesy, +Options
          ]).
:- use_module(library(settings)).

:- setting(http:cors, list(atom), [],
           'Enable CORS for the listed domains.  Use [*] for all domains').

/** <module> Enable CORS: Cross-Origin Resource Sharing

This small module allows  for   enabling  Cross-Origin  Resource Sharing
(CORS) for a specific  request.  Typically,   CORS  is  enabled  for API
services that you want to have useable  from browser client code that is
loaded from another domain. An example are   the LOD and SPARQL services
in ClioPatria.

Because CORS is a security risc  (see   references),  it  is disabled by
default. It is enabled through the setting  http:cors. The value of this
setting is a list of domains  that   are  allowed to access the service.
Because * is used as a wildcard match,  the value [*] allows access from
anywhere.

Services for which CORS is relevant must   call cors_enable/0 as part of
the HTTP response, as shown below. Note that cors_enable/0 is a no-op if
the setting http:cors is set to the empty list ([]).

  ==
  my_handler(Request) :-
        ....,
        cors_enable,
        reply_json(Response, []).
  ==

If a site uses a _Preflight_  =OPTIONS=   request  to  find the server's
capabilities and access politics, cors_enable/2 can be used to formulate
an appropriate reply.  For example:

  ==
  my_handler(Request) :-
        option(method(options), Request), !,
        cors_enable(Request,
                    [ methods([get,post,delete])
                    ]),
        format('~n').                           % 200 with empty body
  ==

@see    http://en.wikipedia.org/wiki/Cross-site_scripting for understanding
        Cross-site scripting.
@see    http://www.w3.org/TR/cors/ for understanding CORS
*/


%!  cors_enable is det.
%
%   Emit  the  HTTP  header   =|Access-Control-Allow-Origin|=  using
%   domains from the setting http:cors.  This   this  setting  is []
%   (default), nothing is written. This  predicate is typically used
%   for replying to API  HTTP-request  (e.g.,   replies  to  an AJAX
%   request that typically serve JSON or XML).

cors_enable :-
    cors_enable_domain,
    !.
cors_enable.                            % CORS not enabled

cors_enable_domain :-
    setting(http:cors, List),
    List \== [],
    !,
    format('Access-Control-Allow-Origin: ', []),
    write_domains(List),
    nl.

write_domains([]).
write_domains([H|T]) :-
    write(H),
    (   T == []
    ->  true
    ;   write(' '),
        write_domains(T)
    ).

%!  cors_enable(+Request, +Options) is det.
%
%   CORS reply to a _Preflight_ =OPTIONS=   request.  Request is the
%   HTTP request. Options provides:
%
%     - methods(+List)
%     List of supported HTTP methods.  The default is =GET=, only
%     allowing for read requests.
%     - headers(+List)
%     List of headers the client asks for and we allow.  The
%     default is to simply echo what has been requested for.
%
%   Both methods and headers may use   Prolog friendly syntax, e.g.,
%   =get= for a method and =content_type= for a header.
%
%   @see http://www.html5rocks.com/en/tutorials/cors/

cors_enable(Request, Options) :-
    cors_enable_domain,
    !,
    option(methods(Methods), Options, [get]),
    cors_methods(Methods),
    (   option(headers(ReqHeaders), Options)
    ->  cors_request_headers(ReqHeaders)
    ;   option(access_control_request_headers(ReqHeader), Request)
    ->  format('Access-Control-Allow-Headers: ~w~n', [ReqHeader])
    ;   true
    ).
cors_enable(_, _).

cors_methods([]) :- !.
cors_methods(Methods) :-
    format('Access-Control-Allow-Methods: '),
    write_methods(Methods),
    nl.

write_methods([H|T]) :-
    upcase_atom(H, U),
    write(U),
    (   T == []
    ->  true
    ;   write(', '),
        write_methods(T)
    ).

cors_request_headers([]) :- !.
cors_request_headers(ReqHeaders) :-
    phrase(field_names(ReqHeaders), String),
    format('Access-Control-Allow-Headers: ~s', String).


field_names([H|T]) -->
    http_header:field_name(H),
    (   {T==[]}
    ->  ""
    ;   ", ",
        field_names(T)
    ).
