/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2017, University of Amsterdam
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

:- module(demo_body,
          [ reply/1
          ]).
:- use_module(library(http/http_client)).

:- if(exists_source(library(pce))).
:- use_module(library(pce)).
:- use_module(library(http/http_image)).        % make XPCE generate images
:- endif.

:- discontiguous
    reply/1.

/** <module> Demo implementation of some HTTP handlers

This module implements some HTTP handlers  using rather low-level simple
primitives. It illustrates the process that HTTP handlers are similar to
CGI handlers in the sense that they need  to write a CGI document to the
`current_output` stream.  For high level primitives:

  - More advanced dispatching of requests over predicates that implement
    them is provided by http_handler/3 from library(http/http_dispatch).
  - High level HTML is generated with library(http/html_write).

@see    http://www.swi-prolog.org/howto/http/ provides a much better
        tutorial introduction to the SWI-Prolog HTTP services.
*/


reply(_) :-
    flag(request, N, N+1),
    fail.

%       /
%
%       Show available locations

reply(Request) :-
    memberchk(path('/'), Request),
    !,
    format('Content-type: text/html~n~n', []),
    format('<html>~n', []),
    format('<h1>Some simple demo queries</h1>', []),
    format('<ul>~n', []),
    format('  <li><a href="quit">Say bye bye</a>'),
    format('  <li><a href="env">Print environment</a>'),
    format('  <li><a href="upload">Upload some data</a>'),
    format('  <li><a href="xml">Reply with an XML document</a>'),
    format('  <li><a href="foreign">Reply Chinese characters</a>'),
    format('  <li><a href="work">Work hard for a while</a>'),
    format('  <li><a href="error">Show what happens on an error</a>'),
    format('  <li><a href="xpce?class=box">Return image (requires xpce)</a>'),
    format('  <li><a href="otherwise">Otherwise, print request</a>'),
    format('</ul>~n', []),
    format('</html>~n', []).

%       /quit
%
%       Explicitely close the connection

reply(Request) :-
    member(path('/quit'), Request),
    !,
    format('Connection: close~n', []),
    format('Content-type: text/html~n~n', []),
    format('Bye Bye~n').

%       /env
%
%       Reply with the output of printenv (Unix systems only).

reply(Request) :-
    member(path('/env'), Request),
    !,
    expand_file_name(~, Home),
    format('Content-type: text/html~n~n', []),
    format('<html>~n', []),
    flag(request, RN, RN),
    format('Request ~d~n', [RN]),
    format('<pre>~n', []),
    format('HOME = ~w~n~n', [Home]),
    open(pipe(printenv), read, Fd),
    copy_stream_data(Fd, current_output),
    close(Fd),
    format('</pre>~n', []),
    format('</html>~n', []).

%       /upload
%       /upload_reply
%
%       Provide a form for uploading a file, and deal with the resulting
%       upload.  Contributed by Nicos Angelopoulos.

reply(Request) :-
    member(path('/upload'), Request),
    !,
    format('Content-type: text/html~n~n', []),
    format('<html>~n', []),
    format('<form action="/upload_reply" enctype="multipart/form-data" method="post">~n', []),
    format('<input type="file" name="datafile">'),
    format('<input type="submit" name="sent">'),
    format('</body>~n', []),
    format('</html>~n', []).

reply(Request) :-
    member(path('/upload_reply'), Request),
    !,
    format('Content-type: text/html~n~n', []),
    format('<html>~n', []),
    format('<pre>~n', []),
    write( req(Request) ), nl,
    http_read_data(Request, Data, []),
    write( data(Data) ), nl,
    format('</pre>'),
    format('</body>~n', []),
    format('</html>~n', []).

%       /xml
%
%       Return a simple formatted XML message.

reply(Request) :-
    member(path('/xml'), Request),
    !,
    format('Content-type: text/xml~n~n', []),
    format('\c
<message>
  <head>
  <from>Jan Wielemaker</from>
  <to>Prolog users</to>
  <subject>The SWI-Prolog web-server</subject>
  </head>
  <body>
<p>
This is the first demo of the web-server serving an XML message
</p>
  </body>
</message>
', []).

%       /foreign
%
%       Test emitting text using UTF-8 encoding

reply(Request) :-
    member(path('/foreign'), Request),
    !,
    format('Content-type: text/html~n~n', []),
    format('\c
<html>
<head><title>Foreign characters</title></head>
<body>
<p>Chinese for book is \u5b66\u4e60
</body>
</html>
').


%       /work
%
%       Do a lot of work and then say 'ok'. Can be used to test
%       concurrent access using the multi-threaded server.

reply(Request) :-
    member(path('/work'), Request),
    format(user_error, 'Starting work ...', []),
    forall(between(1, 10000000, _), atom_codes(_, "hello")),
    format(user_error, 'done!~n', []),
    format('Content-type: text/plain~n~n', []),
    format('ok~n').

%       /error
%
%       Produce an error.  Load http_error to see the effect.

reply(Request) :-
    member(path('/error'), Request),
    A is 1/0,
    format('Content-type: text/plain~n~n', []),
    format('A = ~w~n', [A]).

%       /xpce?class=box
%
%       Make XPCE reply with a graphics image. The demo-body pce_reply/1
%       is called embedded in a  message  to   XPCE  to  force  the XPCE
%       incremental garbage collector to reclaim   objects created while
%       serving the request. pce_reply/1 replies   to ?class=box using a
%       blue box with rounded corners.

:- if(current_predicate(send/3)).
reply(Request) :-
    member(path('/xpce'), Request),
    !,
    send(@(prolog), call, demo_body:pce_reply(Request)).

pce_reply(Request) :-
    memberchk(search(Search), Request),
    memberchk(class=box, Search),
    new(Box, box(200,200)),
    send(Box, radius, 20),
    send(Box, fill_pattern, colour(skyblue)),
    reply_image(Box, []).

:- endif.

%       ... Otherwise
%
%       Print the request itself.

reply(Request) :-
    format('Content-type: text/html~n~n', []),
    format('<html>~n', []),
    format('<table border=1>~n'),
    print_request(Request),
    format('~n</table>~n'),
    format('</html>~n', []).

print_request([]).
print_request([H|T]) :-
    H =.. [Name, Value],
    format('<tr><td>~w<td>~w~n', [Name, Value]),
    print_request(T).



