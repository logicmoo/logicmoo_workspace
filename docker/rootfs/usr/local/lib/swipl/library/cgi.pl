/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2020, University of Amsterdam
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

:- module(cgi,
          [ cgi_get_form/1              % -ListOf Name(Value)
          ]).

:- use_foreign_library(foreign(cgi), install_cgi).

/** <module> Read CGI parameters

Below is a very simple CGI script  that prints the passed parameters. To
test it, compile this program using the   command below, copy it to your
cgi-bin directory (or make it otherwise known  as a CGI-script) and try
the query =|http://myhost.mydomain/cgi-bin/cgidemo?hello=world|=

    ==
    % swipl -o cgidemo --goal=main --toplevel=halt -c cgidemo.pl
    ==

    ==
    :- use_module(library(cgi)).

    main :-
        set_stream(current_output, encoding(utf8)),
        cgi_get_form(Arguments),
        format('Content-type: text/html; charset=UTF-8~n~n', []),
        format('<html>~n', []),
        format('<head>~n', []),
        format('<title>Simple SWI-Prolog CGI script</title>~n', []),
        format('</head>~n~n', []),
        format('<body>~n', []),
        format('<p>', []),
        print_args(Arguments),
        format('</body>~n</html>~n', []).

    print_args([]).
    print_args([A0|T]) :-
        A0 =.. [Name, Value],
        format('<b>~w</b>=<em>~w</em><br>~n', [Name, Value]),
        print_args(T).
    ==
*/

%!  cgi_get_form(-Form)
%
%   Decodes standard input and the environment variables to obtain a
%   list of arguments passed to the  CGI script. This predicate both
%   deals with the CGI *GET* method as well as the *POST* method. If
%   the data cannot be  obtained,   an  existence_error exception is
%   raised.
%
%   @param Form is a list of Name(Value) terms.
