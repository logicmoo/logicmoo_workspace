/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2014, University of Amsterdam, VU University Amsterdam
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

:- module(http_sgml_plugin, []).
:- use_module(http_client, []).                 % library we extend
:- use_module(library(sgml)).
:- use_module(library(debug)).

:- multifile
    http_client:http_convert_data/4.

:- multifile
    markup_type/2.                  % +MimeType, -ParseOptions

/** <module> Parse of HTML and XML documents for the HTTP client libs

This module provides a plugin for the   HTTP  client to handle xml, html
and sgml files using  the   SWI-Prolog  sgml-parser  from library(sgml).
Using this library avoids unnecessary copying of data as the sgml-parser
reads directly from the stream that established the HTTP connection.

This is a plugin for http_get/3 and http_post/4

@see    http_open/3 is now the preferred interface for client side
        processing of HTTP.
*/

http_client:http_convert_data(In, Fields, Data, Options) :-
    memberchk(content_type(Type), Fields),
    debug(sgml_plugin, 'Content type: ~w', [Type]),
    (   markup_type(Type, ParseOptions)
    ->  true
    ;   type_major_props(Type, Major, Props),
        default_markup_type(Major, ParseOptions0),
        type_props(Props, ParseOptions0, ParseOptions)
    ),
    merge_options([ max_errors(-1),
                    syntax_errors(quiet)
                  | ParseOptions
                  ], Options, Merged),
    markup_options(Fields, Merged, MarkupOptions),
    debug(sgml_plugin, 'Markup options: ~p', [MarkupOptions]),
    load_structure(stream(In), Data, MarkupOptions).


type_major_props(Type0, Type, Props) :-
    sub_atom(Type0, B, _, A, ;),
    !,
    sub_atom(Type0, 0, B, _, Major),
    sub_atom(Type0, _, A, 0, Props),
    normalize_space(atom(Type), Major).
type_major_props(Type, Type, '').

type_props('', L, L).
type_props(Props, L0, L) :-
    sub_atom(Props, _, _, A, 'charset='),
    sub_atom(Props, _, A, 0, CharSet0),
    downcase_atom(CharSet0, CharSet),
    known_charset(CharSet),
    L = [encoding(CharSet)|L0].
type_props(_, L, L).

known_charset('iso-8859-1').
known_charset('us-ascii').
known_charset('utf-8').


%!  default_markup_type(+MimeType, -ParseOptions)
%
%   State that the HTTP contents should be parsed with
%   load_structure/3 using the returned options. This predicate may
%   be hooked using the multifile predicate markup_type/2.

default_markup_type('text/xml',
            [ dialect(xmlns)
            ]).
default_markup_type('text/html',
            [ dtd(DTD),
              dialect(sgml),
              shorttag(false)
            ]) :-
    dtd(html, DTD).
default_markup_type('text/x-sgml',
            [ dialect(sgml)
            ]).

markup_options(Fields, Opt0, Options) :-
    (   memberchk(content_length(Bytes), Fields)
    ->  Options = [content_length(Bytes)|Opt0]
    ;   Options = Opt0
    ).

%!  merge_options(+Defaults, +GivenOptions, -Options)
%
%   If an option is not in GivenOptions, use the one from
%   Defaults.

merge_options([], Options, Options).
merge_options([H|T], Options0, Options) :-
    functor(H, Name, Arity),
    functor(H0, Name, Arity),
    memberchk(H0, Options0),
    !,
    merge_options(T, Options0, Options).
merge_options([H|T], Options0, Options) :-
    merge_options(T, [H|Options0], Options).
