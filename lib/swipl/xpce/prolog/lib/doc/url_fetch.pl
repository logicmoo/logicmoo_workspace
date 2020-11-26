/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2000-2013, University of Amsterdam
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

:- module(url_fetch,
          [ get_url_to_file/2,          % +URL, -File
            get_url_to_file/3           % +Base, +Url, -File
          ]).
:- use_module(library(url)).
:- use_module(library(http_client)).
:- use_module(util).

get_url_to_file(URL, File) :-
    parse_url(URL, Parts),
    get_data(Parts, File),
    !.
get_url_to_file(URL, _) :-
    print_message(warning, url_load_failed(URL)),
    fail.

get_url_to_file(Base, URL, File) :-
    parse_url(Base, URL, Parts),
    get_data(Parts, File),
    !.
get_url_to_file(Base, URL, _) :-
    print_message(warning, url_load_failed(Base, URL)),
    fail.


                 /*******************************
                 *            FETCH DATA        *
                 *******************************/

get_data(Parts, File) :-                        % file
    memberchk(protocol(file), Parts),
    !,
    memberchk(path(File), Parts).
get_data(Parts, File) :-                        % HTTP
    memberchk(protocol(http), Parts),
    !,
    cache_file(Parts, File),
    new(F, file(File)),
    send(F, kind, binary),
    send(F, open, write),
    new(Client, http_client(Parts)),
    send(Client, fetch_data, F),
    send(F, close),
    free(Client).


                 /*******************************
                 *          PAGE CACHE          *
                 *******************************/

cache_dir(Dir) :-
    current_prolog_flag(unix, true),
    !,
    expand_file_name('~/.http_cache', [Dir]).
cache_dir(cache).

cache_file(ParsedURL, File) :-
    cache_dir(Dir),
    option(protocol(Protocol), ParsedURL),
    option(host(Host), ParsedURL),
    option(port(Port), ParsedURL, 80),
    option(path(Path0), ParsedURL),
    (   sub_atom(Path0, _, _, 0, /)
    ->  atom_concat(Path0, '.index', Path)
    ;   Path = Path0
    ),
    format(atom(File), '~w/~w/~w:~w~w', [Dir, Protocol, Host, Port, Path]),
    ensure_dir_for_file(File),
    debug(cache, 'Cache file is \'~w\'~n', File).

ensure_dir_for_file(File) :-
    file_directory_name(File, Dir),
    ensure_dir(Dir).

ensure_dir(Dir) :-
    exists_directory(Dir),
    !.
ensure_dir(Dir) :-
    file_directory_name(Dir, Parent),
    ensure_dir(Parent),
    send(directory(Dir), make).     % should be Prolog

:- multifile
    prolog:message/3.

prolog:message(url_load_failed(Base, URL)) -->
    [ 'Failed to get data from ~p (base=~p)'-[URL,Base] ].
prolog:message(url_load_failed(URL)) -->
    [ 'Failed to get data from ~p'-[URL] ].
