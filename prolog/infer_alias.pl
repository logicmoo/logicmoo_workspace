/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.
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

:- module(infer_alias,
          [infer_alias/3,
           fastest_alias/2,
           library_alias/2,
           smallest_alias/2,
           current_alias/2,
           pretty_path/2
          ]).

:- use_module(library(term_size)).

infer_alias(File, CAlias, Options) :-
    select_option(sort(SortL), Options, _, []),
    findall(SortTerm-Alias,
            ( current_alias(File, Alias),
              Alias =.. [AName, _],
              maplist(sort_field(Alias, AName), SortL, SortTerm)
            ), SA),
    sort(SA, [_-CAlias|_]).

sort_field(_, A, alias(L), N) :-
    ( nth0(N, L, A)
    ->true
    ; length(L, N)
    ).
sort_field(_, A,       sols, N) :-
    findall(A, user:file_search_path(A, _), L),
    length(L, N).
sort_field(Alias, _, size, S) :-
    term_size(Alias, S).
sort_field(Alias, _, length, N) :-
    term_to_atom(Alias, Atom),
    atom_length(Atom, N).

fastest_alias(File, Alias) :-
    infer_alias(File, Alias, [sort([sols, size, length])]).

library_alias(File, Alias) :-
    infer_alias(File, Alias, [sort([alias([library]), sols, size, length])]).

smallest_alias(File, Alias) :-
    infer_alias(File, Alias, [sort([length, size, sols])]).

current_alias(File, Alias) :-
    user:file_search_path(A0, ADir),
    A0 \= autoload,
    absolute_file_name(ADir, Dir, [file_type(directory), solutions(all)]),
    directory_file_path(Dir, Base, File),
    file_name_extension(Name, _Ext, Base),
    pretty_path(Name, Path),
    Alias =.. [A0, Path].

pretty_path(Name0, Path/F) :-
    directory_file_path(Dir, F, Name0),
    Dir \= '.',
    !,
    pretty_path(Dir, Path).
pretty_path(Name, Name).
