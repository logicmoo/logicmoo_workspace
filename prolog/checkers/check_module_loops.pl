/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2020, Process Design Center, Breda, The Netherlands.
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

:- module(check_module_loops, []).

:- use_module(library(checkers/checker)).
:- use_module(library(module_loops)).
:- use_module(library(extra_location)).
:- use_module(library(location_utils)).

:- multifile
        prolog:message//1.

prolog:message(acheck(module_loops)) -->
    ['Module loops', nl,
     '------------', nl,
     'Module loops could potentially lead to Demeter\'s law violations', nl, nl].

prolog:message(acheck(module_loops, Loc/Loop)) -->
    Loc,
    ['Module loop found ~w'-[Loop], nl].

checker:check(module_loops, Pairs, Options) :-
    module_loops(Loops, Options),
    maplist(loops_pairs, Loops, Pairs).

loops_pairs(Loop, warning-Loc/Loop) :-
    Loop = [LoadedIn, Module|_],
    ( module_property(Module, file(File)),
      ( loc_declaration(           Alias,     LoadedIn, use_module,   From)
      ; loc_declaration(use_module(Alias, _), LoadedIn, use_module_2, From)
      ),
      absolute_file_name(Alias, AF, [file_errors(fail), file_type(prolog)]),
      File == AF
    ->from_location(From, Loc)
    ; Loc = []
    ).
