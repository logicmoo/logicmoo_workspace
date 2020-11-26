/*  Part of Assertion Reader for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(foreign_interface, []).

:- use_module(library(assertions)).
:- use_module(library(foreign/foreign_generator)).
:- use_module(library(change_alias)).
:- reexport(library(compound_expand)).

term_expansion((:- gen_foreign_library(AliasSO)),
               foreign_generator:gen_foreign_library(M, AliasSO, [])) :-
    '$current_source_module'(M).
term_expansion((:- gen_foreign_library(AliasSO, Init)),
               foreign_generator:gen_foreign_library(M, AliasSO, InitL)) :-
    ( is_list(Init)
    ->InitL = Init
    ; InitL = [Init]
    ),
    '$current_source_module'(M).
term_expansion((:- pkg_foreign_config(Package)),
               foreign_generator:pkg_foreign_config(M, Package)) :-
    '$current_source_module'(M).
term_expansion((:- use_foreign_source(FAlias)),
               foreign_generator:use_foreign_source(M, FAlias)) :-
    '$current_source_module'(M).
term_expansion((:- use_foreign_header(FAlias)),
               foreign_generator:use_foreign_header(M, FAlias)) :-
    '$current_source_module'(M).
term_expansion((:- foreign_dependency(FAlias)),
               foreign_generator:foreign_dependency(M, FAlias)) :-
    '$current_source_module'(M).
term_expansion((:- include_foreign_dir(DAlias)),
               foreign_generator:include_foreign_dir(M, DAlias)) :-
    '$current_source_module'(M).
term_expansion((:- library_foreign_dir(DAlias)),
               foreign_generator:library_foreign_dir(M, DAlias)) :-
    '$current_source_module'(M).
term_expansion((:- extra_compiler_opts(COpt)),
               foreign_generator:extra_compiler_opts(M, COpt)) :-
    '$current_source_module'(M).
term_expansion((:- link_foreign_library(Lib)),
               foreign_generator:link_foreign_library(M, Lib)) :-
    '$current_source_module'(M).

term_expansion(end_of_file, Decl) :-
    '$current_source_module'(M),
    module_property(M, file(File)),
    prolog_load_context(file, File), !,
    gen_foreign_library(M, AliasSO, InitL),
    change_alias(add_suffix('_so'), AliasSO, AliasSOPl),
    generate_library(M, AliasSO, AliasSOPl, InitL, File),
    Decl = [(:- [AliasSOPl]), end_of_file].

add_suffix(Suffix, Name1, Name) :-
    file_name_extension(Name2, _, Name1),
    atom_concat(Name2, Suffix, Name).
