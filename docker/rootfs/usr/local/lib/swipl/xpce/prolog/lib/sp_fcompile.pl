/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

:- module(sp_fcompile,
          [ pce_fcompile/1
          , pce_fcompile_boot_files/0
          , pce_fcompile_directory/1
          , pce_frecompile_directory/1
          , pce_fcompile_libraries/0
          , pce_frecompile_libraries/0
          ]).
:- use_module(library(pce)).
:- require([ chain_list/2
           , forall/2
           , member/2
           , absolute_file_name/3
           ]).

:- prolog_flag(character_escapes, _, off).

                 /*******************************
                 *        TERM_EXPANSION        *
                 *******************************/

:- multifile
    user:term_expansion/2.
:- dynamic
    user:term_expansion/2.

user:term_expansion((:- List), (:- ensure_loaded(List))) :-
    List = [_|_].
user:term_expansion((:- consult(Files)), (:- ensure_loaded(Files))).
user:term_expansion((:- push_compile_operators), _) :-
    pce_expansion:push_compile_operators,
    fail.
user:term_expansion((:- pop_compile_operators), _) :-
    pce_expansion:pop_compile_operators,
    fail.

                 /*******************************
                 *      COMPILE STATEMENTS      *
                 *******************************/


pce_fcompile(File) :-
    use_module(user:File, []),
    fcompile(user:File).


dirpath(Dir, DirPath) :-
    absolute_file_name(Dir,
                       [ file_type(directory),
                         access(read)
                       ],
                       DirPath).


pce_fcompile(Dir, File) :-
    dirpath(Dir, DirPath),
    get(string('%s/%s', DirPath, File), value, Path),
    pce_fcompile(Path).


pce_frecompile(Dir, File) :-
    dirpath(Dir, DirPath),
    get(string('%s/%s', DirPath, File), value, PL),
    get(PL, delete_suffix, '.pl', Base),
    get(Base, ensure_suffix, '.ql', QL),
    (   (   \+ send(file(QL), exists)
            |   send(file(PL)?time, after, file(QL)?time)
        )
    ->  pce_fcompile(PL)
    ;   true
    ).


pce_fcompile_directory(Dir) :-
    dirpath(Dir, DirPath),
    get(directory(DirPath), files, '^.*\\.pl$', Chain),
    send(Chain, delete_all, 'INDEX.pl'),
    chain_list(Chain, Files),
    forall(member(File, Files),
           pce_fcompile(DirPath, File)).


pce_frecompile_directory(Dir) :-
    dirpath(Dir, DirPath),
    get(directory(DirPath), files, '^.*\\.pl$', Chain),
    send(Chain, delete_all, 'INDEX.pl'),
    chain_list(Chain, Files),
    forall(member(File, Files),
           pce_frecompile(DirPath, File)).


pce_fcompile_libraries :-
    forall(pce_prolog_directory(LibDir),
           pce_fcompile_directory(LibDir)).

pce_prolog_directory(pce(library)).
pce_prolog_directory(pce('library/draw')) :-
    ensure_loaded(user:library(pcedraw)).
pce_prolog_directory(pce('library/man')) :-
    ensure_loaded(user:library(pce_manual)).
pce_prolog_directory(pce('library/emacs')) :-
    ensure_loaded(user:library(pce_emacs)),
    user:start_emacs.
pce_prolog_directory(pce('library/dialog')) :-
    ensure_loaded(user:library(edit_dialog)).
pce_prolog_directory(pce('demo')).
pce_prolog_directory(pce('contrib')).

pce_frecompile_libraries :-
    forall(pce_prolog_directory(LibDir),
           pce_frecompile_directory(LibDir)).

boot_file(pce_boot(pce_expand)).
boot_file(pce_boot(pce_sp)).
boot_file(pce_boot(pce_principal)).
boot_file(pce_boot(pce_error)).
boot_file(pce_boot(pce_operator)).
boot_file(pce_boot(pce_global)).
boot_file(pce_boot(pce_expansion)).
boot_file(pce_boot(pce_realise)).
boot_file(pce_boot(pce_autoload)).
boot_file(pce_boot(pce_editor)).
boot_file(library(sp_compatibility)).
boot_file(library(pce)).

pce_fcompile_boot_files :-
    forall(boot_file(BootFile),
           fcompile(user:BootFile)).
