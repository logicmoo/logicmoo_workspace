/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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

:- module(check_non_loaded, []).

:- use_module(library(checkers/checker)).
:- use_module(library(apply)).
:- use_module(library(option_utils)).

:- multifile
    prolog:message//1.

checker:check(non_loaded, Results, Options1) :-
    checker_check_non_loaded(Results, Options1).

checker_check_non_loaded(Results, Options1) :-
    ( ( option(dir(_), Options1)
      ; option(dirs(_), Options1)
      )
    ->Options = Options1
    ; working_directory(Dir, Dir),
      Options = [dir(Dir)|Options]
    ),
    option_filechk(Options, _, MFileChk),
    check_non_loaded(MFileChk, Results).

check_non_loaded(MFileChk, Pairs) :-
    findall(Dir-Name,
            ( call(MFileChk, _, File),
              directory_file_path(Dir, Name, File)
            ), DirNameU),
    sort(DirNameU, DirName),
    group_pairs_by_key(DirName, DirNameG),
    findall(warning-(load_info(Dir, L, N)-Excluded),
            ( member(Dir-NameL, DirNameG),
              findall(Name, ( member(Name, NameL),
                              directory_file_path(Dir, Name, File),
                              \+ source_file_property(File, _)
                            ), ExcludedL),
              ExcludedL \= [],
              length(NameL, N),
              length(ExcludedL, ExN),
              L is N - ExN,
              member(Excluded, ExcludedL)
            ), Pairs).

prolog:message(acheck(non_loaded)) -->
    ['Non Loaded',nl,
     '----------',nl,
     'The following files are not being loaded, which', nl,
     'means you are not analyzing them statically', nl, nl].
prolog:message(acheck(non_loaded, load_info(Dir, L, N)-ExcludedL)) -->
    ['At directory ~w, loaded ~w/~w:'-[Dir, L, N],nl],
    foldl(non_loaded(Dir), ExcludedL).

non_loaded(Dir, Excluded) -->
    ['\t~w/~w:1: non loaded'-[Dir, Excluded], nl].
