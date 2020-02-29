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

:- module(module_loops, [module_loops/2]).

:- use_module(library(option_utils)).

:- thread_local dependency/2.

skip_module(user).
skip_module(system).

%!  module_loops(-Loops, +Options) is det.
%
%   return the module loops, i.e., the module load chain that end up loading the
%   module itself.

module_loops(Loops, Options) :-
    option_files(Options, FileD),
    collect_dependencies(FileD),
    findall(Loop, module_loop(_, [], Loop), Loops).

collect_dependencies(FileD) :-
    retractall(dependency(_, _)),
    forall(( get_dict(File, FileD, _),
             '$load_context_module'(File, LoadedIn, _),
             \+ skip_module(LoadedIn),
             module_property(Module, file(File))
           ),
           assertz(dependency(Module, LoadedIn))).

module_loop(Module, Path1, Path) :-
    ( append(Left, [Module|_], Path1)
    ->append([Module|Left], [Module], Path),
      Path1 = [UM|_],
      retractall(dependency(UM, Module))
    ; dependency(Module, LoadedIn),
      module_loop(LoadedIn, [Module|Path1], Path)
    ).

/*
% Original, raw algorithm:
load_path(File, Path) :-
    '$load_context_module'(File, Module, _),
    module_property(Module, file(LoadedIn)),
    load_path(LoadedIn, [File], Path).

load_path(File, Path1, Path) :-
    ( append(Left, [File|_], Path1)
    ->reverse([File|Left], Rev),
      Path = [File|Rev]
    ; '$load_context_module'(File, Module, _),
      \+ skip_module(Module),
      module_property(Module, file(LoadedIn)),
      load_path(LoadedIn, [File|Path1], Path)
    ).
*/
