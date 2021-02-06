/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2021, Process Design Center, Breda, The Netherlands.
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

:- module(module_trees,
          [ modules_trees/2,
            modrevs_trees/2
          ]).

:- use_module(library(dependency_trees)).

modules_trees(ModuleL, TreeL) :- modules_trees(dep, ModuleL, TreeL).
modrevs_trees(ModuleL, TreeL) :- modules_trees(rev, ModuleL, TreeL).

modules_trees(Direction, ModuleL, TreeL) :-
    working_directory(SourceDir, SourceDir),
    dependency_trees(module_edge(Direction, SourceDir), ModuleL, TreeL).

module_edge(dep, SourceDir, Context, Module) :-
    '$load_context_module'(File, Context, _Options),
    atom(File),
    atom_concat(SourceDir, _, File),
    '$current_module'(Module, File).
module_edge(rev, SourceDir, Module, Context) :-
    '$current_module'(Module, File),
    atom(File),
    atom_concat(SourceDir, _, File),
    '$load_context_module'(File, Context, _Options).
