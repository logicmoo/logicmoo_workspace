/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
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

:- module(compilation_module,
          [ compilation_module/1,
            compilation_module/2,
            (compilation_predicate)/1,
            op(1150, fx, compilation_predicate)
          ]).

:- reexport(library(compound_expand)).

:- meta_predicate
        compilation_module(:),
        compilation_module(:,+),
        compilation_predicate(:).

compilation_module(A)    :- use_module(A).

compilation_module(A, B) :- use_module(A, B).

compilation_predicate(_).

term_expansion((:- compilation_module(Alias)),
               [(:- discontiguous '$compilation_module'/2),
                '$compilation_module'(Alias, all),
                (:- use_module(Alias))]).
term_expansion((:- compilation_module(Alias, Exports)),
               [(:- discontiguous '$compilation_module'/2),
                '$compilation_module'(Alias, Exports),
                (:- use_module(Alias, Exports))]).
term_expansion((:- compilation_predicate(F/A)),
               '$compilation_predicate'(F, A)).

term_expansion(end_of_file, end_of_file) :-
    prolog_load_context(module, Context),
    prolog_load_context(source, Source),
    module_property(Context, file(Source)),
    forall(( current_predicate(Context:'$compilation_module'/2),
             Context:'$compilation_module'(Alias, Exports),
             absolute_file_name(Alias, File, [file_type(prolog), access(read)]),
             module_property(Module, file(File)),
             current_export(Exports, Context, Module, F, A)
           ; current_predicate(Context:'$compilation_predicate'/2),
             Context:'$compilation_predicate'(F, A)
           ),
           abolish(Context:F/A)),
    abolish(Context:'$compilation_module'/2),
    abolish(Context:'$compilation_predicate'/2).

current_export([E|L], _, _, F, A) :- member(F/A, [E|L]).
current_export(all, Context, Module, F, A) :-
    current_predicate(Context:F/A),
    functor(H, F, A),
    predicate_property(Context:H, imported_from(Module)).
