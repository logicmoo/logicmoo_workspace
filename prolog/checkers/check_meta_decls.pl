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

:- module(check_meta_decls, []).

:- use_module(library(checkers/checker)).
:- use_module(library(prolog_metainference), []).
:- use_module(library(is_entry_point)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(from_utils)).

:- multifile
        prolog:message//1.

prolog:message(acheck(meta_decls)) -->
    ['Missing Meta Predicate Declarations', nl,
     '-----------------------------------', nl,
     'The predicates below require a missing meta_predicate declaration.', nl,
     'They have been automatically inferred. Although is not required, it', nl,
     'is recommended to add them by hand or to fix the predicate in order', nl,
     'to facilitate static analysis and refactoring.', nl, nl].

prolog:message(acheck(meta_decls, (Loc/M)-Specs)) -->
    Loc,
    ['(~w):'-M],
    meta_decls(Specs).

meta_decls([]) --> [].
meta_decls([H|T]) -->
    [ '\t:- meta_predicate ~q'-[H]],
    meta_decls2(T),
    ['.'].

meta_decls2([]) --> [].
meta_decls2([H|T]) -->
    [',', nl, '\t\t~q'-[H]],
    meta_decls2(T).

% cleanup_metainference :-
%     retractall(prolog_metainference:inferred_meta_pred(_, _, _)).

% Hook to hide messages:
:- multifile
    hide_missing_meta_pred/2.

hide_missing_meta_pred(generated_predicate(_), prolog).
hide_missing_meta_pred(rename_predicate(_, _), prolog).
hide_missing_meta_pred(prolog_exception_hook(_, _, _, _), user).

checker:check(meta_decls, Pairs, Options) :-
    option_module_files(Options, MFileD),
    findall(warning-((Loc/M)-Spec),
            check_meta_decls_each(MFileD, Loc, M, Spec),
            Pairs).

check_meta_decls_each(MFileD, Loc, M, Spec) :-
    prolog_metainference:inferred_meta_pred(Head, M, Spec),
    get_dict(M, MFileD, FileD),
    \+ predicate_property(M:Head, meta_predicate(_)),
    %% Only exported predicates would require qualification
    %% of meta-arguments -- EMM after JW talk
    is_entry_point(Spec, M),
    \+ hide_missing_meta_pred(Head, M),
    once(( property_from(M:Head, _, From),
           from_to_file(From, File),
           get_dict(File, FileD, _)
         )), % once: only first occurrence
    from_location(From, Loc).
