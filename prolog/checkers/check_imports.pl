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

:- module(check_imports, []).

:- use_module(library(checkers/checker)).
:- use_module(library(apply)).
:- use_module(library(clambda)).
:- use_module(library(expansion_module)).
:- use_module(library(codewalk)).
:- use_module(library(extra_location)).
:- use_module(library(from_utils)).
:- use_module(library(option_utils)).
:- use_module(library(location_utils)).
:- use_module(library(module_files)).

:- multifile
    prolog:message//1.

prolog:message(acheck(imports)) -->
    ['Unused Imports',nl,
     '--------------',nl,
     'The predicates or modules below has been imported, however they', nl,
     'are never used in the importing module, or they do not implement', nl,
     'new clauses for multifile predicates.  Note that modules that', nl,
     'export operators, or that do not export any predicate are not', nl,
     'reported.', nl,
     'You can silent the warnings by declaring use_module/2 with an',nl,
     'empty import list. If they have desirable side effects and still', nl,
     'needs to be imported, you can refactorize your program so that', nl,
     'such side effects are not required anymore.', nl, nl].
prolog:message(acheck(imports, c(Class, Type, Name)-LocElemL)) -->
    ['~w ~w have unused ~w:'-[Class, Name, Type], nl],
    foldl(unused_import, LocElemL).

unused_import(Loc/Elem) -->
    Loc,
    ['unused ~w'-[Elem], nl].

:- dynamic
    used_import/1,
    used_usemod/2.

checker:check(imports, Result, Options) :-
    check_imports(Options, Result).

check_imports(Options, Pairs) :-
    walk_code([source(false),
               on_trace(collect_imports_wc)|Options]),
    option_fromchk(M, _, Options, _, FromChk),
    collect_imports(M, FromChk, Pairs, Tail),
    collect_usemods(M, FromChk, Tail, []),
    cleanup_imports.

:- public collect_imports_wc/3.
collect_imports_wc(M:Goal, Caller, From) :-
    record_location_meta(M:Goal, _, From, all_call_refs, mark_import),
    ( nonvar(Caller),
      caller_module(Caller, From, MC),
      M \= MC,
      \+ used_usemod(M, MC)
    ->assertz(used_usemod(M, MC))
    ; true
    ).

caller_module(M:_,                _, M) :- !.
caller_module('<assertion>'(M:_), _, M) :- !.
caller_module(_, clause(Ptr), M) :- clause_property(Ptr, module(M)).

collect_imports(M, FromChk, Pairs, Tail) :-
    findall(warning-(c(use_module, import, U)-(Loc/(F/A))),
            current_unused_import(M, FromChk, U, Loc, F, A),
            Pairs, Tail).

current_unused_import(M, FromChk, U, Loc, F, A) :-
    clause(loc_declaration(Head, M, import(U), From), _, CRef),
    call(FromChk, From),
    M \= user,
    \+ memberchk(Head, [term_expansion(_,_),
                        term_expansion(_,_,_,_),
                        goal_expansion(_,_),
                        goal_expansion(_,_,_,_),
                        except(_)
                       ]),
    \+ used_import(CRef),
    \+ loc_declaration(Head, M, goal, _),
    module_property(M, class(Class)),
    memberchk(Class, [user]),
    functor(Head, F, A),
    from_location(From, Loc).

:- multifile ignore_import/2.

ignore_import(_, rtchecks_rt).
ignore_import(_, IM) :- is_expansion_module(IM).
ignore_import(_, IM) :-
    '$def_modules'([goal_expansion/4,
                    goal_expansion/2,
                    term_expansion/4,
                    term_expansion/2],
                   MList),
    member(M-PIL, MList),
    member(F/A, PIL),
    functor(H, F, A),
    clause(M:H, _, Ref),
    clause_property(Ref, file(File)),
    module_file(IM, File).

collect_usemods(M, FromChk, Pairs, Tail) :-
    findall(warning-(c(module, use_module, M)-(Loc/U)),
            ( current_used_use_module(M, FromChk, U, From),
              from_location(From, Loc)
            ), Pairs, Tail).

current_used_use_module(M, FromChk, UE, From) :-
    ( UE = use_module(U),
      loc_declaration(U, M, use_module, From),
      ExL = []
    ; UE = use_module(U, except(ExL)),
      loc_declaration(UE, M, use_module_2, From)
    ),
    call(FromChk, From),
    M \= user,
    module_property(M, class(Class)),
    memberchk(Class, [user]),
    from_to_file(From, File),
    \+ findall(I, source_file_property(File, included_in(I, _)),
               [_, _|_]),
    absolute_file_name(U, UFile, [file_type(prolog), access(exist),
                                  file_errors(fail)]),
    current_module(UM, UFile),
    \+ ignore_import(M, UM),
    module_property(UM, exports(EL)),
    EL \= [],
    subtract(EL, ExL, PIL),
    \+ ( module_property(UM, exported_operators(OL)),
         OL \= []
       ),
    \+ ( member(F/A, PIL),
         functor(Head, F, A),
         MHead = UM:Head,
         predicate_property(MHead, implementation_module(IM)),
         ( used_usemod(M, IM)                        % is used
         ; predicate_property(MHead, multifile),   % is extended
           clause(MHead, _, Ref),
           clause_property(Ref, file(File))
         )
       ).

mark_import(Head, CM, _, _, _, _) :-
    nonvar(CM),
    callable(Head),
    predicate_property(CM:Head, implementation_module(M)),
    mark_import(Head, M, CM).

mark_import(Head, M, CM) :-
    forall(( clause(loc_declaration(Head, CM, import(_), _), _, CRef),
             \+ used_import(CRef)),
           assertz(used_import(CRef))),
    ( M \= CM,
      \+used_usemod(CM, M)
    ->assertz(used_usemod(CM, M))
    ; true
    ).

cleanup_imports :-
    retractall(used_import(_)),
    retractall(used_usemod(_, _)).
