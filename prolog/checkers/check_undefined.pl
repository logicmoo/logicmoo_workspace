/*  Part of Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor, http://www.swi-prolog.org
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(check_undefined, []).

% A wrapper from library(check)
:- use_module(checkers(checker)).
:- use_module(library(apply)).
:- use_module(library(clambda)).
:- use_module(library(infer_alias)).
:- use_module(library(normalize_pi)).
:- use_module(library(extra_codewalk)).
:- use_module(library(location_utils)).
:- use_module(library(from_utils)).
:- use_module(library(referenced_by)).
:- use_module(library(group_pairs_or_sort)).
:- use_module(library(infer_meta_if_required)).

:- multifile
    prolog:message//1.

:- dynamic
    undef/3.

checker:check(undefined, Results, OptionL) :-
    check_undefined(OptionL, Results).

check_undefined(OptionL, Results) :-
    infer_meta_if_required,
    extra_walk_code([source(true),
                     trace_reference(-),
                     undefined(trace),
                     on_etrace(collect_undef)|OptionL]),
    findall(File-(AL-(PI-(Loc/['~w'-[CI]]))),
            ( retract(undef(PI, CI, From)),
              find_alternatives(PI, AL),
              from_location(From, Loc),
              from_to_file(From, File)
            ), Pairs),
    group_pairs_or_sort(Pairs, Grouped),
    findall(warning-(File-(Decl-(PI-LocCI))),
            ( member(File-ALPILocCIList, Grouped),
              member(AL-PILocCIList, ALPILocCIList),
              maplist(\ ((_:F/A)-_)^(F/A)^true, PILocCIList, PIL),
              maplist(alternative_decl(PIL), AL, Decl),
              member(PI-LocCIList, PILocCIList),
              member(LocCI, LocCIList)
            ), Results).

alternative_decl(PIL, A-EM/EL, Decl/FL) :-
    ( EL = []
    ->Decl = use_module(A)
    ; length(EL,  EN),
      length(PIL, IN),
      ( EN < IN
      ->Decl = use_module(A, except(EL))
      ; Decl = use_module(A, PIL)
      )
    ),
    subtract(PIL, EM, FL).

hide_undef(M:H) :- hide_undef(H, M).

find_alternatives(M:F/A, AL) :-
    functor(H, F, A),
    findall(AA-EL, ( current_predicate(AM:F/A),
                     AM \= M,
                     \+ predicate_property(AM:H, imported_from(_)),
                     ( module_property(AM, file(AF))
                     ->( library_alias(AF, AA)
                       ->true
                       ; AA = AF
                       )
                     ; AA=AM
                     ),
                     exclude_list(M, AM, EL)
                   ), AU),
    sort(AU, AL).

exclude_list(M, AM, ML/EL) :-
    module_property(AM, exports(MU)),
    sort(MU, ML),
    findall(F/A,
            ( member(F/A, ML),
              functor(H, F, A),
              predicate_property(M:H, defined),
              \+ predicate_property(M:H, imported_from(AM))
            ), EU),
    sort(EU, EL).

% Hook to hide undef messages:
:- multifile hide_undef/2.
hide_undef(asr_head_prop(_,_,_,_, _,_,_), assrt_lib).

:- public collect_undef/3.
collect_undef(MCall, Caller, From) :-
    \+ hide_undef(MCall),
    normalize_pi(MCall, PI),
    normalize_pi(Caller, CI),
    update_fact_from(undef(PI, CI), From).

prolog:message(acheck(undefined)) -->
    ['--------------------',nl,
     'Undefined Predicates',nl,
     '--------------------',nl],
    prolog:message(check(undefined_predicates)).
prolog:message(acheck(undefined, File-ALPILocCIList)) -->
    [ 'Undefined predicates in ~w:'-[File], nl],
    foldl(show_alternatives, ALPILocCIList),
    { pairs_values(ALPILocCIList, PILocCIList) },
    foldl(foldl(show_undefined), PILocCIList).

show_alternatives(AL-_) -->
    ( {AL = []}
    ->[]
    ; ['  Can be fixed by adding '],
      ( {AL = [_]}
      ->{Spc=''},
        []
      ; {Spc='\t'},
        ['one of these:', nl]
      ),
      foldl(show_alternative(Spc), AL)
    ).

show_alternative(Spc, Decl/FL) -->
    ['~a:- ~q.'-[Spc, Decl]],
    ( {FL = []}
    ->[]
    ; [' % add exports: ~q'-[FL]]
    ),
    [nl].

show_undefined(PI-LocCIList) -->
    [ '    ~w undefined, '-[PI], 'referenced by', nl],
    referenced_by(LocCIList).
