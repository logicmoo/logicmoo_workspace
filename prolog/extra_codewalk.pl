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

:- module(extra_codewalk, [extra_walk_code/1,
                           extra_walk_code/3,
                           extra_wcsetup/3,
                           extra_walk_module_body/2,
                           resolve_head/3]).

:- use_module(library(prolog_codewalk)).
:- use_module(library(assrt_lib)).
:- use_module(library(extra_location)).
:- use_module(library(implementation_module)).
:- use_module(library(option_utils)).

:- thread_local
    issues/1.

:- meta_predicate
    extra_walk_code(:),
    extra_walk_code(:,+,-).

%% extra_walk_module_body(-, +) :-
extra_walk_module_body(M, OptionL0 ) :-
    select_option(module(M), OptionL0, OptionL, M),
    ( nonvar(M)
    ->findall(Ref, current_clause_module_body(M, Ref), RefU),
      sort(RefU, RefL),
      prolog_walk_code([source(false), clauses(RefL)|OptionL])
    ; true
    ).

%% extra_walk_code(:) is det.
extra_walk_code(OptionL) :-
    extra_walk_code(OptionL, _, _).

%% extra_walk_code(:, -, -) is det.
extra_walk_code(CM:OptionL0, M, FromChk) :-
    extra_wcsetup(OptionL0, OptionL1, FromChk),
    foldl(select_option_default,
          [source(S)-false,
           walkextras(Extras)-[declaration, asrparts([body])],
           on_etrace(ETracer)-ETracer
          ], OptionL1, OptionL2),
    OptionL = [on_trace(extra_codewalk:pcw_trace(1, CM:ETracer, FromChk))|OptionL2],
    extra_walk_module_body(M, OptionL),
    optimized_walk_code(S, Stage, extra_codewalk:pcw_trace(Stage, CM:ETracer, FromChk), OptionL2),
    prolog_codewalk:meta_options(is_meta, CM:OptionL, QOptions),
    prolog_codewalk:make_walk_option(QOptions, OTerm),
    maplist(walk_extras(OTerm, M, FromChk), Extras).

:- public pcw_trace/6.

:- meta_predicate pcw_trace(+,3,1,+,+,+).
pcw_trace(1, ETracer, FromChk, Goal, Caller, From) :-
    call(FromChk, From),
    call(ETracer, Goal, Caller, From),
    ( From = clause(CRef)
    ->record_issues(CRef)
    ; true
    ).
pcw_trace(2, ETracer, _, Goal, Caller, From) :-
    call(ETracer, Goal, Caller, From).

walk_extras(OTerm, M, FromChk, Extra) :- walk_extras_(Extra, OTerm, M, FromChk).

walk_extras_(declaration, OTerm, M, FromChk) :- walk_from_loc_declaration(OTerm, M, FromChk).
walk_extras_(asrparts(L), OTerm, M, FromChk) :- walk_from_assertion(OTerm, M, FromChk, L).

current_clause_module_body(CM, Ref) :-
    current_predicate(M:F/A),
    M \= CM,
    functor(H, F, A),
    \+ predicate_property(M:H, imported_from(_)),
    ( catch(clause(M:H, Body, Ref), _, fail),
      clause_property(Ref, module(HM)),
      strip_module(HM:Body, CM, _)
    ).

optimized_walk_code(false, 1, Tracer, OptionL) :-
    prolog_walk_code([source(false), on_trace(Tracer)|OptionL]).
optimized_walk_code(true, Stage, Tracer, OptionL) :-
    optimized_walk_code_true(Stage, Tracer, OptionL).

optimized_walk_code_true(1, Tracer, OptionL) :-
    prolog_walk_code([source(false), on_trace(Tracer)|OptionL]),
    fail.
optimized_walk_code_true(2, Tracer, OptionL) :-
    findall(CRef, retract(issues(CRef)), ClausesU),
    sort(ClausesU, Clauses),
    ( Clauses==[]
    ->true
    ; prolog_walk_code([clauses(Clauses), on_trace(Tracer)|OptionL])
    ).

extra_wcsetup(OptionL0, OptionL, FromChk) :-
    option_fromchk(OptionL0, OptionL1, FromChk),
    merge_options(OptionL1,
                  [infer_meta_predicates(false),
                   autoload(false),
                   evaluate(false),
                   trace_reference(_),
                   module_class([user, system, library])
                  ], OptionL).

walk_from_loc_declaration(OTerm, M, FromChk) :-
    forall(( prolog_codewalk:walk_option_caller(OTerm, '<declaration>'),
             clause(loc_declaration(Head, M, goal, From), _, Ref),
             call(FromChk, From)
           ),
           walk_from_goal(Head, M, Ref, OTerm)).

walk_from_goal(Head, M, Ref, OTerm) :-
    prolog_codewalk:( scan_module(M, OTerm), !,
                      walk_option_clause(OTerm, Ref),
                      walk_called_by_body(no_positions, Head, M, OTerm)
                    ).

walk_from_assertion(OTerm, M, FromChk, AsrPartL) :-
    forall(( AHead = assrt_lib:asr_head_prop(Asr, HM, Head, _, _, _, From),
             call(FromChk, From),
             implementation_module(HM:Head, M),
             prolog_codewalk:walk_option_caller(OTerm, '<assertion>'(M:Head)),
             clause(AHead, _, Ref),
             member(AsrPart, AsrPartL),
             assertion_goal(AsrPart, Head, HM, Asr, Goal, CM)
           ),
           walk_from_goal(Goal, CM, Ref, OTerm)).

assertion_goal(head, Head, HM, _, Head, HM).
assertion_goal(body, _, _, Asr, Prop, PM) :-
    ( asr_comp(Asr, PM, Prop, _)
    ; asr_call(Asr, PM, Prop, _)
    ; asr_succ(Asr, PM, Prop, _)
    ; asr_glob(Asr, PM, Prop, _)
      %% arg(1, Prop, HM:Head), but keep it uninstantiated for optimization
    ).

resolve_head(M:H0, _, H) :- !,
    resolve_head(H0, M, H).
resolve_head((A,B), M, H) :- !,
    ( resolve_head(A, M, H)
    ; resolve_head(B, M, H)
    ).
resolve_head((A;B), M, H) :- !,
    ( resolve_head(A, M, H)
    ; resolve_head(B, M, H)
    ).
resolve_head(H, M, M:H).

record_issues(CRef) :-
    assertz(issues(CRef)).
