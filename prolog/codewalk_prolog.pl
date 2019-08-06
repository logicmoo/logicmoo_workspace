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

:- module(codewalk_prolog, []).

:- use_module(library(prolog_codewalk)).
:- use_module(library(assertions)).
:- use_module(library(extra_location)).
:- use_module(library(option_utils)).
:- use_module(library(from_utils)).

:- thread_local
    issues/1.

%!  extra_walk_module_body(-Module, +Options) is det.

extra_walk_module_body(Options) :-
    ( option(module(M), Options)
    ->findall(Ref, current_clause_module_body(M, Ref), RefU),
      sort(RefU, RefL),
      prolog_walk_code([source(false), clauses(RefL)|Options])
    ; true
    ).

codewalk:walk_code(prolog, Options1) :-
    extra_wcsetup(Options1, Options2, MFileD),
    foldl(select_option_default,
          [source(S)-false,
           walkextras(Extras)-[declaration, asrparts([body])],
           on_trace(ETracer)-ETracer
          ], Options2, Options3),
    Options = [on_trace(codewalk_prolog:pcw_trace(1, ETracer, MFileD))|Options3],
    extra_walk_module_body(Options),
    optimized_walk_code(S, Stage, codewalk_prolog:pcw_trace(Stage, ETracer, MFileD), Options3),
    prolog_codewalk:make_walk_option(Options, OTerm),
    maplist(walk_extras_p(OTerm, MFileD), Extras).

:- public pcw_trace/6.
:- meta_predicate pcw_trace(+,3,+,+,+,+).
pcw_trace(1, ETracer, MFileD, M:Goal, Caller, From) :-
    get_dict(M, MFileD, FileD),
    from_to_file(From, File),
    get_dict(File, FileD, _),
    '$set_source_module'(M),
    call(ETracer, M:Goal, Caller, From),
    ( From = clause(CRef)
    ->record_issues(CRef)
    ; true
    ).
pcw_trace(2, ETracer, _, Goal, Caller, From) :-
    call(ETracer, Goal, Caller, From).

walk_extras_p(OTerm, MFileD, Extra) :- walk_extras_(Extra, OTerm, MFileD).

walk_extras_(declaration, OTerm, MFileD) :- walk_from_loc_declaration(OTerm, MFileD).
walk_extras_(asrparts(L), OTerm, MFileD) :- walk_from_assertion(OTerm, MFileD, L).

current_clause_module_body(CM, Ref) :-
    MH = M:_,
    current_predicate(_, MH),
    M \= CM,
    \+ predicate_property(MH, imported_from(_)),
    ( catch(clause(MH, Body, Ref), _, fail),
      clause_property(Ref, module(HM)),
      strip_module(HM:Body, CM, _)
    ).

optimized_walk_code(false, 1, Tracer, Options) :-
    prolog_walk_code([source(false), on_trace(Tracer)|Options]).
optimized_walk_code(true, Stage, Tracer, Options) :-
    optimized_walk_code_true(Stage, Tracer, Options).

optimized_walk_code_true(1, Tracer, Options) :-
    prolog_walk_code([source(false), on_trace(Tracer)|Options]),
    fail.
optimized_walk_code_true(2, Tracer, Options) :-
    findall(CRef, retract(issues(CRef)), ClausesU),
    sort(ClausesU, Clauses),
    ( Clauses==[]
    ->true
    ; prolog_walk_code([clauses(Clauses), on_trace(Tracer)|Options])
    ).

extra_wcsetup(Options1, Options, MFileD) :-
    option_module_files(MFileD, Options1, Options2),
    merge_options(Options2,
                  [infer_meta_predicates(false),
                   autoload(false),
                   evaluate(false),
                   trace_reference(_),
                   module_class([user, system, library])
                  ], Options).

walk_from_loc_declaration(OTerm, MFileD) :-
    forall(( prolog_codewalk:walk_option_caller(OTerm, '<declaration>'),
             clause(loc_declaration(Head, M, goal, From), _, Ref),
             get_dict(M, MFileD, FileD),
             from_to_file(From, File),
             get_dict(File, FileD, _)
           ),
           walk_from_goal(Head, M, Ref, OTerm)).

walk_from_goal(Head, M, Ref, OTerm) :-
    prolog_codewalk:( scan_module(M, OTerm), !,
                      walk_option_clause(OTerm, Ref),
                      walk_called_by_body(no_positions, Head, M, OTerm)
                    ).

walk_from_assertion(OTerm, MFileD, AsrPartL) :-
    option_files([module_files(MFileD)], FileD),
    forall(( AHead = assertions:asr_head_prop(Asr, HM, Head, _, _, _, From),
             clause(AHead, Body, Ref),
             module_property(Ref, module(M)),
             call(M:Body),
             from_to_file(From, File),
             get_dict(File, FileD, _),
             predicate_property(HM:Head, implementation_module(M)),
             prolog_codewalk:walk_option_caller(OTerm, '<assertion>'(M:Head)),
             member(AsrPart, AsrPartL),
             assertion_goal(AsrPart, Head, HM, Asr, Goal, CM)
           ),
           walk_from_goal(Goal, CM, Ref, OTerm)).

assertion_goal(head, Head, HM, _, Head, HM).
assertion_goal(body, _, _, Asr, Prop, PM) :-
    member(Part, [comp, call, succ, glob]),
    % For glob, actually is arg(1, Prop, HM:Head), but we keep it uninstantiated for optimization
    curr_prop_asr(Part, PM:Prop, _, Asr).

record_issues(CRef) :-
    assertz(issues(CRef)).
