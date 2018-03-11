/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(clause_codewalk, []).

:- use_module(library(prolog_xref), []).
:- use_module(library(assrt_lib)).
:- use_module(library(context_values)).
:- use_module(library(extend_args)).
:- use_module(library(extra_location)).
:- use_module(library(from_utils)).
:- use_module(library(implementation_module)).
:- use_module(library(option_utils)).

:- thread_local
    '$file_module_db'/2.

mf_from_chk(M, File, From) :-
    from_to_file(From, File),
    '$file_module_db'(File, M).

codewalk:walk_code(clause, Options1) :-
    foldl(select_option_default,
          [on_trace(OnTrace)-(codewalk:true_3),
           trace_reference(To)-To,
           undefined(Undefined)-ignore,
           if(Loaded)-true,
           walkextras(Extras)-[initialization,
                               declaration,
                               asrparts([body])],
           variable_names(VNL)-VNL],
          Options1, Options2),
    option_allchk(M, File, FileMGen-[if(Loaded)|Options2], true-_),
    forall(FileMGen, assertz('$file_module_db'(File, M))),
    with_context_values(
        setup_call_cleanup(
            ( '$current_source_module'(OldM),
              freeze(M, '$set_source_module'(_, M))
            ),
            ( walk_clause(mf_from_chk(M, File), From),
              maplist(walk_extras(mf_from_chk(M, File), From), Extras)
            ),
            '$set_source_module'(_, OldM)),
        [from, on_trace, trace_reference, undefined],
        [From, OnTrace,  To,              Undefined]),
    retractall('$file_module_db'(_, _)).

walk_extras(FromChk, From, Extra) :-
    walk_extras_(Extra, FromChk, From).

walk_extras_(initialization, FromChk, From) :- walk_from_initialization(FromChk, From).
walk_extras_(declaration,    FromChk, From) :- walk_from_loc_declaration(FromChk, From).
walk_extras_(asrparts(L),    FromChk, From) :- walk_from_assertion(FromChk, From, L).

walk_from_initialization(FromChk, From) :-
    forall(( '$init_goal'(_File, Goal, SourceLocation),
             ( SourceLocation = File:Line
             ->From = file(File, Line, -1, _)
             ; true
             ),
             call(FromChk, From)
           ),
           walk_head_body('<initialization>', Goal)).

walk_from_loc_declaration(FromChk, From) :-
    forall(( loc_declaration(Head, M, goal, From),
             call(FromChk, From)),
           walk_head_body('<declaration>', M:Head)).

walk_from_assertion(FromChk, From, AsrPartL) :-
    forall(( assrt_lib:asr_head_prop(Asr, HM, Head, _, _, VNL, AFrom),
             b_setval('$variable_names', VNL),
             call(FromChk, AFrom),
             implementation_module(HM:Head, M),
             member(AsrPart, AsrPartL),
             assertion_goal(AsrPart, Asr, Goal, CM, From)
           ),
           walk_head_body('<assertion>'(M:Head), CM:Goal)).

assertion_goal(AsrPart, Asr, Prop, PM, From) :-
    member(AsrPart-PartL,
           [head-[head],
            body-[comp, call, succ, glob]]),
    member(Part, PartL),
    % For glob, actually is arg(1, Prop, HM:Head), but we keep it uninstantiated for optimization
    curr_prop_asr(Part, PM:Prop, From, Asr).

walk_clause(FromChk, From) :-
    forall(current_head_body(FromChk, Head, Body, From),
           walk_head_body(Head, Body)).

current_head_body(FromChk, Head, CM:Body, From) :-
    From = clause(Ref),
    Head = _:_,
    current_predicate(_, Head),
    \+ predicate_property(Head, imported_from(_)),
    catch(clause(Head, Body, Ref), _, fail),
    call(FromChk, From),
    clause_property(Ref, module(CM)).

walk_head_body(Head, CM:Body) :-
    term_variables(Head, Vars),
    '$expand':mark_vars_non_fresh(Vars),
    with_context_values(
        walk_called(Body, CM),
        [caller], [Head]), !.
walk_head_body(Head, Body) :-
    writeln(user_error, (Head :- Body)), fail.

walk_called(Goal, M) :-
    walk_called_2(Goal, M),
    term_variables(Goal, Vars),
    '$expand':mark_vars_non_fresh(Vars).

walk_called_2(G, _) :-
    var(G),
    !.
walk_called_2(true, _) :- !.
walk_called_2(M:G, _) :-
    !,
    walk_called(G, M).
walk_called_2((A,B), M) :-
    !,
    walk_called(A, M),
    walk_called(B, M).
walk_called_2((A->B), M) :-
    !,
    walk_called(A, M),
    walk_called(B, M).
walk_called_2((A*->B), M) :-
    !,
    walk_called(A, M),
    walk_called(B, M).
walk_called_2(\+(A), M) :-
    walk_called(A, M).
walk_called_2((A;B), M) :-
    !,
    \+ \+ walk_called(A, M),
    \+ \+ walk_called(B, M).
walk_called_2(Goal, M) :-
    current_context_value(trace_reference, To),
    To \== (-),
    (   subsumes_term(To, M:Goal)
    ->  M2 = M
    ;   predicate_property(M:Goal, imported_from(M2)),
        subsumes_term(To, M2:Goal)
    ),
    current_context_value(on_trace, OnTrace),
    current_context_value(caller,   Caller),
    current_source_location(From),
    call(OnTrace, M2:Goal, Caller, From),
    fail.
walk_called_2(Goal, M) :-
    (   (   predicate_property(M:Goal, imported_from(IM))
        ->  true
        ;   IM = M
        ),
        prolog:called_by(Goal, IM, M, Called)
    ;   prolog:called_by(Goal, Called)
    ),
    Called \== [],
    !,
    walk_called_by(Called, M).
walk_called_2(Meta, M) :-
    (   predicate_property(M:Meta, meta_predicate(Head))
    ;   inferred_meta_predicate(M:Meta, Head)
    ),
    !,
    walk_meta_call(1, Head, Meta, M).
walk_called_2(Goal, Module) :-
    nonvar(Module),
    '$get_predicate_attribute'(Module:Goal, defined, 1),
    !.
walk_called_2(Goal, Module) :-
    callable(Goal),
    nonvar(Module),
    !,
    undefined(Module:Goal).
walk_called_2(_, _).

undefined(_) :-
    current_context_value(undefined, ignore),
    !.
undefined(Goal) :-
    predicate_property(Goal, autoload(_)),
    !.
undefined(Goal) :-
    current_context_value(undefined, trace),
    current_context_value(on_trace,  OnTrace),
    current_context_value(caller,    Caller),
    current_source_location(From),
    call(OnTrace, Goal, Caller, From),
    fail.
undefined(_).

walk_called_by([], _).
walk_called_by([H|T], M) :-
    (   H = G+N
    ->  (   extend(G, N, G1)
        ->  walk_called(G1, M)
        ;   true
        )
    ;   walk_called(H, M)
    ),
    walk_called_by(T, M).

walk_meta_call(I, Head, Meta, M) :-
    arg(I, Head, AS),
    !,
    (   integer(AS)
    ->  arg(I, Meta, MA),
        ( extend(MA, AS, Goal)
        ->walk_called(Goal, M)
        ; true
        )
    ;   AS == (^)
    ->  arg(I, Meta, MA),
        remove_quantifier(MA, Goal, M, MG),
        walk_called(Goal, MG)
    ;   AS == (//)
    ->  arg(I, Meta, DCG),
        walk_dcg_body(DCG, M)
    ;   true
    ),
    succ(I, I2),
    walk_meta_call(I2, Head, Meta, M).
walk_meta_call(_, _, _, _).

walk_dcg_body(Var, _) :-
    var(Var),
    !.
walk_dcg_body([], _Module) :- !.
walk_dcg_body([_|_], _Module) :- !.
walk_dcg_body(String, _Module) :-
    string(String),
    !.
walk_dcg_body(!, _Module) :- !.
walk_dcg_body(M:G, _) :-
    !,
    (   nonvar(M)
    ->  walk_dcg_body(G, M)
    ;   fail
    ).
walk_dcg_body((A,B), M) :-
    !,
    walk_dcg_body(A, M),
    walk_dcg_body(B, M).
walk_dcg_body((A->B), M) :-
    !,
    walk_dcg_body(A, M),
    walk_dcg_body(B, M).
walk_dcg_body((A*->B), M) :-
    !,
    walk_dcg_body(A, M),
    walk_dcg_body(B, M).
walk_dcg_body((A;B), M) :-
    !,
    \+ \+ walk_dcg_body(A, M),
    \+ \+ walk_dcg_body(B, M).
walk_dcg_body((A|B), M) :-
    !,
    \+ \+ walk_dcg_body(A, M),
    \+ \+ walk_dcg_body(B, M).
walk_dcg_body({G}, M) :-
    !,
    walk_called(G, M).
walk_dcg_body(G, M) :-
    extend_args(G, [_, _], G2),
    walk_called(G2, M).

extend(Goal, _, _) :-
    var(Goal),
    !,
    fail.
extend(Goal, 0, Goal) :- !.
extend(M:Goal, N, M:GoalEx) :-
    !,
    extend(Goal, N, GoalEx).
extend(Goal, N, GoalEx) :-
    callable(Goal),
    !,
    length(Extra, N),
    '$expand':mark_vars_non_fresh(Extra),
    extend_args(Goal, Extra, GoalEx).
extend(Goal, _, _) :-
    current_source_location(From),
    print_message(error, error(type_error(callable, Goal), From)),
    fail.

current_source_location(From) :-
    current_context_value(from, From).

remove_quantifier(Goal, Goal, M, M) :-
    var(Goal),
    !.
remove_quantifier(_^Goal0, Goal, M0, M) :-
    !,
    remove_quantifier(Goal0, Goal, M0, M).
remove_quantifier(M1:Goal0, Goal, _, M) :-
    !,
    remove_quantifier(Goal0, Goal, M1, M).
remove_quantifier(Goal, Goal, M, M).
