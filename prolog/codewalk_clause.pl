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

:- module(codewalk_clause, []).

:- use_module(library(prolog_xref), []).
:- use_module(library(apply)).
:- use_module(library(assertions)).
:- use_module(library(context_values)).
:- use_module(library(extend_args)).
:- use_module(library(extra_location)).
:- use_module(library(from_utils)).
:- use_module(library(meta_args)).
:- use_module(library(option_utils)).

:- dynamic
    '$file_db'/1.

:- thread_local
    '$file_db'/1.

f_from_chk(From) :-
    from_to_file(From, File),
    '$file_db'(File).

:- meta_predicate
    walk_clause(1, +).

codewalk:walk_code(clause, Options1) :-
    foldl(select_option_default,
          [on_trace(OnTrace)-(codewalk:true_3),
           on_head(OnHead)-(codewalk:true_2),
           module(M)-M,
           trace_reference(To)-To,
           undefined(Undefined)-ignore,
           if(Loaded)-true,
           trace_variables(TraceVars)-[],
           walkextras(Extras)-[initialization,
                               declaration,
                               asrparts([body])],
           variable_names(VNL)-VNL],
          Options1, Options2),
    option_allchk(M, File, FileMGen-[if(Loaded)|Options2], true-_),
    forall(distinct(File, FileMGen), assertz('$file_db'(File))),
    with_context_values(
        ( walk_clause(f_from_chk, From),
          maplist(walk_extras(f_from_chk, From), Extras)
        ),
        [from, on_trace, on_head, trace_vars, trace_reference, undefined],
        [From, OnTrace,  OnHead,  TraceVars,  To,              Undefined]),
    retractall('$file_db'(_)).

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

current_assertion_goal(FromChk, From, AsrPartL, M:Head, CM:Goal) :-
    assertions:asr_head_prop(Asr, HM, Head, _, _, VNL, AFrom),
    b_setval('$variable_names', VNL),
    call(FromChk, AFrom),
    predicate_property(HM:Head, implementation_module(M)),
    member(AsrPart, AsrPartL),
    assertion_goal(AsrPart, Asr, Goal, CM, From),
    current_context_value(trace_vars, TraceVars),
    maplist(trace_var(M:Head), TraceVars).

walk_from_assertion(FromChk, From, AsrPartL) :-
    forall(current_assertion_goal(FromChk, From, AsrPartL, Head, Goal),
           walk_head_body('<assertion>'(Head), Goal)).

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
    current_context_value(trace_vars, TraceVars),
    catch(clause(Head, Body, Ref), _, fail),
    call(FromChk, From),
    clause_property(Ref, module(CM)),
    maplist(trace_var(Head), TraceVars).

trace_var(Head, non_fresh) :-
    term_variables(Head, Vars),
    '$expand':mark_vars_non_fresh(Vars).
trace_var(Head, meta_arg) :-
    mark_meta_arguments(Head).

walk_head_body(Head, Body) :-
    current_context_value(on_head, OnHead),
    current_source_location(From),
    ignore(call(OnHead, Head, From)),
    walk_called(Body, Head, user),
    !.
walk_head_body(Head, Body) :-
    writeln(user_error, walk_head_body(Head, Body)),
    fail.

walk_called(G, _, _) :-
    var(G),
    !.
walk_called(true, _, _) :- !.
walk_called(M:G, C, _) :-
    !,
    ( atom(M)
    ->setup_call_cleanup(( '$current_source_module'(OldM),
                           '$set_source_module'(_, M)
                         ),
                         walk_called(G, C, M),
                         '$set_source_module'(_, OldM))
    ; true
    ).
walk_called((A,B), C, M) :-
    !,
    walk_called(A, C, M),
    walk_called(B, C, M).
walk_called((A->B), C, M) :-
    !,
    walk_called(A, C, M),
    walk_called(B, C, M).
walk_called((A*->B), C, M) :-
    !,
    walk_called(A, C, M),
    walk_called(B, C, M).
walk_called(\+(A), C, M) :-
    \+ \+ walk_called(A, C, M).
walk_called((A;B), C, M) :-
    !,
    term_variables(A, VA),
    term_variables(B, VB),
    sort(VA, SA),
    sort(VB, SB),
    ord_union(SA, SB, L),
    findall(L-V-Att,
            ( member(E, [A, B]),
              walk_called(E, C, M),
              term_attvars(L, V),
              maplist(get_attrs, V, Att)
            ), LVA),
    maplist(put_attrs_(L), LVA).
walk_called(Goal, C, M) :-
    walk_called_3(Goal, C, M),
    fail.
walk_called(Goal, C, M) :-
    ignore(walk_called_ontrace(Goal, C, M)),
    current_context_value(trace_vars, TraceVars),
    maplist(trace_var(M:Goal), TraceVars).

put_attrs_(L, L-V-A) :- maplist(put_attrs, V, A).

walk_called_ontrace(Goal, Caller, M) :-
    current_context_value(trace_reference, To),
    To \== (-),
    (   subsumes_term(To, M:Goal)
    ->  M2 = M
    ;   predicate_property(M:Goal, imported_from(M2)),
        subsumes_term(To, M2:Goal)
    ),
    current_context_value(on_trace, OnTrace),
    current_source_location(From),
    call(OnTrace, M2:Goal, Caller, From).

walk_called_3(Goal, Caller, M) :-
    (   (   predicate_property(M:Goal, imported_from(IM))
        ->  true
        ;   IM = M
        ),
        prolog:called_by(Goal, IM, M, Called)
    ;   prolog:called_by(Goal, Called)
    ),
    Called \== [],
    !,
    walk_called_by(Called, Caller, M).
walk_called_3(Meta, Caller, M) :-
    (   inferred_meta_predicate(M:Meta, Head)
    ;   predicate_property(M:Meta, meta_predicate(Head))
    ),
    !,
    mark_args_non_fresh(1, Head, Meta),
    walk_meta_call(1, Head, Meta, Caller, M).
walk_called_3(Goal, _, Module) :-
    nonvar(Module),
    '$get_predicate_attribute'(Module:Goal, defined, 1),
    !.
walk_called_3(Goal, Caller, Module) :-
    callable(Goal),
    nonvar(Module),
    !,
    undefined(Module:Goal, Caller).
walk_called_3(_, _, _).

undefined(_, _) :-
    current_context_value(undefined, ignore),
    !.
undefined(Goal, _) :-
    predicate_property(Goal, autoload(_)),
    !.
undefined(Goal, Caller) :-
    current_context_value(undefined, trace),
    current_context_value(on_trace,  OnTrace),
    current_source_location(From),
    call(OnTrace, Goal, Caller, From),
    fail.
undefined(_, _).

walk_called_by([], _, _).
walk_called_by([H|T], C, M) :-
    (   H = G+N
    ->  (   extend(G, N, G1)
        ->  walk_called(G1, C, M)
        ;   true
        )
    ;   walk_called(H, C, M)
    ),
    walk_called_by(T, C, M).

walk_meta_call(I, Head, Meta, Caller, M) :-
    arg(I, Head, AS),
    !,
    (   integer(AS)
    ->  arg(I, Meta, MA),
        ( extend(MA, AS, Goal)
        ->walk_called(Goal, Caller, M)
        ; true
        )
    ;   AS == (^)
    ->  arg(I, Meta, MA),
        remove_quantifier(MA, Goal, M, MG),
        walk_called(Goal, Caller, MG)
    ;   AS == (//)
    ->  arg(I, Meta, DCG),
        walk_dcg_body(DCG, Caller, M)
    ;   true
    ),
    succ(I, I2),
    walk_meta_call(I2, Head, Meta, Caller, M).
walk_meta_call(_, _, _, _, _).

mark_args_non_fresh(I, Head, Meta) :-
    arg(I, Head, AS),
    !,
    ( ( integer(AS)
      ; AS == (^)
      ; AS == (//)
      )
    ->true
    ; arg(I, Meta, MA),
      term_variables(MA, Vars),
      '$expand':mark_vars_non_fresh(Vars)
    ),
    succ(I, I2),
    mark_args_non_fresh(I2, Head, Meta).
mark_args_non_fresh(_, _, _).
    
walk_dcg_body(Var, _, _) :-
    var(Var),
    !.
walk_dcg_body([], _, _) :- !.
walk_dcg_body([_|_], _, _) :- !.
walk_dcg_body(String, _, _) :-
    string(String),
    !.
walk_dcg_body(!, _, _) :- !.
walk_dcg_body(M:G, C, _) :-
    !,
    (   nonvar(M)
    ->  walk_dcg_body(G, C, M)
    ;   fail
    ).
walk_dcg_body((A,B), C, M) :-
    !,
    walk_dcg_body(A, C, M),
    walk_dcg_body(B, C, M).
walk_dcg_body((A->B), C, M) :-
    !,
    walk_dcg_body(A, C, M),
    walk_dcg_body(B, C, M).
walk_dcg_body((A*->B), C, M) :-
    !,
    walk_dcg_body(A, C, M),
    walk_dcg_body(B, C, M).
walk_dcg_body((A;B), C, M) :-
    !,
    \+ \+ walk_dcg_body(A, C, M),
    \+ \+ walk_dcg_body(B, C, M).
walk_dcg_body((A|B), C, M) :-
    !,
    \+ \+ walk_dcg_body(A, C, M),
    \+ \+ walk_dcg_body(B, C, M).
walk_dcg_body({G}, C, M) :-
    !,
    walk_called(G, C, M).
walk_dcg_body(G, C, M) :-
    extend_args(G, [_, _], G2),
    walk_called(G2, C, M).

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
remove_quantifier(_^Goal1, Goal, M1, M) :-
    !,
    remove_quantifier(Goal1, Goal, M1, M).
remove_quantifier(M1:Goal1, Goal, _, M) :-
    !,
    remove_quantifier(Goal1, Goal, M1, M).
remove_quantifier(Goal, Goal, M, M).
