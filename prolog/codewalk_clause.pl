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
:- use_module(library(lists)).
:- use_module(library(thread)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(prolog_metainference)).
:- use_module(library(assertions)).
:- use_module(library(extend_args)).
:- use_module(library(extra_location)).
:- use_module(library(from_utils)).
:- use_module(library(meta_args)).
:- use_module(library(option_utils)).
% :- use_module(library(concurrent_forall)).

:- multifile
    codewalk:walk_code/2.

codewalk:walk_code(clause, Options1) :-
    foldl(select_option_default,
          [on_trace(OnTrace)-(codewalk:true_3),
           on_head(OnHead)-(codewalk:true_2),
           trace_reference(To)-To,
           undefined(Undefined)-ignore,
           trace_variables(TraceVars)-[],
           walkextras(Extras)-[initialization,
                               declaration,
                               asrparts([body])],
           variable_names(VNL)-VNL],
          Options1, Options),
    option_files(Options, FileD),
    Data = data{from:_,
                on_trace:OnTrace,
                on_head:OnHead,
                trace_variables:TraceVars,
                trace_reference:To,
                undefined:Undefined},
    concurrent_maplist(walk_extras_c(FileD, Data), [clause|Extras]).

walk_extras_c(FileD, Opts, Extra) :-
    walk_extras_(Extra, FileD, Opts).

walk_extras_(clause,         FileD, Opts) :- walk_clause(              FileD, Opts).
walk_extras_(initialization, FileD, Opts) :- walk_from_initialization( FileD, Opts).
walk_extras_(declaration,    FileD, Opts) :- walk_from_loc_declaration(FileD, Opts).
walk_extras_(asrparts(L),    FileD, Opts) :- walk_from_assertion(      FileD, Opts, L).

walk_from_initialization(FileD, Opts) :-
    forall(( '$init_goal'(_File, Goal, SourceLocation),
             ( SourceLocation = File:Line
             ->get_dict(File, FileD, _),
               From = file(File, Line, -1, _),
               option(from(From), Opts)
             ; true
             )
           ),
           walk_head_body('<initialization>', Goal, Opts)).

walk_from_loc_declaration(FileD, Opts) :-
    forall(( option(from(From), Opts),
             loc_declaration(Body, M, body, From),
             from_to_file(From, File),
             get_dict(File, FileD, _)
           ),
           walk_head_body('<declaration>', M:Body, Opts)).

current_assertion_goal(FileD, Opts, AsrPartL, M:Head, CM:Goal) :-
    assertions:asr_head_prop(Asr, HM, Head, _, _, VNL, _, AFrom),
    from_to_file(AFrom, File),
    get_dict(File, FileD, _),
    b_setval('$variable_names', VNL),
    predicate_property(HM:Head, implementation_module(M)),
    member(AsrPart, AsrPartL),
    option(from(From), Opts),
    assertion_goal(AsrPart, Asr, Goal, CM, From),
    option(trace_variables(TraceVars), Opts),
    maplist(trace_var(M:Head), TraceVars).

walk_from_assertion(FileD, Opts, AsrPartL) :-
    forall(current_assertion_goal(FileD, Opts, AsrPartL, Head, Goal),
           walk_head_body('<assertion>'(Head), Goal, Opts)).

assertion_goal(AsrPart, Asr, Prop, PM, From) :-
    member(AsrPart-PartL,
           [head-[head],
            body-[comp, call, succ, glob]]),
    member(Part, PartL),
    % For glob, actually is arg(1, Prop, HM:Head), but we keep it uninstantiated for optimization
    curr_prop_asr(Part, PM:Prop, From, Asr).

walk_clause(FileD, Opts) :-
    option(trace_variables(TraceVars), Opts),
    option(from(From), Opts),
    Head = M:_,
    concurrent_forall(
        current_module(M),
        forall(( current_head(Head),
                 current_head_body(FileD, Head, Body, From)
               ),
               ( maplist(trace_var(Head), TraceVars),
                 walk_head_body(Head, Body, Opts)
               ))).

current_head(Head) :-
    current_predicate(_, Head),
    \+ predicate_property(Head, imported_from(_)),
    predicate_property(Head, number_of_clauses(N)),
    N > 0.

current_head_body(FileD, Head, CM:Body, From) :-
    From = clause(Ref),
    catch(clause(Head, Body, Ref), _, fail),
    from_to_file(From, File),
    get_dict(File, FileD, _),
    clause_property(Ref, module(CM)).

trace_var(Head, non_fresh) :-
    term_variables(Head, Vars),
    '$expand':mark_vars_non_fresh(Vars).
trace_var(Head, meta_arg) :-
    mark_meta_arguments(Head).

walk_head_body(Head, Body, Opts) :-
    option(on_head(OnHead), Opts),
    option(from(From), Opts),
    ignore(call(OnHead, Head, From)),
    walk_called(Body, Head, user, Opts),
    !.
walk_head_body(Head, Body, _) :-
    writeln(user_error, walk_head_body(Head, Body, -)),
    fail.

walk_called_mod(G, C, M, CM, Opts) :-
    ( atom(M)
    ->setup_call_cleanup(
          ( '$current_source_module'(OldM),
            '$set_source_module'(CM)
          ),
          walk_called(G, C, M, Opts),
          '$set_source_module'(OldM))
    ; true
    ).

walk_called(G, _, _, _) :-
    var(G),
    !.
walk_called(true, _, _, _) :- !.
walk_called(@(G,CM), C, _, Opts) :-
    !,
    strip_module(CM:G, M, H),
    walk_called_mod(H, C, M, CM, Opts).
walk_called(M:G, C, _, Opts) :-
    !,
    walk_called_mod(G, C, M, M, Opts).
walk_called((A,B), C, M, O) :-
    !,
    walk_called(A, C, M, O),
    walk_called(B, C, M, O).
walk_called((A->B), C, M, O) :-
    !,
    walk_called(A, C, M, O),
    walk_called(B, C, M, O).
walk_called((A*->B), C, M, O) :-
    !,
    walk_called(A, C, M, O),
    walk_called(B, C, M, O).
walk_called(\+(A), C, M, O) :-
    \+ \+ walk_called(A, C, M, O).
walk_called((A;B), C, M, O) :-
    !,
    term_variables(A, VA),
    term_variables(B, VB),
    sort(VA, SA),
    sort(VB, SB),
    ord_union(SA, SB, L),
    findall(L-V-Att,
            ( member(E, [A, B]),
              walk_called(E, C, M, O),
              term_attvars(L, V),
              maplist(get_attrs, V, Att)
            ), LVA),
    maplist(put_attrs_(L), LVA).
walk_called(Goal, C, M, O) :-
    walk_called_3(Goal, C, M, O),
    fail.
walk_called(Goal, C, M, O) :-
    ignore(walk_called_ontrace(Goal, C, M, O)),
    option(trace_variables(TraceVars), O),
    maplist(trace_var(M:Goal), TraceVars).

put_attrs_(L, L-V-A) :- maplist(put_attrs, V, A).

walk_called_ontrace(Goal, Caller, M, Opts) :-
    option(trace_reference(To), Opts),
    To \== (-),
    (   subsumes_term(To, M:Goal)
    ->  M2 = M
    ;   predicate_property(M:Goal, implementation_module(M2)),
        subsumes_term(To, M2:Goal)
    ),
    option(on_trace(OnTrace), Opts),
    option(from(From), Opts),
    call(OnTrace, M2:Goal, Caller, From).

walk_called_3(Goal, Caller, M, Opts) :-
    (   predicate_property(M:Goal, implementation_module(IM)),
        prolog:called_by(Goal, IM, M, Called)
    ;   prolog:called_by(Goal, Called)
    ),
    Called \== [],
    !,
    walk_called_by(Called, Caller, M, Opts).
walk_called_3(Meta, Caller, M, Opts) :-
    (   inferred_meta_predicate(M:Meta, Head)
    ;   predicate_property(M:Meta, meta_predicate(Head))
    ),
    !,
    mark_args_non_fresh(1, Head, Meta),
    '$current_source_module'(CM),
    walk_meta_call(1, Head, Meta, Caller, CM, Opts).
walk_called_3(Goal, _, Module, _) :-
    nonvar(Module),
    '$get_predicate_attribute'(Module:Goal, defined, 1),
    !.
walk_called_3(Goal, Caller, Module, Opts) :-
    callable(Goal),
    nonvar(Module),
    !,
    undefined(Module:Goal, Caller, Opts).
walk_called_3(_, _, _, _).

undefined(_, _, Opts) :-
    option(undefined(ignore), Opts),
    !.
undefined(Goal, _, _) :-
    predicate_property(Goal, autoload(_)),
    !.
undefined(Goal, Caller, Opts) :-
    option(undefined(trace), Opts),
    option(on_trace(OnTrace), Opts),
    option(from(From), Opts),
    call(OnTrace, Goal, Caller, From),
    fail.
undefined(_, _, _).

walk_called_by([], _, _, _).
walk_called_by([H|T], C, CM, O) :-
    (   H = G+N
    ->  (   extend(G, N, G1, O)
        ->  walk_called(@(G1,CM), C, CM, O)
        ;   true
        )
    ;   walk_called(@(H,CM), C, CM, O)
    ),
    walk_called_by(T, C, CM, O).

walk_meta_call(I, Head, Meta, Caller, M, Opts) :-
    arg(I, Head, AS),
    !,
    (   integer(AS)
    ->  arg(I, Meta, MA),
        ( extend(MA, AS, Goal, Opts)
        ->walk_called(Goal, Caller, M, Opts)
        ; true
        )
    ;   AS == (^)
    ->  arg(I, Meta, MA),
        remove_quantifier(MA, Goal, M, MG),
        walk_called(Goal, Caller, MG, Opts)
    ;   AS == (//)
    ->  arg(I, Meta, DCG),
        walk_dcg_body(DCG, Caller, M, Opts)
    ;   true
    ),
    succ(I, I2),
    walk_meta_call(I2, Head, Meta, Caller, M, Opts).
walk_meta_call(_, _, _, _, _, _).

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

walk_dcg_body(Var, _, _, _) :-
    var(Var),
    !.
walk_dcg_body([], _, _, _) :- !.
walk_dcg_body([_|_], _, _, _) :- !.
walk_dcg_body(String, _, _, _) :-
    string(String),
    !.
walk_dcg_body(!, _, _, _) :- !.
walk_dcg_body(M:G, C, _, O) :-
    !,
    (   nonvar(M)
    ->  walk_dcg_body(G, C, M, O)
    ;   fail
    ).
walk_dcg_body((A,B), C, M, O) :-
    !,
    walk_dcg_body(A, C, M, O),
    walk_dcg_body(B, C, M, O).
walk_dcg_body((A->B), C, M, O) :-
    !,
    walk_dcg_body(A, C, M, O),
    walk_dcg_body(B, C, M, O).
walk_dcg_body((A*->B), C, M, O) :-
    !,
    walk_dcg_body(A, C, M, O),
    walk_dcg_body(B, C, M, O).
walk_dcg_body((A;B), C, M, O) :-
    !,
    \+ \+ walk_dcg_body(A, C, M, O),
    \+ \+ walk_dcg_body(B, C, M, O).
walk_dcg_body((A|B), C, M, O) :-
    !,
    \+ \+ walk_dcg_body(A, C, M, O),
    \+ \+ walk_dcg_body(B, C, M, O).
walk_dcg_body({G}, C, M, O) :-
    !,
    walk_called(G, C, M, O).
walk_dcg_body(G, C, M, O) :-
    extend_args(G, [_, _], G2),
    walk_called(G2, C, M, O).

extend(Goal, _, _, _) :-
    var(Goal),
    !,
    fail.
extend(Goal, 0, Goal, _) :- !.
extend(M:Goal, N, M:GoalEx, Opts) :-
    !,
    extend(Goal, N, GoalEx, Opts).
extend(Goal, N, GoalEx, _) :-
    callable(Goal),
    !,
    length(Extra, N),
    '$expand':mark_vars_non_fresh(Extra),
    extend_args(Goal, Extra, GoalEx).
extend(Goal, _, _, Opts) :-
    option(from(From), Opts),
    print_message(error, error(type_error(callable, Goal), From)),
    fail.

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
