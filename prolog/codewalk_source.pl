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

:- module(codewalk_source, []).

:- use_module(library(prolog_source)).
:- use_module(library(prolog_xref), []).
:- use_module(library(context_values)).
:- use_module(library(option_utils)).
:- use_module(library(extend_args)).

codewalk:walk_code(source, Options) :-
    do_source_walk_code(Options).

head_caller(MHead, M:Head) :-
    '$current_source_module'(CM),
    strip_module(CM:MHead, M, Head).

determine_caller((Head :-  _), Caller) :- !, head_caller(Head, Caller).
determine_caller((Head --> _), Caller) :-
    !,
    extend_args(Head, [_, _], EHead),
    head_caller(EHead, Caller).
determine_caller((:- Decl), Caller) :- !, decl_caller(Decl, Caller).
determine_caller(Head, Caller) :- head_caller(Head, Caller).

decl_caller(initialization(_), '<initialization>').
decl_caller(_,                 '<declaration>').

:- public
    check_trace_reference/3,
    do_term_expansion/1,
    do_goal_expansion/3,
    determine_caller/2.

prepare(To, Undefined, p(TRef, GRef)) :-
    ( To \== (-)
    ->( var(To)
      ->assertz((system:goal_expansion(G, P, _, _) :-
                       '$current_source_module'(M),
                       once(do_goal_expansion(M, G, P)),
                       fail), GRef)
      ; To = _:H
      ->functor(H, F, A),
        functor(G, F, A), % speed up goal expansion
        assertz((system:goal_expansion(G, P, _, _) :-
                     '$current_source_module'(M),
                     check_trace_reference(To, M, G),
                     once(do_goal_expansion(M, G, P)),
                     fail), GRef)
      ; true
      )
    ; Undefined = ignore
    ->true
    ; Undefined = trace
    ->assertz((system:goal_expansion(G, P, _, _) :-
                   '$current_source_module'(M),
                   \+ '$get_predicate_attribute'(M:G, defined, 1),
                   \+ predicate_property(M:G, autoload(_)),
                   once(do_goal_expansion(M, G, P)),
                   fail), GRef)
    ; true
    ),
    ( nonvar(GRef)
    ->assertz((system:term_expansion(T, P, T, P) :-
               do_term_expansion(T)), TRef)
    ; true
    ).

cleanup(p(TRef, GRef)) :-
    ( nonvar(TRef)
    ->erase(TRef)
    ; true
    ),
    ( nonvar(GRef)
    ->erase(GRef)
    ; true
    ).

skip((_,_)).
skip((_;_)).
skip((_->_)).
skip((_*->_)).
skip(\+(_)).
skip(module(_, _)).
skip(module(_, _, _)).
skip(_:_).

check_file(File) :-
    current_context_value(file, File),
    prolog_load_context(source, File).

do_term_expansion(Term) :-
    check_file(_),
    determine_caller(Term, Caller),
    set_context_value(caller, Caller).

check_trace_reference(To, M, Goal) :-
    (   subsumes_term(To, M:Goal)
    ->  true
    ;   predicate_property(M:Goal, imported_from(M2)),
        subsumes_term(To, M2:Goal)
    ).

do_goal_expansion(M, Goal, TermPos) :-
    check_file(File),
    \+ skip(Goal),
    ( TermPos \= none
    ->From = file_term_position(File, TermPos)
    ; prolog_load_context(term_position, Pos),
      stream_position_data(line_count, Pos, Line),
      From = file(File, Line, -1, _)
    ),
    current_context_value(on_trace, OnTrace),
    current_context_value(caller,   Caller),
    call(OnTrace, M:Goal, Caller, From).

do_source_walk_code(Options1) :-
    foldl(select_option_default,
          [on_trace(OnTrace)-(codewalk:true_3),
           trace_reference(To)-To,
           undefined(Undefined)-ignore,
           variable_names(VNL)-VNL],
          Options1, Options2),
    option_filechk(Options2, Options, MFileChk),
    freeze(VNL, b_setval('$variable_names', VNL)),
    with_context_values(
        setup_call_cleanup(
            ( '$current_source_module'(OldM),
              freeze(M, '$set_source_module'(_, M)),
              prepare(To, Undefined, Ref)
            ),
            walk_source(M, File, MFileChk, [variable_names(VNL)|Options]),
            ( '$set_source_module'(_, OldM),
              cleanup(Ref)
            )),
        [file, on_trace],
        [File, OnTrace]).

walk_source(M, File, MFileChk, Options) :-
    forall(call(MFileChk, M, File),
           setup_call_cleanup(
               prolog_open_source(File, In),
               fetch_term(In, Options),
               prolog_close_source(In))).

fetch_term(In, Options1) :-
    foldl(select_option_default,
          [subterm_positons(TermPos)-TermPos,
           term_position(Pos)-Pos,
           syntax_errors(SE)-dec10,
           process_comment(PC)-false,
           comments(C)-C
          ], Options1, Options2),
    Options = [subterm_positions(TermPos),
               syntax_errors(SE),
               term_position(Pos),
               process_comment(PC),
               comments(C)
               |Options2
              ],
    repeat,
      read_clause(In, Term, Options),
      prolog_xref:update_condition(Term),
      '$current_source_module'(M),
      prolog_xref:current_condition(Cond),
      ( M:Cond
      ->prolog_source:expand(Term, TermPos, In, Expanded),
        prolog_source:update_state(Term, Expanded, M)
      ; true
      ),
      Term == end_of_file,
    !.
