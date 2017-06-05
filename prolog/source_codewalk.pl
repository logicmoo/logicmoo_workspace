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

:- module(source_codewalk,
          [source_codewalk/1]).

:- use_module(library(prolog_source)).
:- use_module(library(prolog_stack)).
:- use_module(library(prolog_xref), []).
:- use_module(library(context_values)).
:- use_module(library(option_utils)).
:- use_module(library(extend_args)).

:- meta_predicate
    source_codewalk(:).

is_meta(on_trace).

source_codewalk(MOptions) :-
    meta_options(is_meta, MOptions, Options),
    setup_call_cleanup(prepare(Ref),
                       do_source_codewalk(Options),
                       cleanup(Ref)).

head_caller(MHead, M:Head) :-
    '$current_source_module'(CM),
    strip_module(CM:MHead, M, Head).

determine_caller((Head   :-  _), Caller) :- !, head_caller(Head, Caller).
determine_caller((DHead --> _), Caller) :-
    !,
    extend_args(DHead, [_, _], Head),
    head_caller(Head, Caller).
determine_caller(Head, Caller) :- head_caller(Head, Caller).
determine_caller((:- Decl), Caller) :- decl_caller(Decl, Caller).

decl_caller(initialization(_), '<initialization>').
decl_caller(_,                 '<declaration>').

:- public
    true_3/3,
    do_term_expansion/1,
    do_goal_expansion/2,
    determine_caller/2.

prepare(p(TRef, GRef)) :-
    assertz((system:term_expansion(T, P, T, P) :-
                 do_term_expansion(T)), TRef),
    assertz((system:goal_expansion(G, P, _, _) :-
                 once(do_goal_expansion(G, P)), fail), GRef).

cleanup(p(TRef, GRef)) :-
    erase(TRef),
    erase(GRef).

true_3(_, _, _).

skip((_,_)).
skip((_;_)).
skip((_->_)).
skip((_*->_)).
skip(\+(_)).

/*
true_3(Goal, Caller, From) :-
    print_message(information,
                  at_location(From, format("~w :- ~w", [Caller, Goal]))).
*/

check_file(File) :-
    current_context_value(file, File),
    prolog_load_context(source, File).

do_term_expansion(Term) :-
    check_file(_),
    determine_caller(Term, Caller),
    set_context_value(caller, Caller).

do_goal_expansion(Goal, TermPos) :-
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
    '$current_source_module'(M),
    call(OnTrace, M:Goal, Caller, From).

do_source_codewalk(Options1) :-
    foldl(select_option_default,
          [on_trace(OnTrace)  -true_3,
           if(Loaded)-true,
           variable_names(VNL)-VNL],
          Options1, Options2),
    option_allchk(M, File, FileMGen-[if(Loaded)|Options2], true-Options),
    freeze(VNL, b_setval('$variable_names', VNL)),
    with_context_values(
        setup_call_cleanup(
            ( '$current_source_module'(OldM),
              freeze(M, '$set_source_module'(_, M))
            ),
            forall(FileMGen,
                   walk_source(File, [variable_names(VNL)|Options])),
            '$set_source_module'(_, OldM)),
        [file, on_trace],
        [File, OnTrace]).

walk_source(File, Options) :-
    setup_call_cleanup(
        prolog_open_source(File, In),
        fetch_term(In, Options),
        prolog_close_source(In)).

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
