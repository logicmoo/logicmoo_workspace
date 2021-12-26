/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2001-2020, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

:- module(prolog_trace_utils,
          [ trace_setting/2,            % ?Name, ?Value
            trace_setting/3,            % +Name, -Old, +New
            setting/2,                  % +Name, +Value

            canonical_source_file/2,    % +RawFile, -CanonicalFile

            find_source/3,              % +Head, -File|TextBuffer, -Line
            thread_self_id/1            % -Name|Int
          ]).
:- use_module(library(pce),[send/2,pce_open/3,op(_,_,_)]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(listing),[portray_clause/1]).
:- autoload(library(lists),[member/2]).
:- autoload(library(portray_text),[portray_text/1, '$portray_text_enabled'/1]).
:- autoload(library(prolog_clause),[predicate_name/2]).
:- autoload(library(readutil),[read_file_to_terms/3]).

:- use_module(library(pce_config), []). % Get config path alias
:- meta_predicate
    find_source(:, -, -).


                 /*******************************
                 *           SETTINGS           *
                 *******************************/

:- dynamic
    setting/2.                      % what, value

setting(active,            true).       % actually use this tracer
setting(show_unbound,      false).      % show unbound variables
setting(cluster_variables, true).       % cluster variables
setting(list_max_clauses,  25).         % only list this amount of clauses
setting(stack_depth,       10).         % # frames shown
setting(choice_depth,      10).         % # choice-points shown
setting(term_depth,        2).          % nesting for printing terms
setting(portray_codes,     Val) :-
    '$portray_text_enabled'(Val).
setting(auto_raise,        true).       % automatically raise the frame
setting(auto_close,        true).       % automatically raise the frame
setting(console_actions,   false).      % map actions from the console
setting(use_pce_emacs,     true).       % use PceEmacs editor

trace_setting(Name, Value) :-
    setting(Name, Value).

trace_setting(Name, Old, New) :-
    setting(Name, Old),
    Old == New,
    !.
trace_setting(portray_codes, Old, New) :-
    !,
    setting(portray_codes, Old),
    portray_text(New).
trace_setting(Name, Old, New) :-
    clause(setting(Name, Old), true, Ref),
    !,
    erase(Ref),
    assertz(setting(Name, New)).
trace_setting(Name, Old, _) :-
    setting(Name, Old).

save_trace_settings :-
    absolute_file_name(config('Tracer.cnf'), Path,
                       [ access(write),
                         file_errors(fail)
                       ]),
    !,
    setup_call_cleanup(
        open(Path, write, Out),
        forall(( setting(Name, Value),
                 \+ no_save(Name)
               ),
               format(Out, '~q.~n', setting(Name, Value))),
        close(Out)).
save_trace_settings.

no_save(active).

load_trace_settings :-
    read_file_to_terms(config('Tracer.cnf'), Terms,
                       [ file_errors(fail)
                       ]),
    !,
    forall(member(setting(Name, Value), Terms),
           trace_setting(Name, _, Value)).
load_trace_settings.

:- initialization load_trace_settings.
:- at_halt(save_trace_settings).


                 /*******************************
                 *      SOURCE LOCATIONS        *
                 *******************************/

%!  find_source(:HeadTerm, -File, -Line) is det.
%
%   Finds the source-location of the predicate.  If the predicate is
%   not    defined,    it    will    list     the    predicate    on
%   @dynamic_source_buffer and return this buffer.

find_source(Predicate, File, Line) :-
    predicate_property(Predicate, file(File)),
    predicate_property(Predicate, line_count(Line)),
    !.
find_source(Predicate, File, 1) :-
    debug(gtrace(source), 'No source for ~p', [Predicate]),
    File = @dynamic_source_buffer,
    send(File, clear),
    setup_call_cleanup(
        pce_open(File, write, Fd),
        with_output_to(Fd, list_predicate(Predicate)),
        close(Fd)).

list_predicate(Predicate) :-
    predicate_property(Predicate, foreign),
    !,
    predicate_name(user:Predicate, PrintName),
    send(@dynamic_source_buffer, attribute, comment,
         string('Can''t show foreign predicate %s', PrintName)).
list_predicate(Predicate) :-
    predicate_name(user:Predicate, PrintName),
    setting(list_max_clauses, Max),
    '$get_predicate_attribute'(Predicate, number_of_clauses, Num),
    (   Num > Max
    ->  Upto is Max - 1,
        list_clauses(Predicate, 1, Upto),
        Skipped is Num - Max,
        format('~n% <skipped ~d clauses>~n~n', [Skipped]),
        list_clauses(Predicate, Num, Num),
        send(@dynamic_source_buffer, attribute, comment,
             string('Partial decompiled listing of %s', PrintName))
    ;   list_clauses(Predicate, 1, Num),
        send(@dynamic_source_buffer, attribute, comment,
             string('Decompiled listing of %s', PrintName))
    ).


list_clauses(Predicate, From, To) :-
    between(From, To, Nth),
        nth_clause(Predicate, Nth, Ref),
        clause(RawHead, Body, Ref),
        strip_module(user:RawHead, Module, Head),
        tag_module(Module),
        portray_clause((Head :- Body)),
    fail.
list_clauses(_, _, _).

tag_module(Module) :-
    prolog_clause:hidden_module(Module),
    !. % dubious
tag_module(Module) :-
    format('~q:', Module).

                 /*******************************
                 *           SOURCE FILE        *
                 *******************************/

%!  canonical_source_file(+Raw, -Cononical)
%
%   Determine the internal canonical filename from a raw file.

canonical_source_file(Source, File) :-
    absolute_file_name(Source, Canonical),
    (   source_file(Canonical)
    ->  File = Canonical
    ;   file_base_name(Source, Base),
        source_file(File),
        file_base_name(File, Base),
        same_file(Source, File)
    ->  true
    ;   File = Source               % system source files
    ).

%!  thread_self_id(-Id)
%
%   Get the current thread as atom  or   integer.  This is needed to
%   pass the thread id through XPCE. If   the caller is an engine we
%   return the id of the calling thread.

thread_self_id(Id) :-
    thread_self(Self),
    real_thread(Self, Thread),
    debug(gtrace(thread), 'real_thread: ~p --> ~p', [Self, Thread]),
    (   atom(Thread)
    ->  Id = Thread
    ;   thread_property(Thread, id(Id))
    ).

:- if(current_predicate(engine_create/3)).
real_thread(Self, Thread) :-
    thread_property(Self, thread(Thread)),
    !.
:- endif.
real_thread(Thread, Thread).
