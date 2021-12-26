/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2001-2012, University of Amsterdam,
                              VU University Amsterdam
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

:- module(pce_prolog_clause,
          [ pce_clause_info/4,          % +ClauseRef, -File, -TermPos, -VarNames
            clear_clause_info_cache/0,
            predicate_classification/2  % +Goal, -Classification
          ]).
:- use_module(library(pce)).
:- use_module(library(debug)).
:- use_module(library(listing)).
:- use_module(library(prolog_clause)).

/** <module> Clause info for the source level debugger

This module wraps library(prolog_clause) to deal  with clauses that have
no source or for which clause_info/4  fails to provide information about
the clause because it  was  modified   by  e.g.,  term_expansion/2 in an
untracktable way.
*/

%!  prolog:term_compiled(+Source, -Compiled) is semidet.
%!  prolog:term_compiled(-Source, +Compiled) is semidet.
%
%   This hook supports debugging in higher  level languages that are
%   compiled to plain Prolog and whose source is expressed in Prolog
%   terms. The first version compiles a  target language term into a
%   plain Prolog term and can be   compared to term_expansion/2. The
%   second decompiles a Prolog clause into a source-term.
%
%   @tbd    This hook is experimental. It   was added for supporting
%           debugging dynamic objects in  Logtalk.   It  may also be
%           useful for listing/1. We might   add  additional context
%           information, such as the clause reference (if known).

:- multifile
    prolog:term_compiled/2.

:- pce_global(@dynamic_source_buffer,
              make_dynamic_source_buffer).

make_dynamic_source_buffer(B) :-
    start_emacs,
    new(B, emacs_buffer(@nil, '*dynamic code*')).

                 /*******************************
                 *             CACHE            *
                 *******************************/

:- dynamic
    clause_info_cache/4,
    dynamic_info_cache/5.

:- initialization
    prolog_unlisten(erase, clear_clause_info_cache),
    prolog_listen(erase, clear_clause_info_cache).

clear_clause_info_cache(Ref) :-
    retractall(clause_info_cache(Ref, _, _, _)).

clear_clause_info_cache :-
    retractall(clause_info_cache(_, _, _, _)).

%       clause_info(+ClauseRef, -File, -TermPos, -VarNames)
%
%       Fetches source information for the given clause.

pce_clause_info(ClauseRef, File, TermPos, NameOffset) :-
    clause_info_cache(ClauseRef, File, TermPos, NameOffset),
    \+ clause_property(ClauseRef, erased),
    !,
    debug(clause_info, 'clause_info(~p): from cache', [ClauseRef]).
pce_clause_info(ClauseRef, File, TermPos, NameOffset) :-
    clause_info(ClauseRef, File, TermPos, NameOffset),
    !,
    asserta(clause_info_cache(ClauseRef, File, TermPos, NameOffset)),
    debug(clause_info, 'Added to info-cache', []).
pce_clause_info(ClauseRef, S, TermPos, NameOffset) :-
    dynamic_info_cache(ClauseRef, S, TermPos, NameOffset, Gen),
    object(S),
    get(S, generation, Gen),
    !,
    debug(clause_info, 'clause_info(~p): from dynamic cache', [ClauseRef]).
pce_clause_info(ClauseRef, S, TermPos, NameOffset) :-
    retractall(dynamic_info_cache(_,_,_,_,_)),
    setup_call_cleanup(
        '$push_input_context'(clause_info),
        pce_clause_info_2(ClauseRef, S, TermPos, NameOffset),
        '$pop_input_context'),
    get(S, generation, Gen),
    asserta(dynamic_info_cache(ClauseRef, S, TermPos, NameOffset, Gen)).

pce_clause_info_2(ClauseRef, S, TermPos, NameOffset) :-
    debug(clause_info, 'Listing for clause ~p', [ClauseRef]),
    '$clause'(Head, Body, ClauseRef, VarOffset),
    (   Body == true
    ->  Clause0 = Head
    ;   Clause0 = (Head :- Body)
    ),
    start_emacs,
    S = @dynamic_source_buffer,
    clause_name(ClauseRef, ClauseName),
    send(S, attribute, comment,
         string('Decompiled listing of %s', ClauseName)),
    send(S, clear),
    debug(clause_info, 'Writing clause ~p to string ~p ... ', [ClauseRef, S]),
    (   prolog:term_compiled(Clause, Clause0)
    ->  true
    ;   Clause = Clause0
    ),
    setup_call_cleanup(
        pce_open(S, write, Fd),
        portray_clause(Fd, Clause, [max_depth(10)]),
        close(Fd)),
    debug(clause_info, 'ok, reading ... ', []),
    setup_call_cleanup(
        pce_open(S, read, Handle),
        read_source_term_at_location(Handle, ReadClause,
                                     [ module(user),
                                       subterm_positions(TermPos),
                                       variable_names(VarNames)
                                     ]),
        close(Handle)),
    prolog_clause:unify_term(Clause, ReadClause),
    debug(clause_info, 'ok ...', []),
    prolog_clause:make_varnames(Clause, Clause, VarOffset, VarNames, NameOffset),
    debug(clause_info, 'got names', []),
    !.


predicate_classification(Goal, Style) :-
    predicate_property(Goal, Prop),
    map_property(Prop, Style),
    !.
predicate_classification(_, user).

style_property(built_in).
style_property(foreign).
style_property(dynamic).
style_property(undefined).
style_property(transparent).

map_property(Prop, Prop) :-
    style_property(Prop),
    !.


