/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2013, University of Amsterdam
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

:- module(pldoc_register,
          [ process_stored_comments/0
          ]).
:- use_module(doc_process).
:- use_module(library(pldoc)).
:- use_module(library(debug)).
:- set_prolog_flag(generate_debug_info, false).

pldoc_module(pldoc_modes).              % avoid recursive behaviour
pldoc_module(pldoc_process).
pldoc_module(pldoc_wiki).
pldoc_module(pldoc).
pldoc_module(lists).
pldoc_module(apply).
pldoc_module(pairs).

                 /*******************************
                 *            REGISTER          *
                 *******************************/

:- multifile
    prolog:comment_hook/3,
    user:message_hook/3.

:- dynamic
    mydoc/3.                        %  +Comments, +TermPos, +File

do_comment_hook(_, _, _, _) :-
    current_prolog_flag(pldoc_collecting, false),
    !.
do_comment_hook(Comments, TermPos, File, _) :-
    pldoc_loading,
    !,
    assert(mydoc(Comments, TermPos, File)).
do_comment_hook(Comments, TermPos, File, _Term) :-
    \+ current_prolog_flag(xref, true),
    prolog_load_context(module, Module),
    pldoc_module(Module),
    !,
    assert(mydoc(Comments, TermPos, File)).
do_comment_hook(Comments, TermPos, File, _) :-
    process_comments(Comments, TermPos, File).

user:message_hook(load_file(done(0, _F, _A, _M, _T, _H)), _, _) :-
    (   mydoc(_, _, _)
    ->  debug(pldoc, 'Processing delayed comments', []),
        process_stored_comments
    ),
    fail.

%!  prolog:comment_hook(+Comments, +TermPos, +Term) is det.
%
%   Hook called by the compiler and cross-referencer. In addition to
%   the comment, it passes the  term  itself   to  see  what term is
%   commented  as  well  as  the  start-position   of  the  term  to
%   distinguish between comments before the term and inside it.
%
%   @param Comments List of comments read before the end of Term
%   @param TermPos  Start location of Term
%   @param Term     Actual term read

prolog:comment_hook(Comments, TermPos, Term) :-
    source_location(File, _TermLine),
    setup_call_cleanup(
        '$push_input_context'(pldoc),  % Preserve input file and line
        do_comment_hook(Comments, TermPos, File, Term),
        '$pop_input_context').

process_stored_comments :-
    forall(retract(mydoc(Comments, TermPos, File)),
           delayed_process(Comments, TermPos, File)).

delayed_process(Comments, TermPos, File) :-
    module_property(Module, file(File)),
    setup_call_cleanup(
        '$set_source_module'(Old, Module),
        process_comments(Comments, TermPos, File),
        '$set_source_module'(_, Old)).
