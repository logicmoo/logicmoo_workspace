/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2004-2015, University of Amsterdam
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

:- module(emacs_chr_mode, []).
:- use_module(library(pce)).
:- use_module(prolog_mode).
:- use_module(library(operators)).      % push/pop operators
:- use_module(library(chr)).            % get CHR operators.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module deals with colourisation of  .chr files. CHR introduces many
operators and requires different rules for colouring objects.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


                 /*******************************
                 *            CHR MODE          *
                 *******************************/

:- emacs_begin_mode(chr, prolog,
                    "Mode for editing Constraint Handling Rules (CHR) documents",
                    % BINDINGS
                    [
                    ],
                    % SYNTAX TABLE
                    [
                    ]).

colourise_buffer(M) :->
    "Cross-reference the buffer and set up colours"::
    push_chr_operators,
    call_cleanup(send_super(M, colourise_buffer),
                 pop_chr_operators).

:- emacs_end_mode.


                 /*******************************
                 *         SYNTAX HOOKS         *
                 *******************************/

:- multifile
    emacs_prolog_mode:alternate_syntax/3.


emacs_prolog_mode:alternate_syntax(chr,
                                   emacs_chr_mode:push_chr_operators,
                                   emacs_chr_mode:pop_chr_operators).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Note that we could generalise this to deal with all included files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
    chr_operators/1.

push_chr_operators :-
    (   chr_operators(Ops)
    ->  true
    ;   init_chr_operators(Ops),
        assert(chr_operators(Ops))
    ),
    '$set_source_module'(SM, SM),
    push_operators(SM:Ops).

pop_chr_operators :-
    pop_operators.

init_chr_operators(Ops) :-
    absolute_file_name(library('chr/chr_op'),
                       [ file_type(prolog),
                         access(read),
                         file_errors(fail)
                       ],
                       OpFile),
    open(OpFile, read, In),
    read(In, Term),
    read_ops(Term, In, Ops),
    close(In).

read_ops(end_of_file, _, []) :- !.
read_ops((:- op(Pre, Ass, Ops)), In, [ op(Pre, Ass, Ops) |T]) :-
    !,
    read(In, T2),
    read_ops(T2, In, T).
read_ops(_, In, Ops) :-
    read(In, T2),
    read_ops(T2, In, Ops).
