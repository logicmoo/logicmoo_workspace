/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(emacs_xsb_mode, []).
:- use_module(library(pce)).
:- use_module(prolog_mode).
:- use_module(library(prolog_xref)).
:- use_module(library(operators)).      % push/pop operators
:- use_module(library(trace/emacs_debug_modes)).
:- use_module(library(dialect/xsb), []).

/** <module> Colour XSB source files
*/

                 /*******************************
                 *            LOGTALK MODE              *
                 *******************************/

:- emacs_begin_mode(xsb, prolog,
                    "Mode for editing XSB dialect files",
                    % BINDINGS
                    [
                    ],
                    % SYNTAX TABLE
                    [
                    ]).

operators(_M, Ops:prolog) :<-
    "Get mode specific operators"::
    xsb_operators(Ops).

:- emacs_end_mode.


                 /*******************************
                 *         SYNTAX RULES         *
                 *******************************/

:- multifile
    prolog_colour:style/2,
    prolog_colour:term_colours/2,
    prolog_colour:goal_colours/2,
    prolog_colour:directive_colours/2,
    prolog_colour:goal_classification/2,
    prolog_colour:message//1.

:- op(1040, xfx, from).

prolog_colour:goal_colours(import(Preds from Module),
                           xsb-[ keyword(from)-[ Imports,
                                                 File
                                               ]
                               ]) :-
    xsb_module_file(Module, File),
    xsb_imports(Preds, File, Imports).

xsb_module_file(usermod, module(user)) :-
    !.
xsb_module_file(Module, file(Path)) :-
    prolog_load_context(file, Here),
    absolute_file_name(Module, Path,
                       [ extensions(['P', pl, prolog]),
                         access(read),
                         relative_to(Here),
                         file_errors(fail)
                       ]),
    !.
xsb_module_file(Module, file(Path)) :-
    (   Spec = library(dialect/xsb/Module)
    ;   Spec = library(Module)
    ),
    absolute_file_name(Spec, Path,
                       [ extensions(['P', pl, prolog]),
                         access(read),
                         file_errors(fail)
                       ]),
    !.
xsb_module_file(_Module, nofile).

xsb_imports(_, nofile, predicates) :-
    !.
xsb_imports(Var, _, classify) :-
    var(Var),
    !.
xsb_imports((A,B), From, functor-[CA, CB]) :-
    xsb_imports(A, From, CA),
    xsb_imports(B, From, CB).
xsb_imports(_, file(Path), import(Path)).

prolog_colour:style(goal(xsb, _), [colour(blue), underline(true)]).

prolog_colour:message(goal(xsb, _)) -->
    [ 'XSB emulated' ].


                 /*******************************
                 *         SYNTAX HOOKS         *
                 *******************************/

:- multifile
    prolog:alternate_syntax/4.

prolog:alternate_syntax(xsb, Module,
                        emacs_xsb_mode:push_xsb_operators(Module),
                        emacs_xsb_mode:pop_xsb_operators).

:- public
    push_xsb_operators/0,
    pop_xsb_operators/0.

push_xsb_operators :-
    '$set_source_module'(M, M),
    push_xsb_operators(M).

push_xsb_operators(Module) :-
    xsb_operators(Ops),
    push_operators(Module:Ops).

pop_xsb_operators :-
    pop_operators.

xsb_operators([
    op(1050,  fy, import),
    op(1100,  fx, export),
    op(1100,  fx, mode),
    op(1040, xfx, from),
    op(1100,  fy, index),
    op(1100,  fy, ti),
    op(1045, xfx, as),
    op(900,   fy, tnot)
]).
