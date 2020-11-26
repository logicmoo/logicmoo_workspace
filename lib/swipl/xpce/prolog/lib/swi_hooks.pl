/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cwi.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2020, University of Amsterdam
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

:- module(pce_swi_hooks, []).
:- autoload(swi_compatibility, [auto_call/1]).

/** <module> Hook XPCE based graphics tools into IDE

Loading this file enables the graphical  frontends for the online manual
and profiler. This file is normally   loaded from swipl.rc (swipl-win.rc
on Windows); the file that makes XPCE known to Prolog.

This file uses call/1 to call the real work to avoid undefined predicate
messages when using make/0 (calling list_undefined/0).

Since  the  introduction   of   the    more   advanced   autoloader   in
library(prolog_autoload), using call/1 no longer   suffices to stop this
file from being loaded.
*/

:- set_module(class(development)).

:- multifile
    prolog:debug_control_hook/1,
    prolog:show_profile_hook/1,             % new
    prolog:show_profile_hook/2.             % compatibility


                 /*******************************
                 *          DEBUG HOOKS         *
                 *******************************/

prolog:debug_control_hook(spy(Method)) :-
    auto_call(spypce(Method)).
prolog:debug_control_hook(nospy(Method)) :-
    auto_call(nospypce(Method)).


                 /*******************************
                 *           PROFILING          *
                 *******************************/

prolog:show_profile_hook(_Options) :-
    auto_call(pce_show_profile).
prolog:show_profile_hook(_Style, _Top) :-
    auto_call(pce_show_profile).


                 /*******************************
                 *             SOURCE           *
                 *******************************/

%!  prolog:alternate_syntax(?Syntax, +Module, -Setup, -Restore)
%
%   Implements operator handling for reading   arbitrary  terms from
%   XPCE classes.

:- multifile
    prolog:alternate_syntax/4.

prolog:alternate_syntax(pce_class, M, pce_expansion:push_compile_operators(M),
                                      pce_expansion:pop_compile_operators) :-
    current_prolog_flag(xpce, true).

