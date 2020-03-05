/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2020, Process Design Center, Breda, The Netherlands.
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

:- module(module_uses,
          [module_uses/2,
           module_uses/3]).

:- use_module(library(codewalk)).

:- meta_predicate
    collect_module_uses(+,0,+,+).

:- dynamic module_uses/4.

module_uses(LoadedIn, Module, Uses) :-
    module_uses_2(m(Module), LoadedIn, Uses).

module_uses(LoadedIn, Uses) :-
    module_uses_2(all, LoadedIn, Uses).

trace_reference(all,  _).
trace_reference(m(M), M:_).

pattern(all,  PI, PI).
pattern(m(M), M:F/A, F/A).

module_uses_2(Collector, LoadedIn, Uses) :-
    trace_reference(Collector, TR),
    pattern(Collector, M:F/A, Pattern),
    walk_code([module(LoadedIn),
               source(false),
               method(clause),
               trace_reference(TR),
               on_trace(collect_module_uses(LoadedIn))
              ]),
    findall(Pattern, retract(module_uses(LoadedIn, M, F, A)), UsesU),
    sort(UsesU, Uses).

:- public
    collect_module_uses/4.

collect_module_uses(LoadedIn, MGoal, _, _) :-
    strip_module(MGoal, _, Goal),
    functor(Goal, F, A),
    predicate_property(MGoal, implementation_module(Module)),
    assertz(module_uses(LoadedIn, Module, F, A)).
