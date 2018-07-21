/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.
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

:- module(expansion_module,
          [expansion_module/2,
           is_expansion_module/1]).

:- use_module(library(option)).

expansion_module(EM, EM1, L) :-
    current_op(1, fx, EM1:'$compound_expand'),
    '$load_context_module'(CF, EM1, Opts),
    module_property(CM, file(CF)),
    ( CM = compound_expand
    ->EM = EM1
    ; option(reexport(true), Opts),
      expansion_module(EM, CM, [EM1|L])
    ),
    \+ memberchk(EM, L).

%!  expansion_module(+Module, ExpansionModule)
%
%   Kludge: using swipl internals. Perhaps is not a good idea --EMM
%   Warning: could report duplicate solutions
%
expansion_module(M, EM) :-
    '$load_context_module'(EF, M, _),
    module_property(EM1, file(EF)),
    expansion_module(EM, EM1, [M]).

is_expansion_module(EM) :-
    current_op(1, fx, EM:'$compound_expand'),
    CM = compound_expand,
    module_property(CM, file(CF)),
    '$load_context_module'(CF, EM, _).
