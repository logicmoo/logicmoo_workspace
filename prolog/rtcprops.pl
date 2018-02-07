/*  Part of Run-Time Checker for Assertions

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
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

:- module(rtcprops, [acheck/1, acheck/2, acheck/3, no_acheck/1, no_acheck/2]).

:- use_module(library(assertions)).
:- use_module(library(metaprops)).
:- use_module(library(globprops)).
:- use_module(library(foreign/foreign_props)).

:- true comp [(global)/1,
              (global)/2,
              database/1,
              meta_modes/1,
              no_meta_modes/1,
              (declaration)/1,
              (declaration)/2,
              compat/1,
              instan/1,
              (native)/1,
              (native)/2,
              foreign/1,
              foreign/2,
              fimport/1,
              fimport/2,
              returns/2,
              parent/2,
              returns_state/1,
              memory_root/1] + no_acheck(rt).

:- type acstatus/1.

%!  acstatus(Status)
%
%   Status of the assertion checker for a given property. Valid values are:
%
%   - unimplemented: No run-time checker has been implemented for the property.
%                    Althought it can be implemented further.
%
%   - incomplete: The current run-time checker is incomplete, which means, under
%                 certain circunstances, no error is reported if the property is
%                 violated.
%
%   - unknown: We do not know if current implementation of run-time checker is
%              complete or not.
%
%   - complete: The opposite of incomplete, error is reported always
%               that the property is violated. Default.
%
%   - impossible: The property must not be run-time checked (for theoretical or
%                 practical reasons).
%
%

acstatus(unimplemented).
acstatus(incomplete).
acstatus(complete).
acstatus(unknown).
acstatus(exhaustive).
acstatus(impossible).

:- type ctrt/1.

ctrt(ct).
ctrt(rt).

:- global acheck(T, Status, G) : ctrt * acstatus * callable
    # "The ~w assertion check of ~w has the status ~w."-[T, G, Status].

acheck(_, _, Goal) :- call(Goal).

:- global acheck(T, G) + equiv(acheck(T, complete, G))
   # "Equivalent to acheck(~w, complete, ~w)."-[T, G].

acheck(_, Goal) :- call(Goal).

:- global acheck(G) + equiv(acheck(ct, acheck(rt, G))).

acheck(Goal) :- call(Goal).

:- global no_acheck(T, G) + equiv(acheck(T, impossible, G))
    # "Declares that the assertion in which this comp property appears must not
    be checked at run-time.".

no_acheck(_, Goal) :- call(Goal).

:- global no_acheck(G) + equiv(noacheck(ct, noacheck(rt, G))).

no_acheck(Goal) :- call(Goal).
