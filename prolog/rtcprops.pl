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

:- module(rtcprops, [rtcheck/1, rtcheck/2, no_rtcheck/1, rtc_status/1]).

:- use_module(library(assertions)).
:- use_module(library(metaprops)).
:- use_module(library(typeprops)).
:- use_module(library(foreign/foreign_props)).

:- true comp [(global)/1,
              (global)/2,
              database/1,
              is_pred/2,
              mod_qual/1,
              mod_qual/2,
              meta_modes/1,
              no_meta_modes/1,
              (declaration)/1,
              (declaration)/2,
              compat/1,
              instance/1,
              (native)/1,
              (native)/2,
              foreign/1,
              foreign/2,
              fimport/1,
              fimport/2,
              returns/2,
              parent/2,
              returns_state/1,
              memory_root/1,
              nfi/2,
              fi/2] + no_rtcheck.

:- type rtc_status/1.

%!  rtc_status(Status)
%
%   Status of the runtime-check
%   implementation for a given property. Valid values are:
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

rtc_status(unimplemented).
rtc_status(incomplete).
rtc_status(complete).
rtc_status(unknown).
rtc_status(exhaustive).
rtc_status(impossible).

:- global rtcheck(Status, G) : rtc_status * callable
    # "The runtime check of ~w have the status ~w."-[G, Status].

:- true comp (rtcheck)/2.

rtcheck(_, Goal) :- call(Goal).

:- global rtcheck(G) # "Equivalent to rtcheck(~w, complete)."-[G].

:- true comp (rtcheck)/1.

rtcheck(Goal) :- rtcheck(complete, Goal).

:- global no_rtcheck(G)
    # "Declares that the assertion in which this comp property appears must not
    be checked at run-time.  Equivalent to rtcheck(~w, impossible)."-[G].

:- true comp (no_rtcheck)/1.

no_rtcheck(Goal) :- rtcheck(impossible, Goal).
