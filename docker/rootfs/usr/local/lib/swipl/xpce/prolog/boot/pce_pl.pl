/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1985-2020, University of Amsterdam
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

:- module(pce_host, []).
:- require([ send/2,
	     get/3
	   ]).
:- public
    property/1.

                 /*******************************
                 *          PROPERTIES          *
                 *******************************/

property(prolog(swi)).                  % this is SWI-Prolog
property(file_extensions([pl])).        % list of file extensions
property(use_predicate_references).     % use direct predicate refs in methods
property(register_source_locations).    % register the source locations
property(string).                       % Supports string datatype
property(runtime) :-
    get(@(pce), is_runtime_system, @(on)).


                 /*******************************
                 *           ERRORS             *
                 *******************************/

:- consult('../lib/swi_compatibility').


                 /*******************************
                 *             ABORT            *
                 *******************************/

:- multifile
    user:message_hook/3.
:- dynamic
    user:message_hook/3.

user:message_hook('$aborted', _Kind, _Lines) :-
    current_prolog_flag(xpce, true),
    send(@(display), reset),
    fail.
