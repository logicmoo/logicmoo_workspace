/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  1996-2012, University of Amsterdam
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

:- module(pce_compatibility_layer,
          [ auto_call/1,
            callable_predicate/1,
            modified_since_last_loaded/1,
            pce_error/1,
            pce_warn/1,
            pce_info/1
          ]).

/** <module> XPCE Compatibility layer

This layer defines some predicates to   enhance portability with SICStus
and Quintus Prolog. These systems are  no   longer  supported, but it is
probably wise to keep this layer for `just-in-case'.
*/

:- meta_predicate
    auto_call(0),
    callable_predicate(:).

%!  auto_call(:Goal)
%
%   Autoload Goal and call it. If autoloading   is enabled we can simply
%   call  the  target.  Otherwise   we    autoload   the  predicate  and
%   subsequently call it.
%
%   This predicate should be used to open new IDE tools, for example the
%   manual opening the editor, etc.

auto_call(G) :-
    current_prolog_flag(autoload, true),
    !,
    call(G).
auto_call(G) :-
    '$pi_head'(PI, G),
    current_prolog_flag(autoload, Old),
    setup_call_cleanup(
        asserta(user:thread_message_hook(autoload(disabled(_Count)),_,_), Ref),
        setup_call_cleanup(
            set_prolog_flag(autoload, true),
            '$autoload'(PI),
            set_prolog_flag(autoload, Old)),
        erase(Ref)),
    call(G).


                 /*******************************
                 *      DIALOG EDITOR SUPPORT   *
                 *******************************/

%!  callable_predicate(:Head) is semidet.
%
%   Succeeds if Head can be called without raising an exception for
%   an undefined predicate

callable_predicate(M:Head) :-
    callable(Head),
    functor(Head, Name, Arity),
    current_predicate(M:Name/Arity).

%!  modified_since_last_loaded(Path) is semidet.
%
%   True is file has been modified since the last time it was loaded.

modified_since_last_loaded(File) :-
    '$time_source_file'(File, LoadTime, user),
    !,
    time_file(File, Modified),
    Modified > LoadTime.
modified_since_last_loaded(InFile) :-
    '$time_source_file'(File, LoadTime, user),
    same_file(InFile, File),
    !,
    time_file(File, Modified),
    Modified > LoadTime.


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

%!  pce_error(+Term) is det.
%!  pce_warn(+Term) is det.
%!  pce_info(+Term) is det.
%
%   Portability layer wrappers around print_message/2.

pce_error(Term) :-
    (   current_prolog_flag(xref, true)
    ->  true
    ;   print_message(error, Term)
    ).

pce_warn(Term) :-
    (   current_prolog_flag(xref, true)
    ->  true
    ;   print_message(warning, Term)
    ).

pce_info(Term) :-
    print_message(informational, Term).


