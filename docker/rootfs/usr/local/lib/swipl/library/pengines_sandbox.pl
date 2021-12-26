/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017, VU University Amsterdam
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

:- module(pengines_sandbox, []).
:- use_module(library(pengines), []).
:- autoload(library(error),[instantiation_error/1]).
:- autoload(library(lists),[append/3]).

/** <module> Declare Pengine interaction sandbox-safe

This module can be  loaded  alongside   library(pengines)  to  allow for
calling remote pengines from a sandboxed environment. This is disallowed
by default because one of the  use-cases   of  sandboxed  pengines is to
provide a generic application interface. In such   cases you do not want
the application to act as a proxy, in particular not to other systems in
a protected network.
*/

                 /*******************************
                 *    PENGINE SANDBOX SUPPORT   *
                 *******************************/

:- multifile
    sandbox:safe_primitive/1,               % Goal
    sandbox:safe_meta/2.                    % Goal, Calls

%!  sandbox:safe_primitive(+Goal) is semidet.
%
%   Declare the core pengine operations as   safe. If we are talking
%   about  local  pengines,  their  safety   is  guaranteed  by  the
%   sandboxing done for all pengines.
%
%   @tbd    If at some point we allow for `unsafe' pengines, we must
%           reconsider this.

sandbox:safe_primitive(pengines:pengine_destroy(_,_)).
sandbox:safe_primitive(pengines:pengine_event(_, _)).
sandbox:safe_primitive(pengines:pengine_send(_, _, _)).
sandbox:safe_primitive(pengines:pengine_ask(_, _, _)).
sandbox:safe_primitive(pengines:pengine_pull_response(_,_)).
sandbox:safe_primitive(pengines:pengine_user(_)).
sandbox:safe_primitive(system:'#file'(_,_)).

%!  sandbox:safe_meta(+Goal, -Called) is semidet.
%
%   Declare the pengine  meta-predicates  as   safe.  Note  that the
%   pengine calling predicates  are  safe   because  the  safety  is
%   guaranteed by the recieving pengine.

sandbox:safe_meta(pengines:pengine_create(_), []).
sandbox:safe_meta(pengines:pengine_rpc(_, _, _), []).
sandbox:safe_meta(pengines:pengine_event_loop(_,Closure,_,_), [Closure1]) :-
    extend_goal(Closure, [_], Closure1).

extend_goal(Var, _, _) :-
    var(Var),
    !,
    instantiation_error(Var).
extend_goal(M:Term0, Extra, M:Term) :-
    extend_goal(Term0, Extra, Term).
extend_goal(Atom, Extra, Goal) :-
    atom(Atom),
    !,
    Goal =.. [Atom|Extra].
extend_goal(Compound, Extra, Goal) :-
    compound(Compound),
    !,
    compound_name_arguments(Compound, Name, Args0),
    append(Args0, Extra, Args),
    compound_name_arguments(Goal, Name, Args).
