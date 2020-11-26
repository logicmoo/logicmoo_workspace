/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1985-2002, University of Amsterdam
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

%-----------------------------------------------
%   Module Definitions
%-----------------------------------------------

:- module(scan_arguments,
        [ scan_arguments/2      % Arguments x ValueList
        , scan_arguments/3      % Arguments x ValueList -> Rest
        ]).

:- use_module(library(pce)).
:- require([ select/3
           ]).


%-----------------------------------------------
%   Scan Arguments
%-----------------------------------------------

%%   scan_arguments(+Args, [+Name=-Value/+Default])
%%   scan_arguments(+Args, +Spec, -Rest)
%
%   scan_arguments/2 is used to scan a list of arguments and assign
%   values to variables.  Args is a list of instantiated arguments
%   provided by the caller, the second argument contains name/value
%   pairs where the value is unified with the value given in Args.
%   The version Name=Value/Default allows Name to be omitted in Args
%   and still bind Default to Value.
%
%   scan_arguments/3 is equivalent to scan_arguments/2, but unprocessed
%   arguments are bound to `Rest' instead of printing an error message.
%
%   Error messages are printed on missing arguments.
%
%       ?- scan_arguments([hello=world], [hello=X])
%
%       X = world
%
%       ?- scan_arguments([name=anjo], [name=N, city=C/amsterdam]).
%
%       N = anjo
%       C = amsterdam

scan_arguments(Args, List, Rest) :-
    get_arguments(List, Args, Rest).

scan_arguments(Args, List) :-
    get_arguments(List, Args, Rest),
    (   Rest == []
    ->  true
    ;   format(user_error,
               'scan_arguments:Arguments not required: ~w~n', Rest)
    ).

get_arguments([], Args, Args) :- !.
get_arguments([Name = Value|T], Args, RestArgs) :-
    non_default_argument(Value),
    !,
    (   select(Name=Value, Args, Rest)
    ->  get_arguments(T, Rest, RestArgs)
    ;   format(user_error,
               'Argument ~w not present and no default defined', [Name])
    ).
get_arguments([Name = Value / Default|T], Args, RestArgs) :-
    (   select(Name=Value, Args, Rest)
    ->  get_arguments(T, Rest, RestArgs)
    ;   Value = Default,
        get_arguments(T, Args, RestArgs)
    ).


non_default_argument(Value) :- var(Value), !.
non_default_argument(_/_) :- !, fail.
non_default_argument(_).
