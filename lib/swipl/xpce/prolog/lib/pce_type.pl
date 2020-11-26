/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2001-2011, University of Amsterdam
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

:- module(pce_type,
          [ pce_define_type/2
          ]).
:- use_module(library(pce)).

%!  pce_define_type(+Name, +Type) is det.
%
%   Create a type alias name, so we can write more readable code.
%   Typical examples make aliases for `name' (name cannot be subclassed),
%   alias for numeric and name-sets.  Here are some examples:
%
%   ==
%   :- pce_define_type(rdf_resource, name).
%   :- pce_define_type(weekday,     {sunday,monday,tuesday,wednesday,
%                                    thursday,friday,saturday}).
%   :- pce_define_type(natural,     '1..').
%   ==

pce_define_type(Alias, Type) :-
    get(@types, member, Alias, TypeObj),
    !,
    (   get(TypeObj, kind, alias),
        get(TypeObj, context, Aliased),
        get(Aliased, name, Type)
    ->  true
    ;   throw(error(redefine(type, Alias), _))
    ).
pce_define_type(Alias, Type) :-
    (   object(Type)
    ->  TheType = Type
    ;   atom(Type)
    ->  get(@pce, convert, Type, type, TheType)
    ;   format(string(Atom), '~w', Type),
        get(@pce, convert, Atom, type, TheType)
    ),
    new(_, type(Alias, alias, TheType)).


