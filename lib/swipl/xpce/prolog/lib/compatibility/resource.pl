/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1999-2012, University of Amsterdam
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

:- module(pce_resource_compatibility, []).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module deals with backward compatibility   for packages written for
XPCE  versions  upto  4.10.1   (ProWindows    3.2),   using   `resource'
declaractions.

Now, resources are defined to be data associated with an application. In
the old version, resources refered to class-variables and settings.

This code deals with the necessary conversions,   and  should get 99% of
the old code working without modifications.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

                 /*******************************
                 *             METHODS          *
                 *******************************/

:- pce_extend_class(object).

resource_value(O, Name:name, RV:any) :<-
    "COMPATIBILITY: <-class_variable_value"::
    get(O, class_variable_value, Name, RV).

obtain_resources(O) :->
    "COMPATIBILITY: ->obtain_class_variables"::
    send(O, obtain_class_variables).

:- pce_end_class.

:- pce_extend_class(class).

resource(C, Name:name, Resource) :<-
    "COMPATIBILITY: return <-class_variable"::
    get(C, class_variable, Name, Resource).

:- pce_end_class.


                 /*******************************
                 *      CLASS DECLARATIONS      *
                 *******************************/

:- dynamic
    user:pce_pre_expansion_hook.
:- multifile
    user:pce_pre_expansion_hook.

user:pce_pre_expansion_hook(resource(Name, Type, Value),
                            class_variable(Name, Type, NewVal)) :-
    pce_compiling(_Class, Path),
    source_location(Path, _),
    pce_resource_compatibility:rc_convert_value(Value, Type, NewVal).
user:pce_pre_expansion_hook(resource(Name, Type, Value, Summary),
                            class_variable(Name, Type, NewVal, Summary)) :-
    pce_compiling(_Class, Path),
    source_location(Path, _),
    pce_resource_compatibility:rc_convert_value(Value, Type, NewVal).

rc_convert_value(Value, Type, NewVal) :-
    convert_value(Value, Type, NewVal),
    !,
    pce_warn(compatibility(resource(Value, NewVal))).
rc_convert_value(Value, _, Value).

convert_value(Value, _Chain, NewVal) :-         % list --> chain(members)
    is_list(Value),
    !,
    NewVal =.. [chain|Value].
convert_value(Value, _Type, NewVal) :-          % '@name' --> @name
    atom(Value),
    term_to_atom(NewVal, Value),
    NewVal = @(Name),
    atom(Name),
    !.
convert_value(_, Type, _) :-                    % textual types
    atomic_type(Type),
    !,
    fail.
convert_value(Value, Type, NewVal) :-           % Other values
    atom(Value),
    term_to_atom(Term, Value),
    \+ atom(Term),
    !,
    (   convert_value(Term, Type, NewVal)
    ->  true
    ;   NewVal = Term
    ).

atomic_type(string).
atomic_type(name).
atomic_type(char_array).
atomic_type(geometry).
atomic_type([T]) :-
    atomic_type(T).
atomic_type(*(T)) :-
    atomic_type(T).
