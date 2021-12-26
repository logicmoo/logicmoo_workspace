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

:- module(pce_behaviour_item, []).
:- use_module(library(pce)).
:- use_module(util).
:- require([ forall/2
           ]).

:- pce_begin_class(behaviour_item, text_item,
                   "Text item for entering XPCE behaviour or class").

initialise(FI, Name:[name], Def:[any|function], Msg:[code]*) :->
    send(FI, send_super, initialise, Name, Def, Msg),
    send(FI, style, combo_box).

completions(_MI, Spec:'char_array|tuple', Matches:chain) :<-
    new(Matches, chain),
    (   send(Spec, instance_of, char_array)
    ->  send(@classes, for_all,
             if(message(@arg1, prefix, Spec),
                message(Matches, append, @arg1)))
    ;   get(Spec, first, ClassPart),
        get(Spec, second, SelPart),
        new(Re, regex('\\s*(\\w+)\\s*(->|<-|-)')),
        send(Re, match, ClassPart),
        get(Re, register_value, ClassPart, 1, name, ClassName),
        get(Re, register_value, ClassPart, 2, name, Arrow),
        get(@pce, convert, ClassName, class, Class),
        new(Code, if(message(@arg1?name, prefix, SelPart),
                     message(Matches, append, @arg1?name))),
        (   Arrow == (-)
        ->  send(Class?instance_variables, for_all, Code)
        ;   Arrow == (->)
        ->  forall(super_or_delegate_class(Class, TheClass),
                   (send(TheClass?send_methods, for_all, Code),
                    send(TheClass?instance_variables, for_all,
                         if(message(@arg1, send_access), Code))))
        ;   forall(super_or_delegate_class(Class, TheClass),
                   (send(TheClass?get_methods, for_all, Code),
                    send(TheClass?instance_variables, for_all,
                         if(message(@arg1, get_access), Code))))
        )
    ),
    send(Matches, unique),
    send(Matches, sort).

split_completion(_MI, Value:char_array, RVal:'char_array|tuple') :<-
    new(Re, regex('\\s*(\\w+\\s*(->|<-|-))(\\w*)')),
    (   send(Re, match, Value)
    ->  get(Re, register_value, Value, 1, Class),
        get(Re, register_value, Value, 3, Selector),
        new(RVal, tuple(Class, Selector))
    ;   RVal = Value
    ).


selected_completion(MI, Selected:char_array, _Apply:[bool]) :->
    send(MI, send_super, selected_completion, Selected, @off),
    (   get(MI, selection, Selection),
        send(Selection, instance_of, behaviour)
    ->  send(MI, apply, @on)
    ;   true
    ).


selection(MI, S:'behaviour|class*') :<-
    "Get selection as behaviour or class"::
    get(MI, get_super, selection, Text),
    (   send(type('behaviour|class*'), validate, Text)
    ->  S = Text
    ;   get(Text, size, 0)
    ->  S = @nil
    ;   new(Re, regex('\\s*(\\w+)\\s*(->|<-|-)\\s*(\\w+)')),
        (   (   send(Re, match, Text)
            ->  get(Re, register_value, Text, 1, class, Class),
                get(Re, register_value, Text, 2, name, Access),
                get(Re, register_value, Text, 3, name, Selector),
                super_or_delegate_class(Class, TheClass),
                (   Access == (-)
                ->  get(TheClass, instance_variable, Selector, S)
                ;   Access == (->)
                ->  get(TheClass, send_method, Selector, S)
                ;   get(TheClass, get_method, Selector, S)
                )
            ;   get(@pce, convert, Text, class, S)
            )
        ;   send(MI, error, cannot_convert_text, Text, 'behaviour|class'),
            fail
        )
    ).


:- pce_end_class.
