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

:- module(dia_image_item, []).
:- use_module(library(pce)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Library defined class `image_item', which defines a text item that can
be used to specify an image using automatic completion.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(image_item, text_item).

:- pce_global(@image_path_regex, new(regex('[^:]+'))).

initialise(I, Name:[name], Default:[any|function], Msg:[code]*) :->
    "Initialise the value-set"::
    send(I, send_super, initialise, Name, Default, Msg),
    send(I, type, image),
    get(class(image), class_variable_value, path, Path),
    new(ValueSet, chain),
    send(ValueSet, lock_object, @on),
    send(@image_path_regex, for_all, Path,
         and(assign(new(Dir, var),
                    create(directory,
                           ?(@arg1, register_value, @arg2, 0))),
             if(message(Dir, exists),
                message(Dir, scan, ValueSet, ValueSet, '.*\\.bm$')))),
    Object = ?(@pce, object_from_reference, @arg1),
    send(@pce, for_name_reference,
         if(message(Object, '_instance_of', image),
            message(ValueSet, append, create(string, '@%s', @arg1)))),
    send(ValueSet, sort),
    send(ValueSet, unique),
    send(I, value_set, ValueSet),
    send(ValueSet, lock_object, @off).

:- pce_end_class.
