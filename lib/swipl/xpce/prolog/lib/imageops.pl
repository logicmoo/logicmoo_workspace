/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2002-2011, University of Amsterdam
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

:- module(pce_image_ops, []).
:- use_module(library(pce)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This  module  provides  `image<-active:   bool    -->   Image',   return
activated/inactive version of a colour-image. These methods are designed
to manipulate images on buttons.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_extend_class(image).

active(Img, Active:bool, Img2:image) :<-
    "Return image with proper activation"::
    (   Active == @off
    ->  (   get(Img, hypered, inactive, Img2)
        ->  true
        ;   get(Img, hypered, active, _)
        ->  Img2 = Img
        ;   get(Img, greyed, Img2)
        )
    ;   (   get(Img, hypered, active, Img2)
        ->  true
        ;   Img2 = Img
        )
    ).


greyed(Img, Grey:image) :<-
    "Created a greyed version of a colour image"::
    (   get(Img, hypered, inactive, Grey)
    ->  true
    ;   get(Img, size, size(W, H)),
        new(Grey, image(@nil, W, H, pixmap)),
        (   get(Img, mask, Mask),
            send(Mask, instance_of, image)
        ->  send(Grey, mask, new(M, image(@nil, W, H, bitmap))),
            new(MB, bitmap(Mask)),
            send(MB, transparent, @on),
            send(M, draw_in, MB),
            send(M, draw_in, MB, point(1,1))
        ;   true
        ),
        get(Img, monochrome, I2),
        send(Grey, background, black),
        new(B2, bitmap(I2)),
        send(B2, transparent, @on),
        send(B2, colour, white),
        send(Grey, draw_in, B2, point(1,1)),
        get(class(menu), class_variable, inactive_colour, ClassVar),
        get(ClassVar, value, GreyColour),
        send(B2, colour, GreyColour),
        send(Grey, draw_in, B2),
        new(_, hyper(Img, Grey, inactive, active))
    ).

:- pce_end_class.
