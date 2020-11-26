/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2003-2011, University of Amsterdam
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

:- module(scaled_bitmap, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Provide support for resizeable bitmap. The   bitmap can be resized using
->scale or using one of the  default XPCE graphical geometry-controlling
methods (->size, ->width, ->set, ->area, etc).

If ->keep_aspect is @on, the image is   not  distorted using vertical or
horizontal stretching.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(scaled_bitmap, bitmap,
                   "Bitmap that scales its image").

variable(original,    image,         get, "Preserved original image").
variable(scale,       [real|size],   get, "Scale-factor/area").
variable(keep_aspect, bool := @on,   get, "Keep aspect ratio").

initialise(BM, Img:image, Spec:[real|size]) :->
    "Create from image and scaling factor"::
    send(BM, slot, original, Img),
    send(BM, slot, scale, Spec?clone),
    send_super(BM, initialise),
    send(BM, request_compute).


image(BM, Img:image) :->
    "Replace the image"::
    send(BM, slot, original, Img),
    send(BM, apply_scale).
image(BM, Img:image) :<-
    get(BM, original, Img).



apply_scale(BM) :->
    "Apply the requested scaling"::
    get(BM, scale, Scale),
    get(BM, original, Original),
    (   Scale == @default
    ->  send_super(BM, image, Original)
    ;   get(Original, size, size(W0, H0)),
        (   float(Scale),
            W is round(W0*Scale),
            H is round(H0*Scale)
        ;   object(Scale, size(WM, HM)),
            (   get(BM, keep_aspect, @on)
            ->  SF is min(WM/W0, HM/H0),
                W is round(W0*SF),
                H is round(H0*SF)
            ;   W = WM,
                H = HM
            )
        ),
        (   get_super(BM, image, Current),
            get(Current, size, size(W,H))
        ->  true
        ;   get(Original, scale, size(W, H), Scaled),
            send_super(BM, image, Scaled)
        )
    ).


compute(BM) :->
    "Delayed update"::
    send(BM, apply_scale),
    send(BM, request_compute, @nil).


geometry(BM, X:[int], Y:[int], W0:[int], H0:[int]) :->
    (   new_size(BM, W0, H0, W, H)
    ->  send(BM, scale, size(W, H)),
        send_super(BM, geometry, X, Y)
    ;   send_super(BM, geometry, X, Y)
    ).


new_size(Gr, Ws, Hs, W, H) :-
    get(Gr, width, W0),
    get(Gr, height, H0),
    default(Ws, W0, W),
    default(Hs, H0, H),
    (   W \== W0                    % something changed
    ;   H \== H0
    ),
    !.


scale(BM, Scale:[real|size]) :->
    "Change the scale"::
    get(BM, scale, Old),
    (   send(Scale, same_class, Old),
        send(Scale, equal, Old)
    ->  true
    ;   send(BM, slot, scale, Scale?clone),
        send(BM, request_compute)
    ).

keep_aspect(BM, Keep:bool) :->
    send(BM, slot, keep_aspect, Keep),
    send(BM, request_compute).

:- pce_end_class(scaled_bitmap).
