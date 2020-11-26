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

:- module(pce_auto_window, []).
:- use_module(library(pce)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This   library   defines    the     classes    auto_sized_picture    and
auto_sized_dialog, subclasses of picture and dialog that decide on their
size and scrollbars depending on the size of the contents.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_begin_class(auto_sized_picture, window,
                   "Window that automatically fits the contents").

variable(border,   int := 10,   both, "Border around contents").
variable(max_size, size,        both, "Maximum size").

class_variable(max_size, size,  size(700,500), "Maximum size").

initialise(W, L:label=[name], D:display=[display]) :->
    send_super(W, initialise, L, @default, D).

'_compute_desired_size'(W) :->
    get(W, bounding_box, BB),
    get(W, border, B),
    send(W, scroll_to, point(BB?x-B, BB?y-B)),
    get(BB, width, BW),
    get(BB, height, BH),
    get(W, max_size, size(MW, MH)),
    WW is min(BW + 2*B, MW),
    WH is min(BH + 2*B, MH),
    (   WH < BH + 2*B               % force SB to compute!?
    ->  (   WW < BW + 2*B
        ->  send(W, scrollbars, both)
        ;   send(W, scrollbars, vertical)
        )
    ;   (   WW < BW + 2*B
        ->  send(W, scrollbars, horizontal)
        ;   true
        )
    ),
    send(W, size, size(WW, WH)).

:- pce_end_class.


:- pce_begin_class(auto_sized_dialog, dialog,
                   "Dialog with scroll-bars if the contents are too big").

variable(max_size, size,        both, "Maximum size").
class_variable(max_size, size,  size(700,500), "Maximum size").

initialise(W, L:label=[name], D:display=[display]) :->
    send_super(W, initialise, L, @default, D).

'_compute_desired_size'(D) :->
    send_super(D, '_compute_desired_size'),
    get(D, ideal_width, BW),
    get(D, ideal_height, BH),
    get(D, max_size, size(MW, MH)),
    WW is min(BW, MW),
    WH is min(BH, MH),
    (   WH < BH
    ->  send(D, vertical_scrollbar, @on)
    ;   true
    ),
    (   WW < BW
    ->  send(D, horizontal_scrollbar, @on)
    ;   true
    ),
    send(D, set, @default, @default, WW, WH).

:- pce_end_class.
