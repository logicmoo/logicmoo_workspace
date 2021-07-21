/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1997-2011, University of Amsterdam
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

:- module(pce_image_item, []).
:- use_module(library(pce)).
:- require([ ignore/1
           ]).

:- pce_autoload(image_browser, library(pce_image_browser)).

:- pce_begin_class(image_item, label_box,
                   "Display and browse for an image").


:- pce_global(@image_item_nil_image, new(image(@nil, 32, 32))).

variable(center_bitmap, point*,         none, "Center of the bitmap").
variable(directory,     [directory],    both, "Default search directory").
variable(extensions,    [chain],        both, "Default search extensions").

initialise(II, Name:[name], Default:[function|image], Msg:[code]*) :->
    send(II, directory, @default),
    send(II, extensions, @default),
    send(II, send_super, initialise, Name, Msg),
    send(II, append, new(BM, bitmap(@image_item_nil_image))),
    send(II, append, new(Br, button(browse)), right),
    send(BM, pen, 2),
    send(BM, reference, point(0, 0)),       % BM?height/2)),
    send(Br, reference, point(0, 0)),       % Br?height/2)),
    send(Br, label, 'Browse ...'),
    (   Default \== @default
    ->  send(II, default, Default)
    ;   true
    ).


layout_dialog(II) :->
    send(II, send_super, layout_dialog),
    get(II, member, bitmap, BM),
    send(II, slot, center_bitmap, BM?center).


update_layout(_II) :->
    /*      (   get(II, slot, center_bitmap, Center),
            Center \== @nil
        ->  get(II, member, bitmap, BM),
            send(BM, center, Center)
        ;   true
        ),
*/
        true.

:- pce_group(selection).

selection(II, Selection:image) :->
    get(II, member, bitmap, BM),
    send(BM, image, Selection),
    send(II, update_layout),
    send(II, modified, @off).
selection(II, Selection:image) :<-
    get(II, member, bitmap, BM),
    get(BM, image, Selection),
    Selection \== @image_item_nil_image,
    send(II, modified, @off).

clear(II) :->
    "Clear the selection"::
    get(II, member, bitmap, BM),
    send(BM, image, @image_item_nil_image),
    send(II, modified, @off).

:- pce_group(browse).

browse(II) :->
    "Browse for a new image"::
    get(II, display, Display),
    send(Display, busy_cursor),
    get(II, directory, Dir),
    get(II, extensions, Exts),
    new(IB, image_browser(Dir, Exts)),
    send(Display, busy_cursor, @nil),
    send(IB, open_message, message(IB, return, @arg1)),
    send(new(D, dialog), below, IB),
    send(D, append, new(OK, button(ok, message(D, return, IB?selection)))),
    send(D, append, button(cancel, message(D, return, @nil))),
    send(IB, select_message, message(OK, active, @on)),
    send(OK, active, @off),
    send(IB, transient_for, II?frame),
    send(IB, modal, transient),
    get(IB, confirm_centered, II?frame?area?center, Image),
    (   Image \== @nil
    ->  get(II, member, bitmap, BM),
        send(BM, image, Image),
        send(II, update_layout),
        send(II, modified, @on),
        ignore(send(II?device, modified_item, II, @on))
    ;   true
    ),
    send(IB, free).

:- pce_end_class.

/*
test :-
        new(D, dialog),
        send(D, append, new(II, image_item(image, 'pce.bm'))),
        send(II, directory, '$PCEHOME/bitmaps/16x16'),
        send(D, open).
*/
