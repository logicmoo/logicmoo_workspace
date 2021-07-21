/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1995-2011, University of Amsterdam
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

:- module(pce_imageviewer, [image_viewer/0]).
:- use_module(library(pce)).
:- require([ append/3
           , chain_list/2
           , atomic_list_concat/2
           , flatten/2
           , send_list/3
           , shell/1
           ]).

:- use_module(library(file_item)).      % Class directory_item

image_viewer :-
    new(P, picture),
    send(P, scrollbars, vertical),
    send(P, format, format(horizontal, 800, @off)),
    send(P, resize_message,
         message(P, format, width, @arg2?width)),
    send(new(D, dialog), below, P),
    send(P?frame, label, 'Image Viewer'),
    get(@pce, home, Home),
    atom_concat(Home, '/bitmaps', DefDir),
    send(D, append, new(Dir, directory_item(directory, DefDir))),
    send(D, append, new(File, text_item(file_pattern, '*.bm'))),
    new(ValueSet, chain('*.bm', '*.xpm', '*.gif', '*.jpg', '*.jpeg')),
    (   get(@pce, window_system, windows)
    ->  send_list(ValueSet, append, ['*.ico', '*.cur'])
    ;   true
    ),
    send(File, value_set, ValueSet),
    send(D, append, button(apply,
                           message(@prolog, view, P,
                                   Dir?selection, File?selection))),
    send(D, append, button(quit,
                           message(P, destroy))),
    send(D, default_button, apply),
    send(P, open).


view(P, Dir, Pattern) :-
    new(D, directory(Dir)),
    file_pattern_to_regex(Pattern, Regex),
    get(D, files, Regex, Files),
    (   send(Files, empty)
    ->  send(@display, inform, 'No matching images')
    ;   send(P, clear),
        get(D?path, ensure_suffix, /, DirName),
        send(P?frame, label, string('Images from %s%s', DirName, Pattern)),
        chain_list(Files, List),
        show(P, List, D)
    ).


:- pce_global(@image_recogniser, make_image_recogniser).

make_image_recogniser(R) :-
    new(R, handler_group),
    send(R, append, click_gesture(left, '', single,
                                  message(@receiver, inverted,
                                          @receiver?inverted?negate))),
    send(R, append, popup_gesture(new(P, popup))),
    new(Bitmap, @event?receiver),
    new(BitmapName, Bitmap?file?name),
    send_list(P, append,
              [ menu_item(run_bitmap_editor,
                          message(@prolog, edit_image, Bitmap),
                          @default, @on)
              , menu_item(reload,
                          message(@prolog, reload, Bitmap),
                          @default, @off)
              , menu_item(resize,
                          message(@prolog, resize, Bitmap),
                          @default, @off)
              , menu_item(convert_to_x11,
                          message(@prolog, convert, Bitmap),
                          @default, @off)
              , menu_item(remove,
                          and(message(@display, confirm,
                                      'Remove %s', BitmapName),
                              message(@prolog, remove, Bitmap)),
                          @default, @on)
              ]).


convert(Bitmap) :-
    get(Bitmap, file, File),
    send(File, backup),
    send(Bitmap?image, save, File).


remove(Bitmap) :-
    get(Bitmap, file, File),
    send(File, remove),
    send(Bitmap, free).


reload(Bitmap) :-
    send(Bitmap?image, load),
    send(Bitmap, redraw).


edit_image(Bitmap) :-
    get(Bitmap?file, name, File),
    atomic_list_concat(['bitmap ', File, ' &'], Cmd),
    shell(Cmd).


resize(Bitmap) :-
    get(Bitmap, file, File),
    get(File, name, FileName),
    new(D, dialog(string('Resize image %s', FileName))),
    send(D, append, label(image, Bitmap)),
    send(D, append, new(W, text_item(width, Bitmap?width))),
    send(D, append, new(H, text_item(height, Bitmap?height))),
    send(D, append, button(ok,
                           and(message(@prolog, resize,
                                       Bitmap, W?selection, H?selection),
                               message(D, destroy)))),
    send(D, append, button(cancel,
                           message(D, destroy))),
    send(D, default_button, ok),
    send(D, open).

resize(Bitmap, W, H) :-
    get(Bitmap, file, File),
    new(I2, image(@nil, W, H)),
    send(I2, draw_in, Bitmap, point(0,0)),
    send(Bitmap, image, I2),
    send(File, backup),
    send(I2, save, File),
    send(I2, load, File).

:- pce_global(@image_viewer_icon_spatial,
              new(spatial(xref=x+w/2, yref=y+h+5,
                          xref=x+w/2, yref=y))).

show(_, [], _) :- !.
show(P, [F|R], Dir) :-
    get(Dir, file, F, File),
    new(I, image(File?name)),      % first make image to avoid error
    !,
    new(B, bitmap(I)),              % the bitmap
    send(B, recogniser, @image_recogniser),

    new(F2, figure),                % elevate it from the background
    send(F2, border, 3),
    (   get(@display, visual_type, monochrome)
    ->  send(F2, elevation, elevation(image, 2,
                                      relief := @grey50_image,
                                      shadow := colour(black)))
    ;   send(F2, elevation, elevation(image, 2,
                                      colour := colour(grey80)))
    ),
    send(F2, display, B),

    new(D, device),                 % put together with label
    send(D, display, F2),
    send(D, display, new(T, text(F, center))),
    send(@image_viewer_icon_spatial, forwards, F2, T),

    send(P, display, D),            % display (<-format positions)
    show(P, R, Dir).
show(P, [_|R], Dir) :-
    show(P, R, Dir).


                 /*******************************
                 *             UTIL             *
                 *******************************/

file_pattern_to_regex(Pattern, Regex) :-
    atom_codes(Pattern, Chars),
    phrase(file_regex(RegexChars0), Chars),
    flatten(["^", RegexChars0, "$"], RegexChars),
    atom_codes(Regex, RegexChars).

file_regex([]) --> [].
file_regex([".*"|T]) -->
    "*",
    !,
    file_regex(T).
file_regex(["\\."|T]) -->
    ".",
    file_regex(T).
file_regex(["."|T]) -->
    "?",
    !,
    file_regex(T).
file_regex(["[",Set,"]"|T]) -->
    "[",
    !,
    charset(Set),
    file_regex(T).
file_regex([H|T]) -->
    [H],
    file_regex(T).

charset([0']|T]) -->
    "]",
    !,
    charset2(T).
charset([0'^,0']|T]) -->
    "^]",
    !,
    charset2(T).
charset(Set) -->
    charset2(Set).

charset2([]) --> "]", !.
charset2([H|T]) --> [H], charset2(T).

