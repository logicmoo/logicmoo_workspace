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

:- module(pce_fontviewer,
          [ fontviewer/0
          ]).
:- use_module(library(pce)).
:- use_module(library('unicode/blocks')).
:- require([ between/3
           ]).

fontviewer :-
    new(FontViewer, frame('Font Viewer')),
    send(FontViewer, append, new(B, browser(size := size(35, 10)))),
    send(FontViewer, append, new(D, dialog)),
    send(FontViewer, append, new(P, picture(size := size(350,350)))),
    send(P, right, B),
    send(D, below, B),

    send(D, append, new(Open, button(open))),
    send(D, append,
         button(quit, message(FontViewer, destroy))),
    send(D, append, new(ChartMenu, menu(unicode_chart, cycle)), right),
    fill_chart_menu(ChartMenu),

    new(OpenMsg, message(@prolog, show_font,
                         P,
                         B?selection?object,
                         ChartMenu?selection)),
    send_list([Open, ChartMenu], message, OpenMsg),

    send(D, append, label(reporter), right),
    send(D, default_button, open),

    send(B, tab_stops, vector(80, 180)),
    send(B, open_message, message(Open, execute)),
    send(FontViewer, open),

    new(FontList, chain),
    send(@fonts, for_all, message(FontList, append, @arg2)),
    send(FontList, sort,
         ?(@prolog, compare_fonts, @arg1, @arg2)),

    send(FontList, for_all,
         message(@prolog, append_font_browser, B, @arg1)).

compare_fonts(F1, F2, Result) :-
    get(F1?family, compare, F2?family, Result),
    Result \== equal,
    !.
compare_fonts(F1, F2, Result) :-
    get(F1?style, compare, F2?style, Result),
    Result \== equal,
    !.
compare_fonts(F1, F2, Result) :-
    get(F1?points, compare, F2?points, Result).

append_font_browser(B, Font) :-
    get(Font, family, Fam),
    get(Font, style, Style),
    get(Font, points, Points),
    get(Font, object_reference, Name),
    send(B, append, dict_item(Name,
                              string('%s\t%s\t%d', Fam, Style, Points),
                              Font)).

fill_chart_menu(Menu) :-
    forall(unicode_block(Name, _, _),
           send(Menu, append, Name)).

show_font(P, Font, Chart) :-
    unicode_block(Chart, From, To),
    send(P, clear),
    new(F, format(horizontal, 2, @on)),
    send(F, row_sep, 0),
    send(P, format, F),
    new(A, string(x)),
    MaxRow is ((To-From)//15)-1,
    (   between(0, MaxRow, Y),
        I is Y*16+From,
        send(P, display,
             text(string('%03o/0x%02x/%03d:', I, I, I), left, fixed)),
        new(S, string),
        (   between(0, 15, X),
            C is 16*Y + X + From,
            C \== 0, C \== 9, C \== 10, C \== 13,
            send(A, character, 0, C),
            send(S, append, A),
            fail
        ;   send(P, display, font_text(S, left, Font))
        ),
        fail
    ;   true
    ).

:- pce_begin_class(font_text, text,
                   "Show current character").

event(FT, Ev:event) :->
    (   send(FT, send_super, event, Ev)
    ->  true
    ;   send(Ev, is_a, area_exit)
    ->  send(FT, report, status, '')
    ;   get(FT, pointed, Ev, Index),
        get(FT?string, character, Index, C),
        send(FT, report, status, '%c = %03o/0x%02x/%03d', C, C, C, C)
    ).

:- pce_end_class.
