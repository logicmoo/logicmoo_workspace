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

:- module(emacs_buffer_menu, []).
:- use_module(library(pce)).
:- use_module(library(persistent_frame)).
:- require([ send_list/3
           ]).

:- pce_autoload(tool_bar, library(toolbar)).

resource(open,      image, image('16x16/open.xpm')).
resource(saveall,   image, image('16x16/saveall.xpm')).
resource(help,      image, image('16x16/help.xpm')).
resource(bookmarks, image, image('16x16/bookmarks.xpm')).
resource(buffers,   image, image('32x32/buffers.xpm')).

:- pce_begin_class(emacs_buffer_menu, persistent_frame,
                   "List showing all PceEmacs buffers").

class_variable(geometry,        geometry,       '211x190+0+125').

initialise(BM, Emacs:emacs) :->
    "Create menu for buffer-list"::
    send(BM, send_super, initialise,
         'PCE Emacs Buffers', application := Emacs),
    send(BM, icon, resource(buffers)),
    send(BM, name, buffer_menu),
    send(BM, append, new(D, dialog)),
    send(D, pen, 0),
    send(D, gap, size(0, 3)),
    send(D, append, new(TB, tool_bar(Emacs))),
    send_list(TB, append,
              [ tool_button(find_file,
                            resource(open),
                            'Open file for editing'),
                tool_button(save_some_buffers,
                            resource(saveall),
                            'Save all modified buffers'),
                tool_button(show_bookmarks,
                            resource(bookmarks),
                            'Show bookmarks'),
                tool_button(help,
                            resource(help),
                            'Help on PceEmacs')
              ]),

    send(new(B, emacs_buffer_browser(Emacs)), below, D),
    send(new(report_dialog), below, B).

selection(BM, B:emacs_buffer*) :->
    "Select emacs buffer"::
    get(BM, member, browser, Browser),
    (   B == @nil
    ->  send(Browser, selection, @nil)
    ;   get(B, name, Name),
        get(Browser, member, Name, DictItem),
        send(Browser, insert_after, DictItem, @nil), % move to top
        send(Browser, selection, DictItem)
    ).

:- pce_end_class(emacs_buffer_menu).

:- pce_begin_class(emacs_buffer_browser, browser,
                   "Browse the emacs buffers").

initialise(B, Emacs:emacs) :->
    "Create for Emacs"::
    send_super(B, initialise, 'Emacs buffers'),
    send(B, name, browser),
    send(B, open_message, message(@arg1?object, open)),
    send(B, tab_stops, vector(150)),
    send(B, attach_popup),
    send(B, dict, Emacs?buffer_list).

typed(B, Ev:event, Delegate:[bool]) :->
    "Map DEL and backspace to kill selected buffer"::
    (   (   get(Ev, id, 'DEL')
        ;   get(Ev, id, backspace)
        ),
        get(B, selection, _)
    ->  send(B, kill_selection)
    ;   send_super(B, typed, Ev, Delegate)
    ).

kill_selection(B) :->
    get(B, selection, DI),
    send(DI?object, kill).

attach_popup(B) :->
    "Attach the popup menu"::
    send(B, popup, new(P, popup)),

    new(Buffer, @arg1?object),
    send(P, update_message,
         message(B, selection, @arg1)),
    send_list(P, append,
              [ menu_item(open_buffer,
                          message(Buffer, open)),
                menu_item(open_new_window,
                          message(Buffer, open, @on),
                          @default, @on),
                menu_item(identify,
                          message(Buffer, identify),
                          @default, @on),
                menu_item(kill_buffer,
                          message(Buffer, kill))
              ]).


drop_files(B, Files:chain, _At:point) :->
    "Drag-and-drop interface"::
    get(B, application, Emacs),
    send(Files, for_all,
         message(Emacs, open_file, @arg1)).

:- pce_end_class(emacs_buffer_browser).




