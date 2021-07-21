/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (c)  2011, University of Amsterdam
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

:- module(pce_swi_console,
          [
          ]).
:- use_module(library(pce)).
:- use_module(library(toolbar)).
:- use_module(library(persistent_frame)).

/** <module> Overall controller window for SWI-Prolog
*/

:- pce_begin_class(swi_console, persistent_frame,
                   "SWI-Prolog graphical console").

initialise(Con) :->
    send_super(Con, initialise, 'SWI-Prolog console'),
    send(Con, append, new(TD, tool_dialog(Con))),
    send(Con, fill_tool_dialog, TD).

fill_tool_dialog(Con, TD:tool_dialog) :->
    "Fill menu-bar"::
    send_list(TD, append,
              [ new(File, popup(file)),
                new(Tools, popup(tools)),
                new(Compile, popup(compile)),
                new(Help, popup(help))
              ]),
    send_list(File, append,
              [ menu_item(exit, message(Con, destroy))
              ]),
    send_list(Tools, append,
              [ menu_item(navigator,
                          message(@prolog_ide, open_navigator)),
                menu_item(threads,
                          message(@prolog_ide, thread_monitor)),
                menu_item(debug,
                          message(@prolog_ide, open_debug_status)),
                menu_item(exceptions,
                          message(@prolog_ide, open_exceptions)),
                menu_item(cross_referencer,
                          message(@prolog_ide, xref))
              ]),
    send_list(Compile, append,
              [ menu_item(make, message(@prolog, make))
              ]),
    send_list(Help, append,
              [ menu_item(about),
                menu_item('help (on www)', message(Con, help))
              ]).


                 /*******************************
                 *            ACTIONS           *
                 *******************************/

about([ 'SWI-Prolog ~w'+Version-boldhuge,
        'Copyright 1986-2011',
        'University of Amsterdam, VU University Amsterdam',
        'SWI-Prolog comes with ABSOLUTELY NO WARRANTY.',
        'This is free software (LGPL), and you are welcome to',
        'redistribute it under certain conditions.',
        url('http://www.swi-prolog.org')
      ]) :-
    (   current_prolog_flag(version_git, Version)
    ->  true
    ;   current_prolog_flag(version, Version),
        Major is Version // 10000,
        Minor is (Version // 100) mod 100,
        Patch is Version mod 100,
        atomic_list_concat([Major, Minor, Patch], '.', Version)
    ).

about(M) :->
    "Print about and licence info"::
    new(D, dialog('About SWI-Prolog')),
    send(D, transient_for, M),
    about(List),
    maplist(add_about(D), List),
    send(D, append, button(ok, message(D, destroy))),
    send(D, open_centered).

add_about(D, X-Font) :-
    !,
    add_about(X, Font, D).
add_about(D, X) :-
    add_about(X, normal, D).

add_about([], _, _) :- !.
add_about([H|T], Font, D) :-
    !,
    add_about(H, Font, D),
    add_about(T, Font, D).
add_about(url(Url), Font, D) :-
    !,
    send(D, append, new(T, text(Url, center, Font))),
    send(T, underline, @on),
    send(T, colour, blue),
    send(T, recogniser,
         click_gesture(left, '', single,
                       message(@prolog, goto_url, T?string?value))),
    send(T, cursor, hand2),
    send(T, alignment, center).
add_about(Fmt+Args, Font, D) :-
    !,
    format(atom(Text), Fmt, Args),
    send(D, append, new(T, text(Text, center, Font))),
    send(T, alignment, center).
add_about(Text, Font, D) :-
    send(D, append, new(T, text(Text, center, Font))),
    send(T, alignment, center).

goto_url(Url) :-
    send(@display, busy_cursor),
    (   catch(www_open_url(Url), _, fail)
    ->  true
    ;   send(@display, inform, 'Failed to open URL')
    ),
    send(@display, busy_cursor, @nil).

help(_Con) :->
    "Open help (www)"::
    URL = 'http://www.swi-prolog.org/pldoc/index.html',
    (   catch(www_open_url(URL), _, fail)
    ->  true
    ;   send(@display, inform, 'Failed to open: ', URL)
    ).

:- pce_end_class.
