/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (c)  1996-2018, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
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

:- module(emacs_application, []).
:- use_module(library(pce)).
:- use_module(library(pce_history)).
:- if(current_prolog_flag(windows, true)).
:- use_module(dde_server).
:- endif.
:- require([ ignore/1
           , pce_help_file/2
           , member/2
           ]).

:- pce_global(@finder, new(finder)).
:- pce_autoload(finder, library(find_file)).


:- pce_begin_class(emacs, application,
                   "PceEmacs main object").

class_variable(icon_image, image*, @pce_image).

variable(buffer_list,   dict,    get, "List of buffers maintained").
variable(history,       history, get, "History of visited places").


                 /*******************************
                 *            CREATE            *
                 *******************************/

initialise(Emacs, Buffers:dict) :->
    send_super(Emacs, initialise, emacs),
%   send(Emacs, leader, frame('PceEmacs')),
    send(Emacs, kind, service),
    send(Emacs, slot, history,
         history(message(Emacs, goto_history, @arg1, tab))),
    send(Emacs, slot, buffer_list, Buffers),
    new(@emacs_mark_list, emacs_bookmark_editor),
    ignore(send(Emacs, server_start)),
    ignore(send(Emacs, load_user_init_file)),
    register_clean_exit(Emacs).

unlink(Emacs) :->
    unregister_clean_exit(Emacs),
    send_super(Emacs, unlink).

start(_Emacs) :->
    true.

                 /*******************************
                 *         BUFFER MENU          *
                 *******************************/

:- pce_group(buffer).

show_buffer_menu(Emacs) :->
    "Show the buffer menu"::
    (   get(Emacs, member, buffer_menu, Menu)
    ->  send(Menu, expose)
    ;   send(emacs_buffer_menu(Emacs), open)
    ).


selection(Emacs, B:emacs_buffer*) :->
    "Select emacs buffer"::
    (   get(Emacs, member, buffer_menu, Menu)
    ->  send(Menu, selection, B)
    ;   true
    ).


                 /*******************************
                 *      BUFFERS AND FILES       *
                 *******************************/

:- pce_group(file).

buffer(Emacs, Name:name, B:emacs_buffer) :<-
    "Find named buffer"::
    get(Emacs, buffer_list, Dict),
    get(Dict, member, Name, DI),
    get(DI, object, B).

file_buffer(_, File:file, Buffer:emacs_buffer) :<-
    "Find existing buffer holding file"::
    get(File, base_name, Base),
    get(@emacs_base_names, member, Base, Chain),
    get(Chain, find, message(@arg1?file, same, File), Buffer).

buffers(Emacs, Buffers:chain) :<-
    "Chain with all emacs-buffers"::
    get(Emacs?buffer_list?members, map, @arg1?object, Buffers).


open_file(_Emacs, File:file, How:[{here,tab,window}]) :->
    "Open a file"::
    new(B, emacs_buffer(File)),
    send(B, open, How).


find_file(Emacs, Dir:[directory]) :->
    "Find and edit file"::
    get(@finder, file, @on, @default, Dir, FileName),
    send(Emacs, open_file, FileName).

goto_source_location(Emacs,
                     Location:source_location,
                     Where:where=[{here,tab,window}],
                     Title:title=[char_array]*) :->
    "Visit the indicated source-location"::
    (   Title == @nil
    ->  true
    ;   send(Emacs, location_history)
    ),
    get(Location, file_name, File),
    new(B, emacs_buffer(File)),
    get(B, open, Where, Frame),
    send(B, check_modified_file),
    get(Frame, editor, Editor),
    get(Editor, mode, Mode),
    (   get(Location, line_no, Line),
        Line \== @nil
    ->  send(Editor, mark_status, inactive),
        send(Mode, select_line, Line),
        (   get(Location, attribute, linepos, LinePos)
        ->  send(Mode, forward_char, LinePos)
        ;   true
        )
    ;   true
    ),
    (   Title == @nil
    ->  true
    ;   send(Mode, location_history, title := Title)
    ).

location_history(Emacs, Title:title=[char_array]) :->
    "Save current location into history"::
    (   get(Emacs, current_frame, Frame),
        get(Frame, editor, Editor),
        get(Editor, mode, Mode)
    ->  send(Mode, location_history, title := Title)
    ;   true
    ).

goto_history(Emacs, HE:emacs_history_entry, Where:where=[{here,tab,window}]) :->
    "Go back to an old history location"::
    send(Emacs, goto_source_location, HE?source_location, Where, @nil),
    send(Emacs?history, location, HE).

edit(Emacs, Location:source_location) :->
    "Equivalent to ->goto_source_location"::
    send(Emacs, goto_source_location, Location).


existing_file(_Emacs, Dir:[directory], File:file) :<-
    "Find existing file in directory"::
    get(@finder, file, @on, @default, Dir, FileName),
    new(File, file(FileName)).

open_object(_Emacs, Object:prolog, _NewWindow:new_window=[bool]) :->
    "Open from description"::
    edit(Object).

show_bookmarks(_) :->
    "Show PceEmacs bookmarks window"::
    send(@emacs_mark_list, expose).


                 /*******************************
                 *             SAVE             *
                 *******************************/
:- pce_group(save).

save_some_buffers(BM, Confirm:[bool]) :->
    "Save all modified buffers"::
    new(ModifiedItem,
        and(@arg1?object?file \== @nil,
            @arg1?object?modified == @on)),
    (   get(BM?buffer_list, find, ModifiedItem, _)
    ->  send(BM?buffer_list, for_some,
             and(ModifiedItem,
                 or(Confirm == @off,
                    message(@display, confirm, 'Save %s?',
                            @arg1?object?file?name)),
                 message(@arg1?object, save, @arg1?object?file)))
    ;   send(@event, instance_of, event) % GUI initiated
    ->  send(@pce, report, status, 'No buffers need saving')
    ;   true
    ).


                 /*******************************
                 *          CLEAN EXIT          *
                 *******************************/

:- dynamic
    emacs_application/1,
    registered/0.

register_clean_exit(Emacs) :-
    asserta(emacs_application(Emacs)),
    (   registered
    ->  true
    ;   asserta(registered),
        at_halt(exit_emacs)
    ).

unregister_clean_exit(Emacs) :-
    retractall(emacs_application(Emacs)).

exit_emacs :-
    forall(emacs_application(Emacs),
           exit_emacs(Emacs)).

exit_emacs(Emacs) :-
    (   in_pce_thread_sync(send(Emacs, check_saved_at_exit))
    ->  true
    ;   cancel_halt('Unsaved buffers')
    ).

check_saved_at_exit(BM) :->
    "Check for unsaved buffers when called from exit"::
    send(BM, save_some_buffers, @on),
    new(ModifiedItem,
        and(@arg1?object?file \== @nil,
            @arg1?object?modified == @on)),
    (   get(BM?buffer_list, find, ModifiedItem, _)
    ->  send(@display, confirm, 'Discard modified buffers?')
    ;   true
    ).


                 /*******************************
                 *            WINDOWS           *
                 *******************************/

:- pce_group(window).

current_frame(Emacs, Frame:emacs_frame) :<-
    "PceEmacs frame the user is working in"::
    (   send(@event, instance_of, event),
        get(@event, window, Window),
        get(Window, frame, Frame),
        send(Frame, instance_of, emacs_frame)
    ->  true
    ;   get(Emacs?members, find,
            and(message(@arg1, instance_of, emacs_frame),
                message(@arg1, on_current_desktop)),
            Frame)
    ).


                 /*******************************
                 *             MODE             *
                 *******************************/

:- pce_group(mode).

modes(_Emacs, ModeNames:chain) :<-
    "Return chain with known modes"::
    get(@mode_name_type, context, ModeNames).


                 /*******************************
                 *             HELP             *
                 *******************************/

:- pce_group(help).

:- pce_help_file(emacs, pce_help('emacs.hlp')).
:- pce_help_file(emacs_customise, pce_help('customise.hlp')).

help(_Emacs) :->
    "Display general help"::
    send(@helper, give_help, emacs, main).

customise(_Emacs) :->
    "Display customisation help"::
    send(@helper, give_help, emacs_customise, main).


                 /*******************************
                 *              SERVER          *
                 *******************************/

:- pce_group(server).

server_start(Emacs, Force:[bool]) :->
    "Start server-mode (xpce-client interface)"::
    server_start(Emacs, Force).

:- if(current_predicate(start_emacs_dde_server/1)).
server_start(_Emacs, _Force) :-
    (   \+ get(class(socket), send_method, listen, _)
    ;   \+ send(class(socket), has_feature, unix_domain)
    ),
    !,
    start_emacs_dde_server(false).
:- endif.
server_start(_Emacs, _Force) :-
    get(@emacs_server, status, listen),
    !.
server_start(Emacs, Force) :-
    (   send(@emacs_server_address, exists, @off)
    ->  (   Force \== @on,
            pce_catch_error(socket, send(@emacs_server, connect))
        ->  free(@emacs_server),
            send(Emacs, report, status, 'Server on other PceEmacs'),
            fail
        ;   free(@emacs_server), % will recreate!
            ignore(send(Emacs, report, status, 'Restarted server')),
            send(@emacs_server_address, remove)
        )
    ;   true
    ),
    ignore(send(@emacs_server, listen)).

chrome_server(_Emacs) :->
    "Start HTTP server on 9292 for Edit With Emacs"::
    use_module(library(emacs/emacs_chrome_server)),
    member(Goal, [emacs_chrome_server]),        % fool xref
    call(Goal).

:- pce_group(customise).


                 /*******************************
                 *       USER EXTENSIONS        *
                 *******************************/

load_user_extension(_Emacs, Base:name) :->
    "Load Prolog user file with this base-name"::
    (   absolute_file_name(emacs_user_library(Base),
                           [ access(read),
                             file_type(prolog),
                             file_errors(fail)
                           ],
                           Extension)
    ->  ignore(load_files(user:Extension, [autoload(true)]))
    ;   true
    ).


load_user_init_file(_Emacs) :->
    "Load user_profile('.pceemacsrc') or user_profile('pceemacs.ini')"::
    (   get(@pce, operating_system, win32)
    ->  Base = 'pceemacs.ini'
    ;   Base = '.pceemacsrc'
    ),
    (   absolute_file_name(user_profile(Base),
                           [ access(read),
                             file_errors(fail)
                           ],
                           Profile)
    ->  ignore(load_files(user:Profile, [autoload(true)]))
    ;   true
    ).

:- pce_end_class(emacs).

