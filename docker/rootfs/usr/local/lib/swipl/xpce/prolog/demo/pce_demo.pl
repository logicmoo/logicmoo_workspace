/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (c)  1995-2014, University of Amsterdam
                              VU University Amsterdam
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

:- module(pce_demo,
          [ pcedemo/0
          ]).
:- use_module(library(pce)).
:- use_module(contrib(contrib)).
:- use_module(library(persistent_frame)).
:- require([ emacs/1
           , forall/2
           , member/2
           , term_to_atom/2
           ]).

:- multifile                            % So this may be predefined, avoiding
    pcedemo/0.                      % an undefined trap

/** <module> XPCE demo starter

This file defines a demo-starting tool.  The demo's themselves should be
in the library files 'demo/<demo-file>.pl'. At the end of this file is a
list of available demo's.
*/

pcedemo :-
    new(F, persistent_frame('XPCE Demo Programs')),
    send(F, append, new(B, browser(@default, size(60,10)))),
    send(B, confirm_done, @off),
    send(B, tab_stops, vector(150)),
    fill_browser(B),

    send(new(D, dialog), below, B),
    send(D, append, new(O, button(open, message(@prolog, open_demo, B)))),
    send(D, append, button(source, message(@prolog, view_source, B))),
    send(D, append, button(quit, message(D?frame, free))),
    send(D, default_button, open),

    send(B, open_message, message(O, execute)),
    send(B, style, title, style(font := font(helvetica, bold, 14))),

    send(B, open).


fill_browser(B) :-
    forall(demo(Name, Summary, _, _),
           send(B, append, dict_item(Name,
                                     string('%s\t%s', Name, Summary)))),
    send(B, append,
         dict_item('======Contributions====================',
                   style := title)),
    forall(contribution(Name, Summary, _Author, _, _),
           send(B, append, dict_item(Name,
                                     string('%s\t%s', Name, Summary)))).


open_demo(Browser) :-
    get(Browser, selection, DictItem),
    (   (   DictItem == @nil
        ;   get(DictItem, style, title)
        )
    ->  send(@display, inform, 'First select a demo')
    ;   get(DictItem, key, Name),
        (   (   demo(Name, Summary, File, Predicate)
            ;   contribution(Name, Summary, _Author, File, Predicate)
            )
        ->  (   send(@pce, report, progress, 'Loading demo %s ...', Summary),
                use_module(File),
                send(@pce, report, done)
            ->  (   Predicate
                ->  true
                ;   send(@pce, inform, 'Failed to start %s demo', Name)
                )
            ;   send(@pce, inform, 'Can''t find demo sourcefile')
            )
        ;   send(Browser, report, error, 'No such demo: %s', Name)
        )
    ).

view_source(Browser) :-
    get(Browser, selection, DictItem),
    (   DictItem == @nil
    ->  send(@display, inform, 'First select a demo')
    ;   get(DictItem, key, Name),
        (   demo(Name, _, File, _)
        ;   contribution(Name, _, _Author, File, _)
        ),
        (   locate_file(File, Path)
        ->  emacs(Path)
        ;   term_to_atom(File, FileAtom),
            send(Browser, report, error,
                 'Can''t find source from %s', FileAtom)
        )
    ).

locate_file(Base, File) :-
    absolute_file_name(Base,
                       [ file_type(prolog),
                         access(read)
                       ], File).


                /********************************
                *             DEMO'S            *
                ********************************/

demo('PceDraw',                         % Name
     'Drawing tool',                    % Summary
     library(pcedraw),                  % Sources
     pcedraw).                          % Toplevel predicate

demo('Ispell',
     'Graphical interface to ispell (requires ispell 3)',
     demo(ispell),
     ispell) :-
    send(@pce, has_feature, process).

demo('Emacs',
     'Emacs (Epoch) look-alike editor',
     library(pce_emacs),
     emacs).

demo('XMLView',
     'Browse structure of HTML/SGML and XML files',
     library('doc/xml_browse'),
     send(new(xml_browser), open)).

demo('FontViewer',
     'Examine PCE predefined fonts',
     demo(fontviewer),
     fontviewer).

demo('Cursors',
     'Displays browser of available cursors',
     demo(cursor),
     cursor_demo).

demo('Colours',
     'Displays browser of named colours',
     demo(colour),
     colour_browser).

demo('HSV Colours',
     'Colour browser using Hue-Saturnation-Value',
     demo(hsvcolour),
     hsv_browser).

demo('ImageViewer',
     'Examine image files in a directory',
     demo(imageviewer),
     image_viewer).

demo('Events',
     'Display hierarchy of event-types',
     demo(event_hierarchy),
     event_hierarchy).

demo('GraphViewer',
     'Visualise a graph represented as Prolog facts',
     demo(graph),
     graph_viewer).

demo('FtpLog',
     'Examine /usr/adm/xferlog (ftp log file)',
     demo(ftplog),
     ftplog('/usr/adm/xferlog')) :-
    send(@pce, has_feature, process),
    absolute_file_name('/usr/adm/xferlog', Path),
    send(file(Path), access, read).


demo('ChessTool',
     'Simple frontend for /usr/games/chess',
     demo(chess),
     chess) :-
    send(@pce, has_feature, process),
    absolute_file_name('/usr/games/chess', Path),
    send(file(Path), access, execute).

demo('Constraints',
     'Using constraints and relations',
     demo(constraint),
     constraint_demo).

demo('Kangaroo',
     'Jumping kangaroos demo',
     demo(kangaroo),
     kangaroo).

demo('Juggler',
     'Annimation of a juggling creature',
     demo(juggler),
     juggle_demo).

