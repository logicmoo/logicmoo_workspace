/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2002-2015, University of Amsterdam
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

:- module(prolog_preferences,
          [ prolog_edit_preferences/1   % +What
          ]).
:- use_module(library(pce)).
:- use_module(library(pce_tick_box)).

/** <module> Edit preferences files

This  module  provides  prolog_edit_preferences/1,  which   is  used  to
simplify locating the preference files and provide a default if the user
has no such file.

@see    library(win_menu) binds this to the Settings menu of the console on
        the MS-Windows version.
*/

%!  prolog_edit_preferences(+What) is det.
%
%   Edit the specified user preference file.  What is one of
%
%       * =xpce=
%       * =prolog=
%
%   The UI components are started asynchronously in the XPCE thread.

prolog_edit_preferences(What) :-
    in_pce_thread(pce_edit_preferences(What)).

pce_edit_preferences(What) :-
    locate_preferences(What, File),
    auto_call(start_emacs),
    (   \+ access_file(File, exist)
    ->  send(@display, confirm,
             'Preferences file %s doesn''t exist.\nCreate it?', File),
        (   default_preferences(What, DefFile)
        ->  copy_file(DefFile, File)
        ;   true
        )
    ;   access_file(File, write)
    ->  true
    ;   send(@display, inform,
             'You cannot modify the preferences file %s', File)
    ),
    send(@emacs, goto_source_location, File).

locate_preferences(xpce, File) :-
    ensure_xpce_config_dir(Dir),
    get(string('%s/Defaults', Dir), value, File).
locate_preferences(prolog, File) :-
    prolog_init_file(Base),
    (   absolute_file_name(user_profile(Base), File,
                           [ access(read),
                             file_errors(fail)
                           ])
    ->  true
    ;   absolute_file_name(app_preferences(Base), File,
                           [ access(write),
                             file_errors(fail)
                           ])
    ).

%!  prolog_init_file(-Base)
%
%   Get the base-name of the Prolog user initialization file.
%
%   @tbd This should have a public interface.

:- if(current_predicate('$cmd_option_val'/2)).
prolog_init_file(Base) :-
    '$cmd_option_val'(init_file, Base).
:- else.
prolog_init_file(Base) :-
    '$option'(init_file, Base).
:- endif.


%!  default_preferences(+Id, -File)
%
%   If there is a default file for the preferences, return a path to
%   it, so the user can be presented a starting point.

default_preferences(prolog, File) :-
    member(Location,
           [ swi('customize/swipl.ini'),
             swi('customize/dotswiplrc')
           ]),
    absolute_file_name(Location, File,
                       [ access(read),
                         file_errors(fail)
                       ]),
    !.
default_preferences(xpce, File) :-
    absolute_file_name(pce('Defaults.user'), File,
                       [ access(read),
                         file_errors(fail)
                       ]),
    !.


%!  ensure_xpce_config_dir(-Dir:atom)
%
%   Ensure existence of the personal XPCE config directory.

ensure_xpce_config_dir(Dir) :-
    get(@pce, application_data, AppDir),
    (   send(AppDir, exists)
    ->  true
    ;   send(AppDir, make)
    ),
    get(AppDir, path, Dir).


copy_file(From, To) :-
    send(file(To), copy, From).
