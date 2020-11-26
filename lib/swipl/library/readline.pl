/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module(readline,
          [ rl_read_init_file/1,
            rl_add_history/1,
            rl_write_history/1,
            rl_read_history/1
          ]).

readline_ok :-
    \+ current_prolog_flag(console_menu_version, qt),
    \+ current_prolog_flag(readline, editline),
    stream_property(user_input, tty(true)).

:- use_foreign_library(foreign(readline4pl)).
:- if(readline_ok).
:- initialization rl_wrap.
:- endif.

/** <module> GNU readline interface

This library binds GNU  libreadline  to   SWI-Prolog.  The  GNU readline
library provides emacs and vi based  editing   of  queries on the Prolog
toplevel, including TAB-based completion and history.

This library is by default  loaded   into  an interactive Prolog process
that is connected to a  (Unix)  terminal.   Loading  can  be  stopped by
setting the Prolog flag `readline` to `false`.

@license Although the  interface  is   BSD-licensed,  the  GNU  readline
library itself is covered by  the   GPL  (General  Public License). This
implies that loading this  library  in   an  application  makes all code
loaded into the application subject to the GPL conditions.
*/

%!  rl_read_init_file(+File) is det.
%
%   Read a GNU readline config file.  See   the  GNU readline manual for
%   details.

%!  rl_add_history(+Line) is det.
%
%   Add a line to the history.

%!  rl_write_history(+File) is det.
%
%   Save the history to File. This  can   be  reloaded in a next session
%   using rl_read_history/1.

%!  rl_read_history(+File) is det.
%
%   Read a saved history from File.


:- multifile
    prolog:history/2.

prolog:history(_Input, add(Line)) :-
    rl_add_history(Line).
prolog:history(_Input, load(File)) :-
    rl_read_history(File).
prolog:history(_Input, save(File)) :-
    rl_write_history(File).
