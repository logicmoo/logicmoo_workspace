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

:- module(emacs_gdb_mode, []).

:- use_module(library(pce)).

:- pce_begin_class(emacs_gdb_buffer, emacs_process_buffer).

variable(gdb_command,   string*,        get,    "Collected gdb command").

initialise(B, Target:file, Pid:[int]) :->
    "Create GBD buffer for name"::
    (   Pid == @default
    ->  new(P, process(gdb, '-fullname', Target?name))
    ;   new(P, process(gdb, '-fullname', Target?name, Pid))
    ),
    send_super(B, initialise, P, string('*gdb-%s*', Target?name)),
    send(B, prompt_regex, '(gdb) ').

:- pce_global(@gdb_fullname_regex,      % 032 ==26 == Ctrl-Z!
              new(regex('\032\\032\([^:]+):(\\d+):.*'))).
:- pce_global(@gdb_at_regex,
              new(regex('at ([^:]):(\\d+)'))).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
->insert_process_input is a little complicated.  As both the application
and gdb can issue various  prompts,  we   cannot  break  the  input into
records, but we have to  collect   the  gdb source-referencing commands.
Therefore, if the input contains ^Z^Z,   we start building <-gdb_command
until that string contains the newline. We   then handle the command and
send the remainder of the input to the buffer.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

insert_process_input(B, Input:string) :->
    "Trap input from gdb"::
    get(B, gdb_command, CmdString),
    (   CmdString \== @nil
    ->  send(CmdString, append, Input),
        (   get(CmdString, index, 10, EOL)
        ->  get(CmdString, sub, 0, EOL, CmdLine),
            get(CmdString, sub, EOL, RestInput),
            send(B, gdb_command, CmdLine),
            send(B, slot, gdb_command, @nil),
            send(B, insert_process_input, RestInput)
        ;   true
        )
    ;   get(Input, index, 26, SOC)
    ->  get(Input, sub, 0, SOC, PreInput),
        send(B, send_super, insert_process_input, PreInput),
        get(Input, sub, SOC, CmdLine),
        send(B, slot, gdb_command, CmdLine),
        send(B, insert_process_input, string(''))
    ;   send(B, send_super, insert_process_input, Input)
    ).

gdb_command(B, CmdLine:string) :->
    (   send(@gdb_fullname_regex, match, CmdLine),
        send(B, show_match, @gdb_fullname_regex, CmdLine)
    ->  true
    ;   true
    ).


show_match(B, Re:regex, Input) :->
    "Show position of match"::
    get(Re, register_value, Input, 1, P0),
    get(Re, register_value, Input, 2, L0),
    (   send(P0, prefix, /)
    ->  get(@pce, convert, P0, file, Path)
    ;   new(Path, file(string('%s/%s', B?directory?path, P0)))
    ),
    get(@pce, convert, L0, int, Line),
    new(B2, emacs_buffer(Path)),
    send(B2, open),
    send(B2?editors?head, select_line, Line).

:- pce_end_class.
