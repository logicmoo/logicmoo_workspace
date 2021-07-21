/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1996-2011, University of Amsterdam
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

:- module(pce_shell,
          [ pce_shell_command/1
          ]).
:- use_module(library(pce)).
:- require([ atomic_list_concat/3
           ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pce_shell_command(+Command(..+Arg..)
    Run an external command that is no supposed to produce output and
    wait for it to terminate.  The output of the command is captured
    in a text_buffer.  If the command fails, a view is created to show
    the output and this predicate will fail.

    This predicate is generally better then using Prolog's system/1,
    shell/1 or unix/1 as it ensures event-handling during the execution
    of the external command and presentation of possible output in a
    window rather then to the Prolog window.

    Example:

            ...
            pce_shell_command(lpr('-PPostscript', File)),
            ...

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pce_shell_command(Cmd) :-
    Cmd =.. List,
    ProcTerm =.. [process | List],
    new(P, ProcTerm),
    new(TB, text_buffer),
    send(P, input_message,
         and(message(@arg1, translate, 13, @nil),
             message(TB, append, @arg1))),
    send(P, record_separator, @nil),
    atomic_list_concat(List, ' ', CmdAtom),
    send(P, report, progress, 'running %s ...', CmdAtom),
    send(P, open),
    send(P, wait),
    (   get(P, code, 0)
    ->  send(P, report, done),
        free(TB)
    ;   get(P, code, Code),
        (   atom(Code)
        ->  send(P, report, error, 'Caught signal %s', Code)
        ;   send(P, report, error, 'Exit status %s', Code)
        ),
        new(V, view(string('Output of %s', CmdAtom))),
        send(V, text_buffer, TB),
        send(new(D, dialog), below, V),
        send(D, append, button(quit, message(V, destroy))),
        send(V?frame, confirm_done, @off),
        send(V, open),
        fail
    ).
