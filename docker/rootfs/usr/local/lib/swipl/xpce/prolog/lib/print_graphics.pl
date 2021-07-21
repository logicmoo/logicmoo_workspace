/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  2002-2011, University of Amsterdam
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

:- module(pce_print_graphics, []).
:- use_module(library(pce)).
:- use_module(library(pce_template)).
:- use_module(library(pce_shell)).

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Public methods:

        ->print
        Prints the content of the Window as a single page

        ->save_postscript: [file], [directory]
        Save content of the Window as PostScript
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- pce_begin_class(print_graphics, template,
                   "Template defining ->print").

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Print  the image to the default  printer.  Also this  method should be
extended by requesting additional parameters from the user.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print(Canvas) :->
    "Send to default printer"::
    print_canvas(Canvas).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
There are two routes to print.  On   MS-Windows  printing is achieved by
drawing on a GDI representing a printer, after which the Windows printer
driver creates printer-codes and sends them to the printer. The standard
Windows print dialog is shown by   win_printer->setup. Next we need some
calculation effort to place our diagram reasonably on the page.

In the Unix world, things go different. In general you make a PostScript
file and hand this  to  the   print-spooler,  which  will  translate the
device-independant PostScript to whatever the printer needs.

XPCE doesn't (yet)  try  to  hide   the  difference  between  these  two
approaches.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_canvas(Canvas) :-                 % MS-Windows
    get(@pce, convert, win_printer, class, _),
    !,
    (   send(Canvas, has_get_method, default_file),
        get(Canvas, default_file, Job)
    ->  true
    ;   Job = '<unknown job>'
    ),
    new(Prt, win_printer(Job)),
    send(Prt, setup, Canvas),
    send(Prt, open),
    get(Canvas, bounding_box, area(X, Y, W, H)),
    get(@display, dots_per_inch, size(DX, DY)),
    InchW is W/DX,
    InchH is H/DY,

    get(Prt, size, size(PW0, PH0)),
    get(Prt, dots_per_inch, size(RX, RY)),
    MarX is RX,                     % default 1 inch margins
    MarY is RY,
    PrInchW is (PW0-MarX*2)/RX,
    PrInchH is (PH0-MarY*2)/RY,

    send(Prt, map_mode, isotropic),
    (   InchW < PrInchW,
        InchH < PrInchH             % it fits on the page
    ->  OX is MarX + ((PrInchW-InchW)/2)*RX,
        send(Prt, window, area(X, Y, DX, DY)),
        send(Prt, viewport, area(OX, MarY, RX, RY))
    ;   Aspect is min(PrInchW/InchW, PrInchH/InchH),
        ARX is integer(Aspect*RX),
        ARY is integer(Aspect*RY),
        send(Prt, window, area(X, Y, DX, DY)),
        send(Prt, viewport, area(MarX, MarY, ARX, ARY))
    ),
    send(Prt, draw_in, Canvas?graphicals),
    send(Prt, close),
    free(Prt).
print_canvas(Canvas) :-                 % Unix/PostScript
    get(Canvas, print_command, Command),
    new(PsFile, file),
    send(PsFile, open, write),
    send(PsFile, append, Canvas?postscript),
    send(PsFile, append, 'showpage\n'),
    send(PsFile, close),
    get(PsFile, absolute_path, File),
    get(string('%s "%s"', Command, File), value, ShellCommand),
    pce_shell_command('/bin/sh'('-c', ShellCommand)),
    send(PsFile, remove),
    send(PsFile, done),
    send(Canvas, report, status, 'Sent to printer').


print_command(Canvas, Command:name) :<-
    "Get name of the printer"::
    get(Canvas, frame, Frame),
    default_printer(DefPrinter),
    get(Canvas, print_command_template, CmdTempl),
    print_cmd(CmdTempl, DefPrinter, Cmd),
    new(D, dialog(print_command?label_name)),
    send(D, append, new(P, text_item(print_command, Cmd))),
    send(D, append, button(cancel, message(D, return, @nil))),
    send(D, append, button(ok, message(D, return, P?selection))),
    send(D, default_button, ok),
    send(D, transient_for, Frame),
    send(D, modal, transient),
    get(D, confirm_centered, Canvas?frame?area?center, Answer),
    send(D, destroy),
    Answer \== @nil,
    Command = Answer.

default_printer(Printer) :-
    get(@pce, environment_variable, 'PRINTER', Printer),
    !.
default_printer(postscript).


print_job_name(_, Job) :<-
    "Default name of the printer job"::
    Job = 'XPCE/SWI-Prolog'.

print_command_template(_, Command) :<-
    "Default command to send a job to the printer"::
    Command = 'lpr -P%p'.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
print_cmd(+Template, +Printer, +File,  -Command)   determines  the shell
command to execute in order to get `File' printed on `Printer' using the
given template. The substitutions are handled by a regex object.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_cmd(Template, Printer, Cmd) :-
    new(S, string('%s', Template)),
    substitute(S, '%p', Printer),
    get(S, value, Cmd),
    free(S).

substitute(S, F, T) :-
    new(R, regex(F)),
    send(R, for_all, S,
         message(@arg1, replace, @arg2, T)),
    free(R).


                 /*******************************
                 *          POSTSCRIPT          *
                 *******************************/


save_postscript(Canvas, File:file=[file], Directory:directory=[directory]) :->
    "Save content as PostScript to File"::
    (   File == @default
    ->  get(@finder, file, save,
            chain(tuple('PostScript', ps),
                  tuple('Encapsulated PostScript', eps)),
            Directory,
            FileName)
    ;   FileName = File
    ),
    new(PsFile, file(FileName)),
    send(PsFile, open, write),
    send(PsFile, append, Canvas?postscript),
    send(PsFile, append, 'showpage\n'),
    send(PsFile, close),
    send(Canvas, report, status, 'Saved PostScript to %s', PsFile).

:- pce_end_class(print_graphics).

