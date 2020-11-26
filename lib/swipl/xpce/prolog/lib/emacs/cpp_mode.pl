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

:- module(emacs_cpp_mode, []).
:- use_module(library(pce)).
:- require([ forall/2
           , ignore/1
           ]).

:- emacs_begin_mode(cpp, c,
                    "Mode for (XPCE) C++ programs",
                    [ pce_insert_include_files = button(pce),
                      pce_collect_selectors    = button(pce),
                      pce_replace_selectors    = button(pce),
                      pce_unreplace_selectors  = button(pce)
                    ],
                    []).

from_pce('Arg',                 'Pce').
from_pce('Object',              'Pce').
from_pce('Global',              'Pce').
from_pce('Status',              'Pce').

from_pce('Funcall',             'Call').
from_pce('Variable',            'Class').
from_pce('Receiver',            'Class').
from_pce('Cell',                'Chain').
from_pce('Pointer',             'Pointer').

canonicalise(Headers) :-
    forall(from_pce(F, T), ignore(send(Headers, replace, F, T))),
    send(Headers, sort), send(Headers, unique),
    ignore(send(Headers, move_after, 'Pce')).


pce_insert_include_files(M) :->
    "Collect the used Pce classes and insert includes"::
    get(M, collect, regex('#\\s*include\\s+<pce/([A-Za-z]+).h>'), 1, CE),
    canonicalise(CE),

    get(M, collect, regex('\\yPce([A-Z][a-zA-Z]*)'), 1, Ch),
    canonicalise(Ch),

    (   send(CE, equal, Ch)
    ->  send(M, report, status, 'No changes necessary')
    ;   send(Ch, for_all,
             message(M, insert,
                     create(string, '#include <pce/%s.h>\n', @arg1)))
    ).


pce_collect_selectors(M) :->
    "Collect selectors and generate PcN... lines"::
    get(M, collect, regex('\\y(send|get)\\("(\\w+)"'), 2, Used),
    get(M, collect, regex('^PceArg\\s+\\yPcN(\\w+)\\y'), 1, Defined),
    (   send(Used, equal, Defined)
    ->  send(M, report, status, 'No changes necessary')
    ;   send(Used, for_all,
             message(M, insert,
                     create(string, 'static PceArg PcN%s("%s");\n',
                            @arg1, @arg1)))
    ).


pce_replace_selectors(M) :->
    "Replace all ""bla"" by PcN..."::
    get(M, collect,
        regex('^(static\\s+)?PceArg\\s+\\yPcN(\\w+)\\y'), 2, Defined),
    send(Defined, for_all, message(M, pce_replace_selector, @arg1)).

pce_replace_selector(M, Name:char_array) :->
    "Replace all occurrences after the caret of ""name"" by PcNname"::
    get(M, text_buffer, TB),
    send(regex(string('"%s"', Name)), for_all, TB,
               message(@arg1, replace, @arg2, string('PcN%s', Name)),
               M?caret).

pce_unreplace_selectors(M) :->
    "Replace all PcNbla by ""bla"""::
    get(M, text_buffer, TB),
    send(regex('PcN(\\w+)'), for_all, TB,
         message(@arg1, replace, @arg2, '"\\1"'),
         M?caret).


collect(M, Re:regex, Reg:[int], Result) :<-
    "Collect specified register of regex matches"::
    get(M, text_buffer, TB),
    new(Result, chain),
    send(Re, for_all, TB,
         message(Result, append, ?(@arg1, register_value,
                                   @arg2, Reg, name))),
    send(Result, sort),
    send(Result, unique).

:- emacs_end_mode.
