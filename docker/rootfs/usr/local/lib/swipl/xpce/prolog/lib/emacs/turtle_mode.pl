/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org/projects/xpce/
    Copyright (c)  2011-2012, VU University Amsterdam
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

:- module(emacs_turtle_mode, []).
:- use_module(library(pce)).
:- use_module(library(emacs_extend)).
:- use_module(sgml_mode).
:- use_module(library(rdf)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_turtle)).

:- pce_autoload(rdf_diagram, library(rdf_diagram)).

:- emacs_begin_mode(
       turtle, language,
       "Mode for editing Turtle documents",
       [ -                   = button(turtle),
         show_diagram        = button(turtle),
         rdf_make            = key('\\C-c\\C-m') + button(compile),
         rdf_load            = key('\\C-c\\C-b') + button(compile),
         open_document       = button(turtle)
       ],
       []).

open_document(M) :->
    "Insert document header"::
    send(M, format,
         '@prefix  rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n\c
              @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\c
              @prefix  owl: <http://www.w3.org/2002/07/owl#> .\n\c
              \n').

show_diagram(M) :->
    "Show diagram of file"::
    get(M, text_buffer, TB),
    setup_call_cleanup(
        pce_open(TB, read, In),
        rdf_read_turtle(stream(In), Triples, []),
        close(In)),
    new(D, rdf_diagram(string('RDF triple diagram'))),
    send(new(report_dialog), below, D),
    send(D, triples, Triples),
    send(D, open).

rdf_make(M) :->
    "Run rdf_make/0"::
    send(@emacs, save_some_buffers),
    rdf_make,
    send(M, report, status, 'RDF Make done').

rdf_load(M) :->
    "Run rdf_load on the file"::
    get(M?text_buffer, file, File),
    (   send(File, instance_of, file)
    ->  send(M, save_if_modified),
        get(File, name, Path),
        rdf_load(Path),
        send(M, report, status, '%s loaded', Path)
    ;   send(M, report, error,
             'Buffer is not connected to a file')
    ).

:- emacs_end_mode.



