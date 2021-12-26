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

:- module(pce_help_file,
          [ pce_help_file/2,
            pce_help/2
          ]).
:- use_module(library(pce)).
:- require([ atomic_list_concat/2
           , is_absolute_file_name/1
           ]).

:- multifile
    user:file_search_path/2.
:- dynamic
    user:file_search_path/2.

user:file_search_path(pce_help, pce('appl-help')).

:- pce_autoload(helper, library(pce_helper)).
:- pce_global(@helper, new(helper)).

:- dynamic
    resource/3.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module is closely  connected  to   library(pce_helper).   It  is  a
separate module to allow the execution of the directive pce_help_file/2,
to register a new help database without   forcing the entire help system
to be loaded.

Loading this module (normally through the   autoloader  or the require/1
directive)  will  make  the  necessary    pce_global   and  pce_autoload
declarations to load the help-system itself as soon as it is referenced.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%!  pce_help_file(+DataBaseId, +FileName).
%
%   Declare `FileName' to hold a helper-format file holding the
%   help-database `DataBaseId'.  FileName will be converted into
%   an absolute filename.  Normally used as a directive.

pce_help_file(Id, FileName) :-
    (   atom(FileName),
        \+ is_absolute_file_name(FileName)
    ->  prolog_load_context(directory, Cwd),
        atomic_list_concat([Cwd, /, FileName], Path)
    ;   Path = FileName
    ),
    retractall(resource(Id, help, Path)),
    asserta(resource(Id, help, Path)).

%!  pce_help(+DataBaseId, +Label)
%
%   Start @helper/helper on the help module `DataBaseId', searching
%   for a fragment with label `Label'.  Normally invoked through the
%   send directly.

pce_help(DataBase, Label) :-
    send(@helper, give_help, DataBase, Label).

