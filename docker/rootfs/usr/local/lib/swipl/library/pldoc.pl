/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2020, University of Amsterdam
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

:- module(pldoc,
          [ doc_collect/1,              % +Bool

            pldoc_loading/0             % True if we are loading
          ]).
:- dynamic
    pldoc_loading/0.

pldoc_loading.

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.

user:file_search_path(pldoc, library(pldoc)).
user:file_search_path(package_documentation, swi('doc/packages')).

:- multifile
    tag_order/2.                    % +Tag, -Order

:- create_prolog_flag(pldoc_collecting, false, []).

doc_collect(OnOff) :-
    set_prolog_flag(pldoc_collecting, OnOff).

:- doc_collect(true).

:- use_module(pldoc(doc_process)).
:- use_module(pldoc(doc_register)).
:- use_module(pldoc(doc_modes)).
:- use_module(pldoc(doc_wiki)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(operators)).
:- use_module(library(prolog_source)).


                 /*******************************
                 *        DOCUMENTATION         *
                 *******************************/

/** <module> Process source documentation

The pldoc module processes structured comments   in Prolog source files.
These  comments  can  be  saved   to    file.   During  development  the
documentation system can start a web-server to view the documentation of
loaded sources through your browser. The server   is defined in the file
doc_http.pl and started through doc_server/1.

During  development,  a  typical  scenario  is    to   first  start  the
documentation server and start  a   browser  at <http://localhost:4000>.
Note that by default the web-pages allow  for starting an editor only if
the connection comes from =localhost=.  See   doc_server/2  to realise a
different setup.

==
:- doc_server(4000).
:- [application].
==

@author  Jan Wielemaker
@license LGPL
@see     doc_server/1, doc_server/2, doc_collect/1.
*/

%!  doc_collect(+Bool) is det.
%
%   Switch collecting comments true/false.   This autoload predicate
%   can be used to force loading  the   pldoc  library. In a typical
%   development setup loading pldoc  is   normally  triggered  using
%   doc_server/1.

%!  pldoc_loading is semidet.
%
%   True if we are loading the  PlDoc libraries. Required internally
%   to avoid undefined predicates  while   re-loading  and  document
%   itself.

%!  tag_order(?Tag, ?Order) is semidet.
%
%   Hook that allows for defining additional tags.
%
%   @see pldoc_wiki:tag_order/2 for the default definition.


                 /*******************************
                 *           FINISH UP          *
                 *******************************/

:- retract(pldoc_loading),
   process_stored_comments.
