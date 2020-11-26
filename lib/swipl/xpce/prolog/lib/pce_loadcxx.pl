/*  Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (c)  1997-2011, University of Amsterdam
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

:- module(pce_load_cxx,
          [ pce_load_cxx/1              % +File
          ]).
:- use_module(library(pce)).
:- require([ absolute_file_name/3
           , feature/2
           , open_dll/2
           , open_shared_object/2
           ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pce_load_cxx(+Spec)
    Load a Unix shared object or Windows DLL or somilar object, containing
    XPCE/C++ code.  This call deals with OS and Prolog incompatibilities.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pce_load_cxx(File) :-
    current_prolog_flag(open_shared_object, true),
    !,
    absolute_file_name(File,
                       [ extensions([so]),
                         access(read)
                       ],
                       Path),
    send(@pce, succeed),            % ensure XPCE is loaded
    open_shared_object(Path, _Handle).
