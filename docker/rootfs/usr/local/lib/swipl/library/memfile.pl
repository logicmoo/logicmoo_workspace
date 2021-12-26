/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2001-2020, University of Amsterdam
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

:- module(memory_file,
          [ new_memory_file/1,          % -Handle
            free_memory_file/1,         % +Handle
            size_memory_file/2,         % +Handle, -Size
            size_memory_file/3,         % +Handle, -Size, +Encoding
            open_memory_file/3,         % +Handle, +Mode, -Stream
            open_memory_file/4,         % +Handle, +Mode, -Stream, +Options
            insert_memory_file/3,       % +Handle, +Position, +Data
            delete_memory_file/3,       % +Handle, +Position, +Length
            atom_to_memory_file/2,      % +Atom, -Handle
            memory_file_to_atom/2,      % +Handle, -Atom
            memory_file_to_codes/2,     % +Handle, -CodeList
            memory_file_to_string/2,    % +Handle, -String
            memory_file_to_atom/3,      % +Handle, -Atom, +Encoding
            memory_file_to_codes/3,     % +Handle, -CodeList, +Encoding
            memory_file_to_string/3,    % +Handle, -String, +Encoding
            memory_file_substring/5,    % +Handle, +Before, +Length, +After, -String
            memory_file_line_position/4,% +Handle, ?Line, ?ListPos, ?Offset
            utf8_position_memory_file/3 % +Handle, -Here, -Size
          ]).
:- use_foreign_library(foreign(memfile)).

:- predicate_options(open_memory_file/4, 4,
                     [ encoding(encoding),
                       free_on_close(boolean)
                     ]).
