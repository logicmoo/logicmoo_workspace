/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015, VU University Amsterdam
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

:- module(prolog_stream,
          [ open_prolog_stream/4        % +Module, +Mode, -Stream, +Data
          ]).
:- use_foreign_library(foreign(prolog_stream)).

/** <module> A stream with Prolog callbacks

This library defines a Prolog  stream   that  realises its low-level I/O
with callbacks to Prolog.  The  library   was  developed  to bind normal
Prolog I/O to Pengines I/O. This type of I/O redirection is probably the
primary use case.
*/

%!  open_prolog_stream(+Module, +Mode, -Stream, +Options)
%
%   Create  a  new  stream  that  implements   its  I/O  by  calling
%   predicates in Module.  The called predicates are:
%
%     - Module:stream_write(+Stream, +String)
%     Called for a `Mode = write` stream if data is available.
%     String contains the (textual) data that is written
%     to Stream.  The callback is called if the buffer of
%     Stream overflows, the user calls flush_output(Stream)
%     or Stream is closed and there is buffered data.
%     - Module:stream_read(+Stream, -Term)
%     Called for a `Mode == read` stream to get new data.  On
%     success the stream extracts text from the provided Term.
%     Term is typically a string, atom, code or character list.
%     If term is not one of the above, it is handed to writeq/1.
%     To signal end-of-file, unify stream with an empty text,
%     e.g., `stream_read(Stream, "")`.
%     - Module:stream_close(+Stream)
%     Called when the stream is closed.  This predicate must
%     succeed.  The callback can be used to cleanup associated
%     resources.
%
%   The current implementation only  deals   with  text streams. The
%   stream uses the =wchar_t= encoding. The   buffer  size must be a
%   multiple of =wchar_t=, i.e., a multiple of four for portability.
%   The _newline_ mode of the stream   is  =posix= on all platforms,
%   disabling the translation `"\n" --> "\r\n"`.
%
%   @arg Options is currently ignored.
%   @bug    Futher versions might require additional callbacks.  As we
%           demand all callbacks to be defined, existing code needs
%           to implement the new callbacks.
