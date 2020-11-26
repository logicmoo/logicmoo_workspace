/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2020, University of Amsterdam
                              VU University Amsterdam
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

:- module(mime_pack,
          [ mime_pack/3                 % +Input, +Stream, ?Boundary
          ]).
:- autoload(html_write,[print_html/2]).
:- autoload(mimetype,[file_mime_type/2]).
:- autoload(library(error),[instantiation_error/1]).
:- autoload(library(lists),[select/3]).

/** <module> Create a MIME message

Simple and partial implementation of MIME   encoding. MIME is covered by
RFC 2045. This library is used by  e.g., http_post_data/3 when using the
form_data(+ListOfData) input specification.

MIME decoding is now  arranged  through   library(mime)  from  the  clib
package, based on the  external  librfc2045   library.  Most  likely the
functionality of this package will be moved to the same library someday.
Packing however is a lot simpler then parsing.
*/

%!  mime_pack(+Inputs, +Out:stream, ?Boundary) is det.
%
%   Pack a number of inputs into a MIME package using a specified or
%   generated boundary. The  generated  boundary   consists  of  the
%   current time in milliseconds  since  the   epoch  and  10 random
%   hexadecimal numbers. Inputs is a  list   of  _documents_ that is
%   added to the mime message.  Each element is one of:
%
%     * Name = Value
%     Name the document. This emits a header of the form below. The
%     =filename= is present if Value is of the form file(File).
%     Value may be any of remaining value specifications.
%
%       ==
%       Content-Disposition: form-data; name="Name"[; filename="<File>"
%       ==
%
%     * html(Tokens)
%     Tokens is a list of HTML tokens as produced by html//1. The
%     token list is emitted using print_html/1.
%
%     * file(File)
%     Emit the contents of File. The =|Content-type|= is derived
%     from the File using file_mime_type/2.  If the content-type
%     is =|text/_|=, the file data is copied in text mode, which
%     implies that it is read in the default encoding of the system
%     and written using the encoding of the Out stream.  Otherwise
%     the file data is copied binary.
%
%     * stream(In, Len)
%     Content is the next Len units from In.  Data is copied using
%     copy_stream_data/3. Units is bytes for binary streams and
%     characters codes for text streams.
%
%     * stream(In)
%     Content of the stream In, copied using copy_stream_data/2.
%     This is often used with memory files (see new_memory_file/1).
%
%     * mime(Attributes, Value, [])
%     Create a MIME header from Attributes and add Value, which can
%     be any of remaining values of this list. Attributes may
%     contain type(ContentType) and/or character_set(CharSet).  This
%     can be used to give a content-type to values that otherwise
%     do not have a content-type.  For example:
%
%       ==
%       mime([type(text/html)], '<b>Hello World</b>', [])
%       ==
%
%     * mime([], '', Parts)
%     Creates a nested multipart MIME message.  Parts is passed
%     as Inputs to a recursive call to mime_pack/2.
%
%     * Atomic
%     Atomic values are passed to write/1. This embeds simple atoms
%     and numbers.
%
%   @param  Out is a stream opened for writing. Typically, it should
%           be opened in text mode using UTF-8 encoding.
%
%   @bug    Does not validate that the boundary does not appear in
%           any of the input documents.

mime_pack(Inputs, OutputStream, Boundary) :-
    make_boundary(Inputs, Boundary),
    pack_list(Inputs, OutputStream, Boundary).

pack_list([], Out, Boundary) :-
    format(Out, '--~w--\r\n', [Boundary]).
pack_list([H|T], Out, Boundary) :-
    format(Out, '--~w\r\n', [Boundary]),
    pack(H, Out),
    format(Out, '\r\n', []),
    pack_list(T, Out, Boundary).

pack(X, _Out) :-
    var(X),
    !,
    instantiation_error(X).
pack(Name=Value, Out) :-
    !,
    (   Value = file(FileName)
    ->  format(Out, 'Content-Disposition: form-data; name="~w"; filename="~w"\r\n',
               [Name, FileName])
    ;   format(Out, 'Content-Disposition: form-data; name="~w"\r\n', [Name])
    ),
    pack(Value, Out).
pack(html(HTML), Out) :-
    !,
    format(Out, 'Content-Type: text/html\r\n\r\n', []),
    print_html(Out, HTML).
pack(file(File), Out) :-
    !,
    (   file_mime_type(File, Type)
    ->  true
    ;   Type = text/plain
    ),
    format(Out, 'Content-Type: ~w\r\n\r\n', [Type]),
    (   Type = text/_
    ->  setup_call_cleanup(
            open(File, read, In),
            copy_stream_data(In, Out),
            close(In))
    ;   stream_property(Out, encoding(OldEncoding)),
        setup_call_cleanup(
            set_stream(Out, encoding(octet)),
            setup_call_cleanup(
                open(File, read, In, [type(binary)]),
                copy_stream_data(In, Out),
                close(In)),
            set_stream(Out, encoding(OldEncoding)))
    ).
pack(stream(In, Len), Out) :-
    !,
    format(Out, '\r\n', []),
    copy_stream_data(In, Out, Len).
pack(stream(In), Out) :-
    !,
    format(Out, '\r\n', []),
    copy_stream_data(In, Out).
pack(mime(Atts, Data, []), Out) :-             % mime_parse compatibility
    !,
    write_mime_attributes(Atts, Out),
    pack(Data, Out).
pack(mime(_Atts, '', Parts), Out) :-
    make_boundary(Parts, Boundary),
    format('Content-type: multipart/mixed; boundary=~w\r\n\r\n',
           [Boundary]),
    mime_pack(Parts, Out, Boundary).
pack(Atom, Out) :-
    atomic(Atom),
    !,
    format(Out, '\r\n', []),
    write(Out, Atom).
pack(Value, _) :-
    throw(error(type_error(mime_part, Value), _)).

write_mime_attributes([], _) :- !.
write_mime_attributes(Atts, Out) :-
    select(type(Type), Atts, A1),
    !,
    (   select(character_set(CharSet), A1, A2)
    ->  format(Out, 'Content-type: ~w; charset=~w\r\n', [Type, CharSet]),
        write_mime_attributes(A2, Out)
    ;   format(Out, 'Content-type: ~w\r\n', [Type]),
        write_mime_attributes(A1, Out)
    ).
write_mime_attributes([_|T], Out) :-
    write_mime_attributes(T, Out).


%!  make_boundary(+Inputs, ?Boundary) is det.
%
%   Generate a boundary.  This should check all input sources whether
%   the boundary is enclosed.

make_boundary(_, Boundary) :-
    atomic(Boundary),
    !.
make_boundary(_, Boundary) :-
    get_time(Now),
    A is random(1<<16),
    B is random(1<<16),
    C is random(1<<16),
    D is random(1<<16),
    E is random(1<<16),
    format(atom(Boundary), '------~3f~16r~16r~16r~16r~16r',
           [Now, A, B, C, D, E]).

