/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2020, University of Amsterdam
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

:- module(stream_info,
          [ stream_info/1               % +Stream
          ]).
:- autoload(library(error),[existence_error/2]).
:- autoload(library(lists),[member/2]).

:- use_foreign_library(foreign(streaminfo)).

%!  stream_info(+Stream) is det.
%
%   Print detailed information about a stream   or  a file-number to
%   the error output. The  output  of   this  command  is  meant for
%   experts and requires  knowledge  about   the  implementation  of
%   streams. It has been  added  to   diagnose  leaking  streams  in
%   web-servers. For example,  on  linux   systems  we  can  examine
%   process file-descriptors using
%
%   ==
%   % ls -l /proc/<pid>/fd
%   ==
%
%   If now (say) descriptor 15 is open   where  it should not be, we
%   can this command to find the associated Prolog streams and print
%   as mush as possible information about the stream.
%
%   ==
%   ?- stream_info(15).
%   ==
%
%   @param  Stream  A stream-handle, alias name, (integer) system
%           file handle or `'<stream>(address)'` atom.

stream_info(Stream) :-
    is_stream(Stream),
    !,
    forall(stream_property(Stream, P),
           print_property(P)),
    nl,
    catch('$stream_info'(current_output, Stream), E, true),
    (   nonvar(E)
    ->  format('~w:~t~25|~q~n', ['pending exception', E])
    ;   true
    ).
stream_info(FileNo) :-
    integer(FileNo),
    !,
    findall(S, stream_property(S, file_no(FileNo)), Streams),
    length(Streams, Len),
    format('File no ~w is connected to ~d streams~n', [FileNo, Len]),
    forall(member(Stream, Streams),
           (   format('****************~nStream ~p:~n', [Stream]),
               stream_info(Stream))).
stream_info(Atom) :-
    atom(Atom),
    (   stream_property(Stream, type(_)),
        format(atom(Atom), '~p', [Stream])
    ->  stream_info(Stream)
    ;   existence_error(stream, Atom)
    ).

print_property(P) :-
    P =.. [Name,Value],
    !,
    format('~w:~t~25|~q~n', [Name, Value]).
print_property(input) :- !.
print_property(output) :- !.
print_property(P) :-
    format('~p~n', [P]).
