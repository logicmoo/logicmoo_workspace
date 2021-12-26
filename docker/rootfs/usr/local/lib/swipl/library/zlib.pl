/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2016, University of Amsterdam
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

:- module(zlib,
          [ zopen/3,                    % +Stream, -ZStream, +Option
            gzopen/3,                   % +File, +Mode, -Stream
            gzopen/4                    % +File, +Mode, -Stream, +Options
          ]).
:- autoload(library(apply),[partition/4]).
:- autoload(library(error),[must_be/2]).
:- autoload(library(option),[merge_options/3,option/2]).

:- predicate_options(zopen/3, 3,
                     [ format(oneof([gzip,deflate])),
                       multi_part(boolean),
                       close_parent(boolean),
                       level(between(0,9))
                     ]).
:- predicate_options(gzopen/4, 4,
                     [ pass_to(zopen/3, 3),
                       pass_to(system:open/4, 4)
                     ]).

/** <module> Zlib wrapper for SWI-Prolog

Read/write compressed data based on the zlib library.

@author Jan Wielemaker
@see    http://www.zlib.net/
@see    http://www.swi-prolog.org/packages/zlib.html
*/

:- use_foreign_library(foreign(zlib4pl)).
:- public zdebug/1.                     % Set debug level

%!  gzopen(+File, +Mode, -Stream) is det.
%!  gzopen(+File, +Mode, -Stream, +Options) is det.
%
%   Open a file compatible with the  gzip   program.  Note that if a
%   file is opened in =append= mode,  a   second  gzip image will be
%   added to the end of the file.   The gzip standard defines that a
%   file can hold multiple  gzip  images   and  inflating  the  file
%   results in a concatenated stream of all inflated images.
%
%   Options are passed to open/4  and   zopen/3.  Default  format is
%   =gzip=.

gzopen(File, Mode, Stream) :-
    gzopen(File, Mode, Stream, []).

gzopen(File, Mode, Stream, Options) :-
    must_be(oneof([read,write,append]), Mode),
    partition(zoption, Options, ZOptions0, OpenOptions),
    merge_options(ZOptions0,
                  [ format(gzip),
                    close_parent(true)
                  ], ZOptions),
    open(File, Mode, Stream0, OpenOptions),
    zopen(Stream0, Stream, ZOptions),
    set_stream(Stream, file_name(File)),
    (   option(alias(Alias), ZOptions)
    ->  set_stream(Stream, alias(Alias))
    ;   true
    ).


zoption(format(_)).
zoption(multi_part(_)).
zoption(level(_)).
zoption(alias(_)).

%!  http:encoding_filter(+Encoding, +In0, -In) is semidet.
%
%   Act as plugin for library(http/http_open) for processing content
%   with =|Content-encoding: gzip|=

http:encoding_filter(gzip, In0, In) :-
    zopen(In0, In,
          [ close_parent(true)
          ]).
