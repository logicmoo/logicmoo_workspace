/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2015-2016, VU University Amsterdam
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

:- module(md5,
          [ md5_hash/3                  % +Input, -Hash, +Options
          ]).

:- use_foreign_library(foreign(md54pl)).

/** <module> MD5 hashes

Compute MD5 hashes  from  a  Prolog   string.  This  library  provides a
lightweight alternative to the general secure hash interface provided by
library(crypto) from the `ssl` package.

@see library(sha), library(hash_stream) and library(crypto).
*/

%!  md5_hash(+Data, -Hash, +Options) is det.
%
%   Hash is the MD5 hash of Data,   The  conversion is controlled by
%   Options:
%
%     * encoding(+Encoding)
%     If Data is a sequence of character _codes_, this must be
%     translated into a sequence of _bytes_, because that is what
%     the hashing requires.  The default encoding is =utf8=.  The
%     other meaningful value is =octet=, claiming that Data contains
%     raw bytes.
%
%   @arg Data is either an atom, string, code-list or char-list.
%   @arg Hash is an atom holding 32 characters, representing the
%   hash in hexadecimal notation

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(md5:md5_hash(_,_,_)).
