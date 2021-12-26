/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2016, VU University Amsterdam
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

:- module(hash_stream,
          [ open_hash_stream/3,         % +OrgStream, -HashStream, +Options
            stream_hash/2               % +HashStream, -Hash
          ]).
:- use_foreign_library(foreign(hashstream)).
:- predicate_options(open_hash_stream/3, 3,
                     [ close_parent(boolean),
                       algorithm(oneof([md5,sha1,sha224,sha256,sha384,sha512]))
                     ]).

/** <module> Maintain a hash on a stream

This library defines a filter stream that   maintains a hash of the data
that passes through the stream. It can be   used  to compute the hash of
input data while it is being processed.   This is notably interesting if
data is processed from a socket as it avoids the need for collecting the
data first in a temporary file.

A typical processing sequence  is   illustrated  below,  where process/2
somehow processed the data  and  save_result/3   records  the  result as
obtained from `URL` with content digest `SHA256` its `Result`.

  ```
      ...,
      http_open(URL, In0, []),
      open_hash_stream(In0, In, [algorithm(sha256)]),
      process(In, Result),
      stream_hash(In, SHA256),
      close(In),
      save_result(URL, SHA256, Result)
  ```

This library can also be used to compute   the hash for the content of a
file. The advantage is that this code doesn't rely on external tools. It
is considerably faster for short files, but considerably slower on large
files because Prolog I/O  is  based   on  character  streams rather than
blocks.

  ```
  file_hash(Algorithm, File, Hash) :-
      setup_call_cleanup(
          open(File, read, In0, [type(binary)]),
          setup_call_cleanup(
              open_hash_stream(In0, In,
                               [ algorithm(Algorithm),
                                 close_parent(false)
                               ]),
              ( setup_call_cleanup(
                    open_null_stream(Null),
                    copy_stream_data(In, Null),
                    close(Null)),
                stream_hash(In, Hash)
              ),
              close(In)),
          close(In0)).
  ```

@see    In addition to this hash library, SWI-Prolog provides
        library(md5), library(sha) and hash functions through
        library(crypto), part of the `ssl` package.
*/

%!  open_hash_stream(+OrgStream, -HashStream, +Options) is det.
%
%   Open a filter stream on  OrgStream   that  maintains a hash. The
%   hash can be retrieved at any  time using stream_hash/2. Provided
%   options:
%
%     - algorithm(+Algorithm)
%     One of `md5`, `sha1`, `sha224`, `sha256`, `sha384` or
%     `sha512`. Default is `sha1`.
%     - close_parent(+Bool)
%     If `true` (default), closing the filter stream also closes the
%     original (parent) stream.


%!  stream_hash(+HashStream, -Digest:atom) is det.
%
%   Unify Digest with a hash for  the   bytes  send  to or read from
%   HashStream. Note that  the  hash  is   computed  on  the  stream
%   buffers. If the stream is an output  stream, it is first flushed
%   and the Digest represents the hash   at the current location. If
%   the stream is an input stream the  Digest represents the hash of
%   the processed input including the already buffered data.

