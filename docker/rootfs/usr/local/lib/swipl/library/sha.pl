/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2007-2013, University of Amsterdam
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

:- module(crypto_hash,
          [ sha_hash/3,                 % +Data, -Hash, +Options
            sha_new_ctx/2,              % -NewContext, +Options
            sha_hash_ctx/4,             % +OldCtx, +Data, -NewCtx, -Hash
            hmac_sha/4,                 % +Key, +Data, -Hash, +Options
            file_sha1/2,                % +File, -SHA1
            hash_atom/2                 % +Hash, -HexAtom
          ]).
:- use_foreign_library(foreign(sha4pl)).

/** <module> SHA secure hashes

This library provides a  lightweight   implementation  for computing SHA
secure  hashes.  A  general  secure  hash    interface  is  provided  by
library(crypto), part of the `ssl` package.

@see library(md5), library(hash_stream) and library(crypto).
*/

%!  sha_hash(+Data, -Hash, +Options) is det
%
%   Hash is the SHA hash of Data, The conversion is controlled
%   by Options:
%
%     * algorithm(+Algorithm)
%     One of =sha1= (default), =sha224=, =sha256=, =sha384= or
%     =sha512=
%     * encoding(+Encoding)
%     If Data is a sequence of character _codes_, this must be
%     translated into a sequence of _bytes_, because that is what
%     the hashing requires.  The default encoding is =utf8=.  The
%     other meaningful value is =octet=, claiming that Data contains
%     raw bytes.
%
%   @param  Data is either an atom, string or code-list
%   @param  Hash is a packed string

%!  sha_new_ctx(-NewContext, +Options) is det
%
%   NewContext is unified with the empty SHA computation context
%   (which includes the Options.)  It could later be passed to
%   sha_hash_ctx/4. For Options, see sha_hash/3.
%
%   @param  NewContext is an opaque pure Prolog term that is
%           subject to garbage collection.

%!  sha_hash_ctx(+OldContext, +Data, -NewContext, -Hash) is det
%
%   Hash is the SHA hash of Data.  NewContext is the new SHA
%   computation context, while OldContext is the old.  OldContext
%   may be produced by a prior invocation of either sha_new_ctx/3 or
%   sha_hash_ctx/4 itself.
%
%   This predicate allows a SHA function to be computed in chunks,
%   which may be important while working with Metalink (RFC 5854),
%   BitTorrent or similar technologies, or simply with big files.

%!  hmac_sha(+Key, +Data, -Hash, +Options) is det
%
%   For Options, see sha_hash/3.

%!  file_sha1(+File, -SHA1:atom) is det.
%
%   True when SHA1 is the SHA1 hash for the content of File. Options
%   is passed to open/4 and typically used to control whether binary
%   or text encoding must be used. The   output is compatible to the
%   =sha1sum= program found in many systems.

file_sha1(File, Hash) :-
    setup_call_cleanup(
        open(File, read, In, [type(binary)]),
        stream_sha1(In, Hash),
        close(In)).

stream_sha1(Stream, Hash) :-
    sha_new_ctx(Ctx0, [encoding(octet)]),
    update_hash(Stream, Ctx0, _Ctx, 0, HashCodes),
    hash_atom(HashCodes, Hash).

update_hash(In, Ctx0, Ctx, Hash0, Hash) :-
    at_end_of_stream(In),
    !,
    Ctx = Ctx0,
    Hash = Hash0.
update_hash(In, Ctx0, Ctx, _Hash0, Hash) :-
    read_pending_codes(In, Data, []),
    sha_hash_ctx(Ctx0, Data, Ctx1, Hash1),
    update_hash(In, Ctx1, Ctx, Hash1, Hash).



%!  hash_atom(+HashCodes, -HexAtom) is det.
%
%   Convert a list of bytes (integers 0..255) into the usual
%   hexadecimal notation.  E.g.
%
%     ==
%     ?- sha_hash('SWI-Prolog', Hash, []),
%        hash_atom(Hash, Hex).
%     Hash = [61, 128, 252, 38, 121, 69, 229, 85, 199|...],
%     Hex = '3d80fc267945e555c730403bd0ab0716e2a68c68'.
%     ==

hash_atom(Codes, Hash) :-
    phrase(bytes_hex(Codes), HexCodes),
    atom_codes(Hash, HexCodes).

bytes_hex([]) --> [].
bytes_hex([H|T]) -->
    { High is H>>4,
      Low is H /\ 0xf,
      code_type(C0, xdigit(High)),
      code_type(C1, xdigit(Low))
    },
    [C0,C1],
    bytes_hex(T).


		 /*******************************
		 *            SANDBOX		*
		 *******************************/

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(crypto_hash:sha_hash(_,_,_)).
sandbox:safe_primitive(crypto_hash:hmac_sha(_,_,_,_)).
sandbox:safe_primitive(crypto_hash:hash_atom(_,_)).
