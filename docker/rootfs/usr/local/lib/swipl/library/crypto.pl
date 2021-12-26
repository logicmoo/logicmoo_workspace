/*  Part of SWI-Prolog

    Author:        Markus Triska and Matt Lilley
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2017, SWI-Prolog Foundation
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

:- module(crypto,
          [ crypto_n_random_bytes/2,    % +N, -Bytes
            crypto_data_hash/3,         % +Data, -Hash, +Options
            crypto_file_hash/3,         % +File, -Hash, +Options
            crypto_context_new/2,       % -Context, +Options
            crypto_data_context/3,      % +Data, +C0, -C
            crypto_context_hash/2,      % +Context, -Hash
            crypto_open_hash_stream/3,  % +InStream, -HashStream, +Options
            crypto_stream_hash/2,       % +HashStream, -Hash
            crypto_password_hash/2,     % +Password, ?Hash
            crypto_password_hash/3,     % +Password, ?Hash, +Options
            crypto_data_hkdf/4,         % +Data, +Length, -Bytes, +Options
            ecdsa_sign/4,               % +Key, +Data, -Signature, +Options
            ecdsa_verify/4,             % +Key, +Data, +Signature, +Options
            crypto_data_decrypt/6,      % +CipherText, +Algorithm, +Key, +IV, -PlainText, +Options
            crypto_data_encrypt/6,      % +PlainText, +Algorithm, +Key, +IV, -CipherText, +Options
            hex_bytes/2,                % ?Hex, ?List
            rsa_private_decrypt/4,      % +Key, +Ciphertext, -Plaintext, +Enc
            rsa_private_encrypt/4,      % +Key, +Plaintext, -Ciphertext, +Enc
            rsa_public_decrypt/4,       % +Key, +Ciphertext, -Plaintext, +Enc
            rsa_public_encrypt/4,       % +Key, +Plaintext, -Ciphertext, +Enc
            rsa_sign/4,                 % +Key, +Data, -Signature, +Options
            rsa_verify/4,               % +Key, +Data, +Signature, +Options
            crypto_modular_inverse/3,   % +X, +M, -Y
            crypto_generate_prime/3,    % +N, -P, +Options
            crypto_is_prime/2,          % +P, +Options
            crypto_name_curve/2,        % +Name, -Curve
            crypto_curve_order/2,       % +Curve, -Order
            crypto_curve_generator/2,   % +Curve, -Generator
            crypto_curve_scalar_mult/4  % +Curve, +Scalar, +Point, -Result
          ]).
:- autoload(library(apply),[foldl/4,maplist/3]).
:- autoload(library(base64),[base64_encoded/3]).
:- autoload(library(error),[must_be/2,domain_error/2]).
:- autoload(library(lists),[select/3,reverse/2]).
:- autoload(library(option),[option/3,option/2]).

:- use_foreign_library(foreign(crypto4pl)).


/** <module> Cryptography and authentication library

This library provides bindings  to  functionality   of  OpenSSL  that is
related to cryptography and authentication,   not  necessarily involving
connections, sockets or streams.

The  hash functionality  of this  library subsumes  and extends  that of
`library(sha)`, `library(hash_stream)` and `library(md5)` by providing a
unified interface to all available digest algorithms.

The underlying  OpenSSL library  (`libcrypto`) is dynamically  loaded if
_either_ `library(crypto)`  or `library(ssl)` are loaded.  Therefore, if
your application uses `library(ssl)`,  you can use `library(crypto)` for
hashing without increasing the memory  footprint of your application. In
other cases, the specialised hashing  libraries are more lightweight but
less general alternatives to `library(crypto)`.

@author [Markus Triska](https://www.metalevel.at)
@author Matt Lilley
*/

%%  crypto_n_random_bytes(+N, -Bytes) is det
%
%   Bytes is unified with a list of N cryptographically secure
%   pseudo-random bytes. Each byte is an integer between 0 and 255. If
%   the internal pseudo-random number generator (PRNG) has not been
%   seeded with enough entropy to ensure an unpredictable byte
%   sequence, an exception is thrown.
%
%   One way to relate such a list of bytes to an _integer_ is to use
%   CLP(FD) constraints as follows:
%
%   ==
%   :- use_module(library(clpfd)).
%
%   bytes_integer(Bs, N) :-
%           foldl(pow, Bs, 0-0, N-_).
%
%   pow(B, N0-I0, N-I) :-
%           B in 0..255,
%           N #= N0 + B*256^I0,
%           I #= I0 + 1.
%   ==
%
%   With this definition, you can generate a random 256-bit integer
%   _from_ a list of 32 random _bytes_:
%
%   ==
%   ?- crypto_n_random_bytes(32, Bs),
%      bytes_integer(Bs, I).
%   Bs = [98, 9, 35, 100, 126, 174, 48, 176, 246|...],
%   I = 109798276762338328820827...(53 digits omitted).
%   ==
%
%   The above relation also works in the other direction, letting you
%   translate an integer _to_ a list of bytes. In addition, you can
%   use hex_bytes/2 to convert bytes to _tokens_ that can be easily
%   exchanged in your applications. This also works if you have
%   compiled SWI-Prolog without support for large integers.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   SHA256 is the current default for several hash-related predicates.
   It is deemed sufficiently secure for the foreseeable future.  Yet,
   application programmers must be aware that the default may change in
   future versions. The hash predicates all yield the algorithm they
   used if a Prolog variable is used for the pertaining option.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

default_hash(sha256).

functor_hash_options(F, Hash, Options0, [Option|Options]) :-
        Option =.. [F,Hash],
        (   select(Option, Options0, Options) ->
            (   var(Hash) ->
                default_hash(Hash)
            ;   must_be(atom, Hash)
            )
        ;   Options = Options0,
            default_hash(Hash)
        ).


%%  crypto_data_hash(+Data, -Hash, +Options) is det
%
%   Hash is the hash of Data. The conversion is controlled
%   by Options:
%
%    * algorithm(+Algorithm)
%    One of =md5= (_insecure_), =sha1= (_insecure_), =ripemd160=,
%    =sha224=, =sha256=, =sha384=, =sha512=, =sha3_224=, =sha3_256=,
%    =sha3_384=, =sha3_512=, =blake2s256= or =blake2b512=. The BLAKE
%    digest algorithms require OpenSSL 1.1.0 or greater, and the SHA-3
%    algorithms require OpenSSL 1.1.1 or greater. The default is a
%    cryptographically secure algorithm. If you specify a variable,
%    then that variable is unified with the algorithm that was used.
%    * encoding(+Encoding)
%    If Data is a sequence of character _codes_, this must be
%    translated into a sequence of _bytes_, because that is what
%    the hashing requires.  The default encoding is =utf8=.  The
%    other meaningful value is =octet=, claiming that Data contains
%    raw bytes.
%    * hmac(+Key)
%    If this option is specified, a _hash-based message authentication
%    code_ (HMAC) is computed, using the specified Key which is either
%    an atom, string or list of _bytes_. Any of the available digest
%    algorithms can be used with this option. The cryptographic
%    strength of the HMAC depends on that of the chosen algorithm and
%    also on the key. This option requires OpenSSL 1.1.0 or greater.
%
%  @param Data is either an atom, string or code-list
%  @param Hash is an atom that represents the hash in hexadecimal encoding.
%
%  @see hex_bytes/2 for conversion between hexadecimal encoding and
%  lists of bytes.
%  @see crypto_password_hash/2 for the important use case of passwords.

crypto_data_hash(Data, Hash, Options) :-
    crypto_context_new(Context0, Options),
    crypto_data_context(Data, Context0, Context),
    crypto_context_hash(Context, Hash).

%!  crypto_file_hash(+File, -Hash, +Options) is det.
%
%   True if  Hash is the hash  of the content of  File. For Options,
%   see crypto_data_hash/3.

crypto_file_hash(File, Hash, Options) :-
    setup_call_cleanup(open(File, read, In, [type(binary)]),
                       crypto_stream_hash(In, Hash, Options),
                       close(In)).

crypto_stream_hash(Stream, Hash, Options) :-
    crypto_context_new(Context0, Options),
    update_hash(Stream, Context0, Context),
    crypto_context_hash(Context, Hash).

update_hash(In, Context0, Context) :-
    (   at_end_of_stream(In)
    ->  Context = Context0
    ;   read_pending_codes(In, Data, []),
        crypto_data_context(Data, Context0, Context1),
        update_hash(In, Context1, Context)
    ).


%!  crypto_context_new(-Context, +Options) is det.
%
%   Context is  unified with  the empty  context, taking  into account
%   Options.  The  context can be used  in crypto_data_context/3.  For
%   Options, see crypto_data_hash/3.
%
%   @param Context is an opaque pure  Prolog term that is subject to
%          garbage collection.

crypto_context_new(Context, Options0) :-
    functor_hash_options(algorithm, _, Options0, Options),
    '_crypto_context_new'(Context, Options).


%!  crypto_data_context(+Data, +Context0, -Context) is det
%
%   Context0 is an existing computation  context, and Context is the
%   new context  after hashing  Data in  addition to  the previously
%   hashed data.  Context0 may be  produced by a prior invocation of
%   either crypto_context_new/2 or crypto_data_context/3 itself.
%
%   This predicate allows a hash to be computed in chunks, which may
%   be important while working  with Metalink (RFC 5854), BitTorrent
%   or similar technologies, or simply with big files.

crypto_data_context(Data, Context0, Context) :-
    '_crypto_hash_context_copy'(Context0, Context),
    '_crypto_update_hash_context'(Data, Context).


%!  crypto_context_hash(+Context, -Hash)
%
%   Obtain the  hash code of  Context. Hash is an  atom representing
%   the hash code  that is associated with the current  state of the
%   computation context Context.

crypto_context_hash(Context, Hash) :-
    '_crypto_hash_context_copy'(Context, Copy),
    '_crypto_hash_context_hash'(Copy, List),
    hex_bytes(Hash, List).

%!  crypto_open_hash_stream(+OrgStream, -HashStream, +Options) is det.
%
%   Open a filter stream on OrgStream  that maintains a hash. The hash
%   can be retrieved at any time using crypto_stream_hash/2. Available
%   Options in addition to those of crypto_data_hash/3 are:
%
%     - close_parent(+Bool)
%     If `true` (default), closing the filter stream also closes the
%     original (parent) stream.

crypto_open_hash_stream(OrgStream, HashStream, Options) :-
    crypto_context_new(Context, Options),
    '_crypto_open_hash_stream'(OrgStream, HashStream, Context).


%!  crypto_stream_hash(+HashStream, -Hash) is det.
%
%   Unify  Hash with  a hash  for  the bytes  sent to  or read  from
%   HashStream.  Note  that  the  hash is  computed  on  the  stream
%   buffers. If the stream is an  output stream, it is first flushed
%   and the Digest  represents the hash at the  current location. If
%   the stream is an input stream  the Digest represents the hash of
%   the processed input including the already buffered data.

crypto_stream_hash(Stream, Hash) :-
    '_crypto_stream_hash_context'(Stream, Context),
    crypto_context_hash(Context, Hash).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   The so-called modular crypt format (MCF) is a standard for encoding
   password hash strings. However, there's no official specification
   document describing it. Nor is there a central registry of
   identifiers or rules. This page describes what is known about it:

   https://pythonhosted.org/passlib/modular_crypt_format.html

   As of 2016, the MCF is deprecated in favor of the PHC String Format:

   https://github.com/P-H-C/phc-string-format/blob/master/phc-sf-spec.md

   This is what we are using below. For the time being, it is best to
   treat these hashes as opaque atoms in applications. Please let me
   know if you need to rely on any specifics of this format.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%!  crypto_password_hash(+Password, ?Hash) is semidet.
%
%   If  Hash is  instantiated,  the predicate  succeeds  _iff_ the  hash
%   matches the  given password.  Otherwise, the  call is  equivalent to
%   crypto_password_hash(Password,    Hash,   [])    and   computes    a
%   password-based hash using the default options.

crypto_password_hash(Password, Hash) :-
    (   nonvar(Hash) ->
        must_be(atom, Hash),
        split_string(Hash, "$", "$", ["pbkdf2-sha512",Ps,SaltB64,HashB64]),
        atom_to_term(Ps, t=Iterations, []),
        bytes_base64(SaltBytes, SaltB64),
        bytes_base64(HashBytes, HashB64),
        '_crypto_password_hash'(Password, SaltBytes, Iterations, HashBytes)
    ;   crypto_password_hash(Password, Hash, [])
    ).

%!  crypto_password_hash(+Password, -Hash, +Options) is det.
%
%   Derive  Hash  based  on  Password.  This  predicate  is  similar  to
%   crypto_data_hash/3  in  that it  derives  a  hash from  given  data.
%   However,   it   is  tailored   for   the   specific  use   case   of
%   _passwords_. One  essential distinction is  that for this  use case,
%   the  derivation  of a  hash  should  be  _as  slow as  possible_  to
%   counteract brute-force attacks over possible passwords.
%
%   Another important  distinction is  that equal passwords  must yield,
%   with  very high  probability, _different_  hashes. For  this reason,
%   cryptographically strong  random numbers are automatically  added to
%   the password before a hash is derived.
%
%   Hash is unified with an atom that contains the computed hash and all
%   parameters  that were  used, except  for the  password.  Instead  of
%   storing passwords,  store these  hashes. Later,  you can  verify the
%   validity of  a password  with crypto_password_hash/2,  comparing the
%   then entered password to the stored hash. If you need to export this
%   atom, you should treat it as opaque  ASCII data with up to 255 bytes
%   of length. The maximal length may increase in the future.
%
%   Admissible options are:
%
%     - algorithm(+Algorithm)
%       The algorithm to use. Currently, the only available algorithm
%       is =|pbkdf2-sha512|=, which is therefore also the default.
%     - cost(+C)
%       C is an integer, denoting the binary logarithm of the number
%       of _iterations_ used for the derivation of the hash. This
%       means that the number of iterations is set to 2^C. Currently,
%       the default is 17, and thus more than one hundred _thousand_
%       iterations. You should set this option as high as your server
%       and users can tolerate. The default is subject to change and
%       will likely increase in the future or adapt to new algorithms.
%     - salt(+Salt)
%       Use the given list of bytes as salt. By default,
%       cryptographically secure random numbers are generated for this
%       purpose. The default is intended to be secure, and constitutes
%       the typical use case of this predicate.
%
%   Currently,  PBKDF2  with SHA-512  is  used  as the  hash  derivation
%   function, using 128 bits of  salt. All default parameters, including
%   the algorithm, are subject to change, and other algorithms will also
%   become available  in the  future.  Since  computed hashes  store all
%   parameters that were used during their derivation, such changes will
%   not affect the  operation of existing deployments.  Note though that
%   new hashes will then be computed with the new default parameters.
%
%   @see crypto_data_hkdf/4 for generating keys from Hash.

crypto_password_hash(Password, Hash, Options) :-
    must_be(list, Options),
    option(cost(C), Options, 17),
    Iterations is 2^C,
    Algorithm = 'pbkdf2-sha512', % current default and only option
    option(algorithm(Algorithm), Options, Algorithm),
    (   option(salt(SaltBytes), Options) ->
        true
    ;   crypto_n_random_bytes(16, SaltBytes)
    ),
    '_crypto_password_hash'(Password, SaltBytes, Iterations, HashBytes),
    bytes_base64(HashBytes, HashB64),
    bytes_base64(SaltBytes, SaltB64),
    format(atom(Hash),
           "$pbkdf2-sha512$t=~d$~w$~w", [Iterations,SaltB64,HashB64]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Bidirectional Bytes <-> Base64 conversion as required by PHC format.

   Note that *no padding* must be used, and that we must be able
   to encode the whole range of bytes, not only UTF-8 sequences!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

bytes_base64(Bytes, Base64) :-
    (   var(Bytes) ->
        base64_encoded(Atom, Base64, [padding(false)]),
        atom_codes(Atom, Bytes)
    ;   atom_codes(Atom, Bytes),
        base64_encoded(Atom, Base64, [padding(false)])
    ).


%!  crypto_data_hkdf(+Data, +Length, -Bytes, +Options) is det.
%
%   Concentrate possibly dispersed entropy of Data and then expand it to
%   the desired  length.  Bytes  is unified  with a  list of  _bytes_ of
%   length  Length,  and  is  suitable  as  input  keying  material  and
%   initialization vectors to the symmetric encryption predicates.
%
%   Admissible options are:
%
%      - algorithm(+Algorithm)
%        A hashing algorithm as specified to crypto_data_hash/3. The
%        default is a cryptographically secure algorithm. If you
%        specify a variable, then it is unified with the algorithm
%        that was used.
%      - info(+Info)
%        Optional context and application specific information,
%        specified as an atom, string or list of _bytes_. The default
%        is the zero length atom ''.
%      - salt(+List)
%        Optionally, a list of _bytes_ that are used as salt. The
%        default is all zeroes.
%      - encoding(+Atom)
%        Either =|utf8|= (default) or =|octet|=, denoting
%        the representation of Data as in crypto_data_hash/3.
%
%   The `info/1`  option can be  used to  generate multiple keys  from a
%   single  master key,  using for  example values  such as  =|key|= and
%   =|iv|=, or the name of a file that is to be encrypted.
%
%   This predicate requires OpenSSL 1.1.0 or greater.
%
%   @see crypto_n_random_bytes/2 to obtain a suitable salt.


crypto_data_hkdf(Data, L, Bytes, Options0) :-
        functor_hash_options(algorithm, Algorithm, Options0, Options),
        option(salt(SaltBytes), Options, []),
        option(info(Info), Options, ''),
        option(encoding(Enc), Options, utf8),
        '_crypto_data_hkdf'(Data, SaltBytes, Info, Algorithm, Enc, L, Bytes).

%!  ecdsa_sign(+Key, +Data, -Signature, +Options)
%
%   Create  an ECDSA  signature for  Data with  EC private  key Key.
%   Among the most  common cases is signing a hash  that was created
%   with crypto_data_hash/3 or other predicates of this library. For
%   this reason, the  default encoding (`hex`) assumes  that Data is
%   an atom,  string, character list  or code list  representing the
%   data in hexadecimal notation. See rsa_sign/4 for an example.
%
%   Options:
%
%     - encoding(+Encoding)
%     Encoding to use for Data.  Default is `hex`.  Alternatives
%     are `octet`, `utf8` and `text`.

ecdsa_sign(private_key(ec(Private,Public0,Curve)), Data0, Signature, Options) :-
    option(encoding(Enc0), Options, hex),
    hex_encoding(Enc0, Data0, Enc, Data),
    hex_bytes(Public0, Public),
    '_crypto_ecdsa_sign'(ec(Private,Public,Curve), Data, Enc, Signature).

hex_encoding(hex, Data0, octet, Data) :- !,
    hex_bytes(Data0, Data).
hex_encoding(Enc, Data, Enc, Data).

%!  ecdsa_verify(+Key, +Data, +Signature, +Options) is semidet.
%
%   True iff Signature can be verified as the ECDSA signature for
%   Data, using the EC public key Key.
%
%   Options:
%
%     - encoding(+Encoding)
%     Encoding to use for Data.  Default is `hex`.  Alternatives
%     are `octet`, `utf8` and `text`.

ecdsa_verify(public_key(ec(Private,Public0,Curve)), Data0, Signature0, Options) :-
    option(encoding(Enc0), Options, hex),
    hex_encoding(Enc0, Data0, Enc, Data),
    hex_bytes(Public0, Public),
    hex_bytes(Signature0, Signature),
    '_crypto_ecdsa_verify'(ec(Private,Public,Curve), Data, Enc, Signature).


%!  hex_bytes(?Hex, ?List) is det.
%
%   Relation between a hexadecimal sequence  and a list of bytes.  Hex
%   is  an atom,  string,  list  of characters  or  list  of codes  in
%   hexadecimal  encoding.   This  is  the  format  that  is  used  by
%   crypto_data_hash/3 and  related predicates to  represent _hashes_.
%   Bytes is a list of _integers_ between 0 and 255 that represent the
%   sequence as a  list of bytes.  At least one  of the arguments must
%   be instantiated. When converting List  _to_ Hex, an _atom_ is used
%   to represent the sequence of hexadecimal digits.
%
%   Example:
%
%   ==
%   ?- hex_bytes('501ACE', Bs).
%   Bs = [80, 26, 206].
%   ==
%
%  @see base64_encoded/3 for Base64 encoding, which is often used to
%  transfer or embed binary data in applications.

hex_bytes(Hs, Bytes) :-
    (   ground(Hs) ->
        string_chars(Hs, Chars),
        (   phrase(hex_bytes(Chars), Bytes)
        ->  true
        ;   domain_error(hex_encoding, Hs)
        )
    ;   must_be(list(between(0,255)), Bytes),
        phrase(bytes_hex(Bytes), Chars),
        atom_chars(Hs, Chars)
    ).

hex_bytes([]) --> [].
hex_bytes([H1,H2|Hs]) --> [Byte],
    { char_type(H1, xdigit(High)),
      char_type(H2, xdigit(Low)),
      Byte is High*16 + Low },
    hex_bytes(Hs).

bytes_hex([]) --> [].
bytes_hex([B|Bs]) -->
    { High is B>>4,
      Low is B /\ 0xf,
      char_type(C0, xdigit(High)),
      char_type(C1, xdigit(Low))
    },
    [C0,C1],
    bytes_hex(Bs).

%!  rsa_private_decrypt(+PrivateKey, +CipherText, -PlainText, +Options) is det.
%!  rsa_private_encrypt(+PrivateKey, +PlainText, -CipherText, +Options) is det.
%!  rsa_public_decrypt(+PublicKey, +CipherText, -PlainText, +Options) is det.
%!  rsa_public_encrypt(+PublicKey, +PlainText, -CipherText, +Options) is det.
%
%   RSA Public key encryption and   decryption  primitives. A string
%   can be safely communicated by first   encrypting it and have the
%   peer decrypt it with the matching  key and predicate. The length
%   of the string is limited by  the   key  length.
%
%   Options:
%
%     - encoding(+Encoding)
%     Encoding to use for Data.  Default is `utf8`.  Alternatives
%     are `utf8` and `octet`.
%
%     - padding(+PaddingScheme)
%     Padding scheme to use.  Default is `pkcs1`.  Alternatives
%     are `pkcs1_oaep`, `sslv23` and `none`. Note that `none` should
%     only be used if you implement cryptographically sound padding
%     modes in your application code as encrypting unpadded data with
%     RSA is insecure
%
%   @see load_private_key/3, load_public_key/2 can be use to load
%   keys from a file.  The predicate load_certificate/2 can be used
%   to obtain the public key from a certificate.
%
%   @error ssl_error(Code, LibName, FuncName, Reason)   is raised if
%   there is an error, e.g., if the text is too long for the key.

%!  rsa_sign(+Key, +Data, -Signature, +Options) is det.
%
%   Create an RSA signature for Data with private key Key.  Options:
%
%     - type(+Type)
%     SHA algorithm used to compute the digest.  Values are
%     `sha1`, `sha224`, `sha256`, `sha384` or `sha512`. The
%     default is a cryptographically secure algorithm. If you
%     specify a variable, then it is unified with the algorithm that
%     was used.
%
%     - encoding(+Encoding)
%     Encoding to use for Data.  Default is `hex`.  Alternatives
%     are `octet`, `utf8` and `text`.
%
%   This predicate can be used to compute a =|sha256WithRSAEncryption|=
%   signature as follows:
%
%     ```
%     sha256_with_rsa(PemKeyFile, Password, Data, Signature) :-
%         Algorithm = sha256,
%         read_key(PemKeyFile, Password, Key),
%         crypto_data_hash(Data, Hash, [algorithm(Algorithm),
%                                       encoding(octet)]),
%         rsa_sign(Key, Hash, Signature, [type(Algorithm)]).
%
%     read_key(File, Password, Key) :-
%         setup_call_cleanup(
%             open(File, read, In, [type(binary)]),
%             load_private_key(In, Password, Key),
%             close(In)).
%     ```
%
%   Note that a hash that is computed by crypto_data_hash/3 can be
%   directly used in rsa_sign/4 as well as ecdsa_sign/4.

rsa_sign(Key, Data0, Signature, Options0) :-
    functor_hash_options(type, Type, Options0, Options),
    option(encoding(Enc0), Options, hex),
    hex_encoding(Enc0, Data0, Enc, Data),
    rsa_sign(Key, Type, Enc, Data, Signature).


%!  rsa_verify(+Key, +Data, +Signature, +Options) is semidet.
%
%   Verify an RSA signature for Data with public key Key.
%
%   Options:
%
%     - type(+Type)
%     SHA algorithm used to compute the digest.  Values are `sha1`,
%     `sha224`, `sha256`, `sha384` or `sha512`. The default is the
%     same as for rsa_sign/4.  This option must match the algorithm
%     that was used for signing. When operating with different parties,
%     the used algorithm must be communicated over an authenticated
%     channel.
%
%     - encoding(+Encoding)
%     Encoding to use for Data.  Default is `hex`.  Alternatives
%     are `octet`, `utf8` and `text`.

rsa_verify(Key, Data0, Signature0, Options0) :-
    functor_hash_options(type, Type, Options0, Options),
    option(encoding(Enc0), Options, hex),
    hex_encoding(Enc0, Data0, Enc, Data),
    hex_bytes(Signature0, Signature),
    rsa_verify(Key, Type, Enc, Data, Signature).

%!  crypto_data_decrypt(+CipherText,
%!                      +Algorithm,
%!                      +Key,
%!                      +IV,
%!                      -PlainText,
%!                      +Options).
%
%   Decrypt  the   given  CipherText,  using  the   symmetric  algorithm
%   Algorithm, key Key, and initialization vector IV, to give PlainText.
%   CipherText must  be a string, atom  or list of codes  or characters,
%   and PlainText  is created  as a  string.  Key  and IV  are typically
%   lists  of _bytes_,  though  atoms and  strings  are also  permitted.
%   Algorithm must be an algorithm which your copy of OpenSSL knows. See
%   crypto_data_encrypt/6 for an example.
%
%     - encoding(+Encoding)
%     Encoding to use for CipherText.  Default is `utf8`.
%     Alternatives are `utf8` and `octet`.
%
%     - padding(+PaddingScheme)
%     For block ciphers, the padding scheme to use.  Default is
%     `block`.  You can disable padding by supplying `none` here.
%
%     - tag(+Tag)
%     For authenticated encryption schemes, the tag must be specified as
%     a list of bytes exactly as they were generated upon encryption.
%     This option requires OpenSSL 1.1.0 or greater.
%
%     - min_tag_length(+Length)
%     If the tag length is smaller than 16, this option must be used
%     to permit such shorter tags. This is used as a safeguard against
%     truncation attacks, where an attacker provides a short tag that
%     is easier to guess.

crypto_data_decrypt(CipherText, Algorithm, Key, IV, PlainText, Options) :-
        (   option(tag(Tag), Options) ->
            option(min_tag_length(MinTagLength), Options, 16),
            length(Tag, TagLength),
            compare(C, TagLength, MinTagLength),
            tag_length_ok(C, Tag)
        ;   Tag = []
        ),
        '_crypto_data_decrypt'(CipherText, Algorithm, Key, IV,
                               Tag, PlainText, Options).

% This test is important to prevent truncation attacks of the tag.

tag_length_ok(=, _).
tag_length_ok(>, _).
tag_length_ok(<, Tag) :- domain_error(tag_is_too_short, Tag).


%!  crypto_data_encrypt(+PlainText,
%!                      +Algorithm,
%!                      +Key,
%!                      +IV,
%!                      -CipherText,
%!                      +Options).
%
%   Encrypt  the   given  PlainText,   using  the   symmetric  algorithm
%   Algorithm, key Key, and initialization vector (or nonce) IV, to give
%   CipherText.
%
%   PlainText must be a string, atom or list of codes or characters, and
%   CipherText is created  as a string.  Key and IV  are typically lists
%   of _bytes_, though atoms and  strings are also permitted.  Algorithm
%   must   be  an   algorithm   which  your   copy   of  OpenSSL   knows
%   about.
%
%   Keys  and   IVs  can  be   chosen  at  random  (using   for  example
%   crypto_n_random_bytes/2) or derived from input keying material (IKM)
%   using for example crypto_data_hkdf/4.  This  input is often a shared
%   secret, such as a negotiated point on an elliptic curve, or the hash
%   that was computed from a  password via crypto_password_hash/3 with a
%   freshly generated and specified _salt_.
%
%   Reusing the same combination of Key  and IV typically leaks at least
%   _some_  information about  the  plaintext.   For example,  identical
%   plaintexts will  then correspond to identical  ciphertexts. For some
%   algorithms, reusing an  IV with the same Key  has disastrous results
%   and  can  cause  the  loss  of all  properties  that  are  otherwise
%   guaranteed.   Especially in  such  cases,  an IV  is  also called  a
%   _nonce_  (number used  once).   If  an IV  is  not  needed for  your
%   algorithm (such as =|'aes-128-ecb'|=) then any value can be provided
%   as it will  be ignored by the underlying  implementation.  Note that
%   such  algorithms do  not provide  _semantic security_  and are  thus
%   insecure. You should use stronger algorithms instead.
%
%   It is safe to store and  transfer the used initialization vector (or
%   nonce) in plain text, but the key _must be kept secret_.
%
%   Commonly used algorithms include:
%
%       $ =|'chacha20-poly1305'|= :
%       A powerful and efficient _authenticated_ encryption scheme,
%       providing secrecy and at the same time reliable protection
%       against undetected _modifications_ of the encrypted data. This
%       is a very good choice for virtually all use cases. It is a
%       _stream cipher_ and can encrypt data of any length up to 256 GB.
%       Further, the encrypted data has exactly the same length
%       as the original, and no padding is used. It requires OpenSSL
%       1.1.0 or greater. See below for an example.
%
%       $ =|'aes-128-gcm'|= :
%       Also an authenticated encryption scheme. It uses a 128-bit
%       (i.e., 16 bytes) key and a 96-bit (i.e., 12 bytes) nonce. It
%       requires OpenSSL 1.1.0 or greater.
%
%       $ =|'aes-128-cbc'|= :
%       A _block cipher_ that provides secrecy, but does not protect
%       against unintended modifications of the cipher text. This
%       algorithm uses 128-bit (16 bytes) keys and initialization
%       vectors.  It works with all supported versions of OpenSSL. If
%       possible, consider using an authenticated encryption scheme
%       instead.
%
%   Options:
%
%     - encoding(+Encoding)
%     Encoding to use for PlainText.  Default is `utf8`.  Alternatives
%     are `utf8` and `octet`.
%
%     - padding(+PaddingScheme)
%     For block ciphers, the padding scheme to use.  Default is
%     `block`.  You can disable padding by supplying `none` here. If
%     padding is disabled for block ciphers, then the length of the
%     ciphertext must be a multiple of the block size.
%
%     - tag(-List)
%     For authenticated encryption schemes, List is unified with a
%     list of _bytes_ holding the tag. This tag must be provided for
%     decryption. Authenticated encryption requires OpenSSL 1.1.0 or
%     greater.
%
%     - tag_length(+Length)
%     For authenticated encryption schemes, the desired length of the
%     tag, specified as the number of bytes.  The default is
%     16. Smaller numbers are not recommended.
%
%   For example, with OpenSSL 1.1.0 and greater, we can use the ChaCha20
%   stream cipher  with the Poly1305  authenticator. This cipher  uses a
%   256-bit  key  and  a  96-bit  _nonce_,  i.e.,  32  and  12  _bytes_,
%   respectively:
%
%     ```
%     ?- Algorithm = 'chacha20-poly1305',
%        crypto_n_random_bytes(32, Key),
%        crypto_n_random_bytes(12, IV),
%        crypto_data_encrypt("this is some input", Algorithm,
%                    Key, IV, CipherText, [tag(Tag)]),
%        crypto_data_decrypt(CipherText, Algorithm,
%                    Key, IV, RecoveredText, [tag(Tag)]).
%     Algorithm = 'chacha20-poly1305',
%     Key = [65, 147, 140, 197, 27, 60, 198, 50, 218|...],
%     IV = [253, 232, 174, 84, 168, 208, 218, 168, 228|...],
%     CipherText = <binary string>,
%     Tag = [248, 220, 46, 62, 255, 9, 178, 130, 250|...],
%     RecoveredText = "this is some input".
%     ```
%
%   In this  example, we use  crypto_n_random_bytes/2 to generate  a key
%   and  nonce  from  cryptographically   secure  random  numbers.   For
%   repeated applications,  you must  ensure that a  nonce is  only used
%   _once_ together  with the same  key.  Note that  for _authenticated_
%   encryption schemes, the _tag_ that was computed during encryption is
%   necessary for decryption.  It is safe  to store and transfer the tag
%   in plain text.
%
%   @see crypto_data_decrypt/6.
%   @see hex_bytes/2 for conversion between bytes and hex encoding.

crypto_data_encrypt(PlainText, Algorithm, Key, IV, CipherText, Options) :-
        (   option(tag(AuthTag), Options) ->
            option(tag_length(AuthLength), Options, 16)
        ;   AuthTag = _,
            AuthLength = -1
        ),
        '_crypto_data_encrypt'(PlainText, Algorithm, Key, IV,
                               AuthLength, AuthTag, CipherText, Options).


%%  crypto_modular_inverse(+X, +M, -Y) is det
%
%   Compute the modular multiplicative inverse of the integer X. Y is
%   unified with an integer such that X*Y is congruent to 1 modulo M.


crypto_modular_inverse(X, M, Y) :-
    integer_serialized(X, XS),
    integer_serialized(M, MS),
    '_crypto_modular_inverse'(XS, MS, YHex),
    hex_to_integer(YHex, Y).

integer_serialized(I, serialized(S)) :-
    must_be(integer, I),
    integer_atomic_sign(I, Sign),
    Abs is abs(I),
    format(atom(A0), "~16r", [Abs]),
    atom_length(A0, L),
    Rem is L mod 2,
    hex_pad(Rem, Sign, A0, S).

integer_atomic_sign(I, S) :-
    Sign is sign(I),
    sign_atom(Sign, S).

sign_atom(-1, '-').
sign_atom( 0, '').
sign_atom( 1, '').

hex_pad(0, Sign, A0, A) :- atom_concat(Sign, A0, A).
hex_pad(1, Sign, A0, A) :- atomic_list_concat([Sign,'0',A0], A).

pow256(Byte, N0-I0, N-I) :-
    N is N0 + Byte*256^I0,
    I is I0 + 1.

hex_to_integer(Hex, N) :-
    hex_bytes(Hex, Bytes0),
    reverse(Bytes0, Bytes),
    foldl(pow256, Bytes, 0-0, N-_).

%%  crypto_generate_prime(+N, -P, +Options) is det
%
%   Generate a prime P with at least N bits. Options is a list of options.
%   Currently, the only supported option is:
%
%   * safe(Boolean)
%     If `Boolean` is `true` (default is `false`), then a _safe_ prime
%     is generated. This means that P is of the form 2*Q + 1 where Q
%     is also prime.

crypto_generate_prime(Bits, P, Options) :-
        must_be(list, Options),
        option(safe(Safe), Options, false),
        '_crypto_generate_prime'(Bits, Hex, Safe, Options),
        hex_to_integer(Hex, P).

%%  crypto_is_prime(+P, +Options) is semidet
%
%   True iff P passes a probabilistic primality test. Options is a
%   list of options. Currently, the only supported option is:
%
%   * iterations(N)
%     N is the number of iterations that are performed. If this option
%     is not specified, a number of iterations is used such that the
%     probability of a false positive is at most 2^(-80).

crypto_is_prime(P0, Options) :-
        must_be(integer, P0),
        must_be(list, Options),
        option(iterations(N), Options, -1),
        integer_serialized(P0, P),
        '_crypto_is_prime'(P, N).

%%  crypto_name_curve(+Name, -Curve) is det
%
%   Obtain a handle for a _named_ elliptic curve. Name is an atom, and
%   Curve is unified with an opaque object that represents the curve.
%   Currently, only elliptic curves over prime fields are
%   supported. Examples of such curves are `prime256v1` and
%   `secp256k1`.
%
%   If you have OpenSSL installed, you can get a list of supported
%   curves via:
%
%   ==
%   $ openssl ecparam -list_curves
%   ==

%%  crypto_curve_order(+Curve, -Order) is det
%
%   Obtain the order of an elliptic curve. Order is an integer,
%   denoting how many points on the curve can be reached by
%   multiplying the curve's generator with a scalar.

crypto_curve_order(Curve, Order) :-
    '_crypto_curve_order'(Curve, Hex),
    hex_to_integer(Hex, Order).


%%  crypto_curve_generator(+Curve, -Point) is det
%
%   Point is the _generator_ of the elliptic curve Curve.

crypto_curve_generator(Curve, point(X,Y)) :-
    '_crypto_curve_generator'(Curve, X0, Y0),
    hex_to_integer(X0, X),
    hex_to_integer(Y0, Y).

%% crypto_curve_scalar_mult(+Curve, +N, +Point, -R) is det
%
%  R is the result of N times Point on the elliptic curve Curve. N
%  must be an integer, and Point must be a point on the curve.

crypto_curve_scalar_mult(Curve, S0, point(X0,Y0), point(A,B)) :-
    maplist(integer_serialized, [S0,X0,Y0], [S,X,Y]),
    '_crypto_curve_scalar_mult'(Curve, S, X, Y, A0, B0),
    hex_to_integer(A0, A),
    hex_to_integer(B0, B).


                 /*******************************
                 *          Sandboxing          *
                 *******************************/

:- multifile sandbox:safe_primitive/1.

sandbox:safe_primitive(crypto:hex_bytes(_,_)).
sandbox:safe_primitive(crypto:crypto_n_random_bytes(_,_)).

sandbox:safe_primitive(crypto:crypto_data_hash(_,_,_)).
sandbox:safe_primitive(crypto:crypto_data_context(_,_,_)).
sandbox:safe_primitive(crypto:crypto_context_new(_,_)).
sandbox:safe_primitive(crypto:crypto_context_hash(_,_)).

sandbox:safe_primitive(crypto:crypto_password_hash(_,_)).
sandbox:safe_primitive(crypto:crypto_password_hash(_,_,_)).
sandbox:safe_primitive(crypto:crypto_data_hkdf(_,_,_,_)).

sandbox:safe_primitive(crypto:ecdsa_sign(_,_,_,_)).
sandbox:safe_primitive(crypto:ecdsa_verify(_,_,_,_)).

sandbox:safe_primitive(crypto:rsa_sign(_,_,_,_)).
sandbox:safe_primitive(crypto:rsa_verify(_,_,_,_)).
sandbox:safe_primitive(crypto:rsa_public_encrypt(_,_,_,_)).
sandbox:safe_primitive(crypto:rsa_public_decrypt(_,_,_,_)).
sandbox:safe_primitive(crypto:rsa_private_encrypt(_,_,_,_)).
sandbox:safe_primitive(crypto:rsa_private_decrypt(_,_,_,_)).

sandbox:safe_primitive(crypto:crypto_data_decrypt(_,_,_,_,_,_)).
sandbox:safe_primitive(crypto:crypto_data_encrypt(_,_,_,_,_,_)).

sandbox:safe_primitive(crypto:crypto_modular_inverse(_,_,_)).
sandbox:safe_primitive(crypto:crypto_generate_prime(_,_,_)).
sandbox:safe_primitive(crypto:crypto_is_prime(_,_)).

sandbox:safe_primitive(crypto:crypto_name_curve(_,_)).
sandbox:safe_primitive(crypto:crypto_curve_order(_,_)).
sandbox:safe_primitive(crypto:crypto_curve_generator(_,_)).
sandbox:safe_primitive(crypto:crypto_curve_scalar_mult(_,_,_,_)).

                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:error_message//1.

prolog:error_message(ssl_error(ID, _Library, Function, Reason)) -->
    [ 'SSL(~w) ~w: ~w'-[ID, Function, Reason] ].
