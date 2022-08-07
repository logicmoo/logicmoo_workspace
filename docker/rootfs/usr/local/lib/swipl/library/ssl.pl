/*  Part of SWI-Prolog

    Author:        Jan van der Steen, Matt Lilley and Jan Wielemaker,
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2020, SWI-Prolog Foundation
                              VU University Amsterdam
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

:- module(ssl,
	  [ certificate_field/2,          % +Certificate, ?Field
	    load_certificate/2,           % +Stream, -Certificate
	    load_private_key/3,           % +Stream, +Password, -Key
            load_public_key/2,            % +Stream, -Key
	    load_crl/2,                   % +Stream, -Crl
	    write_certificate/3,          % +Stream, -X509, +Options
            system_root_certificates/1,   % -List
            cert_accept_any/5,            % +SSL, +ProblemCertificate,
                                          % +AllCertificates, +FirstCertificate,
                                          % +Error
            same_certificate/2,           % +CertificateA, +CertificateB
            ssl_context/3,                % +Role, -Config, :Options
            ssl_add_certificate_key/4,    % +Config, +Cert, +Key, -Config
            ssl_set_options/3,            % +Config0, -Config, +Options
            ssl_property/2,               % +Config, ?Property
            ssl_negotiate/5,              % +Config, +PlainRead, +PlainWrite,
                                          %          -SSLRead,   -SSLWrite
            ssl_peer_certificate/2,       % +Stream, -Certificate
            ssl_peer_certificate_chain/2, % +Stream, -Certificates
            ssl_session/2,                % +Stream, -Session
	    ssl_secure_ciphers/1,         % -Ciphers,
	    verify_certificate/3,         % +X509, +AuxiliaryCertificates, +TrustedCertificates
	    verify_certificate_issuer/2,  % +Certificate, +IssuerCertificate

            ssl_upgrade_legacy_options/2  % +OptionsIn, -OptionsOut
          ]).
:- autoload(library(option),[select_option/4,select_option/3]).
:- use_module(library(settings),[setting/4,setting/2]).

:- use_module(library(crypto), []).     % force initialization of libcrypto

:- use_foreign_library(foreign(ssl4pl)).

:- meta_predicate
    ssl_context(+, -, :),
    ssl_set_options(+, -, :).

:- predicate_options(ssl_context/3, 3,
                     [ host(atom),
                       port(integer),
                       cacerts(list),
                       certificate_file(atom),
                       key_file(atom),
                       certificate_key_pairs(any),
                       password(any),
                       cipher_list(any),
                       ecdh_curve(any),
                       pem_password_hook(callable),
                       cacert_file(any),
                       crl(any),
                       require_crl(boolean),
                       cert_verify_hook(callable),
                       peer_cert(boolean),
                       close_parent(boolean),
                       close_notify(boolean),
                       sni_hook(callable),
                       alpn_protocols(any),
                       alpn_protocol_hook(callable)
                     ]).

/** <module> Secure Socket Layer (SSL) library

An SSL server and client can be built with the (abstracted)
predicate calls from the table below.  The `tcp_` predicates
are provided by library(socket).  The predicate ssl_context/3
defines properties of the SSL connection, while ssl_negotiate/5
establishes the SSL connection based on the wire streams created
by the TCP predicates and the context.

        | *The SSL Server*      | *The SSL Client*      |
        | ssl_context/3         | ssl_context/3         |
        | tcp_socket/1          |                       |
        | tcp_accept/3          | tcp_connect/3         |
        | tcp_open_socket/3     | stream_pair/3         |
        | ssl_negotiate/5       | ssl_negotiate/5       |

The library is abstracted to communication over streams, and is not
reliant on those streams being directly attached to sockets. The `tcp_`
calls here are simply the most common way to use the library. Other
two-way communication channels such as (named), pipes can just as
easily be used.

@see library(socket), library(http/http_open), library(crypto)
*/

:- setting(secure_ciphers, atom,
           'EECDH+AESGCM:EDH+AESGCM:EECDH+AES256:EDH+AES256:EECDH+CHACHA20:EDH+CHACHA20',
           "Default set of ciphers considered secure").

%!  ssl_context(+Role, -SSL, :Options) is det.
%
%   Create an  SSL context.  The context  defines several properties
%   of  the   SSL  connection  such  as   involved  keys,  preferred
%   encryption, and passwords. After  establishing a context, an SSL
%   connection can be negotiated  using ssl_negotiate/5, turning two
%   arbitrary  plain Prolog  streams into  encrypted streams.   This
%   predicate processes the options below.
%
%     * host(+HostName)
%     For the client, the host to which it connects. This option
%     _should_ be specified when Role is `client`. Otherwise,
%     certificate verification may fail when negotiating a
%     secure connection.
%     * certificate_file(+FileName)
%     Specify where the certificate file can be found. This can be the
%     same as the key_file(+FileName) option.  A server _must_ have at
%     least one certificate before clients can connect. A client
%     _must_ have a certificate only if the server demands the client
%     to identify itself with a client certificate using the
%     peer_cert(true) option. If a certificate is provided, it is
%     necessary to also provide a matching _private key_ via the
%     key_file/1 option. To configure multiple certificates, use the
%     option certificate_key_pairs/1 instead. Alternatively, use
%     ssl_add_certificate_key/4 to add certificates and keys to an
%     existing context.
%     * key_file(+FileName)
%     Specify where the private key that matches the certificate can
%     be found.  If the key is encrypted with a password, this must
%     be supplied using the password(+Text) or
%     =|pem_password_hook(:Goal)|= option.
%     * certificate_key_pairs(+Pairs)
%     Alternative method for specifying certificates and keys. The
%     argument is a list of _pairs_ of the form Certificate-Key,
%     where each component is a string or an atom that holds,
%     respectively, the PEM-encoded certificate and key. To each
%     certificate, further certificates of the chain can be
%     appended. Multiple types of certificates can be present at
%     the same time to enable different ciphers. Using multiple
%     certificate types with completely independent certificate
%     chains requires OpenSSL 1.0.2 or greater.
%     * password(+Text)
%     Specify the password the private key is protected with (if
%     any). If you do not want to store the password you can also
%     specify an application defined handler to return the password
%     (see next option).  Text is either an atom or string.  Using
%     a string is preferred as strings are volatile and local
%     resources.
%     * pem_password_hook(:Goal)
%     In case a password is required to access the private key the
%     supplied predicate will be called to fetch it. The hook is
%     called as call(Goal, +SSL, -Password) and typically unifies
%     `Password` with a _string_ containing the password.
%     * require_crl(+Boolean)
%     If true (default is false), then all certificates will be
%     considered invalid unless they can be verified as not being
%     revoked. You can do this explicity by passing a list of CRL
%     filenames via the crl/1 option, or by doing it yourself in
%     the cert_verify_hook. If you specify require_crl(true) and
%     provide neither of these options, verification will necessarily
%     fail
%     * crl(+ListOfFileNames)
%     Provide a list of filenames of PEM-encoded CRLs that will be
%     given to the context to attempt to establish that a chain of
%     certificates is not revoked. You must also set require_crl(true)
%     if you want CRLs to actually be checked by OpenSSL.
%     * cacert_file(+FileName)
%     Deprecated. Use cacerts/1 instead.
%     Specify a file containing certificate keys of _trusted_
%     certificates. The peer is trusted if its certificate is
%     signed (ultimately) by one of the provided certificates. Using
%     the FileName `system(root_certificates)` uses a list of
%     trusted root certificates as provided by the OS. See
%     system_root_certificates/1 for details.
%     * cacerts(+ListOfCATerms)
%     Specify a list of sources of _trusted_ certificates.
%     Each element in the list should be one of the following:
%        * file(Filename): A file containing one or more PEM-encoded
%          certificates
%        * certificate(Blob): A certificate blob
%        * system(root_certificates): A special term which refers to
%          the certificates trusted by the host OS.
%
%     Additional verification of the peer certificate as well as
%     accepting certificates that are not trusted by the given set
%     can be realised using the hook
%     cert_verify_hook(:Goal).
%     * cert_verify_hook(:Goal)
%     The predicate ssl_negotiate/5 calls Goal as follows:
%
%       ==
%       call(Goal, +SSL,
%            +ProblemCertificate, +AllCertificates, +FirstCertificate,
%            +Error)
%       ==
%
%     In case the certificate was verified by one of the provided
%     certifications from the `cacert_file` option, Error is unified
%     with the atom `verified`. Otherwise it contains the error
%     string passed from OpenSSL. Access will be granted iff the
%     predicate succeeds. See load_certificate/2 for a description
%     of the certificate terms. See cert_accept_any/5 for a dummy
%     implementation that accepts any certificate.
%     * cipher_list(+Atom)
%     Specify a cipher preference list (one or more cipher strings
%     separated by colons, commas or spaces). See ssl_secure_ciphers/1.
%     * ecdh_curve(+Atom)
%     Specify a curve for ECDHE ciphers. If this option is not
%     specified, the OpenSSL default parameters are used.  With
%     OpenSSL prior to 1.1.0, `prime256v1` is used by default.
%     * peer_cert(+Boolean)
%     Trigger the request of our peer's certificate while
%     establishing the SSL layer. This option is automatically
%     turned on in a client SSL socket.  It can be used in a server
%     to ask the client to identify itself using an SSL certificate.
%     * close_parent(+Boolean)
%     If `true`, close the raw streams if the SSL streams are closed.
%     Default is `false`.
%     * close_notify(+Boolean)
%     If `true` (default is `false`), the server sends TLS
%     `close_notify` when closing the connection. In addition,
%     this mitigates _truncation attacks_ for both client and
%     server role: If EOF is encountered without having received a
%     TLS shutdown, an exception is raised. Well-designed
%     protocols are self-terminating, and this attack is therefore
%     very rarely a concern.
%     * min_protocol_version(+Atom)
%     Set the _minimum_ protocol version that can be negotiated.
%     Atom is one of `sslv3`, `tlsv1`, `tlsv1_1`, `tlsv1_2` and
%     `tlsv1_3`. This option is available with OpenSSL 1.1.0 and
%     later, and should be used instead of `disable_ssl_methods/1`.
%     * max_protocol_version(+Atom)
%     Set the _maximum_ protocol version that can be negotiated.
%     Atom is one of `sslv3`, `tlsv1`, `tlsv1_1`, `tlsv1_2` and
%     `tlsv1_3`. This option is available with OpenSSL 1.1.0 and
%     later, and should be used instead of `disable_ssl_methods/1`.
%     * disable_ssl_methods(+List)
%     A list of methods to disable. Unsupported methods will be
%     ignored. Methods include `sslv2`, `sslv3`, `sslv23`,
%     `tlsv1`, `tlsv1_1` and `tlsv1_2`. This option is deprecated
%     starting with OpenSSL 1.1.0. Use min_protocol_version/1 and
%     max_protocol_version/1 instead.
%     * ssl_method(+Method)
%     Specify the explicit Method to use when negotiating. For
%     allowed values, see the list for `disable_ssl_methods` above.
%     Using this option is discouraged. When using OpenSSL 1.1.0
%     or later, this option is ignored, and a version-flexible method
%     is used to negotiate the connection. Using version-specific
%     methods is deprecated in recent OpenSSL versions, and this
%     option will become obsolete and ignored in the future.
%     * sni_hook(:Goal)
%     This option provides Server Name Indication (SNI) for SSL
%     servers. This means that depending on the host to which a
%     client connects, different options (certificates etc.) can
%     be used for the server. This TLS extension allows you to host
%     different domains using the same IP address and physical
%     machine. When a TLS connection is negotiated with a client
%     that has provided a host name via SNI, the hook is called as
%     follows:
%
%     ==
%     call(Goal, +SSL0, +HostName, -SSL)
%     ==
%
%     Given the current context SSL0, and the host name of the
%     client request, the predicate computes SSL which is used as
%     the context for negotiating the connection. The first solution
%     is used.  If the predicate fails, the default options are
%     used, which are those of the encompassing ssl_context/3
%     call. In that case, if no default certificate and key are
%     specified, the client connection is rejected.
%     * alpn_protocols(+ListOfProtoIdentifiers)
%     Provide a list of acceptable ALPN protocol identifiers as atoms.
%     ALPN support requires OpenSSL 1.0.2 or greater.
%     * alpn_protocol_hook(:Goal)
%     This options provides a callback for a server context to use to
%     select an ALPN protocol. It will be called as follows:
%
%     ===
%     call(Goal, +SSLCtx0, +ListOfClientProtocols, -SSLCtx1, -SelectedProtocol)
%     ===
%
%     If this option is unset and the `alpn_protocols/1` option is
%     set, then the first common protocol between client & server will
%     be selected.
%
%   @arg Role is one of `server` or `client` and denotes whether the
%   SSL  instance  will  have  a  server   or  client  role  in  the
%   established connection.
%   @arg SSL is a SWI-Prolog _blob_ of type `ssl_context`, i.e., the
%   type-test for an SSL context is `blob(SSL, ssl_context)`.

ssl_context(Role, SSL, Module:Options) :-
    select_option(ssl_method(Method), Options, O1, sslv23),
    ssl_upgrade_legacy_options(O1, O2),
    (   select_option(cacerts(_), O2, _)
    ->  O3 = O2
    ;   O3 = [cacerts([system(root_certificates)])|O2]
    ),
    '_ssl_context'(Role, SSL, Module:O3, Method).

%!  ssl_upgrade_legacy_options(+OptionsIn, -Options) is det.
%
%   Handle deprecated cacert_file(Spec) option and  map   it  to the new
%   cacerts(+List) option.

ssl_upgrade_legacy_options(O1, O4) :-
    select_option(cacert_file(CACertFile), O1, O2),
    !,
    print_message(warning, deprecated(ssl_option(cacert_file(CACertFile)))),
    (   atom(CACertFile)
    ->  Term = file(CACertFile)
    ;   Term = CACertFile                % e.g., system(root_certificates)
    ),
    select_option(cacerts(CACerts), O2, O3, []),
    ssl_upgrade_legacy_options([cacerts([Term|CACerts])|O3], O4).
ssl_upgrade_legacy_options(Options, Options).


%!  ssl_add_certificate_key(+SSL0, +Certificate, +Key, -SSL)
%
%   Add an additional certificate/key pair to SSL0, yielding SSL.
%   Certificate and Key are either strings or atoms that hold the
%   PEM-encoded certificate plus certificate chain and private key,
%   respectively. Using strings is preferred for security reasons.
%
%   This predicate allows dual-stack RSA and ECDSA servers (for
%   example), and is an alternative for using the
%   `certificate_key_pairs/1` option. As of OpenSSL 1.0.2, multiple
%   certificate types with completely independent certificate chains
%   are supported. If a certificate of the same type is added
%   repeatedly to a context, the result is undefined. Currently, up to
%   12 additional certificates of different types are admissible.

ssl_add_certificate_key(SSL0, Cert, Key, SSL) :-
    ssl_copy_context(SSL0, SSL),
    '_ssl_add_certificate_key'(SSL, Cert, Key).

%!  ssl_set_options(+SSL0, -SSL, +Options)
%
%   SSL is the same as SSL0, except for the options specified in
%   Options.  The following options are supported: close_notify/1,
%   close_parent/1, host/1, peer_cert/1, ecdh_curve/1,
%   min_protocol_version/1, max_protocol_version/1,
%   disable_ssl_methods/1, sni_hook/1, cert_verify_hook/1,
%   alpn_protocols/1, and alpn_protocol_hook/1. See ssl_context/3 for
%   more information about these options. This predicate allows you to
%   tweak existing SSL contexts, which can be useful in hooks when
%   creating servers with the HTTP infrastructure.

ssl_set_options(SSL0, SSL, Options) :-
    ssl_copy_context(SSL0, SSL),
    '_ssl_set_options'(SSL, Options).

%!  ssl_property(+SSL, ?Property) is semidet.
%
%   True when Property is a property of SSL. Defined properties are:
%
%     - close_parent(?Bool)
%
%   @tbd This version is a very   minimal  implementation of the generic
%   property interface. Future versions  will   add  more properties and
%   non-determinism.

%!  ssl_negotiate(+SSL,
%!                +PlainRead, +PlainWrite,
%!                -SSLRead, -SSLWrite) is det.
%
%   Once a connection is established and a read/write stream pair is
%   available, (PlainRead and PlainWrite),  this   predicate  can be
%   called to negotiate an SSL  session   over  the  streams. If the
%   negotiation is successful, SSLRead and SSLWrite are returned.
%
%   After a successful handshake and finishing the communication the
%   user  must  close  SSLRead  and   SSLWrite,  for  example  using
%   call_cleanup(close(SSLWrite),  close(SSLRead)).  If    the   SSL
%   _context_   (created   with   ssl_context/3   has   the   option
%   close_parent(true)  (default  `false`),  closing    SSLRead  and
%   SSLWrite also closes  the  original   PlainRead  and  PlainWrite
%   streams. Otherwise these must be closed explicitly by the user.
%
%   @error ssl_error(Code, LibName, FuncName, Reason) is raised
%   if the negotiation fails. The streams PlainRead and PlainWrite
%   are *not* closed, but an unknown amount of data may have been
%   read and written.

%!  ssl_peer_certificate(+Stream, -Certificate) is semidet.
%
%   True if the peer certificate  is   provided  (this is always the
%   case for a client connection) and   Certificate unifies with the
%   peer certificate. The example below  uses   this  to  obtain the
%   _Common Name_ of the peer  after   establishing  an https client
%   connection:
%
%     ==
%       http_open(HTTPS_url, In, []),
%       ssl_peer_certificate(In, Cert),
%       memberchk(subject(Subject), Cert),
%       memberchk('CN' = CommonName), Subject)
%     ==

%!  ssl_peer_certificate_chain(+Stream, -Certificates) is det.
%
%   Certificates  is the  certificate  chain provided  by the  peer,
%   represented as a list of certificates.

%!  ssl_session(+Stream, -Session) is det.
%
%   Retrieves (debugging) properties from the SSL context associated
%   with Stream. If Stream  is  not   an  SSL  stream, the predicate
%   raises  a  domain  error.  Session  is  a  list  of  properties,
%   containing the members described below.   Except  for `Version`,
%   all information are byte arrays that   are represented as Prolog
%   strings holding characters in the range 0..255.
%
%     * ssl_version(Version)
%     The negotiated version of the session as an integer.
%     * cipher(Cipher)
%     The negotiated cipher for this connection.
%     * session_key(Key)
%     The key material used in SSLv2 connections (if present).
%     * master_key(Key)
%     The key material comprising the master secret. This is
%     generated from the server_random, client_random and pre-master
%     key.
%     * client_random(Random)
%     The random data selected by the client during handshaking.
%     * server_random(Random)
%     The random data selected by the server during handshaking.
%     * session_id(SessionId)
%     The SSLv3 session ID. Note that if ECDHE is being used (which
%     is the default for newer versions of OpenSSL), this data will
%     not actually be sent to the server.
%     * alpn_protocol(Protocol)
%     The negotiated ALPN protocol, if supported. If no protocol was
%     negotiated, this will be an empty string.

%!  load_certificate(+Stream, -Certificate) is det.
%
%   Loads a certificate from a PEM- or DER-encoded stream, returning
%   a certificate. The fields of the certificate can be inspected
%   using certificate_field(+Certificate, ?Field).
%
%   Note that the OpenSSL `CA.pl`  utility creates certificates that
%   have a human readable textual representation in front of the PEM
%   representation. You can  use  the  following   to  skip  to  the
%   certificate if you know it is a PEM certificate:
%
%     ==
%     skip_to_pem_cert(In) :-
%           repeat,
%           (   peek_char(In, '-')
%           ->  !
%           ;   skip(In, 0'\n),
%               at_end_of_stream(In), !
%           ).
%     ==

%!  write_certificate(+Stream, +Certificate, +Options) is det.
%
%   Writes a certificate to the stream Stream. Options is reserved
%   for future use.

%!  load_crl(+Stream, -CRL) is det.
%
%   Loads a CRL from a PEM- or  DER-encoded stream, returning a term
%   containing  terms  hash/1,   signature/1,    issuer_name/1   and
%   revocations/1,  which  is  a  list   of  revoked/2  terms.  Each
%   revoked/2 term is of the form revoked(+Serial, DateOfRevocation)

%!  system_root_certificates(-List) is det.
%
%   List is a list of trusted root   certificates as provided by the
%   OS. This is the list used by ssl_context/3 when using the option
%   `system(root_certificates)`.  The list is obtained using an OS
%   specific process.  The current implementation is as follows:
%
%       - On Windows, CertOpenSystemStore() is used to import
%         the `"ROOT"` certificates from the OS.
%       - On MacOSX, the trusted keys are loaded from the
%         _SystemRootCertificates_ key chain.  The Apple API
%         for this requires the SSL interface to be compiled
%         with an XCode compiler, i.e., *not* with native gcc.
%       - Otherwise, certificates are loaded from a file defined
%         by the Prolog flag `system_cacert_filename`.  The initial
%         value of this flag is operating system dependent.  For
%         security reasons, the flag can only be set prior to using
%         the SSL library.  For example:
%
%           ==
%           :- use_module(library(ssl)).
%           :- set_prolog_flag(system_cacert_filename,
%                              '/home/jan/ssl/ca-bundle.crt').
%           ==

%!  load_private_key(+Stream, +Password, -PrivateKey) is det.
%
%   Load  a private  key PrivateKey  from the  given stream  Stream,
%   using Password to decrypt the key  if it is encrypted. Note that
%   the  password  is  currently   only  supported  for  PEM  files.
%   DER-encoded keys which are password protected will not load. The
%   key must be an RSA or EC key. DH and DSA keys are not supported,
%   and PrivateKey will  be bound to an atom (dh_key  or dsa_key) if
%   you  try and  load such  a  key.  Otherwise  PrivateKey will  be
%   unified with private_key(KeyTerm) where KeyTerm is an rsa/8 term
%   representing an RSA key, or ec/3 for EC keys.

%!  load_public_key(+Stream, -PublicKey) is det.
%
%   Load  a  public key  PublicKey  from  the given  stream  Stream.
%   Supports loading both DER- and PEM-encoded keys. The key must be
%   an  RSA or  EC  key. DH  and  DSA keys  are  not supported,  and
%   PublicKey will  be bound to an  atom (dh_key or dsa_key)  if you
%   try and  load such  a key. Otherwise  PublicKey will  be unified
%   with  public_key(KeyTerm)   where  KeyTerm  is  an   rsa/8  term
%   representing an RSA key, or ec/3 for EC keys.


%!  cert_accept_any(+SSL,
%!                  +ProblemCertificate, +AllCertificates, +FirstCertificate,
%!                  +Error) is det.
%
%   Implementation  for  the  hook   `cert_verify_hook(:Hook)`  that
%   accepts _any_ certificate. This is   intended for http_open/3 if
%   no certificate verification is desired as illustrated below.
%
%     ==
%       http_open('https:/...', In,
%                 [ cert_verify_hook(cert_accept_any)
%                 ])
%     ==

%!  same_certificate(+CertificateA,
%!                   +CertificateB).
%
%   True if CertificateA is logically the same as CertificateB, even if
%   they are stored in different blobs

%!  verify_certificate_issuer(+Certificate,
%!			      +Issuer).
%
%   True if Certificate is a certificate which was issued by the
%   certificate Issuer.

%!  verify_certificate(+Certificate,
%!		       +AuxiliaryCertificates,
%!		       +TrustedCertificates).
%
%   True if it is possible to build a chain of trust from Certificate to
%   one of the certificates in TrustedCertificates, optionally using the
%   (untrusted) certificates in AuxiliaryCertificates to complete the
%   chain.
%   To use the system built-in trust store, specify the special term
%   system(root_certificates) for TrustedCertificates.

%!  certificate_field(+Certificate,
%!		      ?Field) is nondet.
%
%   Retrieve the field matching Field from Certificate. May be
%   one of the following:
%     * subject/1 to retrieve the subject
%     * issuer/1  to retrieve the issuer's subject
%     * version/1  to retrieve the version
%     * serial/1  to retrieve the serial number
%     * not_before/1 to retrieve the start date
%     * not_after/1  to retrieve the expiry date
%     * public_key/1 to retrieve the public key
%     * crls/1 to retrieve a list of the CRLs
%     * sans/1 to retrieve a list of the Subject Alternative Names
%     * signature/1 to retrieve the certificate signature
%     * signature_algorithm/1 to retrieve the signing algorithm
%     * hash/1 to retrieve the certificate hash
%     * to_be_signed/1 to retrieve the data on the certificate which
%        must be signed



cert_accept_any(_SSL,
                _ProblemCertificate, _AllCertificates, _FirstCertificate,
                _Error).

%!  ssl_secure_ciphers(-Ciphers:atom) is det.
%
%   Ciphers is a  secure cipher preference list that can  be used in the
%   cipher_list/1 option of ssl_context/3.
%
%   Secure ciphers must guarantee forward secrecy, and must mitigate all
%   known critical attacks.  As of  2018, using these ciphers allows you
%   to obtain grade A on  https://www.ssllabs.com. For A+, you must also
%   enable HTTP Strict  Transport Security (HSTS) by  sending a suitable
%   header field in replies.
%
%   Note that obsolete ciphers *must* be   disabled  to reliably prevent
%   protocol downgrade attacks.
%
%   The Ciphers list is read from   the setting `ssl:secure_ciphers` and
%   can be controlled using  set_setting/2   and  other  predicates from
%   library(settings).
%
%   *BEWARE*: This list must be changed when attacks on these ciphers
%             become known! Keep an eye on this setting and adapt it
%             as necessary in the future.

ssl_secure_ciphers(Cs) :-
    setting(secure_ciphers, Cs).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:error_message//1,
    prolog:deprecated//1.

prolog:error_message(ssl_error(ID, _Library, Function, Reason)) -->
    [ 'SSL(~w) ~w: ~w'-[ID, Function, Reason] ].
prolog:deprecated(ssl_option(cacert_file(CACertFile))) -->
    [ 'SSL: cacert_file(~q) has need deprecated.'-[CACertFile],
      'Please use the option cacerts(List) instead'
    ].
