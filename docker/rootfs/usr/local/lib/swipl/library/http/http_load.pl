/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013, VU University Amsterdam
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

:- module(prolog_http_load, []).
:- use_module(library(http/http_open)).
:- use_module(library(uri)).

:- multifile
    user:prolog_load_file/2.

/** <module> Load Prolog code from a web server

This module provides a hook into load_files/2 that allows loading Prolog
code from HTTP and HTTPS servers.  Below is an example session:

  ==
  ?- [library(http/http_load)].
  ...
  true.
  ?- ['http://www.swi-prolog.org/download/demo/likes'].
  % http://www.swi-prolog.org/download/demo/likes.pl compiled 0.00 sec, 17 clauses
  true.
  ==

*Warning* Loading code from untrusted HTTP resources may compromise your
security.
*/

%!  user:prolog_load_file(+URL, +Options)
%
%   Hook into load_files/2 that loads   =|http://|= and =|https://|=
%   resources directly from the web.
%
%   @bug    Loading =https= does not validate the certificate.

user:prolog_load_file(Spec, Options) :-
    strip_module(Spec, Module, URL),
    atom(URL),
    (   is_http_url(URL)
    ->  GlobalURL = URL
    ;   prolog_load_context(file, Parent),
        is_http_url(Parent),
        uri_resolve(URL, Parent, GlobalURL)
    ),
    ensure_extension(GlobalURL, pl, FinalURL),
    setup_call_cleanup(
        http_open(FinalURL, In,
                  [ cert_verify_hook(ssl_verify)
                  ]),
        load_files(Module:FinalURL, [stream(In)|Options]),
        close(In)).

:- public ssl_verify/5.

%!  ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%   Currently we accept  all  certificates.

ssl_verify(_SSL,
           _ProblemCertificate, _AllCertificates, _FirstCertificate,
           _Error).

is_http_url(URL) :-
    uri_is_global(URL),
    uri_components(URL, Components),
    uri_data(scheme, Components, Scheme),
    nonvar(Scheme),
    http_scheme(Scheme).

http_scheme(http).
http_scheme(https) :-
    catch(use_module(library(http/http_ssl_plugin)),
          E, (print_message(warning, E), fail)).


%!  ensure_extension(+URL, +Ext, -PlParts)
%
%   If the HTTP location is a plain path without extension, add the
%   .pl extension. This ensures extension-less files appearing in
%   file-loading directives are processed correctly.

ensure_extension(URL0, Ext, URL) :-
    uri_components(URL0, Components0),
    uri_data(path, Components0, Path0),
    ensure_path_extension(Path0, Ext, Path),
    (   Path0 == Path
    ->  URL = URL0
    ;   uri_data(path, Components0, Path, Components),
        uri_components(URL, Components)
    ).

ensure_path_extension(Path0, Ext, Path) :-
    file_name_extension(Base, '', Path0),
    !,
    file_name_extension(Base, Ext, Path).
ensure_path_extension(Path, _, Path).

