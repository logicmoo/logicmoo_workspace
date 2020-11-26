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

:- module(yadis,
          [ xrds_dom/2,                 % +URI, -XRDS_DOM
            xrds_location/2             % +Xid, -URL
          ]).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(library(sgml)).

/** <module> Yadis discovery

@see http://en.wikipedia.org/wiki/Yadis
*/

:- multifile
    xrds_specified_location/2.

%!  xrds_dom(+Id, -XRDS_DOM) is det.
%
%   True when XRDS_DOM is  a  parsed   XML  document  for  the given
%   resource.

xrds_dom(Xid, XRDS_DOM) :-
    xrds_location(Xid, XRDSLocation),
    xrds_load(XRDSLocation, XRDS_DOM).

%!  xid_normalize(+OpenID, -URL) is det.
%
%   Translate the user-specified  OpenID  agent   into  a  URL. This
%   follows appendix A.1. (Normalization), RFC3986).
%
%   @tbd This does not implement XRI identifiers.

xid_normalize(Xid, URL) :-
    add_component(scheme, Xid, URL0, http),
    add_component(path,   URL0, URL, /).

add_component(Field, URL0, URL, Default) :-
    uri_components(URL0, Comp),
    uri_data(Field, Comp, Value),
    (   var(Value)
    ->  (   Field == scheme
        ->  atomic_list_concat([Default, '://', URL0], URL)
        ;   Value = Default,
            uri_components(URL, Comp)
        )
    ;   Field == path,
        Value = ''
    ->  uri_data(path, Comp, Default, Comp2),
        uri_components(URL, Comp2)
    ;   URL = URL0
    ).


%!  xrds_location(+Id, -XRDSLocation) is semidet.
%
%   Discover the location of the XRDS document from the given Id.

xrds_location(Xid, XRDSLocation) :-
    xid_normalize(Xid, URL),
    (   xrds_specified_location(URL, XRDSLocation)
    ->  XRDSLocation \== (-)
    ;   catch(xrds_location_direct(URL, XRDSLocation),
              E, yadis_failed(E))
    ->  true
    ;   catch(xrds_location_html(URL, XRDSLocation),
              E, yadis_failed(E))
    ).

yadis_failed(E) :-
    (   debugging(yadis)
    ->  print_message(warning, E)
    ;   true
    ),
    fail.

xrds_location_direct(URL, XRDSLocation) :-
    setup_call_cleanup(
        http_open(URL, In,
                  [ method(head),
                    request_header(accept='application/xrds+xml'),
                    header(x_xrds_location, Reply),
                    cert_verify_hook(ssl_verify)
                  ]),
        true,
        close(In)),
    Reply \== '',
    !,
    XRDSLocation = Reply.

xrds_location_html(URL, XRDSLocation) :-
    setup_call_cleanup(
        http_open(URL, In,
                  [ cert_verify_hook(ssl_verify)
                  ]),
        html_head_dom(In, DOM),
        close(In)),
    xpath(DOM, meta(@'http-equiv'=Equiv, @content), Content),
    downcase_atom(Equiv, 'x-xrds-location'),
    !,
    XRDSLocation = Content.

%!  xrds_load(+XRDSLocation, -XRDS_DOM) is det.
%
%   Parse the XRDS document at XRDSLocation.

xrds_load(XRDSLocation, XRDS_DOM) :-
    setup_call_cleanup(
        http_open(XRDSLocation, In,
                  [ request_header(accept='application/xrds+xml'),
                    cert_verify_hook(ssl_verify)
                  ]),
        load_structure(In, XRDS_DOM,
                       [ dialect(xmlns),
                         space(remove)
                       ]),
        close(In)).

:- public ssl_verify/5.

%!  ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%   Accept all certificates.

ssl_verify(_SSL,
           _ProblemCertificate, _AllCertificates, _FirstCertificate,
           _Error).


%!  html_head_dom(+Stream, -HeadDOM) is semidet.
%
%   Extract the HTML head content from   the  given stream. Does not
%   parse the remainder of the document.

:- thread_local
    html_head_dom/1.

html_head_dom(Stream, HeadDOM) :-
    dtd(html, DTD),
    new_sgml_parser(Parser, [dtd(DTD)]),
    call_cleanup(
        sgml_parse(Parser,
                   [ source(Stream),
                     syntax_errors(quiet),
                     call(begin, on_begin)
                   ]),
        free_sgml_parser(Parser)),
    retract(html_head_dom(HeadDOM)).

on_begin(head, Attrs, Parser) :-
    sgml_parse(Parser,
               [ document(DOM),
                 parse(content)
               ]),
    asserta(html_head_dom(element(head, Attrs, DOM))).

%!  xrds_specified_location(+URL, -XRDSLocation) is nondet.
%
%   Hook that allows for specifying locations of XRDS documents. For
%   example, Google does not reply to   Yadis discovery messages. We
%   can fake it does using:
%
%     ==
%     yadis:xrds_specified_location('http://google.com/',
%                                   'https://www.google.com/accounts/o8/id').
%     ==
%
%   If this hook succeeds with XRDSLocation bound to `-` (minus), we
%   assume there is no XRDS document associated to URL.  This can be
%   used to avoid retrieving misleading or broken XRDS documents.
