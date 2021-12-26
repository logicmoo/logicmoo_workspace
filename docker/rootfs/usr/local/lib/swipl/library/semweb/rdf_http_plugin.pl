/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2015, University of Amsterdam
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

:- module(rdf_http_plugin, []).
:- use_module(library(semweb/rdf_db), []). % we define hooks for this

:- autoload(library(date),[parse_time/2]).
:- autoload(library(error),[domain_error/2]).
:- autoload(library(lists),[append/2]).
:- autoload(library(option),[option/3]).
:- autoload(library(http/http_header),[http_timestamp/2]).
:- autoload(library(http/http_open),[http_open/3]).

/** <module> RDF HTTP Plugin

This module allows loading data into   the semantic web library directly
from an HTTP server. The following example  loads the RDF core data into
the RDF database.

    ==
    :- use_module(library(semweb/rdf_db)).
    :- use_module(library(semweb/rdf_http_plugin)).

        ...,
        rdf_load('http://www.w3.org/1999/02/22-rdf-syntax-ns')
    ==
*/

:- multifile
    rdf_db:rdf_open_hook/8,
    rdf_db:url_protocol/1,
    rdf_db:rdf_storage_encoding/2,
    rdf_db:rdf_file_type/2,
    rdf_content_type/3.

rdf_db:url_protocol(http).
rdf_db:url_protocol(https).


% define `rdf_format` as a type.
:- multifile error:has_type/2.
error:has_type(rdf_format, Term):-
    error:has_type(oneof([nquads,ntriples,rdfa,trig,turtle,xml]), Term).

%!  rdf_extra_headers(-RequestHeaders:list(compound), +Options:list) is det.
%
%   Send extra headers with the request. Note that, although we also
%   process RDF embedded in HTML, we do  not explicitely ask for it.
%   Doing so causes some   (e.g., http://w3.org/2004/02/skos/core to
%   reply with the HTML description rather than the RDF).
%
%   When given, option format(+atom) is used in order to prioritize
%   the corresponding RDF content types.

rdf_extra_headers([ cert_verify_hook(ssl_verify),
                    request_header('Accept'=AcceptValue)
                  ], Options) :-
    option(format(Format), Options, _VAR),
    rdf_accept_header_value(Format, AcceptValue).


%!  rdf_db:rdf_open_hook(+Scheme, +URL, +HaveModified,
%!                       -Stream, -Cleanup, -Modified, -Format,
%!                       +Options) is semidet.
%
%   Load hook implementation for HTTP(S) URLs.
%
%   @arg HaveModified is bound to a timestamp (number) if we already
%        have a copy and that copy was modified at HaveModified.
%   @arg Modified is bound to =unknown=, =not_modified= or a
%        timestamp.

rdf_db:rdf_open_hook(https, SourceURL, HaveModified, Stream, Cleanup,
                     Modified, Format, Options) :-
    rdf_db:rdf_open_hook(http, SourceURL, HaveModified, Stream, Cleanup,
                         Modified, Format, Options).
rdf_db:rdf_open_hook(http, SourceURL, HaveModified, Stream, Cleanup,
                     Modified, Format, Options) :-
    modified_since_header(HaveModified, Header),
    TypeHdr = [ header(content_type, ContentType),
                header(last_modified, ModifiedText)
              ],
    rdf_extra_headers(Extra, Options),
    append([Extra, TypeHdr, Header, Options], OpenOptions),
    catch(http_open(SourceURL, Stream0,
                    [ status_code(Code)
                    | OpenOptions
                    ]), E, true),
    (   Code == 200
    ->  (   open_envelope(ContentType, SourceURL,
                          Stream0, Stream, Format)
        ->  Cleanup = close(Stream),
            (   nonvar(ModifiedText),
                parse_time(ModifiedText, ModifiedStamp)
            ->  Modified = last_modified(ModifiedStamp)
            ;   Modified = unknown
            )
        ;   close(Stream0),
            domain_error(content_type, ContentType)
        )
    ;   Code == 304
    ->  Modified = not_modified,
        Cleanup = true
    ;   var(E)
    ->  throw(error(existence_error(url, SourceURL),
                    context(_, status(Code,_))))
    ;   throw(E)
    ).

:- public ssl_verify/5.

%!  ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%   Currently we accept  all  certificates.

ssl_verify(_SSL,
           _ProblemCertificate, _AllCertificates, _FirstCertificate,
           _Error).

%!  modified_since_header(+LastModified, -ExtraHeaders) is det.
%
%   Add an =|If-modified-since|= if we have a version with the given
%   time-stamp.

modified_since_header(HaveModified, []) :-
    var(HaveModified),
    !.
modified_since_header(HaveModified,
                      [ request_header('If-modified-since' =
                                       Modified)
                      ]) :-
    http_timestamp(HaveModified, Modified).

%!  open_envelope(+ContentType, +SourceURL, +Stream0, -Stream,
%!                ?Format) is semidet.
%
%   Open possible envelope formats.

open_envelope(ContentType, SourceURL, Stream0, Stream, Format) :-
    (   ContentType == 'application/x-gzip'
    ;   ContentType == 'application/octet-stream',
        file_name_extension(_, gz, SourceURL)
    ),
    !,
    rdf_db:rdf_storage_encoding(_, gzip),
    !,
    (   var(Format)
    ->  file_name_extension(BaseURL, _GzExt, SourceURL),
        file_name_extension(_, Ext, BaseURL),
        rdf_db:rdf_file_type(Ext, Format)
    ;   true
    ),
    stream_pair(Stream0, Read, _),
    rdf_zlib_plugin:zopen(Read, Stream, []).
open_envelope(_, _, Stream, Stream, Format) :-
    nonvar(Format),
    !.
open_envelope(ContentType, SourceURL, Stream, Stream, Format) :-
    major_content_type(ContentType, Major),
    (   rdf_content_type(Major, _, Format)
    ->  true
    ;   Major == 'text/plain'       % server is not properly configured
    ->  file_name_extension(_, Ext, SourceURL),
        rdf_db:rdf_file_type(Ext, Format)
    ).

major_content_type(ContentType, Major) :-
    sub_atom(ContentType, Pre, _, _, (;)),
    !,
    sub_atom(ContentType, 0, Pre, _, Major).
major_content_type(Major, Major).


%% rdf_accept_header_value(?Format:rdf_format, -AcceptValue:atom) is det.

rdf_accept_header_value(Format, AcceptValue) :-
    findall(AcceptValue, accept_value(Format, AcceptValue), AcceptValues),
    atomic_list_concat(['*/*;q=0.001'|AcceptValues], ',', AcceptValue).

accept_value(Format, AcceptValue) :-
    rdf_content_type(MediaType, QValue0, Format0),
    (   Format == Format0
    ->  QValue = 1.0
    ;   QValue = QValue0
    ),
    format(atom(AcceptValue), '~a;q=~3f', [MediaType,QValue]).


%!  rdf_content_type(?MediaType:atom, ?QualityValue:between(0.0,1.0),
%!                   ?Format:rdf_format) is nondet.
%
%   Quality values are intended to be   used  in accordance with RFC
%   2616. Quality values  are  determined   based  on  the following
%   criteria:
%
%       | **Label** | **Criterion**             | **Value** |
%       | A         | Supported RDF parser      | 0.43      |
%       | B         | RDF-specific content type | 0.33      |
%       | C         | Official content type     | 0.23      |
%
%   For example, `text/turtle` has quality value 0.99 because it is
%   an official content type that is RDF-specific and that has a parser
%   in Semweb.
%
%   This intentionally allows the user to add another content type with
%   a higher Q-value (i.e., >0.99).
%
%   Deduce the RDF encoding from the   mime-type.  This predicate is
%   defined as multifile such that the user can associate additional
%   content types to RDF formats.
%
%   @bug The turtle parser only parses a subset of n3.
%        (The N3 format is treated as if it were Turtle.)
%   @see Discussion http://richard.cyganiak.de/blog/2008/03/what-is-your-rdf-browsers-accept-header/
%   @see N-Quadruples http://www.w3.org/ns/formats/N-Quads
%   @see N-Triples http://www.w3.org/ns/formats/N-Triples
%   @see N3 http://www.w3.org/ns/formats/N3
%   @see RDFa http://www.w3.org/ns/formats/RDFa
%   @see TriG http://www.w3.org/ns/formats/TriG
%   @see Turtle http://www.w3.org/ns/formats/Turtle
%   @see XML/RDF http://www.w3.org/ns/formats/RDF_XML

rdf_content_type('application/n-quads',    0.99, nquads  ). %ABC
rdf_content_type('application/n-triples',  0.99, ntriples). %ABC
rdf_content_type('application/rdf',        0.76, xml     ). %AB
rdf_content_type('application/rdf+turtle', 0.76, turtle  ). %AB
rdf_content_type('application/rdf+xml',    0.76, xml     ). %AB
rdf_content_type('application/rss+xml',    0.66, xml     ). %AC
rdf_content_type('application/trig',       0.99, trig    ). %ABC
rdf_content_type('application/turtle',     0.76, turtle  ). %AB
rdf_content_type('application/x-trig',     0.76, trig    ). %AB
rdf_content_type('application/x-turtle',   0.76, turtle  ). %AB
rdf_content_type('application/xhtml+xml',  0.66, rdfa    ). %AC
rdf_content_type('application/xml',        0.66, xml     ). %AC
rdf_content_type('text/html',              0.66, rdfa    ). %AC
rdf_content_type('text/n3',                0.56, turtle  ). %BC (N3)
rdf_content_type('text/rdf',               0.76, xml     ). %AB
rdf_content_type('text/rdf+n3',            0.33, turtle  ). %B (N3)
rdf_content_type('text/rdf+xml',           0.76, xml     ). %AB
rdf_content_type('text/turtle',            0.99, turtle  ). %ABC
rdf_content_type('text/xml',               0.66, xml     ). %AC
rdf_content_type('application/x-gzip',     0.23, gzip    ). %C
