/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2018, University of Amsterdam
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

:- module(mimetype,
          [ file_mime_type/2,           % +Path, -Type
            file_content_type/2,        % +Path, -Type
            file_content_type/3         % +Path, ?MediaType, -Type
          ]).

/** <module> Determine mime-type for a file

Simple library to guess the mime-type from   the extension of a file. As
various applications need  to  do  this   type  ofinferencing  it  seems
worthwhile to place this functionality in an extensible library.

@tbd    Consider content handling (using the Unix file command)
@tbd    Allow parameters? (e.g. text/html; charset=UTF-8)
*/

:- multifile
    mime:mime_extension/2,
    mime:text_mimetype/1,
    mime:charset/3.

%!  file_mime_type(+FileName, -MimeType) is semidet.
%
%   True when MimeType is  the  mime-type   to  be  used for sending
%   FileName. The default rules can be overridden and extended using
%   the hook mime:mime_extension/2.
%
%   @param MimeType is a compound term of the form Type/SubType.

file_mime_type(File, MimeType) :-
    file_name_extension(_, Ext, File),
    (   current_prolog_flag(windows, true)
    ->  downcase_atom(Ext, Lower),
        mime_extension(Lower, MimeType)
    ;   mime_extension(Ext, M0)
    ->  MimeType = M0
    ;   downcase_atom(Ext, Lower),
        mime_extension(Lower, MimeType)
    ),
    !.
file_mime_type(File, MimeType) :-
    file_base_name(File, Base),
    downcase_atom(Base, Lower),
    name_mimetype(Lower, Mime),
    !,
    MimeType = Mime.
file_mime_type(_, MimeType) :-
    default_mimetype(MimeType).

%!  mime:mime_extension(+Ext, -MimeType) is semidet.
%
%   Hook that is called by file_mime_type/2 before the default table
%   is examined.

mime_extension(Ext, MimeType) :-
    (   mime:mime_extension(Ext, Mime)
    ->  MimeType = Mime
    ;   ext_mimetype(Ext, Mime)
    ->  MimeType = Mime
    ).

%!  default_mimetype(-MimeType) is semidet.
%
%   If the mime-type cannot be determined   from the file extension,
%   this predicate is used as fallback.  It takes the value from the
%   Prolog flag =default_mimetype=. To change the default, use e.g.,
%
%     ==
%     :- set_prolog_flag(default_mimetype, text/plain).
%     ==
%
%   The initial default mime-type   is  =|application/unknown|=. Use
%   the value =|-|= to denote there is no default.

:- create_prolog_flag(default_mimetype, application/unknown, [keep(true)]).

default_mimetype(MimeType) :-
    current_prolog_flag(default_mimetype, MimeType),
    MimeType = _/_.


%!  ext_mimetype(+Extension, -MimeType) is semidet.
%
%   Built-in table of file-name extension to mime-type mappings.
%
%   @tbd    Update this list, e.g., from
%           http://www.webmaster-toolkit.com/mime-types.shtml

                                        % plain text
ext_mimetype(txt,  text/plain).
                                        % markup
ext_mimetype(htm,  text/html).
ext_mimetype(html, text/html).
ext_mimetype(xhtml, application/'xhtml+xml').
ext_mimetype(sgml, text/'x-sgml').
ext_mimetype(sgm,  text/'x-sgml').
ext_mimetype(xml,  text/xml).
ext_mimetype(css,  text/css).
ext_mimetype(xsl,  text/xml).           % Unclear what this should be.
ext_mimetype(md,   text/markdown).
                                        % Other data markup
ext_mimetype(json, application/json).
ext_mimetype(yaml, application/yaml).   % Not official
                                        % semantic web stuff
ext_mimetype(rdf,  application/'rdf+xml').
ext_mimetype(rdfs, application/'rdf+xml').
ext_mimetype(owl,  application/'rdf+xml').
ext_mimetype(ttl,  application/turtle).
ext_mimetype(nt,   application/'n-triples').
ext_mimetype(nq,   application/'n-quads').
                                        % Prolog source
ext_mimetype(pl,   text/plain).
                                        % Other languages
ext_mimetype(c,    text/'x-c').
ext_mimetype(h,    text/'x-c').
ext_mimetype(cc,   text/'x-c').
ext_mimetype(py,   text/'x-python').
ext_mimetype(java, text/'x-java').
ext_mimetype(sh,   text/plain).
                                        % Packaged formats
ext_mimetype(gz,   application/'x-gzip').
ext_mimetype(zip,  application/zip).
ext_mimetype(tgz,  application/'x-gtar').
                                        % Some document formats
ext_mimetype(pdf,  application/pdf).
ext_mimetype(doc,  application/msword).
                                        % Java classes
ext_mimetype(class, application/'octet-stream').
ext_mimetype(jar,  application/'x-java-archive').
                                        % JavaScript
ext_mimetype(js,   text/javascript).
                                        % Visual Basic Script :-(
ext_mimetype(vbs,  text/vbscript).
                                        % Some image formats
ext_mimetype(jpg,  image/jpeg).
ext_mimetype(jpeg, image/jpeg).
ext_mimetype(gif,  image/gif).
ext_mimetype(png,  image/png).
ext_mimetype(tif,  image/tiff).
ext_mimetype(tiff, image/tiff).
ext_mimetype(xpm,  image/'x-xpixmap').
ext_mimetype(ico,  image/'x-ico').
ext_mimetype(svg,  image/'svg+xml').
                                        % Google earth
ext_mimetype(kml,  application/'vnd.google-earth.kml+xml').
ext_mimetype(kmz,  application/'vnd.google-earth.kmz').

                                        % Flash
ext_mimetype(swf,  application/'x-shockwave-flash').
ext_mimetype(flv,  video/'x-flv').
                                        % MP3
ext_mimetype(mp3,  audio/mpeg).
                                        % Downloads
ext_mimetype(rpm,  application/'x-rpm').
ext_mimetype(exe,  application/'x-executable').

%!  name_mimetype(+DownCaseFileName, -MimeType) is semidet.
%
%   Determine the mime-type of files based on the entire filename.

name_mimetype(makefile,       text/plain).
name_mimetype(configure,      text/plain).
name_mimetype('configure.in', text/plain).
name_mimetype('configure.ac', text/plain).
name_mimetype('makefile.in',  text/plain).
name_mimetype('makefile.am',  text/plain).
name_mimetype('readme.in',    text/plain).

%!  text_mimetype(+MimeType) is semidet.
%
%   True when documents of MimeType are text documents and thus may need
%   a charset specification.

text_mimetype(MimeType) :-
    mime:text_mimetype(MimeType),
    !.
text_mimetype(text/_).

%!  file_content_type(+File:atom, -ContentType:atom) is det.
%!  file_content_type(+File:atom, ?MediaType, -ContentType:atom) is det.
%
%   True if File should be served using =|ContentType:|= ContentType. It
%   takes the following steps:
%
%     1. Determine the media type using file_mime_type/2, unless
%        already specified using file_content_type/3.
%     2. Determine it is a text file using text_mimetype/1
%     3. Use the charset from the Prolog flag `default_charset`
%
%   The behavior is controlled by several hooks and a flag.
%
%     - mime:mime_extension/2 defines the media type
%     - mime:text_mimetype/1 defines the media type is text
%     - mime:charset/3 derives the charset for a file with a given
%       media type, if the media type is text according to
%	mime:text_mimetype/1.
%     - If mime:text_mimetype/1 succeeds and mime:charset/3 fails, the
%       flag `default_charset` defines the charset unless it is set
%       to `-`.  The flag set by default to =UTF-8= if the Prolog
%       flag `encoding` is set to `utf8`.

file_content_type(File, ContentType) :-
    file_content_type(File, _, ContentType).
file_content_type(File, MediaType, ContentType) :-
    (   ground(MediaType)
    ->  true
    ;   file_mime_type(File, MediaType)
    ),
    (   text_mimetype(MediaType),
        (   mime:charset(File, MediaType, Charset0)
        ->  Charset = Charset0
        ;   default_charset(Charset)
        )
    ->  format(atom(ContentType), '~w; charset=~w', [MediaType, Charset])
    ;   format(atom(ContentType), '~w', [MediaType])
    ).

%!  mime:charset(+File, +MediaType, -Charset) is semidet.
%
%   Hook that determines the  Charset  for   File  that  has  media type
%   MediaType. This hook allows overruling file_content_type/2.
%
%   @see mime:text_mimetype/1.

default_charset(Charset) :-
    current_prolog_flag(default_charset, Charset),
    Charset \== (-).

set_default_charset :-
    current_prolog_flag(default_charset, _),
    !.
set_default_charset :-
    current_prolog_flag(encoding, utf8),
    !,
    set_prolog_flag(default_charset, 'UTF-8').
set_default_charset.

:- initialization(set_default_charset).
