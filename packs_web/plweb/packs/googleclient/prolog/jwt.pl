/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(jwt,
	  [ jwt/2			% +String, -Data
	  ]).
:- use_module(library(codesio)).
:- use_module(library(base64)).
:- use_module(library(utf8)).
:- use_module(library(http/json)).

/** <module> JSON Web Token library

This library is a very  early  start   to  deal  with  JOSE: JSON Object
Signing and Encryption. This is needed   for OpenID Connect. The current
library only extracts the claimed object  from a non-encrypted JWT (JSON
Web Token). This is enough to deal   with Google's OpenID Connect, which
guarantees that the token comes from Google in other ways.

@see https://tools.ietf.org/html/draft-jones-json-web-token
*/

%%	jwt(+String, -Object) is det.
%
%	True if Object is claimed in the JWT represented in String.
%
%	@tbd Currently does not validate the claim using the signature.

jwt(String, Object) :-
	nonvar(String),
	split_string(String, ".", "", [Header64,Object64|_Parts]),
	base64url_json(Header64, _Header),
	base64url_json(Object64, Object).

%%	base64url_json(+String, -JSONDict) is semidet.
%
%	True when JSONDict is represented  in   the  Base64URL and UTF-8
%	encoded String.

base64url_json(String, JSON) :-
	string_codes(String, Codes),
	phrase(base64url(Bytes), Codes),
	phrase(utf8_codes(Text), Bytes),
	setup_call_cleanup(
	    open_codes_stream(Text, Stream),
	    json_read_dict(Stream, JSON),
	    close(Stream)).

