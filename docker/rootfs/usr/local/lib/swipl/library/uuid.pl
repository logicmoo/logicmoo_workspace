/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012, VU University Amsterdam
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

:- module(uuid,
          [ uuid/1,                     % -UUID
            uuid/2                      % -UUID, +Options
          ]).

/** <module> Universally Unique Identifier (UUID) Library

The library provides operations on UUIDs.   Please consult other sources
for understanding UUIDs and  the  implications   of  the  different UUID
versions.  Some typical calls are given below:

  ==
  ?- uuid(X).
  X = 'ea6589fa-19dd-11e2-8a49-001d92e1879d'.

  ?- uuid(X, [url('http://www.swi-prolog.org')]).
  X = '73a07870-6a90-3f2e-ae2b-ffa538dc7c2c'.
  ==

@tbd Compare UUIDs, extract time and version from UUIDs
@see http://www.ossp.org/pkg/lib/uuid/
*/

:- use_foreign_library(foreign(uuid)).

%!  uuid(-UUID) is det.
%
%   UUID is an atom representing a  new   UUID.  This is the same as
%   calling uuid(UUID, []).  See uuid/2 for options.

uuid(UUID) :-
    uuid(UUID, []).

%!  uuid(-UUID, +Options) is det.
%
%   Create a new UUID according to   Options.  The following options
%   are defined:
%
%     * version(+Versions)
%     Integer in the range 1..5, which specifies the UUID version
%     that is created.  Default is 1.
%
%     * dns(DNS)
%     * url(URL)
%     * oid(OID)
%     * x500(X500)
%     Provide additional context information for UUIDs using version
%     3 or 5.  If there is no explicit version option, UUID version
%     3 is used.
%
%     * format(+Format)
%     Representation of the UUID.  Default is =atom=, yielding atoms
%     such as =|8304efdd-bd6e-5b7c-a27f-83f3f05c64e0|=. The
%     alternative is =integer=, returning a large integer that
%     represents the 128 bits of the UUID.
