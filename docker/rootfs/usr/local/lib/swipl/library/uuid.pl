/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2021, VU University Amsterdam
                              SWI-Prolog Solutions b.v.
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
            uuid/2,                     % -UUID, +Options
            uuid_property/2             % +UUID, ?Property
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
@see https://en.wikipedia.org/wiki/Universally_unique_identifier
*/

link_uuid :-
    catch(load_foreign_library(foreign(uuid)), _, true).

:- initialization(link_uuid, now).

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
%
%   If SWI-Prolog was not built with the  OSSP UUID dependency library a
%   simple Prolog alternative that  only   implements  version  4 random
%   UUIDs is provided. In this case  the   default  version is 4 and the
%   only admissible options are version(4) and format(Format).

:- if(current_predicate(ossp_uuid/2)).
uuid(UUID, Options) :-
    ossp_uuid(UUID, Options).

:- else.

uuid(UUID, []) :-
    !,
    random_uuid(UUID).
uuid(UUID, Options) :-
    option(version(4), Options, 4),
    option(format(Format), Options, atom),
    (   Format == atom
    ->  !, random_uuid(UUID)
    ;   Format == integer
    ->  !, random_int_uuid(UUID)
    ).
uuid(_UUID, Options) :-
    domain_error(uuid_options, Options).

random_uuid(UUID) :-
    Version = 4,
    A is random(0xffffffff),
    B is random(0xffff),
    C is random(0x0fff) \/ Version<<12,
    D is random(0xffff) \/ 0x8000,
    E is random(0xffffffffffff),
    format(atom(UUID),
           '~`0t~16r~8+-~|\c
            ~`0t~16r~4+-~|\c
            ~`0t~16r~4+-~|\c
            ~`0t~16r~4+-~|\c
            ~`0t~16r~12+', [A,B,C,D,E]).

random_int_uuid(UUID) :-
    Version = 4,
    A is random(0xffffffff),
    B is random(0xffff),
    C is random(0x0fff) \/ Version<<12,
    D is random(0xffff) \/ 0x8000,
    E is random(0xffffffffffff),
    UUID is (A<<96)+(B<<80)+(C<<84)+(D<<48)+E.

:- endif.

%!  uuid_property(+UUID, ?Property)
%
%   True when UUID is a property of the given UUID. Supported properties
%   are:
%
%     - version(V)
%       Return the version of the UUID (1..5)
%     - time(-Stamp)
%       Time using SWI-Prolog's time stamp (float with seconds
%       since January 1, 1970, UTC).  Only for version 1 and 2
%       UUIDs
%
%   @tbd Implement more properties.

uuid_property(UUID, P) :-
    property_uuid(P, UUID).

property_uuid(version(V), UUID) :-
    split_string(UUID, "-", "", [_A,_B,C,_D,_E]),
    sub_string(C, 0, 1, _, F),
    char_type(F, xdigit(V)).
property_uuid(time(Time), UUID) :-
    split_string(UUID, "-", "", [A,B,C,_D,_E]),
    sub_atom(C, 0, 1, _, F),
    has_time(F),
    sub_string(C, 1, _, 0, T),
    atomics_to_string(["0x",T,B,A], Hex),
    number_string(Nanos, Hex),
    Offset is 24*60*60*141427*10 000 000,
    Time is (Nanos-Offset)/10000000.0.

has_time('1').
has_time('2').
