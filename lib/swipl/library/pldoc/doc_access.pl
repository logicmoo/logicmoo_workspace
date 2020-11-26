/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2012, University of Amsterdam
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

:- module(doc_access,
          [ host_access_options/2       % +AllOptions, -NoAccessOptions
          ]).
:- use_module(library(http/http_hook)).
:- use_module(library(dcg/basics)).

:- dynamic
    can_edit/1,
    allow_from/1,
    deny_from/1.

:- meta_predicate
    match_peer(1, +, +).

%       http:authenticate(+Type, +Request, -Extra) is semidet.
%
%       PlDoc specific access control. The access   control  is based on
%       these options that are passed from starting PlDoc. If PlDoc runs
%       on  top  of   an   external    dispatch   loop,   the  predicate
%       host_access_options/2 can be used to specify access rights.
%
%           * allow(+IP)
%           * deny(+IP)
%           * edit(+Bool)

http:authenticate(pldoc(read), Request, []) :-
    !,
    memberchk(peer(Peer), Request),
    allowed_peer(Peer).
http:authenticate(pldoc(edit), Request, []) :-
    !,
    (   can_edit(false)
    ->  fail
    ;   (   memberchk(x_forwarded_for(Forwarded), Request),
            primary_forwarded_host(Forwarded, IPAtom),
            parse_ip(IPAtom, Peer)
        ->  true
        ;   memberchk(peer(Peer), Request)
        ),
        match_peer(localhost, +, Peer)
    ).


%!  host_access_options(+AllOptions, -NoAuthOptions) is det.
%
%   Filter the authorization options from   AllOptions,  leaving the
%   remaining options in NoAuthOptions.

host_access_options([], []).
host_access_options([H|T0], T) :-
    host_access_option(H),
    !,
    host_access_options(T0, T).
host_access_options([H|T0], [H|T]) :-
    host_access_options(T0, T).

host_access_option(allow(From)) :-
    assert(allow_from(From)).
host_access_option(deny(From)) :-
    assert(deny_from(From)).
host_access_option(edit(Bool)) :-
    assert(can_edit(Bool)).

%!  match_peer(:RuleSet, +PlusMin, +Peer) is semidet.
%
%   True if Peer is covered by the   ruleset RuleSet. Peer is a term
%   ip(A,B,C,D). RuleSet is a predicate with   one  argument that is
%   either  a  partial  ip  term,  a    hostname  or  a  domainname.
%   Domainnames start with a '.'.
%
%   @param PlusMin  Positive/negative test.  If IP->Host fails, a
%                   positive test fails, while a negative succeeds.
%                   I.e. deny('.com') succeeds for unknown IP
%                   addresses.

match_peer(Spec, _, Peer) :-
    call(Spec, Peer),
    !.
match_peer(Spec, PM, Peer) :-
    (   call(Spec, HOrDom), atom(HOrDom)
    ->  (   catch(tcp_host_to_address(Host, Peer), E, true),
            var(E)
        ->  call(Spec, HostOrDomain),
            atom(HostOrDomain),
            (   sub_atom(HostOrDomain, 0, _, _, '.')
            ->  sub_atom(Host, _, _, 0, HostOrDomain)
            ;   HostOrDomain == Host
            )
        ;   PM == (+)
        ->  !, fail
        ;   true
        )
    ).

%!  allowed_peer(+Peer) is semidet.
%
%   True if Peer is allowed according to the rules.

allowed_peer(Peer) :-
    match_peer(deny_from, -, Peer),
    !,
    match_peer(allow_from, +, Peer).
allowed_peer(Peer) :-
    allow_from(_),
    !,
    match_peer(allow_from, +, Peer).
allowed_peer(_).


:- dynamic
    can_edit/1.

%!  primary_forwarded_host(+Spec, -Host) is det.
%
%   x_forwarded host contains multiple hosts separated   by  ', ' if
%   there are multiple proxy servers in   between.  The first one is
%   the one the user's browser knows about.

primary_forwarded_host(Spec, Host) :-
    sub_atom(Spec, B, _, _, ','),
    !,
    sub_atom(Spec, 0, B, _, Host).
primary_forwarded_host(Host, Host).


localhost(ip(127,0,0,1)).
localhost(localhost).

parse_ip(Atom, IP) :-
    atom_codes(Atom, Codes),
    phrase(ip(IP), Codes).

%!  ip(?IP)// is semidet.
%
%   Parses A.B.C.D into ip(A,B,C,D)

ip(ip(A,B,C,D)) -->
    integer(A), ".", integer(B), ".", integer(C), ".", integer(D).

