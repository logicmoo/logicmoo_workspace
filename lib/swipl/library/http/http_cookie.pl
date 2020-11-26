/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011, VU University Amsterdam
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

:- module(http_cookie,
          [ cookie_remove_client/1,     % +ClientId
            cookie_remove_all_clients/0,
            cookie_current_cookie/4     % ?ClientId, ?Name, ?Value, ?Options
          ]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(option),[option/3]).
:- autoload(library(http/http_header),[http_parse_header_value/3]).

/** <module> HTTP client cookie handling

This module implements the cookie hooks  called from http_open/3, adding
cookie handling to the client.

This library supports a notion of _clients_. A client is a (ground) term
to which a cookie database is  connected.   This  allows a single Prolog
process to act  as  multiple  clients.   The  default  client  is called
=default=. Use the option client(+ClientId) to select another client.

The client and cookie database can be  inspected and cleared using these
predicates.

  * cookie_remove_client/1
  * cookie_remove_all_clients/0
  * cookie_current_cookie/4

@tbd add hooks to http_get/3 and http_post/4
*/

:- multifile
    http:write_cookies/3,           % +Out, +Parts, +Options
    http:update_cookies/3.          % +CookieData, +Parts, +Options

:- dynamic
    client_cookie/5.                % Id, CanName, Name, Value, Options

%!  http:write_cookies(+Out, +Parts, +Options) is det.
%
%   Emit a cookie header for the current request.

http:write_cookies(Out, Parts, Options) :-
    option(client(ClientId), Options, default),
    cookie(ClientId, Parts, Cookie),
    format(Out, 'Cookie: ~s\r\n', [Cookie]).

%!  cookie(+ClientId, +Parts, -Cookie) is semidet.
%
%   Cookie is the cookie for Parts for the given ClientId

cookie(ClientId, Parts, Cookie) :-
    request_host(Parts, Host),
    request_path(Parts, Path),
    findall(N=V, current_cookie(ClientId, Host, Path, N, V), Cookies),
    Cookies \== [],
    !,
    debug(http(cookie), 'Cookies for ~w at ~w~w: ~p',
          [ClientId, Host, Path, Cookies]),
    cookie_value(Cookies, Cookie).

request_host(Parts, Host) :-
    memberchk(host(Host), Parts).

request_path(Parts, Path) :-
    (   memberchk(path(Path), Parts)
    ->  true
    ;   Path = (/)
    ).

%!  cookie_value(+NameValueList, -CookieString) is det.
%
%   Create a cookie value string with name=value, separated by ";".

cookie_value(List, Cookie) :-
    with_output_to(string(Cookie),
                   write_cookies(List)).

write_cookies([]).
write_cookies([Name=Value|T]) :-
    format('~w=~w', [Name, Value]),
    (   T == []
    ->  true
    ;   format('; ', []),
        write_cookies(T)
    ).

%!  http:update_cookies(+CookieData, +Parts, +Options) is semidet.
%
%   Update the client  cookie  database.

http:update_cookies(CookieData, Parts, Options) :-
    http_parse_header_value(set_cookie, CookieData,
                            set_cookie(Name, Value, COptions)),
    !,
    option(client(ClientId), Options, default),
    request_host(Parts, Host),
    request_path(Parts, Path),
    with_mutex(http_cookie,
               update_cookie(ClientId, Host, Path, Name, Value, COptions)).

update_cookie(ClientId, Host, Path, Name, Value, Options) :-
    downcase_atom(Name, CName),
    remove_cookies(ClientId, Host, Path, CName, Options),
    debug(http(cookie), 'New for ~w: ~w=~p', [ClientId, Name, Value]),
    assert(client_cookie(ClientId, CName, Name, Value, [host=Host|Options])).

%!  remove_cookies(+ClientId, +Host, +Path, +Name, +SetOptions) is det.
%
%   Remove all cookies that conflict with the new set-cookie
%   command.

remove_cookies(ClientId, Host, Path, CName, SetOptions) :-
    (   client_cookie(ClientId, CName, Name, Value, OldOptions),
        cookie_match_host(Host, SetOptions, OldOptions),
        cookie_match_path(Path, SetOptions, OldOptions),
        debug(cookie, 'Del for ~w: ~w=~p', [ClientId, Name, Value]),
        retract(client_cookie(ClientId, CName, Name, Value, OldOptions)),
        fail
    ;   true
    ).

cookie_match_host(Host, SetOptions, OldOptions) :-
    (   memberchk(domain=Domain, SetOptions)
    ->  cookie_match_host(Domain, OldOptions)
    ;   cookie_match_host(Host, OldOptions)
    ).

cookie_match_path(Path, SetOptions, OldOptions) :-
    (   memberchk(path=PathO, SetOptions)
    ->  cookie_match_path(PathO, OldOptions)
    ;   cookie_match_path(Path, OldOptions)
    ).

%!  current_cookie(+ClientId, +Host, +Path, -Name, -Value) is nondet.
%
%   Find cookies that match the given request.

current_cookie(ClientId, Host, Path, Name, Value) :-
    client_cookie(ClientId, _CName, Name, Value, Options),
    cookie_match_host(Host, Options),
    cookie_match_path(Path, Options),
    cookie_match_expire(Options).

cookie_match_host(Host, Options) :-
    (   memberchk(domain=Domain, Options)
    ->  downcase_atom(Host, LHost),
        downcase_atom(Domain, LDomain),
        sub_atom(LHost, _, _, 0, LDomain)   % TBD: check '.'?
    ;   memberchk(host=CHost, Options),
        downcase_atom(Host, LHost),
        downcase_atom(CHost, LHost)
    ).

cookie_match_path(Path, Options) :-
    (   memberchk(path=Root, Options)
    ->  sub_atom(Path, 0, _, _, Root)       % TBD: check '/'?
    ;   true
    ).

cookie_match_expire(Options) :-
    (   memberchk(expire=Expire, Options)
    ->  get_time(Now),
        Now =< Expire
    ;   true
    ).

%!  cookie_remove_client(+ClientId) is det.
%
%   Fake user quitting a browser.   Removes all cookies that do
%   not have an expire date.

cookie_remove_client(ClientId) :-
    var(ClientId),
    !,
    throw(error(instantiation_error, _)).
cookie_remove_client(ClientId) :-
    (   client_cookie(ClientId, CName, Name, Value, Options),
        \+ memberchk(expire=_, Options),
        retract(client_cookie(ClientId, CName, Name, Value, Options)),
        fail
    ;   true
    ).

%!  cookie_remove_all_clients is det.
%
%   Simply logout all clients.  See http_remove_client/1.

cookie_remove_all_clients :-
    forall(current_client(ClientId),
           cookie_remove_client(ClientId)).

%!  current_client(?ClientId) is nondet.
%
%   True if ClientId is the identifier of a client.

current_client(ClientId) :-
    client_cookie(ClientId, _CName, _Name, _Value, _Options).

%!  http_current_cookie(?ClientId, ?Name, ?Value, ?Options) is nondet.
%
%   Query current cookie database. If Name   is given, it is matched
%   case insensitive against the known cookies.   If  it is unbound,
%   the  cookie  name  is  returned  in    its  oiginal  case  (case
%   preserving).

cookie_current_cookie(ClientId, Name, Value, Options) :-
    nonvar(Name),
    !,
    downcase_atom(Name, CName),
    client_cookie(ClientId, CName, Name, Value, Options).
cookie_current_cookie(ClientId, Name, Value, Options) :-
    client_cookie(ClientId, _CName, Name, Value, Options).
