/*  This file is part of ClioPatria.

    Author:
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(http_cookie,
	  [ http_get/4,			% +ClientId, +Request, -Reply, +Options
	    http_remove_client/1,	% +ClientId
	    http_remove_all_clients/0,
	    http_current_cookie/4	% ?ClientId, ?Name, ?Value, ?Options
	  ]).
:- use_module(library('http/http_client')).
:- use_module(library(url)).
:- use_module(library(debug)).

/** <module> HTTP client cookie handling

This module defines http_get/4, a wrapper around http_get/3 where we can
define multiple `virtual' clients, each managing a cookie database.

It was designed to deal  with   cookie-based  session  management in the
client, where the same Prolog process   is client in multiple concurrent
sessions.
*/

:- dynamic
	client_cookie/4.		% Id, Name, Value, Options


%%	http_get(+ClientId, +Request, -Reply, +Options) is det.
%
%	Add cookie handling to http_get/3.  ClientId   is  a ground term
%	representing the client. The library takes  care of cookies send
%	by the server and updates its cookie information.

http_get(ClientId, Request0, Reply, Options) :-
	break_url(Request0, Request),
	(   memberchk(reply_header(Header), Options)
	->  GetOptions = Options
	;   GetOptions = [reply_header(Header)|Options]
	),
	add_cookies(ClientId, Request, GetOptions, AllOptions),
	http_get(Request, Reply, AllOptions),
	update_cookies(ClientId, Request, Header).

%%	break_url(+UrlOrRequest, -Parts) is det.
%
%	Break a URL into parts.  Returns   input  if it already contains
%	parts.  See parse_url/2 for the format.

break_url(URL, Request) :-
	atomic(URL), !,
	parse_url(URL, Request).
break_url(Request, Request).

%%	add_cookies(+ClientId, +Request, +Options0, -Options) is det.
%
%	Add cookies to an HTTP request.

add_cookies(ClientId, Request, Options,
	    [request_header('Cookie'=Cookie)|Options]) :-
	request_host(Request, Host),
	request_path(Request, Path),
	findall(N=V, current_cookie(ClientId, Host, Path, N, V), Cookies),
	Cookies \== [], !,
	debug(cookie, 'Cookies for ~w at ~w~w: ~p',
	      [ClientId, Host, Path, Cookies]),
	cookie_value(Cookies, Cookie).
add_cookies(_, _, Options, Options).

request_host(Request, Host) :-
	(   memberchk(host(Host), Request)
	->  true
	;   throw(error(existence_error(parameter, host), _))
	).

request_path(Request, Path) :-
	(   memberchk(path(Path), Request)
	->  true
	;   Path = (/)
	).
		
%%	cookie_value(+NameValueList, -CookieString) is det.
%
%	Create a cookie value string with name=value, seperated by ";".

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

%%	update_cookies(+ClientId, +Request, +Header) is det.
%
%	Update the client  cookie  database.   Request  is  the original
%	request. Header is the HTTP reply-header.

update_cookies(ClientId, Request, Header) :-
	memberchk(set_cookie(set_cookie(Name, Value, Options)), Header), !,
	request_host(Request, Host),
	request_path(Request, Path),
	with_mutex(http_cookie,
		   update_cookie(ClientId, Host, Path, Name, Value, Options)).
update_cookies(_, _, _).

update_cookie(ClientId, Host, Path, Name, Value, Options) :-
	remove_cookies(ClientId, Host, Path, Name, Options),
	debug(cookie, 'New for ~w: ~w=~p', [ClientId, Name, Value]),
	assert(client_cookie(ClientId, Name, Value, [host=Host|Options])).

%%	remove_cookies(+ClientId, +Host, +Path, +Name, +SetOptions) is det.
% 
%	Remove all cookies that conflict with the new set-cookie
%	command.

remove_cookies(ClientId, Host, Path, Name, SetOptions) :-
	(   client_cookie(ClientId, Name, Value, OldOptions),
	    cookie_match_host(Host, SetOptions, OldOptions),
	    cookie_match_path(Path, SetOptions, OldOptions),
	    debug(cookie, 'Del for ~w: ~w=~p', [ClientId, Name, Value]),
	    retract(client_cookie(ClientId, Name, Value, OldOptions)),
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

%%	current_cookie(+ClientId, +Host, +Path, -Name, -Value) is nondet.
% 
%	Find cookies that match the given request.

current_cookie(ClientId, Host, Path, Name, Value) :-
	client_cookie(ClientId, Name, Value, Options),
	cookie_match_host(Host, Options),
	cookie_match_path(Path, Options),
	cookie_match_expire(Options).

cookie_match_host(Host, Options) :-
	(   memberchk(domain=Domain, Options)
	->  downcase_atom(Host, LHost),
	    downcase_atom(Domain, LDomain),
	    sub_atom(LHost, _, _, 0, LDomain) 	% TBD: check '.'?
	;   memberchk(host=CHost, Options),
	    downcase_atom(Host, LHost),
	    downcase_atom(CHost, LHost)
	).

cookie_match_path(Path, Options) :-
	(   memberchk(path=Root, Options)
	->  sub_atom(Path, 0, _, _, Root)	% TBD: check '/'?
	;   true
	).

cookie_match_expire(Options) :-
	(   memberchk(expire=Expire, Options)
	->  get_time(Now),
	    Now =< Expire
	;   true
	).

%%	http_remove_client(+ClientId) is det.
%
%	Fake user quitting a browser.   Removes all cookies that do
%	not have an expire date.

http_remove_client(ClientId) :-
	var(ClientId), !,
	throw(error(instantiation_error, _)).
http_remove_client(ClientId) :-
	(   client_cookie(ClientId, Name, Value, Options),
	    \+ memberchk(expire=_, Options),
	    retract(client_cookie(ClientId, Name, Value, Options)),
	    fail
	;   true
	).

%%	http_remove_all_clients is det.
%
%	Simply logout all clients.  See http_remove_client/1.

http_remove_all_clients :-
	forall(current_client(ClientId),
	       http_remove_client(ClientId)).

%%	current_client(?ClientId) is nondet.
%
%	True if ClientId is the identifier of a client.

current_client(ClientId) :-
	client_cookie(ClientId, _Name, _Value, _Options).

%%	http_current_cookie(?ClientId, ?Name, ?Value, ?Options)
%
%	Query current cookie database

http_current_cookie(ClientId, Name, Value, Options) :-
	client_cookie(ClientId, Name, Value, Options).
