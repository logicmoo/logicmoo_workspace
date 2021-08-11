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

:- module(google_client,
	  [ oauth_authenticate/3,	% +Request, +Site, +Options
	    openid_connect_discover/2	% +Site, -DiscoveryDict
	  ]).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_path), []).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(uri)).
:- use_module(library(lists)).
:- use_module(library(debug)).

:- use_module(jwt).

/** <module> Sign in with Google OpenID Connect

This  module  deals   with   the    Google   OpenID   Connect  federated
authentication method.  An HTTP handler that wishes to establish a login
using Google uses the following flow of control.

  - Call oauth_authenticate/3. This predicates redirects to Google,
    which in turn redirects to oath2(auth_redirect), implemented by
    oauth_handle_redirect/1.

  - The predicate oauth_handle_redirect/1 establishes the Google unique
    user identification (a string holding large integer) and email.  It
    calls the multifile hook google_client:login_existing_user/1, which
    logs in the user (e.g., by starting an HTTP session and associating
    the user with the session) and replies with a web page (or
    redirect).

  - If google_client:login_existing_user/1 *fails*, this library fetches
    user profile information from Google and calls the hook
    google_client:create_user/1.  The create_user hook is passed the
    basic Google profile information.  Its task is to create a new user.

@see https://developers.google.com/accounts/docs/OpenIDConnect
*/

:- multifile
	login_existing_user/1,		% +Claim
	create_user/1,			% +Profile
	key/2.				% +Name, -Value

http:location(oath2, root(oauth2), [priority(-100)]).

:- http_handler(oath2(auth_redirect), oauth_handle_redirect, []).

:- dynamic
	forgery_state/5.		% State, Site, Redirect, ClientData, Time

%%	oauth_authenticate(+Request, +Site, +Options)
%
%	Step 2: redirect to Google for  obtaining an authorization code.
%	Google redirects back to oauth_handle_response/1.  Options:
%
%	  - realm(+Realm)
%	  Value for `openid.realm`.  Normally, this is the site's
%	  root URL.  By default, it is not sent.
%	  - login_hint(+Hint)
%	  Hint to select the right account.  Typically an email
%	  address.  By default, it is not sent.
%	  - client_data(+Data)
%	  Add the given Data (any Prolog term) to the dict that is
%	  passed to the login hooks.

oauth_authenticate(Request, Site, Options) :-
	oauth_options(Options, Params),
	openid_connect_discover(Site, DiscDoc),
	key(client_id, ClientId),
	http_link_to_id(oauth_handle_redirect, [], LocalRedirect),
	public_url(Request, LocalRedirect, Redirect),
	option(client_data(ClientData), Options, _),
	anti_forgery_state(AntiForgery),
	get_time(Now),
	asserta(forgery_state(AntiForgery, Site, Redirect, ClientData, Now)),
	url_extend(search([ client_id(ClientId),
			    response_type(code),
			    scope('openid email profile'),
			    state(AntiForgery),
			    redirect_uri(Redirect)
			  | Params
			  ]),
		   DiscDoc.authorization_endpoint,
		   URL),
	http_redirect(moved_temporary, URL, Request).

oauth_options([], []).
oauth_options([H0|T0], [H|T]) :-
	name_value(H0, Name, Value),
	oauth_option(Name, NameTo), !,
	H =.. [NameTo,Value],
	oauth_options(T0, T).
oauth_options([_|T0], T) :-
	oauth_options(T0, T).

oauth_option(realm,      'openid.realm').
oauth_option(login_hint, login_hint).

name_value(Name = Value, Name, Value) :- !.
name_value(Term, Name, Value) :-
	Term =.. [Name,Value].


%%	oauth_handle_redirect(Request)
%
%	HTTP handler that deals with the  redirect back from Google that
%	provides us the authorization code. This  Implements steps 3 and
%	4 of the OpenID Connect process:
%
%	  - Confirm anti-forgery state token
%	  - Exchange code for access token and ID token

oauth_handle_redirect(Request) :-
	http_parameters(Request,
			[ state(State, []),
			  code(Code, [])
			],
			[ %form_data(Form)
			]),
	validate_forgery_state(State, Site, Redirect, ClientData),
	openid_connect_discover(Site, DiscDoc),
	key(client_id, ClientId),
	key(client_secret, ClientSecret),
	http_open(DiscDoc.token_endpoint,
		  In,
		  [ cert_verify_hook(cert_verify),
		    post(form([ code(Code),
				client_id(ClientId),
				client_secret(ClientSecret),
				redirect_uri(Redirect),
				grant_type(authorization_code)
			      ]))
		  ]),
	call_cleanup(json_read_dict(In, Response),
		     close(In)),
	jwt(Response.id_token, Claim),
	oauth_login(Claim, Response, DiscDoc, ClientData).

%%	oauth_login(+Claim, +Response, +DiscDoc, +ClientData)
%
%	Handle the oauth claim. At least from Google, the claim contains
%	the following interesting fields:
%
%	  - sub:   (long) integer representing the id in Google
%	  - email: The user's email
%	  - email_verified: boolean
%
%	We now have two tasks. If `sub` is   known, we are done. If not,
%	we must make a new account. To  do   so,  we can prefill info by
%	extracting the Google  _user  profile   information_  using  the
%	_OpenID Connect_ method.
%
%	@see https://developers.google.com/accounts/docs/OpenIDConnect#obtaininguserprofileinformation

oauth_login(Claim, _, _, ClientData) :-
	add_client_data(ClientData, Claim, Claim1),
	login_existing_user(Claim1), !.
oauth_login(_Claim, Response, DiscDoc, ClientData) :-
	key(client_id, ClientId),
	key(client_secret, ClientSecret),
	url_extend(search([ access_token(Response.access_token),
			    client_id(ClientId),
			    client_secret(ClientSecret)
			  ]),
		   DiscDoc.userinfo_endpoint,
		   URL),
	http_open(URL,
		  In,
		  [ cert_verify_hook(cert_verify)
		  ]),
	call_cleanup(json_read_dict(In, Profile),
		     close(In)),
	add_client_data(ClientData, Profile, Profile1),
	create_user(Profile1).

add_client_data(ClientData, Dict, Dict) :- var(ClientData), !.
add_client_data(ClientData, Dict, Dict.put(client_data, ClientData)).

validate_forgery_state(State, Site, Redirect, ClientData) :-
	(   forgery_state(State, Site, Redirect, ClientData, Stamp)
	->  retractall(forgery_state(State, Site, Redirect, ClientData, Stamp))
	;   throw(http_reply(not_acceptable('Invalid state parameter')))
	).

anti_forgery_state(State) :-
	Rand is random(1<<100),
	variant_sha1(Rand, State).

%%	openid_connect_discover(+Site, -Dict) is det.
%
%	True when Dicr represents _The Discovery document_.

:- dynamic
	discovered_data/3.		% URL, Time, Data

openid_connect_discover(Site, Dict) :-
	openid_connect_discover_url(Site, URL),
	(   discovered_data(URL, Dict0)
	->  Dict = Dict0
	;   discover_data(URL, Expires, Dict0),
	    cache_data(URL, Expires, Dict0),
	    Dict = Dict0
	).

discover_data(URL, Expires, Dict) :-
	http_open(URL, In,
                  [ cert_verify_hook(cert_verify),
		    header(expires, Expires)
		  ]),
	json_read_dict(In, Dict),
	close(In).

discovered_data(URL, Data) :-
	discovered_data(URL, Expires, Data0),
	get_time(Now),
	(   Now =< Expires
	->  Data = Data0
	;   retractall(discovered_data(URL, Expires, _)),
	    fail
	).

cache_data(URL, Expires, Data) :-
	parse_time(Expires, _Format, Stamp), !,
	asserta(discovered_data(URL, Stamp, Data)).
cache_data(_, _, _).

:- multifile
	openid_connect_discover_url/2.

openid_connect_discover_url(
    'google.com',
    'https://accounts.google.com/.well-known/openid-configuration').


		 /*******************************
		 *	      HOOKS		*
		 *******************************/

%%	key(+Which, -Key) is det.
%
%	This hook must provide the Google API   keys.  Key is one of the
%	values below. The keys are obtained  from Google as explained in
%	https://developers.google.com/+/web/signin/add-button
%
%	  - client_id
%	  - client_secret

%%	login_existing_user(+Claim) is semidet.
%
%	Called after establishing the identify of the logged in user.
%	Claim is a dict containing
%
%	  - sub:string
%	  String that uniquely indentifies the user inside Google.
%	  - email:string
%	  Email address of the user.
%	  - client_data:Term
%	  Present if oauth_authenticate/3 was called with the option
%	  client_data(Term).  Note that the term passed is a copy.
%
%	This call must return an HTML  document indicating that the user
%	logged in successfully or redirect  to   the  URL  supplied with
%	return to using http_redirect/3.

%%	create_user(+Profile) is det.
%
%	Called after login_existing_user/1 fails and  the Google profile
%	for the user has been fetched. Contains  the same info as passed
%	to  login_existing_user/1  as   well    as   additional  profile
%	information  such  as  `family_name`,   `gender`,  `given_name`,
%	`locale`, `name`, `picture` and `profile`. Check the Google docs
%	for details.
%
%	This call creates a new user, typically after verifying that the
%	user   is   human    and    completing     the    profile.    As
%	login_existing_user/1, it must return a web page or redirect.


		 /*******************************
		 *	    SSL SUPPORT		*
		 *******************************/

%%	cert_verify(SSL, ProblemCert, AllCerts, FirstCert, Error) is det.
%
%	Used by SSL to verify the certificate.

:- public cert_verify/5.

cert_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :-
        debug(ssl(cert_verify),'~s', ['Accepting certificate']).


		 /*******************************
		 *	    URI GOODIES		*
		 *******************************/

%%	url_extend(+Extend, +URL0, -URL)
%
%	Extend a URL, typically by adding parameters to it.

url_extend(search(Params), URL0, URL) :-
	uri_components(URL0, Components0),
	uri_data(search, Components0, Search0),
	extend_search(Search0, Params, Search),
	uri_data(search, Components0, Search, Components),
	uri_components(URL, Components).

extend_search(Var, Params, String) :-
	var(Var), !,
	uri_query_components(String, Params).
extend_search(String0, Params, String) :-
	uri_query_components(String0, Params0),
	append(Params0, Params, AllParams),
	uri_query_components(String, AllParams).


%%	public_url(+Request, +Path, -URL) is det.
%
%	True when URL is a publically useable  URL that leads to Path on
%	the current server. Needed for  the   redirect  URL that we must
%	present with the authentication request.

public_url(Request, Path, URL) :-
	http_current_host(Request, Host, Port,
			  [ global(true)
			  ]),
	setting(http:public_scheme, Scheme),
	set_port(Scheme, Port, AuthC),
	uri_authority_data(host, AuthC, Host),
	uri_authority_components(Auth, AuthC),
	uri_data(scheme, Components, Scheme),
	uri_data(authority, Components, Auth),
	uri_data(path, Components, Path),
	uri_components(URL, Components).

set_port(Scheme, Port, _) :-
	scheme_port(Scheme, Port), !.
set_port(_, Port, AuthC) :-
	uri_authority_data(port, AuthC, Port).

scheme_port(http, 80).
scheme_port(https, 443).
