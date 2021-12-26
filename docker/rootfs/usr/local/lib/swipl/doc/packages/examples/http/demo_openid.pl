/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2011, University of Amsterdam,
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

:- asserta(file_search_path(library, '..')).

:- use_module(library(uri)).
:- use_module(library(http/http_openid)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_error)).

http:location(openid, root(openid), []).

:- multifile
    http_openid:openid_hook/1.

http_openid:openid_hook(trusted(_OpenID, Server)) :-
    debug(openid(test), 'Trusting server ~q', [Server]).


%!  server
%
%   Create demo server  and  client.   After  starting  the  server,
%   contact http://localhost:8000/

server :-
    debug(openid(_)),
    Port = 8000,
    http_server(http_dispatch,
                [ port(Port)
                ]).


assoc :-
    openid_associate('http://localhost:8000/openid/server', Handle, Assoc),
    writeln(Handle-Assoc).

%!  secret(+Request) is det.
%
%   Example of a handler that requires an  OpenID login. If the user
%   is not logged it, it will be  redirected to the login page, from
%   there to the OpenID server and back here. All this is completely
%   transparent to us.

:- http_handler(root('secret'), secret, []).

secret(Request) :-
    openid_user(Request, User, []),
    reply_html_page(title('Secret'),
                    [ 'You\'ve reached the secret page as user ', %'
                      a(href(User), User)
                    ]).

%!  root(+Request).
%!  allow(+Request).
%
%   Shows an indirect login.

:- http_handler(root(.),             root,                              []).
:- http_handler(root('test/verify'), openid_verify([return_to(allow)]), []).
:- http_handler(root('test/allow'),  allow,                             []).

root(_Request) :-
    reply_html_page(title('Demo OpenID consumer'),
                    [ h1('OpenID consumer'),
                      form([ name(login),
                             action('/test/verify'),
                             method('GET')
                           ],
                           [ div([ 'OpenID: ',
                                   input([ name(openid_url),
                                           size(40),
                                           value('http://localhost:8000/user/bob') % test
                                         ]),
                                   input([type(submit), value('Verify!')])
                                 ])
                           ]),
                      p([ 'Or go directly to the ', a(href=secret, 'secret page') ])
                    ]).


allow(Request) :-
    openid_user(Request, OpenID, []),
    openid_server(_OpenIDLogin, OpenID, Server),
    reply_html_page(title('Success'),
                    [ h1('OpenID login succeeded'),
                      p([ 'The OpenID server ',
                          a(href(Server),Server),
                          ' verified you as ',
                          a(href(OpenID), OpenID)
                        ])
                    ]).


                 /*******************************
                 *         OpenID SERVER        *
                 *******************************/

:- http_handler(root('user/'),  user_page,         [prefix]).
:- http_handler(openid(server), openid_server([]), []).
:- http_handler(openid(grant),  openid_grant, []).

:- multifile
    http_openid:openid_hook/1.

http_openid:openid_hook(grant(_Request, Options)) :-
    debug(openid(test), 'Granting access to ~p', [Options]).

%!  user_page(+Request) is det.
%
%   Generate a page for user as /user/<user>.

user_page(Request) :-
    http_current_host(Request, Host, Port,
                      [ global(true)
                      ]),
    http_location_by_id(openid_server, ServerLocation),
    uri_authority_data(host, AComp, Host),
    uri_authority_data(port, AComp, Port),
    uri_authority_components(Authority, AComp),
    uri_data(scheme, Components, http),
    uri_data(authority, Components, Authority),
    uri_data(path, Components, ServerLocation),
    uri_components(OpenIDServer, Components),
    memberchk(path_info(User), Request),
    reply_html_page([ link([ rel('openid.server'),
                             href(OpenIDServer)
                           ]),
                      title('OpenID page of ~w'-[User])
                    ],
                    h1('OpenID page of ~w'-[User])).


                 /*******************************
                 *              DEBUG           *
                 *******************************/

:- http_handler(root(.), print_request, [prefix]).

print_request(Request) :-
    format('Content-type: text/plain~n~n'),
    pp(Request).
