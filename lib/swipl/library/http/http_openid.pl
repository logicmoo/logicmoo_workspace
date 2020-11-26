/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2015, University of Amsterdam,
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

:- module(http_openid,
          [ openid_login/1,             % +OpenID
            openid_logout/1,            % +OpenID
            openid_logged_in/1,         % -OpenID

                                        % transparent login
            openid_user/3,              % +Request, -User, +Options

                                        % low-level primitives
            openid_verify/2,            % +Options, +Request
            openid_authenticate/4,      % +Request, -Server, -Identity, -ReturnTo
            openid_associate/3,         % +OpenIDServer, -Handle, -Association
            openid_associate/4,         % +OpenIDServer, -Handle, -Association,
                                        % +Options
            openid_server/2,            % +Options, +Request
            openid_server/3,            % ?OpenIDLogin, ?OpenID, ?Server
            openid_grant/1,             % +Request

            openid_login_form//2,       % +ReturnTo, +Options, //

            openid_current_url/2,       % +Request, -URL
            openid_current_host/3       % +Request, -Host, -Port
          ]).
:- use_module(library(http/http_open)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_host)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_server_files), []).
:- use_module(library(http/yadis)).
:- use_module(library(http/ax)).
:- use_module(library(utf8)).
:- use_module(library(error)).
:- use_module(library(xpath)).
:- use_module(library(sgml)).
:- use_module(library(uri)).
:- use_module(library(occurs)).
:- use_module(library(base64)).
:- use_module(library(debug)).
:- use_module(library(record)).
:- use_module(library(option)).
:- use_module(library(sha)).
:- use_module(library(lists)).
:- use_module(library(settings)).

:- predicate_options(openid_login_form/4, 2,
                     [ action(atom),
                       buttons(list),
                       show_stay(boolean)
                     ]).
:- predicate_options(openid_server/2, 1,
                     [ expires_in(any)
                     ]).
:- predicate_options(openid_user/3, 3,
                     [ login_url(atom)
                     ]).
:- predicate_options(openid_verify/2, 1,
                     [ return_to(atom),
                       trust_root(atom),
                       realm(atom),
                       ax(any)
                     ]).

/** <module> OpenID consumer and server library

This library implements the OpenID protocol (http://openid.net/). OpenID
is a protocol to share identities on   the  network. The protocol itself
uses simple basic  HTTP,  adding   reliability  using  digitally  signed
messages.

Steps, as seen from the _consumer_ (or _|relying partner|_).

        1. Show login form, asking for =openid_identifier=
        2. Get HTML page from =openid_identifier= and lookup
           =|<link rel="openid.server" href="server">|=
        3. Associate to _server_
        4. Redirect browser (302) to server using mode =checkid_setup=,
           asking to validate the given OpenID.
        5. OpenID server redirects back, providing digitally signed
           conformation of the claimed identity.
        6. Validate signature and redirect to the target location.

A *consumer* (an application that allows OpenID login) typically uses
this library through openid_user/3. In addition, it must implement the
hook http_openid:openid_hook(trusted(OpenId, Server)) to define accepted
OpenID servers. Typically, this hook is used to provide a white-list of
acceptable servers. Note that accepting any OpenID server is possible,
but anyone on the internet can setup a dummy OpenID server that simply
grants and signs every request. Here is an example:

    ==
    :- multifile http_openid:openid_hook/1.

    http_openid:openid_hook(trusted(_, OpenIdServer)) :-
        (   trusted_server(OpenIdServer)
        ->  true
        ;   throw(http_reply(moved_temporary('/openid/trustedservers')))
        ).

    trusted_server('http://www.myopenid.com/server').
    ==

By default, information who is logged on  is maintained with the session
using http_session_assert/1 with the term   openid(Identity).  The hooks
login/logout/logged_in can be used to provide alternative administration
of logged-in users (e.g., based on client-IP, using cookies, etc.).

To create a *server*,  you  must  do   four  things:  bind  the handlers
openid_server/2  and  openid_grant/1  to  HTTP    locations,  provide  a
user-page for registered users and   define  the grant(Request, Options)
hook to verify  your  users.  An  example   server  is  provided  in  in
<plbase>/doc/packages/examples/demo_openid.pl
*/

                 /*******************************
                 *        CONFIGURATION         *
                 *******************************/

http:location(openid, root(openid), [priority(-100)]).

%!  openid_hook(+Action)
%
%   Call hook on the OpenID management library.  Defined hooks are:
%
%     * login(+OpenID)
%     Consider OpenID logged in.
%
%     * logout(+OpenID)
%     Logout OpenID
%
%     * logged_in(?OpenID)
%     True if OpenID is logged in
%
%     * grant(+Request, +Options)
%     Server: Reply positive on OpenID
%
%     * trusted(+OpenID, +Server)
%     True if Server is a trusted OpenID server
%
%     * ax(Values)
%     Called if the server provided AX attributes
%
%     * x_parameter(+Server, -Name, -Value)
%     Called to find additional HTTP parameters to send with the
%     OpenID verify request.

:- multifile
    openid_hook/1.                  % +Action

                 /*******************************
                 *       DIRECT LOGIN/OUT       *
                 *******************************/

%!  openid_login(+OpenID) is det.
%
%   Associate the current  HTTP  session   with  OpenID.  If another
%   OpenID is already associated, this association is first removed.

openid_login(OpenID) :-
    openid_hook(login(OpenID)),
    !,
    handle_stay_signed_in(OpenID).
openid_login(OpenID) :-
    openid_logout(_),
    http_session_assert(openid(OpenID)),
    handle_stay_signed_in(OpenID).

%!  openid_logout(+OpenID) is det.
%
%   Remove the association of the current session with any OpenID

openid_logout(OpenID) :-
    openid_hook(logout(OpenID)),
    !.
openid_logout(OpenID) :-
    http_session_retractall(openid(OpenID)).

%!  openid_logged_in(-OpenID) is semidet.
%
%   True if session is associated with OpenID.

openid_logged_in(OpenID) :-
    openid_hook(logged_in(OpenID)),
    !.
openid_logged_in(OpenID) :-
    http_in_session(_SessionId),            % test in session
    http_session_data(openid(OpenID)).


                 /*******************************
                 *            TOPLEVEL          *
                 *******************************/

%!  openid_user(+Request:http_request, -OpenID:url, +Options) is det.
%
%   True if OpenID is a validated OpenID associated with the current
%   session. The scenario for which this predicate is designed is to
%   allow  an  HTTP  handler  that  requires    a   valid  login  to
%   use the transparent code below.
%
%     ==
%     handler(Request) :-
%           openid_user(Request, OpenID, []),
%           ...
%     ==
%
%   If the user is not yet logged on a sequence of redirects will
%   follow:
%
%     1. Show a page for login (default: page /openid/login),
%        predicate reply_openid_login/1)
%     2. By default, the OpenID login page is a form that is
%        submitted to the =verify=, which calls openid_verify/2.
%     3. openid_verify/2 does the following:
%        - Find the OpenID claimed identity and server
%        - Associate to the OpenID server
%        - redirects to the OpenID server for validation
%     4. The OpenID server will redirect here with the authetication
%        information.  This is handled by openid_authenticate/4.
%
%   Options:
%
%     * login_url(Login)
%       (Local) URL of page to enter OpenID information. Default
%       is the handler for openid_login_page/1
%
%   @see openid_authenticate/4 produces errors if login is invalid
%   or cancelled.

:- http_handler(openid(login),        openid_login_page,   [priority(-10)]).
:- http_handler(openid(verify),       openid_verify([]),   []).
:- http_handler(openid(authenticate), openid_authenticate, []).
:- http_handler(openid(xrds),         openid_xrds,         []).

openid_user(_Request, OpenID, _Options) :-
    openid_logged_in(OpenID),
    !.
openid_user(Request, _OpenID, Options) :-
    http_link_to_id(openid_login_page, [], DefLoginPage),
    option(login_url(LoginPage), Options, DefLoginPage),
    openid_current_url(Request, Here),
    redirect_browser(LoginPage,
                     [ 'openid.return_to' = Here
                     ]).

%!  openid_xrds(Request)
%
%   Reply to a request  for   "Discovering  OpenID Relying Parties".
%   This may happen as part of  the provider verification procedure.
%   The  provider  will   do   a    Yadis   discovery   request   on
%   =openid.return=  or  =openid.realm=.  This  is    picked  up  by
%   openid_user/3, pointing the provider to   openid(xrds).  Now, we
%   reply with the locations marked =openid=  and the locations that
%   have actually been doing OpenID validations.

openid_xrds(Request) :-
    http_link_to_id(openid_authenticate, [], Autheticate),
    public_url(Request, Autheticate, Public),
    format('Content-type: text/xml\n\n'),
    format('<?xml version="1.0" encoding="UTF-8"?>\n'),
    format('<xrds:XRDS\n'),
    format('    xmlns:xrds="xri://$xrds"\n'),
    format('    xmlns="xri://$xrd*($v*2.0)">\n'),
    format('  <XRD>\n'),
    format('    <Service>\n'),
    format('      <Type>http://specs.openid.net/auth/2.0/return_to</Type>\n'),
    format('      <URI>~w</URI>\n', [Public]),
    format('    </Service>\n'),
    format('  </XRD>\n'),
    format('</xrds:XRDS>\n').


%!  openid_login_page(+Request) is det.
%
%   Present a login-form for OpenID. There  are two ways to redefine
%   this  default  login  page.  One  is    to  provide  the  option
%   =login_url= to openid_user/3 and the other   is  to define a new
%   handler for =|/openid/login|= using http_handler/3.

openid_login_page(Request) :-
    http_open_session(_, []),
    http_parameters(Request,
                    [ 'openid.return_to'(Target, [])
                    ]),
    reply_html_page([ title('OpenID login')
                    ],
                    [ \openid_login_form(Target, [])
                    ]).

%!  openid_login_form(+ReturnTo, +Options)// is det.
%
%   Create the OpenID  form.  This  exported   as  a  separate  DCG,
%   allowing applications to redefine /openid/login   and reuse this
%   part of the page.  Options processed:
%
%     - action(Action)
%     URL of action to call.  Default is the handler calling
%     openid_verify/1.
%     - buttons(+Buttons)
%     Buttons is a list of =img= structures where the =href=
%     points to an OpenID 2.0 endpoint.  These buttons are
%     displayed below the OpenID URL field.  Clicking the
%     button sets the URL field and submits the form.  Requires
%     Javascript support.
%
%     If the =href= is _relative_, clicking it opens the given
%     location after adding 'openid.return_to' and `stay'.
%     - show_stay(+Boolean)
%     If =true=, show a checkbox that allows the user to stay
%     logged on.

openid_login_form(ReturnTo, Options) -->
    { http_link_to_id(openid_verify, [], VerifyLocation),
      option(action(Action), Options, VerifyLocation),
      http_session_retractall(openid(_)),
      http_session_retractall(openid_login(_,_,_,_)),
      http_session_retractall(ax(_))
    },
    html(div([ class('openid-login')
             ],
             [ \openid_title,
               form([ name(login),
                      id(login),
                      action(Action),
                      method('GET')
                    ],
                    [ \hidden('openid.return_to', ReturnTo),
                      div([ input([ class('openid-input'),
                                    name(openid_url),
                                    id(openid_url),
                                    size(30),
                                    placeholder('Your OpenID URL')
                                  ]),
                            input([ type(submit),
                                    value('Verify!')
                                  ])
                          ]),
                      \buttons(Options),
                      \stay_logged_on(Options)
                    ])
             ])).

stay_logged_on(Options) -->
    { option(show_stay(true), Options) },
    !,
    html(div(class('openid-stay'),
             [ input([ type(checkbox), id(stay), name(stay), value(yes)]),
               'Stay signed in'
             ])).
stay_logged_on(_) --> [].

buttons(Options) -->
    { option(buttons(Buttons), Options),
      Buttons \== []
    },
    html(div(class('openid-buttons'),
             [ 'Sign in with '
             | \prelogin_buttons(Buttons)
             ])).
buttons(_) --> [].

prelogin_buttons([]) --> [].
prelogin_buttons([H|T]) --> prelogin_button(H), prelogin_buttons(T).

%!  prelogin_button(+Image)// is det.
%
%   Handle OpenID 2.0 and other pre-login  buttons. If the image has
%   a =href= attribute that is absolute, it   is  taken as an OpenID
%   2.0 endpoint. Otherwise it is taken  as   a  link on the current
%   server. This allows us to present  non-OpenId logons in the same
%   screen. The dedicated  handler  is  passed  the  HTTP parameters
%   =openid.return_to= and =stay=.

prelogin_button(img(Attrs)) -->
    { select_option(href(HREF), Attrs, RestAttrs),
      uri_is_global(HREF), !
    },
    html(img([ onClick('javascript:{$("#openid_url").val("'+HREF+'");'+
                       '$("form#login").submit();}'
                      )
                 | RestAttrs
             ])).
prelogin_button(img(Attrs)) -->
    { select_option(href(HREF), Attrs, RestAttrs)
    },
    html(img([ onClick('window.location = "'+HREF+
                       '?openid.return_to="'+
                       '+encodeURIComponent($("#return_to").val())'+
                       '+"&stay="'+
                       '+$("#stay").val()')
             | RestAttrs
             ])).


                 /*******************************
                 *          HTTP REPLIES        *
                 *******************************/

%!  openid_verify(+Options, +Request)
%
%   Handle the initial login  form  presented   to  the  user by the
%   relying party (consumer). This predicate   discovers  the OpenID
%   server, associates itself with  this   server  and redirects the
%   user's  browser  to  the  OpenID  server,  providing  the  extra
%   openid.X name-value pairs. Options is,  against the conventions,
%   placed in front of the Request   to allow for smooth cooperation
%   with http_dispatch.pl.  Options processes:
%
%     * return_to(+URL)
%     Specifies where the OpenID provider should return to.
%     Normally, that is the current location.
%     * trust_root(+URL)
%     Specifies the =openid.trust_root= attribute.  Defaults to
%     the root of the current server (i.e., =|http://host[.port]/|=).
%     * realm(+URL)
%     Specifies the =openid.realm= attribute.  Default is the
%     =trust_root=.
%     * ax(+Spec)
%     Request the exchange of additional attributes from the
%     identity provider.  See http_ax_attributes/2 for details.
%
%   The OpenId server will redirect to the =openid.return_to= URL.
%
%   @throws http_reply(moved_temporary(Redirect))

openid_verify(Options, Request) :-
    http_parameters(Request,
                    [ openid_url(URL, [length>1]),
                      'openid.return_to'(ReturnTo0, [optional(true)]),
                      stay(Stay, [optional(true), default(no)])
                    ]),
    (   option(return_to(ReturnTo1), Options)       % Option
    ->  openid_current_url(Request, CurrentLocation),
        global_url(ReturnTo1, CurrentLocation, ReturnTo)
    ;   nonvar(ReturnTo0)
    ->  ReturnTo = ReturnTo0                        % Form-data
    ;   openid_current_url(Request, CurrentLocation),
        ReturnTo = CurrentLocation                  % Current location
    ),
    public_url(Request, /, CurrentRoot),
    option(trust_root(TrustRoot), Options, CurrentRoot),
    option(realm(Realm), Options, TrustRoot),
    openid_resolve(URL, OpenIDLogin, OpenID, Server, ServerOptions),
    trusted(OpenID, Server),
    openid_associate(Server, Handle, _Assoc),
    assert_openid(OpenIDLogin, OpenID, Server, ReturnTo),
    stay(Stay),
    option(ns(NS), Options, 'http://specs.openid.net/auth/2.0'),
    (   realm_attribute(NS, RealmAttribute)
    ->  true
    ;   domain_error('openid.ns', NS)
    ),
    findall(P=V, openid_hook(x_parameter(Server, P, V)), XAttrs, AXAttrs),
    debug(openid(verify), 'XAttrs: ~p', [XAttrs]),
    ax_options(ServerOptions, Options, AXAttrs),
    http_link_to_id(openid_authenticate, [], AuthenticateLoc),
    public_url(Request, AuthenticateLoc, Authenticate),
    redirect_browser(Server, [ 'openid.ns'           = NS,
                               'openid.mode'         = checkid_setup,
                               'openid.identity'     = OpenID,
                               'openid.claimed_id'   = OpenID,
                               'openid.assoc_handle' = Handle,
                               'openid.return_to'    = Authenticate,
                               RealmAttribute        = Realm
                             | XAttrs
                             ]).

realm_attribute('http://specs.openid.net/auth/2.0', 'openid.realm').
realm_attribute('http://openid.net/signon/1.1',     'openid.trust_root').


%!  stay(+Response)
%
%   Called if the user  ask  to  stay   signed  in.  This  is called
%   _before_ control is handed to the   OpenID server. It leaves the
%   data openid_stay_signed_in(true) in the current session.

stay(yes) :-
    !,
    http_session_assert(openid_stay_signed_in(true)).
stay(_).

%!  handle_stay_signed_in(+OpenID)
%
%   Handle stay_signed_in option after the user has logged on

handle_stay_signed_in(OpenID) :-
    http_session_retract(openid_stay_signed_in(true)),
    !,
    http_set_session(timeout(0)),
    ignore(openid_hook(stay_signed_in(OpenID))).
handle_stay_signed_in(_).

%!  assert_openid(+OpenIDLogin, +OpenID, +Server, +Target) is det.
%
%   Associate the OpenID  as  typed  by   the  user,  the  OpenID as
%   validated by the Server with the current HTTP session.
%
%   @param OpenIDLogin Canonized OpenID typed by user
%   @param OpenID OpenID verified by Server.

assert_openid(OpenIDLogin, OpenID, Server, Target) :-
    openid_identifier_select_url(OpenIDLogin),
    openid_identifier_select_url(OpenID),
    !,
    assert_openid_in_session(openid_login(Identity, Identity, Server, Target)).
assert_openid(OpenIDLogin, OpenID, Server, Target) :-
    assert_openid_in_session(openid_login(OpenIDLogin, OpenID, Server, Target)).

assert_openid_in_session(Term) :-
    (   http_in_session(Session)
    ->  debug(openid(verify), 'Assert ~p in ~p', [Term, Session])
    ;   debug(openid(verify), 'No session!', [])
    ),
    http_session_assert(Term).

%!  openid_server(?OpenIDLogin, ?OpenID, ?Server) is nondet.
%
%   True if OpenIDLogin is the typed id for OpenID verified by
%   Server.
%
%   @param OpenIDLogin ID as typed by user (canonized)
%   @param OpenID ID as verified by server
%   @param Server URL of the OpenID server

openid_server(OpenIDLogin, OpenID, Server) :-
    openid_server(OpenIDLogin, OpenID, Server, _Target).

openid_server(OpenIDLogin, OpenID, Server, Target) :-
    http_in_session(Session),
    (   http_session_data(openid_login(OpenIDLogin, OpenID, Server, Target))
    ->  true
    ;   http_session_data(openid_login(OpenIDLogin1, OpenID1, Server1, Target1)),
        debug(openid(verify), '~p \\== ~p',
              [ openid_login(OpenIDLogin, OpenID, Server, Target),
                openid_login(OpenIDLogin1, OpenID1, Server1, Target1)
              ]),
        fail
    ;   debug(openid(verify), 'No openid_login/4 term in session ~p', [Session]),
        fail
    ).


%!  public_url(+Request, +Path, -URL) is det.
%
%   True when URL is a publically useable  URL that leads to Path on
%   the current server.

public_url(Request, Path, URL) :-
    openid_current_host(Request, Host, Port),
    setting(http:public_scheme, Scheme),
    set_port(Scheme, Port, AuthC),
    uri_authority_data(host, AuthC, Host),
    uri_authority_components(Auth, AuthC),
    uri_data(scheme, Components, Scheme),
    uri_data(authority, Components, Auth),
    uri_data(path, Components, Path),
    uri_components(URL, Components).

set_port(Scheme, Port, _) :-
    scheme_port(Scheme, Port),
    !.
set_port(_, Port, AuthC) :-
    uri_authority_data(port, AuthC, Port).

scheme_port(http, 80).
scheme_port(https, 443).


%!  openid_current_url(+Request, -URL) is det.
%
%   Find the public URL for Request that   we  can make available to our
%   identity provider. This must be an  absolute   URL  where  we can be
%   contacted.   Before   trying   a     configured    version   through
%   http_public_url/2, we try to see wether the login message contains a
%   referer parameter or wether the browser provided one.

openid_current_url(Request, URL) :-
    option(request_uri(URI), Request),
    uri_components(URI, Components),
    uri_data(path, Components, Path),
    (   uri_data(search, Components, QueryString),
        nonvar(QueryString),
        uri_query_components(QueryString, Query),
        memberchk(referer=Base, Query)
    ->  true
    ;   option(referer(Base), Request)
    ), !,
    uri_normalized(Path, Base, URL).
openid_current_url(Request, URL) :-
    http_public_url(Request, URL).

%!  openid_current_host(Request, Host, Port)
%
%   Find current location of the server.
%
%   @deprecated     New code should use http_current_host/4 with the
%                   option global(true).

openid_current_host(Request, Host, Port) :-
    http_current_host(Request, Host, Port,
                      [ global(true)
                      ]).


%!  redirect_browser(+URL, +FormExtra)
%
%   Generate a 302 temporary redirect to  URL, adding the extra form
%   information from FormExtra. The specs says   we  must retain the
%   search specification already attached to the URL.

redirect_browser(URL, FormExtra) :-
    uri_components(URL, C0),
    uri_data(search, C0, Search0),
    (   var(Search0)
    ->  uri_query_components(Search, FormExtra)
    ;   uri_query_components(Search0, Form0),
        append(FormExtra, Form0, Form),
        uri_query_components(Search, Form)
    ),
    uri_data(search, C0, Search, C),
    uri_components(Redirect, C),
    throw(http_reply(moved_temporary(Redirect))).


                 /*******************************
                 *             RESOLVE          *
                 *******************************/

%!  openid_resolve(+URL, -OpenIDOrig, -OpenID, -Server, -ServerOptions)
%
%   True if OpenID is the claimed  OpenID   that  belongs to URL and
%   Server is the URL of the  OpenID   server  that  can be asked to
%   verify this claim.
%
%   @param  URL The OpenID typed by the user
%   @param  OpenIDOrig Canonized OpenID typed by user
%   @param  OpenID Possibly delegated OpenID
%   @param  Server OpenID server that must validate OpenID
%   @param  ServerOptions provides additional XRDS information about
%           the server.  Currently supports xrds_types(Types).
%   @tbd    Implement complete URL canonization as defined by the
%           OpenID 2.0 proposal.

openid_resolve(URL, OpenID, OpenID, Server, [xrds_types(Types)]) :-
    xrds_dom(URL, DOM),
    xpath(DOM, //(_:'Service'), Service),
    findall(Type, xpath(Service, _:'Type'(text), Type), Types),
    memberchk('http://specs.openid.net/auth/2.0/server', Types),
    xpath(Service, _:'URI'(text), Server),
    !,
    debug(openid(yadis), 'Yadis: server: ~q, types: ~q', [Server, Types]),
    (   xpath(Service, _:'LocalID'(text), OpenID)
    ->  true
    ;   openid_identifier_select_url(OpenID)
    ).
openid_resolve(URL, OpenID0, OpenID, Server, []) :-
    debug(openid(resolve), 'Opening ~w ...', [URL]),
    dtd(html, DTD),
    setup_call_cleanup(
        http_open(URL, Stream,
                  [ final_url(OpenID0),
                    cert_verify_hook(ssl_verify)
                  ]),
        load_structure(Stream, Term,
                       [ dtd(DTD),
                         dialect(sgml),
                         shorttag(false),
                         syntax_errors(quiet)
                       ]),
        close(Stream)),
    debug(openid(resolve), 'Scanning HTML document ...', []),
    contains_term(element(head, _, Head), Term),
    (   link(Head, 'openid.server', Server)
    ->  debug(openid(resolve), 'OpenID Server=~q', [Server])
    ;   debug(openid(resolve), 'No server in ~q', [Head]),
        fail
    ),
    (   link(Head, 'openid.delegate', OpenID)
    ->  debug(openid(resolve), 'OpenID = ~q (delegated)', [OpenID])
    ;   OpenID = OpenID0,
        debug(openid(resolve), 'OpenID = ~q', [OpenID])
    ).

openid_identifier_select_url(
    'http://specs.openid.net/auth/2.0/identifier_select').

:- public ssl_verify/5.

%!  ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%   Accept all certificates. We do not care  too much. Only the user
%   cares s/he is not entering her  credentials with a spoofed side.
%   As we redirect, the browser will take care of this.

ssl_verify(_SSL,
           _ProblemCertificate, _AllCertificates, _FirstCertificate,
           _Error).


link(DOM, Type, Target) :-
    sub_term(element(link, Attrs, []), DOM),
    memberchk(rel=Type, Attrs),
    memberchk(href=Target, Attrs).


                 /*******************************
                 *         AUTHENTICATE         *
                 *******************************/

%!  openid_authenticate(+Request)
%
%   HTTP handler when redirected back from the OpenID provider.

openid_authenticate(Request) :-
    memberchk(accept(Accept), Request),
    Accept = [media(application/'xrds+xml',_,_,_)],
    !,
    http_link_to_id(openid_xrds, [], XRDSLocation),
    http_absolute_uri(XRDSLocation, XRDSServer),
    debug(openid(yadis), 'Sending XRDS server: ~q', [XRDSServer]),
    format('X-XRDS-Location: ~w\n', [XRDSServer]),
    format('Content-type: text/plain\n\n').
openid_authenticate(Request) :-
    openid_authenticate(Request, _OpenIdServer, OpenID, _ReturnTo),
    openid_server(User, OpenID, _, Target),
    openid_login(User),
    redirect_browser(Target, []).


%!  openid_authenticate(+Request, -Server:url, -OpenID:url,
%!                      -ReturnTo:url) is semidet.
%
%   Succeeds if Request comes from the   OpenID  server and confirms
%   that User is a verified OpenID   user. ReturnTo provides the URL
%   to return to.
%
%   After openid_verify/2 has redirected the   browser to the OpenID
%   server, and the OpenID server did   its  magic, it redirects the
%   browser back to this address.  The   work  is fairly trivial. If
%   =mode= is =cancel=, the OpenId server   denied. If =id_res=, the
%   OpenId server replied positive, but  we   must  verify  what the
%   server told us by checking the HMAC-SHA signature.
%
%   This call fails silently if their is no =|openid.mode|= field in
%   the request.
%
%   @throws openid(cancel)
%           if request was cancelled by the OpenId server
%   @throws openid(signature_mismatch)
%           if the HMAC signature check failed

openid_authenticate(Request, OpenIdServer, Identity, ReturnTo) :-
    memberchk(method(get), Request),
    http_parameters(Request,
                    [ 'openid.mode'(Mode, [optional(true)])
                    ]),
    (   var(Mode)
    ->  fail
    ;   Mode == cancel
    ->  throw(openid(cancel))
    ;   Mode == id_res
    ->  debug(openid(authenticate), 'Mode=id_res, validating response', []),
        http_parameters(Request,
                        [ 'openid.identity'(Identity, []),
                          'openid.assoc_handle'(Handle, []),
                          'openid.return_to'(ReturnTo, []),
                          'openid.signed'(AtomFields, []),
                          'openid.sig'(Base64Signature, []),
                          'openid.invalidate_handle'(Invalidate,
                                                     [optional(true)])
                        ],
                        [ form_data(Form)
                        ]),
        atomic_list_concat(SignedFields, ',', AtomFields),
        check_obligatory_fields(SignedFields),
        signed_pairs(SignedFields,
                     [ mode-Mode,
                       identity-Identity,
                       assoc_handle-Handle,
                       return_to-ReturnTo,
                       invalidate_handle-Invalidate
                     ],
                     Form,
                     SignedPairs),
        (   openid_associate(OpenIdServer, Handle, Assoc)
        ->  signature(SignedPairs, Assoc, Sig),
            atom_codes(Base64Signature, Base64SigCodes),
            phrase(base64(Signature), Base64SigCodes),
            (   Sig == Signature
            ->  true
            ;   throw(openid(signature_mismatch))
            )
        ;   check_authentication(Request, Form)
        ),
        ax_store(Form)
    ).

%!  signed_pairs(+FieldNames, +Pairs:list(Field-Value),
%!               +Form, -SignedPairs) is det.
%
%   Extract the signed field in the order they appear in FieldNames.

signed_pairs([], _, _, []).
signed_pairs([Field|T0], Pairs, Form, [Field-Value|T]) :-
    memberchk(Field-Value, Pairs),
    !,
    signed_pairs(T0, Pairs, Form, T).
signed_pairs([Field|T0], Pairs, Form, [Field-Value|T]) :-
    atom_concat('openid.', Field, OpenIdField),
    memberchk(OpenIdField=Value, Form),
    !,
    signed_pairs(T0, Pairs, Form, T).
signed_pairs([Field|T0], Pairs, Form, T) :-
    format(user_error, 'Form = ~p~n', [Form]),
    throw(error(existence_error(field, Field),
                context(_, 'OpenID Signed field is not present'))),
    signed_pairs(T0, Pairs, Form, T).


%!  check_obligatory_fields(+SignedFields:list) is det.
%
%   Verify fields from obligatory_field/1 are   in  the signed field
%   list.
%
%   @error  existence_error(field, Field)

check_obligatory_fields(Fields) :-
    (   obligatory_field(Field),
        (   memberchk(Field, Fields)
        ->  true
        ;   throw(error(existence_error(field, Field),
                        context(_, 'OpenID field is not in signed fields')))
        ),
        fail
    ;   true
    ).

obligatory_field(identity).


%!  check_authentication(+Request, +Form) is semidet.
%
%   Implement the stateless verification method.   This seems needed
%   for stackexchange.com, which provides the   =res_id=  with a new
%   association handle.

check_authentication(_Request, Form) :-
    openid_server(_OpenIDLogin, _OpenID, Server),
    debug(openid(check_authentication),
          'Using stateless verification with ~q form~n~q', [Server, Form]),
    select('openid.mode' = _, Form, Form1),
    setup_call_cleanup(
        http_open(Server, In,
                  [ post(form([ 'openid.mode' = check_authentication
                              | Form1
                              ])),
                    cert_verify_hook(ssl_verify)
                  ]),
        read_stream_to_codes(In, Reply),
        close(In)),
    debug(openid(check_authentication),
          'Reply: ~n~s~n', [Reply]),
    key_values_data(Pairs, Reply),
    forall(member(invalidate_handle-Handle, Pairs),
           retractall(association(_, Handle, _))),
    memberchk(is_valid-true, Pairs).


                 /*******************************
                 *          AX HANDLING         *
                 *******************************/

%!  ax_options(+ServerOptions, +Options, +AXAttrs) is det.
%
%   True when AXAttrs is a  list   of  additional attribute exchange
%   options to add to the OpenID redirect request.

ax_options(ServerOptions, Options, AXAttrs) :-
    option(ax(Spec), Options),
    option(xrds_types(Types), ServerOptions),
    memberchk('http://openid.net/srv/ax/1.0', Types),
    !,
    http_ax_attributes(Spec, AXAttrs),
    debug(openid(ax), 'AX attributes: ~q', [AXAttrs]).
ax_options(_, _, []) :-
    debug(openid(ax), 'AX: not supported', []).

%!  ax_store(+Form)
%
%   Extract reported AX data and  store   this  into the session. If
%   there is a non-empty list of exchanged values, this calls
%
%       openid_hook(ax(Values))
%
%   If this hook fails, Values are added   to the session data using
%   http_session_assert(ax(Values)).

ax_store(Form) :-
    debug(openid(ax), 'Form: ~q', [Form]),
    ax_form_attributes(Form, Values),
    debug(openid(ax), 'AX: ~q', [Values]),
    (   Values \== []
    ->  (   openid_hook(ax(Values))
        ->  true
        ;   http_session_assert(ax(Values))
        )
    ;   true
    ).


                 /*******************************
                 *         OPENID SERVER        *
                 *******************************/

:- dynamic
    server_association/3.           % URL, Handle, Term

%!  openid_server(+Options, +Request)
%
%   Realise the OpenID server. The protocol   demands a POST request
%   here.

openid_server(Options, Request) :-
    http_parameters(Request,
                    [ 'openid.mode'(Mode)
                    ],
                    [ attribute_declarations(openid_attribute),
                      form_data(Form)
                    ]),
    (   Mode == associate
    ->  associate_server(Request, Form, Options)
    ;   Mode == checkid_setup
    ->  checkid_setup_server(Request, Form, Options)
    ).

%!  associate_server(+Request, +Form, +Options)
%
%   Handle the association-request. If successful,   create a clause
%   for server_association/3 to record the current association.

associate_server(Request, Form, Options) :-
    memberchk('openid.assoc_type'         = AssocType,   Form),
    memberchk('openid.session_type'       = SessionType, Form),
    memberchk('openid.dh_modulus'         = P64,         Form),
    memberchk('openid.dh_gen'             = G64,         Form),
    memberchk('openid.dh_consumer_public' = CPX64,       Form),
    base64_btwoc(P, P64),
    base64_btwoc(G, G64),
    base64_btwoc(CPX, CPX64),
    Y is 1+random(P-1),             % Our secret
    DiffieHellman is powm(CPX, Y, P),
    btwoc(DiffieHellman, DHBytes),
    signature_algorithm(SessionType, SHA_Algo),
    sha_hash(DHBytes, SHA1, [encoding(octet), algorithm(SHA_Algo)]),
    CPY is powm(G, Y, P),
    base64_btwoc(CPY, CPY64),
    mackey_bytes(SessionType, MacBytes),
    new_assoc_handle(MacBytes, Handle),
    random_bytes(MacBytes, MacKey),
    xor_codes(MacKey, SHA1, EncKey),
    phrase(base64(EncKey), Base64EncKey),
    DefExpriresIn is 24*3600,
    option(expires_in(ExpriresIn), Options, DefExpriresIn),

    get_time(Now),
    ExpiresAt is integer(Now+ExpriresIn),
    make_association([ session_type(SessionType),
                       expires_at(ExpiresAt),
                       mac_key(MacKey)
                     ],
                     Record),
    memberchk(peer(Peer), Request),
    assert(server_association(Peer, Handle, Record)),

    key_values_data([ assoc_type-AssocType,
                      assoc_handle-Handle,
                      expires_in-ExpriresIn,
                      session_type-SessionType,
                      dh_server_public-CPY64,
                      enc_mac_key-Base64EncKey
                    ],
                    Text),
    format('Content-type: text/plain~n~n~s', [Text]).

mackey_bytes('DH-SHA1',   20).
mackey_bytes('DH-SHA256', 32).

new_assoc_handle(Length, Handle) :-
    random_bytes(Length, Bytes),
    phrase(base64(Bytes), HandleCodes),
    atom_codes(Handle, HandleCodes).


%!  checkid_setup_server(+Request, +Form, +Options)
%
%   Validate an OpenID for a TrustRoot and redirect the browser back
%   to the ReturnTo argument.  There   are  many  possible scenarios
%   here:
%
%           1. Check some cookie and if present, grant immediately
%           2. Use a 401 challenge page
%           3. Present a normal grant/password page
%           4. As (3), but use HTTPS for the exchange
%           5. etc.
%
%   First thing to check is the immediate acknowledgement.

checkid_setup_server(_Request, Form, _Options) :-
    memberchk('openid.identity'       = Identity,  Form),
    memberchk('openid.assoc_handle'   = Handle,    Form),
    memberchk('openid.return_to'      = ReturnTo,  Form),
    (   memberchk('openid.realm'      = Realm,     Form) -> true
    ;   memberchk('openid.trust_root' = Realm, Form)
    ),

    server_association(_, Handle, _Association),            % check

    reply_html_page(
        [ title('OpenID login')
        ],
        [ \openid_title,
          div(class('openid-message'),
              ['Site ', a(href(TrustRoot), TrustRoot),
               ' requests permission to login with OpenID ',
               a(href(Identity), Identity), '.'
              ]),
          table(class('openid-form'),
                [ tr(td(form([ action(grant), method('GET') ],
                             [ \hidden('openid.grant', yes),
                               \hidden('openid.identity', Identity),
                               \hidden('openid.assoc_handle', Handle),
                               \hidden('openid.return_to', ReturnTo),
                               \hidden('openid.realm', Realm),
                               \hidden('openid.trust_root', Realm),
                               div(['Password: ',
                                    input([ type(password),
                                            name('openid.password')
                                          ]),
                                    input([ type(submit),
                                            value('Grant')
                                          ])
                                   ])
                             ]))),
                  tr(td(align(right),
                        form([ action(grant), method('GET') ],
                             [ \hidden('openid.grant', no),
                               \hidden('openid.return_to', ReturnTo),
                               input([type(submit), value('Deny')])
                             ])))
                ])
        ]).

hidden(Name, Value) -->
    html(input([type(hidden), id(return_to), name(Name), value(Value)])).


openid_title -->
    { http_absolute_location(icons('openid-logo-square.png'), SRC, []) },
    html_requires(css('openid.css')),
    html(div(class('openid-title'),
             [ a(href('http://openid.net/'),
                 img([ src(SRC), alt('OpenID') ])),
               span('Login')
             ])).


%!  openid_grant(+Request)
%
%   Handle the reply from checkid_setup_server/3.   If  the reply is
%   =yes=, check the authority (typically the   password) and if all
%   looks good redirect the browser to   ReturnTo, adding the OpenID
%   properties needed by the Relying Party to verify the login.

openid_grant(Request) :-
    http_parameters(Request,
                    [ 'openid.grant'(Grant),
                      'openid.return_to'(ReturnTo)
                    ],
                    [ attribute_declarations(openid_attribute)
                    ]),
    (   Grant == yes
    ->  http_parameters(Request,
                        [ 'openid.identity'(Identity),
                          'openid.assoc_handle'(Handle),
                          'openid.trust_root'(TrustRoot),
                          'openid.password'(Password)
                        ],
                        [ attribute_declarations(openid_attribute)
                        ]),
        server_association(_, Handle, Association),
        grant_login(Request,
                    [ identity(Identity),
                      password(Password),
                      trustroot(TrustRoot)
                    ]),
        SignedPairs = [ 'mode'-id_res,
                        'identity'-Identity,
                        'assoc_handle'-Handle,
                        'return_to'-ReturnTo
                      ],
        signed_fields(SignedPairs, Signed),
        signature(SignedPairs, Association, Signature),
        phrase(base64(Signature), Bas64SigCodes),
        string_codes(Bas64Sig, Bas64SigCodes),
        redirect_browser(ReturnTo,
                         [ 'openid.mode' = id_res,
                           'openid.identity' = Identity,
                           'openid.assoc_handle' = Handle,
                           'openid.return_to' = ReturnTo,
                           'openid.signed' = Signed,
                           'openid.sig' = Bas64Sig
                         ])
    ;   redirect_browser(ReturnTo,
                         [ 'openid.mode' = cancel
                         ])
    ).


%!  grant_login(+Request, +Options) is det.
%
%   Validate login from Request (can  be   used  to get cookies) and
%   Options, which contains at least:
%
%           * identity(Identity)
%           * password(Password)
%           * trustroot(TrustRoot)

grant_login(Request, Options) :-
    openid_hook(grant(Request, Options)).

%!  trusted(+OpenID, +Server)
%
%   True if we  trust  the  given   OpenID  server.  Must  throw  an
%   exception, possibly redirecting to a   page with trusted servers
%   if the given server is not trusted.

trusted(OpenID, Server) :-
    openid_hook(trusted(OpenID, Server)).


%!  signed_fields(+Pairs, -Signed) is det.
%
%   Create a comma-separated  atom  from   the  field-names  without
%   'openid.' from Pairs.

signed_fields(Pairs, Signed) :-
    signed_field_names(Pairs, Names),
    atomic_list_concat(Names, ',', Signed).

signed_field_names([], []).
signed_field_names([H0-_|T0], [H|T]) :-
    (   atom_concat('openid.', H, H0)
    ->  true
    ;   H = H0
    ),
    signed_field_names(T0, T).

%!  signature(+Pairs, +Association, -Signature)
%
%   Determine the signature for Pairs

signature(Pairs, Association, Signature) :-
    key_values_data(Pairs, TokenContents),
    association_mac_key(Association, MacKey),
    association_session_type(Association, SessionType),
    signature_algorithm(SessionType, SHA),
    hmac_sha(MacKey, TokenContents, Signature, [algorithm(SHA)]),
    debug(openid(crypt),
          'Signed:~n~s~nSignature: ~w', [TokenContents, Signature]).

signature_algorithm('DH-SHA1',   sha1).
signature_algorithm('DH-SHA256', sha256).


                 /*******************************
                 *            ASSOCIATE         *
                 *******************************/

:- dynamic
    association/3.                  % URL, Handle, Data

:- record
    association(session_type='DH-SHA1',
                expires_at,         % time-stamp
                mac_key).           % code-list

%!  openid_associate(?URL, ?Handle, ?Assoc) is det.
%
%   Calls openid_associate/4 as
%
%       ==
%       openid_associate(URL, Handle, Assoc, []).
%       ==

openid_associate(URL, Handle, Assoc) :-
    openid_associate(URL, Handle, Assoc, []).

%!  openid_associate(+URL, -Handle, -Assoc, +Options) is det.
%!  openid_associate(?URL, +Handle, -Assoc, +Options) is semidet.
%
%   Associate with an open-id server.  We   first  check for a still
%   valid old association. If there is  none   or  it is expired, we
%   esstablish one and remember it.  Options:
%
%     * ns(URL)
%     One of =http://specs.openid.net/auth/2.0= (default) or
%     =http://openid.net/signon/1.1=.
%
%   @tbd    Should we store known associations permanently?  Where?

openid_associate(URL, Handle, Assoc, _Options) :-
    nonvar(Handle),
    !,
    debug(openid(associate),
          'OpenID: Lookup association with handle ~q', [Handle]),
    (   association(URL, Handle, Assoc)
    ->  true
    ;   debug(openid(associate),
              'OpenID: no association with handle ~q', [Handle]),
        fail
    ).
openid_associate(URL, Handle, Assoc, _Options) :-
    must_be(atom, URL),
    association(URL, Handle, Assoc),
    association_expires_at(Assoc, Expires),
    get_time(Now),
    (   Now < Expires
    ->  !,
        debug(openid(associate),
              'OpenID: Reusing association with ~q', [URL])
    ;   retractall(association(URL, Handle, _)),
        fail
    ).
openid_associate(URL, Handle, Assoc, Options) :-
    associate_data(Data, P, _G, X, Options),
    debug(openid(associate), 'OpenID: Associating with ~q', [URL]),
    setup_call_cleanup(
        http_open(URL, In,
                  [ post(form(Data)),
                    cert_verify_hook(ssl_verify)
                  ]),
        read_stream_to_codes(In, Reply),
        close(In)),
    debug(openid(associate), 'Reply: ~n~s', [Reply]),
    key_values_data(Pairs, Reply),
    shared_secret(Pairs, P, X, MacKey),
    expires_at(Pairs, ExpiresAt),
    memberchk(assoc_handle-Handle, Pairs),
    memberchk(session_type-Type, Pairs),
    make_association([ session_type(Type),
                       expires_at(ExpiresAt),
                       mac_key(MacKey)
                     ], Assoc),
    assert(association(URL, Handle, Assoc)).


%!  shared_secret(+Pairs, +P, +X, -Secret:list(codes))
%
%   Find the shared secret from the peer's reply and our data. First
%   clause deals with the (deprecated) non-encoded version.

shared_secret(Pairs, _, _, Secret) :-
    memberchk(mac_key-Base64, Pairs),
    !,
    atom_codes(Base64, Base64Codes),
    phrase(base64(Base64Codes), Secret).
shared_secret(Pairs, P, X, Secret) :-
    memberchk(dh_server_public-Base64Public, Pairs),
    memberchk(enc_mac_key-Base64EncMacKey, Pairs),
    memberchk(session_type-SessionType, Pairs),
    base64_btwoc(ServerPublic, Base64Public),
    DiffieHellman is powm(ServerPublic, X, P),
    atom_codes(Base64EncMacKey, Base64EncMacKeyCodes),
    phrase(base64(EncMacKey), Base64EncMacKeyCodes),
    btwoc(DiffieHellman, DiffieHellmanBytes),
    signature_algorithm(SessionType, SHA_Algo),
    sha_hash(DiffieHellmanBytes, DHHash,
             [encoding(octet), algorithm(SHA_Algo)]),
    xor_codes(DHHash, EncMacKey, Secret).


%!  expires_at(+Pairs, -Time) is det.
%
%   Unify Time with  a  time-stamp   stating  when  the  association
%   exires.

expires_at(Pairs, Time) :-
    memberchk(expires_in-ExpAtom, Pairs),
    atom_number(ExpAtom, Seconds),
    get_time(Now),
    Time is integer(Now)+Seconds.


%!  associate_data(-Data, -P, -G, -X, +Options) is det.
%
%   Generate the data to initiate an association using Diffie-Hellman
%   shared secret key negotiation.

associate_data(Data, P, G, X, Options) :-
    openid_dh_p(P),
    openid_dh_g(G),
    X is 1+random(P-1),                     % 1<=X<P-1
    CP is powm(G, X, P),
    base64_btwoc(P, P64),
    base64_btwoc(G, G64),
    base64_btwoc(CP, CP64),
    option(ns(NS), Options, 'http://specs.openid.net/auth/2.0'),
    (   assoc_type(NS, DefAssocType, DefSessionType)
    ->  true
    ;   domain_error('openid.ns', NS)
    ),
    option(assoc_type(AssocType), Options, DefAssocType),
    option(assoc_type(SessionType), Options, DefSessionType),
    Data = [ 'openid.ns'                 = NS,
             'openid.mode'               = associate,
             'openid.assoc_type'         = AssocType,
             'openid.session_type'       = SessionType,
             'openid.dh_modulus'         = P64,
             'openid.dh_gen'             = G64,
             'openid.dh_consumer_public' = CP64
           ].

assoc_type('http://specs.openid.net/auth/2.0',
           'HMAC-SHA256',
           'DH-SHA256').
assoc_type('http://openid.net/signon/1.1',
           'HMAC-SHA1',
           'DH-SHA1').


                 /*******************************
                 *            RANDOM            *
                 *******************************/

%!  random_bytes(+N, -Bytes) is det.
%
%   Bytes is a list of N random bytes (integers 0..255).

random_bytes(N, [H|T]) :-
    N > 0,
    !,
    H is random(256),
    N2 is N - 1,
    random_bytes(N2, T).
random_bytes(_, []).


                 /*******************************
                 *           CONSTANTS          *
                 *******************************/

openid_dh_p(155172898181473697471232257763715539915724801966915404479707795314057629378541917580651227423698188993727816152646631438561595825688188889951272158842675419950341258706556549803580104870537681476726513255747040765857479291291572334510643245094715007229621094194349783925984760375594985848253359305585439638443).

openid_dh_g(2).


                 /*******************************
                 *             UTIL             *
                 *******************************/

%!  key_values_data(+KeyValues:list(Key-Value), -Data:list(code)) is det.
%!  key_values_data(-KeyValues:list(Key-Value), +Data:list(code)) is det.
%
%   Encoding  and  decoding  of  key-value  pairs  for  OpenID  POST
%   messages  according  to   Appendix   C    of   the   OpenID  1.1
%   specification.

key_values_data(Pairs, Data) :-
    nonvar(Data),
    !,
    phrase(data_form(Pairs), Data).
key_values_data(Pairs, Data) :-
    phrase(gen_data_form(Pairs), Data).

data_form([Key-Value|Pairs]) -->
    utf8_string(KeyCodes), ":", utf8_string(ValueCodes), "\n",
    !,
    { atom_codes(Key, KeyCodes),
      atom_codes(Value, ValueCodes)
    },
    data_form(Pairs).
data_form([]) -->
    ws.

%!  utf8_string(-Codes)// is nondet.
%
%   Take a short UTF-8 code-list from input. Extend on backtracking.

utf8_string([]) -->
    [].
utf8_string([H|T]) -->
    utf8_codes([H]),
    utf8_string(T).

ws -->
    [C],
    { C =< 32 },
    !,
    ws.
ws -->
    [].


gen_data_form([]) -->
    [].
gen_data_form([Key-Value|T]) -->
    field(Key), ":", field(Value), "\n",
    gen_data_form(T).

field(Field) -->
    { to_codes(Field, Codes)
    },
    utf8_codes(Codes).

to_codes(Codes, Codes) :-
    is_list(Codes),
    !.
to_codes(Atomic, Codes) :-
    atom_codes(Atomic, Codes).

%!  base64_btwoc(+Int, -Base64:list(code)) is det.
%!  base64_btwoc(-Int, +Base64:list(code)) is det.
%!  base64_btwoc(-Int, +Base64:atom) is det.

base64_btwoc(Int, Base64) :-
    integer(Int),
    !,
    btwoc(Int, Bytes),
    phrase(base64(Bytes), Base64).
base64_btwoc(Int, Base64) :-
    atom(Base64),
    !,
    atom_codes(Base64, Codes),
    phrase(base64(Bytes), Codes),
    btwoc(Int, Bytes).
base64_btwoc(Int, Base64) :-
    phrase(base64(Bytes), Base64),
    btwoc(Int, Bytes).


%!  btwoc(+Integer, -Bytes) is det.
%!  btwoc(-Integer, +Bytes) is det.
%
%   Translate between a big integer and and its representation in
%   bytes.  The first bit is always 0, as Integer is nonneg.

btwoc(Int, Bytes) :-
    integer(Int),
    !,
    int_to_bytes(Int, Bytes).
btwoc(Int, Bytes) :-
    is_list(Bytes),
    bytes_to_int(Bytes, Int).

int_to_bytes(Int, Bytes) :-
    int_to_bytes(Int, [], Bytes).

int_to_bytes(Int, Bytes0, [Int|Bytes0]) :-
    Int < 128,
    !.
int_to_bytes(Int, Bytes0, Bytes) :-
    Last is Int /\ 0xff,
    Int1 is Int >> 8,
    int_to_bytes(Int1, [Last|Bytes0], Bytes).


bytes_to_int([B|T], Int) :-
    bytes_to_int(T, B, Int).

bytes_to_int([], Int, Int).
bytes_to_int([B|T], Int0, Int) :-
    Int1 is (Int0<<8)+B,
    bytes_to_int(T, Int1, Int).


%!  xor_codes(+C1:list(int), +C2:list(int), -XOR:list(int)) is det.
%
%   Compute xor of two strings.
%
%   @error  length_mismatch(L1, L2) if the two lists do not have equal
%           length.

xor_codes([], [], []) :- !.
xor_codes([H1|T1], [H2|T2], [H|T]) :-
    !,
    H is H1 xor H2,
    !,
    xor_codes(T1, T2, T).
xor_codes(L1, L2, _) :-
    throw(error(length_mismatch(L1, L2), _)).


                 /*******************************
                 *        HTTP ATTRIBUTES       *
                 *******************************/

openid_attribute('openid.mode',
                 [ oneof([ associate,
                           checkid_setup,
                           cancel,
                           id_res
                         ])
                 ]).
openid_attribute('openid.assoc_type',
                 [ oneof(['HMAC-SHA1'])
                 ]).
openid_attribute('openid.session_type',
                 [ oneof([ 'DH-SHA1',
                           'DH-SHA256'
                         ])
                 ]).
openid_attribute('openid.dh_modulus',         [length > 1]).
openid_attribute('openid.dh_gen',             [length > 1]).
openid_attribute('openid.dh_consumer_public', [length > 1]).
openid_attribute('openid.assoc_handle',       [length > 1]).
openid_attribute('openid.return_to',          [length > 1]).
openid_attribute('openid.trust_root',         [length > 1]).
openid_attribute('openid.identity',           [length > 1]).
openid_attribute('openid.password',           [length > 1]).
openid_attribute('openid.grant',              [oneof([yes,no])]).
