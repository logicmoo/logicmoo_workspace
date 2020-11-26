:- module(http_login_demo,
          [ server/1                            % +Port
          ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/html_write)).

/** <module> Login demo

This module provides  a  demonstration  on   how  to  use  HTTP  request
filtering to realise login using an login page and cookies.
*/

%!  server(+Port)
%
%   Start the server at http://localhost:Port

server(Port) :-
    http_set_session_options(
        [ create(noauto)
        ]),
    http_server(http_dispatch,
                [ port(Port)
                ]).

% handle declarations. All pretty standard, except  the secret page that
% is protected using role(trusted). This  is   used  by the RBAC request
% rewrite rule.

:- http_handler(root(.),        http_redirect(moved, location_by_id(home)),
                []).
:- http_handler(root(home),     home,
                []).
:- http_handler(root(secret),   secret,
                [role(trusted)]).
:- http_handler(root(login),    login,
                []).
:- http_handler(root(do_login), do_login,
                [ method(post) ]).
:- http_handler(root(logout),   logout,
                []).

%!  home(+Request)
%
%   Send the home page.  This  uses   the  user(User)  attribute  of the
%   request added by the `user` request rewrite   rule to make some page
%   elements depending on the login state.

home(Request) :-
    reply_html_page(
        title('Login demo'),
        [ h1('Login demo'),
          \logged_in(Request),
          ul([ \login_or_logout(Request),
               li(a(href(location_by_id(secret)), 'Secret page'))
             ])
        ]).

logged_in(Request) -->
    { memberchk(user(User), Request) },
    !,
    html(p('You are logged in as ~p'-[User])).
logged_in(_Request) -->
    html(p('You are not logged in')).

login_or_logout(Request) -->
    { memberchk(user(_User), Request) },
    !,
    html(li(a(href(location_by_id(logout)), 'Logout'))).
login_or_logout(_Request) -->
    html(li(a(href(location_by_id(login)),  'Login'))).

%!  secret(+Request)
%
%   The protected page.  The handler is not special.

secret(_Request) :-
    reply_html_page(
        title('Secret page'),
        [ h1('Psss ...'),
          p('Prolog is a secret weapon!')
        ]).

%!  login(+Request)
%
%   The login page. This is a completely   trivial page providing a form
%   to enter name and password. It takes two optional parameters:
%
%     - reason(String)
%       The reason why the login page was opened.  Deals with
%       reopening due to wrong password and opening due to trying
%       to access a protected page.
%     - return_to(URL)
%       After successfull login, return to the given page.  Default
%       is the referer.  This is a little dubious as this may not
%       be present.  In that case it might be better to return to
%       a dedicated welcome page.

login(Request) :-
    http_parameters(
        Request,
        [ reason(Message, [optional(true)]),
          return_to(Here, [optional(true)])
        ]),
    (   var(Here)
    ->  memberchk(referer(Here), Request)
    ;   true
    ),
    reply_html_page(
        title('Login'),
        [ \login_title(Message),
          form([ action(location_by_id(do_login)),
                 method(post)
               ],
               [ input([ type(hidden), name(return_to), value(Here) ]),
                 table([ tr([ th('Name'),
                              td(input([name(name)]))
                            ]),
                         tr([ th('Password'),
                              td(input([name(passwd), type(password)]))
                            ]),
                         tr([ td([ colspan(2),
                                   align(right)
                                 ],
                                 input([type(submit), value('Login')]))
                            ])
                       ])
               ])
        ]).

login_title(Message) -->
    { var(Message) },
    !,
    html(h1('Login')).
login_title(Message) -->
    html([ h1('Login'),
           p(class(message), Message)
         ]).

%!  do_login(+Request)
%
%   Handle the form data from the _login page_.  Does one of these:
%
%     - If login is ok, open a session, assert the user is logged on and
%       redirect to `return_to`
%     - Otherwise, go back to the login page, indicating the login
%       failed.

do_login(Request) :-
    http_parameters(
        Request,
        [ name(Name, []),
          passwd(Passwd, []),
          return_to(Return, [])
        ]),
    (   valid_passwd(Name, Passwd)
    ->  http_open_session(_SessionId, []),
        http_session_assert(user(Name)),
        http_redirect(see_other, Return, Request)
    ;   http_link_to_id(login,
                        [ reason('Login failed'),
                          return_to(Return)
                        ], HREF),
        http_redirect(see_other, HREF, Request)
    ).

%!  logout(+Request)
%
%   Logout handler.  Clears the login state and returns to the origin.
%

logout(Request) :-
    http_parameters(
        Request,
        [ return_to(Return, [])
        ]),
    (   var(Return)
    ->  memberchk(referer(Return), Request)
    ;   true
    ),
    http_session_retractall(user(_User)),
    http_redirect(see_other, Return, Request).

		 /*******************************
		 *            USER DATA		*
		 *******************************/


%!  valid_passwd(+User, +Passwd) is semidet.
%
%   True if Passwd is the correct password for User.

valid_passwd(User, Passwd) :-
    user(User, password(Passwd)).

%!  user(?User, ?Property) is nondet.
%
%   True when Property is a property of user. In a real application this
%   should of course be  a  proper   persistent  database  and passwords
%   should be properly hashed.

user(jan, password(geheim)).
user(jan, role(trusted)).

user(bob, password(secret)).


		 /*******************************
		 *            EXPAND		*
		 *******************************/

:- http_request_expansion(user, 100).
:- http_request_expansion(rbac, 200).

%!  user(+Request0, -Request, +Options) is semidet.
%
%   HTTP request rewriter that figures out whether someone is logged in.
%   using this technique we can use   different  techniques to establish
%   the logged in status.

user(Request0, Request, _Options) :-
    http_in_session(_),
    http_session_data(user(User)),
    Request = [user(User)|Request0].

%!  rbac(+Request0, -Request, +Options) is semidet.
%
%   Establish whether the user  may  proceed   if  the  handler  options
%   contain a term role(Role).  Acts as follows:
%
%     1. If the user is logged in
%        - If the user has the desired role, succeed.
%        - Otherwise indicate the user is not authorized.  The
%          3rd argument of the `http_reply` exception provides
%          arbitrary context for the error page.
%     2. Otherwise redirect to the login page

rbac(Request, Request, Options) :-
    memberchk(role(Role), Options),
    (   memberchk(user(User), Request)
    ->  (   user(User, role(Role))
        ->  true
        ;   memberchk(path(Path), Request),
            throw(http_reply(forbidden(Path), [], [no_role(User, Role)]))
        )
    ;   memberchk(request_uri(Return), Request),
        http_link_to_id(login,
                        [ reason('The requested location requires login'),
                          return_to(Return)
                        ], HREF),
        http_redirect(see_other, HREF, Request)
    ).


		 /*******************************
		 *            ERROR		*
		 *******************************/

:- multifile
    http:status_page/3.

%!  http:status_page(+Term, +Context, -HTML)
%
%   Provide a custom error page for the forbidden action.

http:status_page(forbidden(Path), Context, HTML) :-
    phrase(page([ title('Access denied')
                ],
                [ h1('Access denied'),
                  p(['You do not have sufficient privileges to access ',
                     Path]),
                  \forbidden_reason(Context)
                ]),
           HTML).

forbidden_reason(Context) -->
    { memberchk(no_role(User, Role), Context) },
    html(p('The user ~p does not have role ~p'-[User,Role])).
