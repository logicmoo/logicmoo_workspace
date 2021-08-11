/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013-2015, VU University Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(plweb_openid,
	  [ site_user/2,		% +Request, -User
	    site_user_logged_in/1,	% -User
	    site_user_property/2,	% +User, ?Property
	    grant/2,			% +User, +Token
	    revoke/2,			% +User, +Token
	    authenticate/3,		% +Request, +Token, -Fields
	    user_profile_link//1,	% +User
	    current_user//1,		% +PageStyle
	    current_user//0,
	    login_link//1,		% +Request
	    redirect_master/1		% +Request
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_openid)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_path)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_json)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_host)).
:- use_module(library(http/recaptcha)).
:- use_module(library(http/http_stream)).
:- use_module(library(persistency)).
:- use_module(library(settings)).
:- use_module(library(debug)).
:- use_module(library(uuid)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(google_client)).

:- use_module(parms).
:- use_module(review).
:- use_module(pack).
:- use_module(wiki).
:- use_module(markitup).
:- use_module(tagit).
:- use_module(post).

/** <module> Handle users of the SWI-Prolog website

This module provide the OpenID interface  for the SWI-Prolog website. If
you want to run this for local installations, make sure that your server
is accessible through the public network   and first direct your browser
to the public network. Logging in using   Google  should work than. Some
other providers have more strict requirements.

You can fake OpenID login using the debug interface:

    ==
    ?- debug(openid_fake('WouterBeek')).
    ==
*/

:- multifile
	http_openid:openid_hook/1.

:- persistent
	openid_user_server(user:atom,
			   server:atom),
	site_user(uuid:atom,
		  openid:atom,
		  name:atom,
		  email:atom,
		  home_url:atom),
	user_description(uuid:atom,
			 description:atom),
	stay_signed_in(openid:atom,
		       cookie:atom,
		       peer:atom,
		       time:integer,
		       expires:integer),
	granted(uuid:atom,
		token:atom).

:- initialization
	db_attach('openid.db',
		  [ sync(close)
		  ]).

:- http_handler(root(user/create_profile),  create_profile, []).
:- http_handler(root(user/submit_profile),  submit_profile, []).
:- http_handler(root(user/logout),	    logout,         []).
:- http_handler(root(user/view_profile),    view_profile,   []).
:- http_handler(root(user/verify),          verify_user,    []).
:- http_handler(root(user/list),            list_users,     []).
:- http_handler(root(user/grant),           grant_user,     []).


		 /*******************************
		 *	    USER ADMIN		*
		 *******************************/

site_user_property(UUID, uuid(UUID)) :-
	(   site_user(UUID, _, _, _, _)
	->  true
	).
site_user_property(UUID, openid(OpenId)) :-
	site_user(UUID, OpenId, _, _, _).
site_user_property(UUID, name(Name)) :-
	site_user(UUID, _, Name, _, _).
site_user_property(UUID, email(Email)) :-
	site_user(UUID, _, _, Email, _).
site_user_property(UUID, home_url(Home)) :-
	site_user(UUID, _, _, _, Home).
site_user_property(UUID, granted(Token)) :-
	granted(UUID, Token).
site_user_property(UUID, granted_list(Tokens)) :-
	(   site_user(UUID, _, _, _, _)
	->  findall(Token, granted(UUID, Token), Tokens)
	).

set_user_property(UUID, Prop) :-
	site_user_property(UUID, Prop), !.
set_user_property(UUID, openid(OpenId)) :-
	retract_site_user(UUID, _OldID, Name, Email, Home),
	assert_site_user(UUID, OpenId, Name, Email, Home).


		 /*******************************
		 *	      RIGHTS		*
		 *******************************/

%%	grant(+User, +Token) is det.
%%	revoke(+User, +Token) is det.
%
%	Grant/revoke User (a UUID) the right to access Token.

grant(User, Token) :-
	ground_user(User),
	must_be(atom, Token),
	granted(User, Token), !.
grant(User, Token) :-
	assert_granted(User, Token).

revoke(User, Token) :-
	ground_user(User),
	must_be(atom, Token),
	\+ granted(User, Token), !.
revoke(User, Token) :-
	retract_granted(User, Token).

ground_user(User) :-
	must_be(atom, User),
	site_user(User, _, _, _, _), !.
ground_user(User) :-
	existence_error(user, User).


%%	grant_user(+Request)
%
%	HTTP handler to grant or revoke rights for a user.

grant_user(Request) :-
	catch(( http_read_json_dict(Request, Data),
		debug(grant, '~q', [Data]),
		admin_granted(Request),
		atom_string(UUID, Data.uuid),
		atom_string(Token, Data.token),
		(   Data.value == true
		->  grant(UUID, Token)
		;   revoke(UUID, Token)
		)
	      ), E,
	      throw(http_reply(bad_request(E)))),
	throw(http_reply(no_content)).

admin_granted(_Request) :-
	site_user_logged_in(User),
	site_user_property(User, granted(admin)), !.
admin_granted(Request) :-
	memberchk(path(Path), Request),
	throw(http_reply(forbidden(Path))).

%%	authenticate(+Request, +Token, -Fields)
%
%	Get authentication for editing wiki pages.  This now first tries
%	the OpenID login.

authenticate(Request, Token, [UUID,Name]) :-
	site_user_logged_in(UUID),
	(   site_user_property(UUID, granted(Token))
	->  site_user_property(UUID, name(Name))
	;   option(path(Path), Request),
	    permission_error(access, http_location, Path)
	).
authenticate(Request, Token, Fields) :-
	redirect_master(Request),
	(   http_authenticate(basic(passwd), Request, Fields)
	->  true
	;   format(atom(Msg), 'SWI-Prolog ~w authoring', [Token]),
		   throw(http_reply(authorise(basic, Msg)))
	).


		 /*******************************
		 *	 USER INTERACTION	*
		 *******************************/

:- multifile recaptcha:key/2.

:- setting(recaptcha:public_key, atom, '',
	   'reCAPTCHA public key').
:- setting(recaptcha:private_key, atom, '',
	   'reCAPTCHA private key').

recaptcha:key(public,  Key) :- setting(recaptcha:public_key,  Key).
recaptcha:key(private, Key) :- setting(recaptcha:private_key, Key).

%%	site_user(+Request, -User)
%
%	Demand the user to be logged on and, if this is the first logon,
%	verify the user and create a profile.

site_user(Request, User) :-
	openid_user(Request, OpenID, []),
	ensure_profile(OpenID, User).

ensure_profile(OpenID, User) :-
	(   site_user_property(User, openid(OpenID))
	->  true
	;   http_current_request(Request),
	    option(request_uri(RequestURI), Request),
	    http_link_to_id(create_profile, [return(RequestURI)], HREF),
	    http_redirect(moved_temporary, HREF, Request)
	).

%%	site_user_logged_in(-User) is semidet.
%
%	True when User is logged on.  Does not try to logon the user.

site_user_logged_in(User) :-
	openid_logged_in(OpenID),
	site_user_property(User, openid(OpenID)).


%%	create_profile(+Request).
%
%	Create a new user profile, and on success return to the original
%	location.

create_profile(Request) :-
	openid_user(Request, OpenID, []),
	http_parameters(Request,
			[ return(Return, [])
			]),
	reply_html_page(
	    user(create_profile),
	    title('Create user profile for SWI-Prolog'),
	    \create_profile(OpenID, Return)).


create_profile(OpenID, Return) -->
	{ (   site_user_property(User, openid(OpenID))
	  ->  Op = 'Update profile'
	  ;   uuid(User),		% new user
	      Op = 'Create profile'
	  )
	},
	html(h1(class(wiki), Op)),
	{ http_link_to_id(submit_profile, [], Action),
	  user_init_property(User, name(Name), ''),
	  user_init_property(User, email(Email), ''),
	  user_init_property(User, home_url(HomeURL), '')
	},
	html(form([ class(create_profile), method('POST'), action(Action) ],
		  [ input([type(hidden), name(return), value(Return)]),
		    input([type(hidden), name(uuid), value(User)]),
		    table([ tr([th('OpenID'),   td(input([ name(openid),
							   value(OpenID),
							   disabled(disabled)
							 ]))]),
			    tr([th('Name'),     td(input([ name(name),
							   value(Name),
							   placeholder('Displayed name')
							 ]))]),
			    tr([th('Email'),    td(input([ name(email),
							   value(Email),
							   placeholder('Your E-mail address')
							 ]))]),
			    tr([th('Home URL'), td(input([ name(home_url),
							   value(HomeURL),
							   placeholder('http://')
							 ]))]),
			    \description(User),
			    tr(td(colspan(2), \recaptcha([]))),
			    tr(td([colspan(2), align(right)],
				  input([type(submit), value(Op)])))
			  ])
		  ])),
	expain_create_profile.

user_init_property(User, P, Default) :-
	(   site_user_property(User, P)
	->  true
	;   http_session_data(ax(AX)),
	    ax(P, AX)
	->  true
	;   arg(1, P, Default)
	).

ax(email(AX.get(email)), AX).
ax(name(AX.get(name)), AX) :- !.
ax(name(Name), AX) :-
	atomic_list_concat([AX.get(firstname), AX.get(lastname)], ' ', Name), !.
ax(name(AX.get(nickname)), AX).

expain_create_profile -->
	html({|html||
	       <div class="smallprint">
	       On this page, we ask you to proof you are human and
	       create a minimal profile. Your name is displayed along with comments
	       that you create.  Your E-mail and home URL are used to detect authorship of
	       packs. Your E-mail and home URL will not be displayed,
	       nor be used for spamming and not be handed to third parties.
	       The editor can be used to add a short description about yourself.
	       This description is shown on your profile page that collects
	       your packages and ratings and reviews you performed.
	       </div>
	       |}).

%%	description(+UUID)//
%
%	Provide field for entering a description about the user.

description(UUID) -->
	{ (   user_description(UUID, Description)
	  ->  Extra = [value(Description)]
	  ;   Extra = []
	  )
	},
	html(tr(td(colspan(2),
		   \markitup([ id(description),
			       markup(pldoc),
			       cold(60),
			       rows(10)
			     | Extra
			     ])))).

%%	submit_profile(+Request)
%
%	Handle submission of the user profile

submit_profile(Request) :-
	openid_user(Request, OpenID, []),
	recaptcha_parameters(ReCAPTCHA),
	http_parameters(Request,
			[ uuid(User,         []),
			  name(Name0,        [optional(true), default(anonymous)]),
			  email(Email0,      [optional(true), default('')]),
			  home_url(Home0,    [optional(true), default('')]),
			  description(Descr, [optional(true), default('')]),
			  return(Return, [])
			| ReCAPTCHA
			]),
	(   catch(recaptcha_verify(Request, ReCAPTCHA), E, true)
	->  (   var(E)
	    ->  retractall_site_user(User, OpenID, _, _, _),
		normalize_space(atom(Name),  Name0),
		normalize_space(atom(Email), Email0),
		normalize_space(atom(Home),  Home0),
		assert_site_user(User, OpenID, Name, Email, Home),
		update_description(User, Descr),
		http_redirect(moved_temporary, Return, Request)
	    ;	E = error(domain_error(recaptcha_response, _), _)
	    ->	retry_captcha('CAPTCHA required', '')
	    ;	message_to_string(E, Msg)
	    ->	retry_captcha('CAPTCHA processing error', Msg)
	    )
	;   retry_captcha('CAPTCHA verification failed', '')
	).

retry_captcha(Why, Warning) :-
	reply_html_page(
	    plain,
	    title('CAPTCHA failed'),
	    [ h1(class(wiki), Why),
	      p(class(error), Warning),
	      p([ 'Please use the back button of your browser and ',
		  'try again'
		])
	    ]).


update_description(UUID, '') :- !,
	retractall_user_description(UUID, _).
update_description(UUID, Description) :- !,
	retractall_user_description(UUID, _),
	assert_user_description(UUID, Description).

%%	view_profile(+Request) is det.
%
%	HTTP handler showing the public  profile   for  a  user. Viewing
%	options:
%
%	  | Requested user is logged on | [view(private), edit_link(true)] |
%	  | Logged on is =admin=        | [view(admin)] |
%	  | Not logged on		| [view(public) |

view_profile(Request) :-
	http_parameters(Request,
			[ user(UUID, [ optional(true) ])
			]),
	(   site_user_logged_in(User)
	->  (   User = UUID
	    ->  (   site_user_property(User, granted(admin))
		->  Options = [view(admin), edit_link(true)]
		;   Options = [view(private), edit_link(true)]
		)
	    ;	site_user_property(User, granted(admin))
	    ->	Options = [view(admin)]
	    ;	Options = [view(public)]
	    )
	;   (   var(UUID)
	    ->	existence_error(http_parameter, user)
	    ;	Options = [view(public)]
	    )
	),
	site_user_property(UUID, name(Name)),
	reply_html_page(
	    user(view_profile(UUID)),
	    title('User ~w'-[Name]),
	    [ \edit_link(UUID, Options),
	      \view_profile(UUID, Options)
	    ]).

view_profile(UUID, Options) -->
	private_profile(UUID, Options),
	user_description(UUID, Options),
	user_tags(UUID, []),
	user_posts(UUID, annotation),
	user_posts(UUID, news),
	user_packs(UUID),
	profile_reviews(UUID).

%%	private_profile(+UUID, +Options)// is det.
%
%	If the user is viewing his/her own profile or the logged on user
%	has =admin= rights, show a  table   holding  the private profile
%	information.

private_profile(UUID, Options) -->
	{ option(view(private), Options)
	; option(view(admin), Options)
	}, !,
	html([ div(class('private-profile'),
		   [ h2(class(wiki),
			[ 'Private profile data',
			  \link_list_users
			]),
		     table([ \profile_data(UUID, 'Name',      name),
			     \profile_data(UUID, 'OpenID',    openid),
			     \profile_data(UUID, 'E-Mail',    email),
			     \profile_data(UUID, 'Home page', home_url)
			   | \admin_profile(UUID, Options)
			   ])
		   ]),
	       div(class(smallprint),
		   'The above private information is shown only to the owner.')
	     ]).
private_profile(_, _) --> [].

admin_profile(UUID, Options) -->
	{ option(view(admin), Options) }, !,
	html([ \profile_data(UUID, 'UUID',    uuid),
	       \profile_data(UUID, 'Granted', granted_list)
	     ]).
admin_profile(_, _) --> [].

link_list_users -->
	{ http_link_to_id(list_users, [], HREF)
	},
	html(a([ class('list-other-users'),
		 style('float:right;'),
		 href(HREF)
	       ], 'other users')).

create_profile_link(HREF) :-
	http_current_request(Request),
	option(request_uri(Here), Request),
	http_link_to_id(create_profile, [return(Here)], HREF).

profile_data(UUID, Label, Field) -->
	{ Term =.. [Field,Value],
	  site_user_property(UUID, Term),
	  (   value_dom(Field, UUID, Value, DOM)
	  ->  true
	  )
	},
	html(tr([ th([Label,:]),
		  td(DOM)
		])).

value_dom(name,		_,    Name,    Name).
value_dom(uuid,		_,    UUID,    UUID).
value_dom(email,	_,    Email,   a(href('mailto:'+Email), Email)).
value_dom(granted_list,	UUID, Tokens, \token_list(UUID, Tokens, [edit(true)])).
value_dom(_,		_,    URL,     a(href(URL), URL)).

%%	user_description(UUID, +Options)// is det.
%
%	Show user description

user_description(UUID, _Options) -->
	{ user_description(UUID, Description),
	  Description \== '', !,
	  atom_codes(Description, Codes),
	  wiki_file_codes_to_dom(Codes, /, DOM0),
	  clean_dom(DOM0, DOM)
	},
	html(DOM).
user_description(_UUID, Options) -->
	{ option(edit_link(true), Options),
	  create_profile_link(Edit)
	},
	html([ i('No description.'),
	       ' Click ', a(href(Edit), here), ' to create one'
	     ]).
user_description(_, _) --> [].

clean_dom([p(X)], X) :- !.
clean_dom(X, X).

edit_link(_UUID, Options) -->
	{ option(edit_link(true), Options), !,
	  create_profile_link(Edit)
	},
	html(div(class('edit-profile'),
		 [ a(href(Edit), 'Edit'), ' profile'])).
edit_link(_, _) --> [].


%%	user_packs(UUID)// is det.
%
%	Show a filtered version of the pack table, holding the packs
%	created by this user.

user_packs(UUID) -->
	{ setof(Pack, current_pack([author(UUID)], Pack), Packs), !,
	  sort_packs(rating, Packs, Sorted),
	  site_user_property(UUID, name(Name))
	},
	html([ h2(class(wiki), 'Packages by ~w'-[Name])
	     ]),
	pack_table(Sorted, []),
	html([ div(class(smallprint),
		   [ 'This list contains packages whose author name, e-mail ',
		     'or homepage url matches the profile information.'
		   ])
	     ]).
user_packs(_) -->
	[].


%%	list_users(+Request)
%
%	HTTP handler to list known users.

list_users(_Request) :-
	site_user_logged_in(User), !,
	(   site_user_property(User, granted(admin))
	->  ShowAdmin = true
	;   ShowAdmin = false
	),
	findall(Kudos-Details,
		site_kudos(_UUID, Details, Kudos),
		Pairs),
	keysort(Pairs, Sorted),
	pairs_values(Sorted, Users),
	reverse(Users, BestFirst),
	reply_html_page(
	    user(list),
	    title('SWI-Prolog site users'),
	    [ \explain_user_listing,
	      \html_requires(css('stats.css')),
	      table(class(block),
		    [ \user_table_header(ShowAdmin)
		    | \user_rows(BestFirst, ShowAdmin)
		    ])
	    ]).
list_users(_Request) :-
	reply_html_page(
	    user(list),
	    title('Permission denied'),
	    [ \explain_user_listing_not_logged_on
	    ]).

site_kudos(UUID, Details, Kudos) :-
	Details = _{ user:UUID,
		     news:NewsArticles,
		     annotations:Annotations,
		     reviews:Reviews,
		     tags:Tags,
		     votes:Up-Down
		   },
	site_user(UUID, _, _, _, _),
	user_post_count(UUID, news, NewsArticles),
	user_post_count(UUID, annotation, Annotations),
	user_review_count(UUID, Reviews),
	user_tag_count(UUID, Tags),
	user_vote_count(UUID, Up, Down),
	Kudos is ( NewsArticles*20 +
		   Reviews*10 +
		   Annotations*10 +
		   Tags*2 +
		   Up+Down
		 ).

explain_user_listing -->
	html({|html||
	      <p>Below is a listing of all registered users with some
	      basic properties.  This is list only visible to other
	      registered users.
	     |}).

explain_user_listing_not_logged_on -->
	html({|html||
	      <h1 class="wiki">Permission denied</h1>

	      <p class="warning">A listing of all registered users is only
	      available to users who are logged in.
	     |}).

user_rows([], _) --> [].
user_rows([H|T], ShowAdmin) --> user_row(H, ShowAdmin), user_rows(T, ShowAdmin).

user_table_header(ShowAdmin) -->
	html(tr([th('User'),
		 th('#Comments'),
		 th('#Reviews'),
		 th('#Votes'),
		 th('#Tags'),
		 \admin_header(ShowAdmin)
		])).

admin_header(true) --> !,
	html([ th('Granted'),
	       th('E-mail')
	     ]).
admin_header(_) --> [].

user_row(Details, ShowAdmin) -->
	{ Up-Down = Details.votes },
	html(tr([td(\user_profile_link(Details.user)),
		 td(Details.annotations),
		 td(Details.reviews),
		 td('+~d-~d'-[Up,Down]),
		 td(Details.tags),
		 \admin_columns(Details.user, ShowAdmin)
		])).

admin_columns(UUID, true) --> !,
	{ site_user_property(UUID, granted_list(Tokens)),
	  site_user_property(UUID, email(Email))
	},
	html([ td(\token_list(UUID, Tokens, [])),
	       td(\email(Email))
	     ]).
admin_columns(_, _) --> [].

token_list(UUID, Tokens, Options) -->
	{ option(edit(true), Options), !,
	  http_link_to_id(grant_user, [], Action)
	},
	html([ \token(wiki,  UUID, Tokens),
	       \token(news,  UUID, Tokens),
	       \token(admin, UUID, Tokens)
	     ]),
	html_post(script, \granted_script(Action)).
token_list(_, Tokens, _Options) -->
	token_list(Tokens).

token_list([]) --> [].
token_list([H|T]) -->
	html(H),
	(   {T==[]}
	->  []
	;   html([', ']),
	    token_list(T)
	).

token(Token, UUID, Active) -->
	{   memberchk(Token, Active)
	->  Extra = [checked(checked)]
	;   Extra = []
	},
	html([ input([ type(checkbox),
		       class(grant),
		       name(Token),
		       value(UUID)
		     | Extra
		     ]),
	       Token
	     ]).

granted_script(Action) -->
	js_script({|javascript(Action)||
$(document).ready(function() {
  $("input.grant").click(function(e)
  { e.preventDefault();
    var checkbox = $(this);
    var checked  = checkbox.prop("checked");
    var token    = checkbox.prop("name");
    var UUID     = checkbox.prop("value");
    $.ajax(Action,
	   { "contentType": "application/json; charset=utf-8",
	     "dataType": "json",
	     "data": JSON.stringify({ uuid:  UUID,
				      value: checked,
				      token: token
				    }),
	     "success": function() {
		checkbox.prop("checked", checked);
	     },
	     "type": "POST"
	   });
  });
});
		  |}).


email(Mail) -->
	html(a(href('mailto:'+Mail), Mail)).


		 /*******************************
		 *	     COMPONENTS		*
		 *******************************/

%%	user_profile_link(+UUID)//
%
%	Create a link to the profile of a user.

user_profile_link(UUID) -->
	{ site_user_property(UUID, name(Name)),
	  http_link_to_id(view_profile, [user(UUID)], HREF)
	}, !,
	html(a([class(user), href(HREF)], Name)).


		 /*******************************
		 *     OPENID CUSTOMIZATION	*
		 *******************************/

stay_login_cookie(swipl_login).

http_openid:openid_hook(trusted(OpenId, Server)) :-
	openid_user_server(OpenId, Server), !.
http_openid:openid_hook(trusted(OpenId, Server)) :-
	assert_openid_user_server(OpenId, Server), !.
http_openid:openid_hook(stay_signed_in(OpenId)) :-
	assertion(in_header_state),
	http_session_cookie(Cookie),
	get_time(NowF),
	Now is round(NowF),
	http_current_request(Request),
	http_peer(Request, Peer),
	Expires is Now+31*24*60*60,	% 31 days from now
	assert_stay_signed_in(OpenId, Cookie, Peer, Now, Expires),
	http_session_option(path(Path)),
	debug(openid(stay_signed_in),
	      'Created stay-signed-in for ~q', [OpenId]),
	http_timestamp(Expires, RFC1123),
	stay_login_cookie(CookieName),
	format('Set-Cookie: ~w=~w; Expires=~w; path=~w\r\n',
	       [CookieName, Cookie, RFC1123, Path]).
http_openid:openid_hook(logout(OpenId)) :-
	nonvar(OpenId),
	assertion(in_header_state),
	retractall_stay_signed_in(OpenId, _, _, _, _),
	http_session_option(path(Path)),
	stay_login_cookie(CookieName),
	format('Set-Cookie: ~w=; \c
	        expires=Tue, 01-Jan-1970 00:00:00 GMT; \c
		path=~w\r\n',
	       [CookieName, Path]),
	fail.
http_openid:openid_hook(logged_in(OpenId)) :-
	(   debugging(openid_fake(User)),
	    atom(User)
	->  debug(openid_fake(User), 'Fake login for ~q.', [User]),
	    OpenId = User
	;   http_in_session(_),
	    http_session_data(openid(OpenId))
	->  true
	;   http_current_request(Request),
	    memberchk(cookie(Cookies), Request),
	    memberchk(swipl_login=Cookie, Cookies),
	    stay_signed_in(OpenId, Cookie, _Peer, _Time, _Expires)
	->  http_open_session(_, []),
	    http_session_assert(openid(OpenId)),
	    debug(openid(stay_signed_in),
		  'Granted stay-signed-in for ~q', [OpenId])
	).
% see https://developers.google.com/accounts/docs/OpenID#shutdown-timetable
http_openid:openid_hook(x_parameter('https://www.google.com/accounts/o8/ud',
				    openid_shutdown_ack,
				    '2015-04-20')).


%%	yadis:xrds_specified_location(+Server, -XRDSLocation)
%
%	Hacks to deal with broken Yadis support.
%
%	  - Google does not support Yadis discovery, but does have an
%	    XRSD document, so we fake its location.
%	  - stackexchange.com serves an _OP Identifier Element_ instead
%	    of an _Claimed Identifier Element_ when doing Yadis
%	    discovery on the real OpenID.

:- multifile
	yadis:xrds_specified_location/2.

yadis:xrds_specified_location('http://google.com/',
			      'https://www.google.com/accounts/o8/id').
yadis:xrds_specified_location(StackOverFlow, -) :-
	sub_atom(StackOverFlow, 0, _, A, 'https://openid.stackexchange.com/'),
	A > 0.


in_header_state :-
	current_output(CGI),
	cgi_property(CGI, state(header)), !.

:- http_handler(openid(login),  plweb_login_page, [id(swipl_login)]).

%%	plweb_login_page(+Request)
%
%	HTTP handler that  overrules  the   location  openid(login)  for
%	customizating the -very basic- login page.

plweb_login_page(Request) :-
	redirect_master(Request),
	memberchk(host(localhost), Request),
	\+ ( debugging(openid_fake(User)),
	     atom(User)
	   ),
	\+ http_public_host(Request, localhost, _, []),
	openid_current_url(Request, URL), !,
	throw(http_reply(see_other(URL))).
plweb_login_page(Request) :-
	http_open_session(_, []),
	http_parameters(Request,
			[ 'openid.return_to'(ReturnTo, [])
			]),
	http_link_to_id(verify_user, [], Action),
	quick_buttons(Buttons),
	reply_html_page(user(login),
			[ title('SWI-Prolog login')
			],
			[ \openid_login_form(
			       ReturnTo,
			       [ show_stay(true),
				 action(Action),
				 buttons(Buttons)
			       ]),
			  \explain
			]).

explain -->
	html([ div(class(smallprint),
		   [ p([ 'Actions such as rating, commenting and tagging ',
			 'requires you to be signed in. ',
			 'We use ', a(href('http://openid.net/'), 'OpenID'), '. ',
			 'Currently, we accept any OpenID provider. ',
			 'Tested with ', \openid_ok
		       ]),
		     p([ 'After logging in for the first time, we will ask for ',
			 'some additional information.  All information is ',
			 'optional.'
		       ])
		   ])
	     ]).


%%	quick_buttons(-Buttons) is det.
%
%	Create a list of img(Attributes) terms for quick login.

quick_buttons(Buttons) :-
	findall(Img, quick_button(Img), Buttons).

quick_button(img([ src(Icon),
		   href(Provider),
		   alt(Name),
		   title('Sign in with '+Name)
		 ])) :-
	openid_provider(2, Provider, Name, ImgName),
	http_absolute_location(icons(ImgName), Icon, []).

openid_provider(2, LoginWithGoogle, 'Google', 'social_google_box.png') :-
	http_link_to_id(login_with_google, [], LoginWithGoogle).
openid_provider(2, 'http://me.yahoo.com',  'Yahoo', 'social_yahoo_box_lilac.png').
openid_provider(1, 'https://openid.stackexchange.com/%user%', 'StackExchange', -).

openid_ok -->
	{ Term = openid_provider(_Version, _URL, _Name, _Icon),
	  findall(Term, Term, Terms)
	},
	openid_ok(Terms).

openid_ok([]) --> [].
openid_ok([H|T]) -->
	openid_ok1(H),
	(   {T == []}
	->  []
	;   html(', '),
	    openid_ok(T)
	).

openid_ok1(openid_provider(2, URL, Name, _Icon)) --> !,
	html(a(href(URL), Name)).
openid_ok1(openid_provider(1, URL, Name, _Icon)) --> !,
	html([ Name, ' using the url ',
	       span(class('openid-url-pattern'), URL)
	     ]).


%%	verify_user(+Request)
%
%	HTTP handler for SWI-Prolog  site   login.  Calls openid_verify,
%	asking for additional attribute exchange.

verify_user(Request) :-
	openid_verify([ ax([ email(_, [required]),
			     nickname(_),
			     fullname(_),
			     firstname(_),
			     lastname(_)
			   ])
		      ], Request).


		 /*******************************
		 *	   GOOGLE LOGIN		*
		 *******************************/

:- if(current_predicate(oauth_authenticate/3)).

:- http_handler(root(user/login_with_google), login_with_google, []).

:- setting(google:client_id, atom, '',
	   'Google project ClientID code').
:- setting(google:client_secret, atom, '',
	   'Google project ClientSecret code').

%%	login_with_google(+Request)
%
%	HTTP handler to login with Google.

login_with_google(Request) :-
	http_parameters(Request,
			[ 'openid.return_to'(ReturnTo, [default(/)]),
			  stay(Stay, [default(false)])
			]),
	oauth_authenticate(Request, 'google.com',
			   [client_data(_{return_to:ReturnTo, stay:Stay})]).

:- multifile
	google_client:key/2,
	google_client:login_existing_user/1,
	google_client:create_user/1.

google_client:key(client_id, ClientID) :-
	setting(google:client_id, ClientID).
google_client:key(client_secret, ClientSecret) :-
	setting(google:client_secret, ClientSecret).

%%	google_client:login_existing_user(+Claim) is semidet.
%
%	True if the user is know to us and thus we can perform the login
%	without further interaction.

google_client:login_existing_user(Claim) :-
	google_fake_open_id(Claim, GoogleID),
	site_user_property(_User, openid(GoogleID)), !,
	google_login(Claim).
google_client:login_existing_user(Claim) :-
	downcase_atom(Claim.get(email), ClaimedEmail),
	site_user_property(UUID, email(Email)),
	downcase_atom(Email, ClaimedEmail), !,
	debug(google, 'Found ~p with ~p', [UUID, Claim.email]),
	google_fake_open_id(Claim, GoogleID),
	set_user_property(UUID, openid(GoogleID)),
	google_login(Claim).

%%	google_client:create_user(+Profile) is det.
%
%	Create a new user for the given Google Profile.

google_client:create_user(Profile) :-
	http_session_assert(ax(Profile)),
	google_login(Profile).

google_login(Claim) :-
	http_open_session(_, []),
	google_fake_open_id(Claim, GoogleID),
	http_session_retractall(openid(_)),
	http_session_assert(openid(GoogleID)),
	http_current_request(Request),
	(   true(Claim.client_data.stay)
	->  debug(google, 'Stay signed in: ~p', [GoogleID]),
	    http_openid:openid_hook(stay_signed_in(GoogleID))
	;   true
	),
	http_redirect(moved_temporary, Claim.client_data.return_to, Request).

google_fake_open_id(Claim, GoogleID) :-
	atomic_list_concat(['http://google.com/fake_open_id/', Claim.sub],
			   GoogleID).

true(true).
true(yes).

:- endif.


		 /*******************************
		 *	      LOGOUT		*
		 *******************************/

%%	logout(+Request)
%
%	Logout  the  current  user.  If  openid.return_to  is  provided,
%	provide a back-link

logout(Request) :-
	openid_logged_in(OpenId), !,
	openid_logout(OpenId),
	reply_html_page(
	    user(logout),
	    title('Logged out'),
	    [ p('Thanks for using www.swi-prolog.org'),
	      \logout_back_link(Request)
	    ]).
logout(Request) :-
	reply_html_page(
	    user(logout),
	    title('Not logged in'),
	    [ p(class(warning), 'You are not logged in'),
	      \logout_back_link(Request)
	    ]).


logout_back_link(Request) -->
	{ http_parameters(Request,
			  [ 'openid.return_to'(Return, [optional(true)])
			  ]),
	  nonvar(Return)
	}, !,
	html(p(['Go ', a(href(Return), back), '.'])).
logout_back_link(_) -->
	[].


%%	current_user//

current_user -->
	current_user(default).

current_user(Style) -->
	{ Style \== create_profile,
	  openid_logged_in(OpenID), !,
	  ensure_profile(OpenID, User),
	  (   site_user_property(User, name(Name)),
	      Name \== ''
	  ->  Display = Name
	  ;   Display = OpenID
	  ),
	  http_link_to_id(view_profile, [], Profile)
	},
	html(div(class('current-user'),
		 [ a([href(Profile)], Display),
		   ' (', \logout_link, ')'
		 ])).
current_user(Style) -->
	{ Style \== create_profile,
	  http_current_request(Request), !
	},
	html(div(class('current-user'),
		 \login_link(Request))).
current_user(_Style) -->
	[].

%%	login_link(+Request)//
%
%	Create a link to login, which returns to the current page.

login_link(Request) -->
	{ (   memberchk(request_uri(Here), Request)
	  ->  Attrs = ['openid.return_to'(Here)]
	  ;   Attrs = []
	  ),
	  http_link_to_id(swipl_login, Attrs, Login)
	},
	html(a([class(signin), href(Login)], login)).

%%	logout_link//
%
%	Create a link to logout

logout_link -->
	{ http_link_to_id(logout, [], Logout) },
	html(a([href(Logout)], 'logout')).


%%	redirect_master(+Request)
%
%	Redirect a request to the master server,   so  we do not have to
%	deal with multiple versions of the database files.

redirect_master(Request) :-
	option(host(Host), Request),
	server(_, Host),
	server(master, Master),
	Host \== Master, !,
	option(request_uri(URI), Request),
	format(string(To), 'https://~w~w', [Master, URI]),
	http_redirect(see_other, To, Request).

