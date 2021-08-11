/*  Part of SWI-Prolog

    Author:        Wouter Beek & Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, VU University Amsterdam

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

:- module(post,
	  [ find_posts/3,		% +Kind:oneof([annotation,news])
					% :CheckId
					% -Ids:list(atom)
	    fresh/1,			% ?Id:atom
	    all/1,			% ?Id:atom
	    post/3,			% ?Post:or([atom,compound])
					% ?Name:atom
					% ?Value
	    post//2,			% +Post, +Options
	    posts//4,			% +Kind, +Object, +Ids, +Options
	    add_post_link//2,		% +Kind, +Object
	    relevance/2,		% +Id:atom
					% -Relevance:between(0.0,1.0)
	    post_process/2,		% +Request:list, +Id:atom
	    sort_posts/2,		% +Ids:list(atom), -SortedIds:list(atom)

	    user_posts//2,		% +User, +KInd
	    user_post_count/3,		% +User, +Kind, -Count
	    user_vote_count/3		% +User, -Up, -Down
	  ]).

/** <module> Posts

@author Wouter Beek
@tbd Type-based JS response.
     After DELETE: remove that post from DOM.
     After POST: add that post to DOM.
     After PUT: update that post in DOM.
*/

:- use_module(library(error)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_path)).
:- use_module(library(http/js_write)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(persistency)).
:- use_module(library(pldoc/doc_html)).
:- use_module(library(uri)).
:- use_module(library(md5)).
:- use_module(library(dcg/basics)).
:- use_module(library(aggregate)).

:- use_module(object_support).
:- use_module(openid).
:- use_module(notify).
:- use_module(generics).

:- meta_predicate
	find_posts(+,1,-).

:- html_resource(css('post.css'), []).
:- html_resource(js('markitup/sets/pldoc/set.js'),
		 [ requires([ js('markitup/jquery.markitup.js'),
			      js('markitup/skins/markitup/style.css'),
			      js('markitup/sets/pldoc/style.css')
			    ])
		 ]).

:- persistent
	post(id:atom,
	     post:dict),
	vote(id:atom,			% post id
	     value:integer,		% value (up:1, down:-1)
	     user:atom,			% user who voted
	     time:integer).		% time of the vote

:- initialization
	db_attach('post.db', [sync(close)]).

:- http_handler(root(vote), vote, []).

:- op(100, xf, ?).

post_type(post{kind:oneof([annotation,news]),
	       title:string?,
	       content:string,
	       meta:meta{id:atom,
			 author:atom,
			 object:any?,
			 importance:between(0.0,1.0)?,
			 time:time{created:number,
				   modified:number?,
				   'freshness-lifetime':number?}}}).

%%	convert_post(+Dict0, -Dict) is det.
%
%	@error	May throw type and instantiation errors.
%	@tbd	Introduce type-testing support in library(error).

convert_post(Post0, Post) :-
	post_type(Type),
	convert_dict(Type, Post0, Post).

%%	convert_dict(+Type, +DictIn, -DictOut) is det.

convert_dict(TypeDict, Dict0, Dict) :-
	is_dict(TypeDict), !,
	dict_pairs(TypeDict, Tag, TypePairs),
	dict_values(TypePairs, Dict0, Pairs),
	dict_pairs(Dict, Tag, Pairs).
convert_dict(atom, String, Atom) :- !,
	atom_string(Atom, String).
convert_dict(oneof(Atoms), String, Atom) :-
	maplist(atom, Atoms), !,
	atom_string(Atom, String),
	must_be(oneof(Atoms), Atom).
convert_dict(float, Number, Float) :- !,
	Float is float(Number).
convert_dict(list(Type), List0, List) :- !,
	must_be(list, List0),
	maplist(convert_dict(Type), List0, List).
convert_dict(Type, Value, Value) :-
	must_be(Type, Value).

dict_values([], _, []).
dict_values([Name-Type|TP], Dict, [Name-Value|TV]) :-
	dict_value(Type, Name, Dict, Value), !,
	dict_values(TP, Dict, TV).
dict_values([_|TP], Dict, TV) :-
	dict_values(TP, Dict, TV).

dict_value(Type?, Name, Dict, Value) :- !,
	get_dict(Name, Dict, Value0),
	Value0 \== null,
	convert_dict(Type, Value0, Value).
dict_value(Type, Name, Dict, Value) :-
	convert_dict(Type, Dict.Name, Value).

%%	retract_post(+Id)
%
%	Remove post Id from the database.

retract_post(Id):-
	retract_post(Id, _).

%%	convert_post(+JSON, +Kind, +Id, +Author, +TimeProperty, -Post) is det.
%
%	Convert a post object into its Prolog equivalent.

convert_post(Post0, Kind, Id, Author, TimeProperty, Post) :-
	get_time(Now),
	(   atom_string(ObjectID, Post0.meta.get(about)),
	    object_id(Object, ObjectID)
	->  Post1 = Post0.put(meta/object, Object)
	;   Post1 = Post0
	),
	Post2 = Post1.put(kind, Kind)
	             .put(meta/id, Id)
		     .put(meta/author, Author)
		     .put(meta/time/TimeProperty, Now),
	convert_post(Post2, Post).


%%	post_url(+Id, -HREF) is det.
%
%	True when HREF is a link to post Id.

post_url(Id, HREF) :-
	post(Id, kind, Kind),
	(   kind_handler(Kind, HandlerId)
	->  http_link_to_id(HandlerId, path_postfix(Id), HREF)
	;   domain_error(kind, Kind)
	).

kind_handler(news,	 news_process).
kind_handler(annotation, annotation_process).

%%	post_link(+Id)
%
%	Generate a link to post Id.

post_link(Id) -->
	{ post_url(Id, HREF)
	},
	html(a(href(HREF), \post_link_text(Id))).

post_link_text(Id) -->
	{ post(Id, title, Title) },
	html(Title).
post_link_text(Id) -->
	{ post(Id, object, Object),
	  object_label(Object, Label)
	},
	html(Label).

%%	post_process(+Request, ?Kind) is det.
%
%	HTTP handler that implements a REST interface for postings.
%
%	@arg	Kind is the type of post, and is one of =news= or
%		=annotation=.

post_process(Request, Kind) :-
	request_to_id(Request, Kind, Id),
	must_be(oneof([news,annotation]), Kind),
	memberchk(method(Method), Request),
	(   site_user_logged_in(User)
	->  true
	;   User = anonymous
	),
	post_process(Method, Request, Kind, User, Id).

%%	post_process(+Method, +Request, +Kind, +Id, +User) is det.
%
%	Implement the REST replies.

% DELETE
post_process(delete, Request, Kind, User, Id) :-
	post_authorized(Request, User, Kind),
	post(Id, author, Author), !,
	(   (   Author == User
	    ;	site_user_property(User, granted(admin))
	    )
	->  post(Id, about, About),
	    retract_post(Id, OldPost),
	    notify(About, post_deleted(OldPost)),
	    throw(http_reply(no_content))	% 204
	;   memberchk(path(Path), Request),
	    throw(http_reply(forbidden(Path)))	% 403
	).
post_process(delete, Request, _, _, _) :-
	http_404([], Request).

% GET
post_process(get, _, _, _, Id):-
	post(Id, Post), !,
	reply_json(Post).
post_process(get, Request, _, _, _):-
	http_404([], Request).

% POST
post_process(post, Request, Kind, User, _):-
	post_authorized(Request, User, Kind),
	catch(( http_read_json_dict(Request, Post0),
		uuid(Id),
		convert_post(Post0, Kind, Id, User, created, NewPost),
		assert_post(Id, NewPost)
	      ),
	      E,
	      throw(http_reply(bad_request(E)))),
	post(Id, about, About),
	notify(About, post_created(NewPost)),
	memberchk(path(Path), Request),
	atom_concat(Path, Id, NewLocation),
	format('Location: ~w~n', [NewLocation]),
	reply_json(_{created:Id, href:NewLocation},
		   [status(201)]).

% PUT
post_process(put, Request, Kind, User, Id):-
	post_authorized(Request, User, Kind),
	post(Id, created, Created),
	catch(( http_read_json_dict(Request, Post0),
		convert_post(Post0.put(meta/time/created, Created),
			     Kind, Id, User, modified,
			     NewPost)
	      ),
	      E,
	      throw(http_reply(bad_request(E)))),
	(   post(Id, author, Author)
	->  (   Author == User
	    ->  retract_post(Id, OldPost),
		assert_post(Id, NewPost),
		post(Id, about, About),
		notify(About, post_updated(OldPost, NewPost)),
		throw(http_reply(no_content))
	    ;   memberchk(path(Path), Request),
		throw(http_reply(forbidden(Path)))
	    )
	;   http_404([], Request)
	).

:- dynamic debug_allow_all_posts/0.

%%	debug_posts is det
%
%	Defeats normal authorization checking for posts,
%	so during development we don't need to struggle with OAuth,
%	emails, etc.
%
debug_posts :-
	writeln('Anyone may now debug posts'),
	asserta(debug_allow_all_posts).

%%	nodebug_posts is det
%
%	remove the effects of debug_posts
%
nodebug_posts :-
	writeln('Back to normal post control'),
	retractall(debug_allow_all_posts).


%%	post_authorized(+Request, +User, +Kind) is det.
%
%	@throws	http_reply(forbidden(Path)) if the user is not allowed
%		to post.
%	@tbd	If the user is =anonymous=, we should reply 401 instead
%		of 403, but we want OpenID login

post_authorized(_Request, User, Kind) :-
	post_granted(User, Kind), !.
post_authorized(Request, _User, _Kind) :-
	memberchk(path(Path), Request),
	throw(http_reply(forbidden(Path))).

post_granted(_, _) :- debug_allow_all_posts.
post_granted(User, Kind) :-
	site_user_property(User, granted(Kind)), !.
post_granted(User, annotation) :-
	User \== anonymous.


%!	post(+Post, +Name:atom, -Value) is semidet.
%!	post(+Post, ?Name:atom, -Value) is nondet.
%!	post(-Post, ?Name:atom, -Value) is nondet.
%
%	True if Post have Value for the given attribute.
%
%	@arg	If Post is given, it is either the id of a post or a dict
%		describing the post.  When generated, Post is the (atom)
%		identifier of the post.

post(PostOrId, Name, Value) :-
	nonvar(PostOrId), !,
	(   atom(PostOrId)
	->  post(PostOrId, Post)
	;   Post = PostOrId
	),
	post1(Name, Post, Value),
	Value \== null.
post(Id, Name, Value) :-
	post(Id, Post),
	post1(Name, Post, Value).

post1(object, Post, Object) :-
	Object = Post.meta.get(object).
post1(about, Post, About) :-			% used for notification
	(   About = Post.meta.get(object)
	->  true
	;   About = Post.kind
	).
post1(author, Post, Author) :-
	Author = Post.meta.author.
post1(content, Post, Content) :-
	Content = Post.content.
post1('freshness-lifetime', Post, FreshnessLifetime ) :-
	FreshnessLifetime = Post.meta.time.'freshness-lifetime'.
post1(id, Post, Id) :-
	Id = Post.meta.id.
post1(importance, Post, Importance) :-
	Importance = Post.meta.importance.
post1(kind, Post, Kind) :-
	Kind = Post.kind.
post1(meta, Post, Meta) :-
	Meta = Post.meta.
post1(created, Post, Posted) :-
	Posted = Post.meta.time.created.
post1(modified, Post, Posted) :-
	Posted = Post.meta.time.modified.
post1(time, Post, Time):-
	Time = Post.meta.time.
post1(title, Post, Title) :-
	Title = Post.get(title).
post1(votes, Post, Votes) :-
	aggregate_all(sum(Vote), vote(Post.meta.id, Vote), Votes).
post1(votes_up, Post, Up) :-
	aggregate_all(sum(Vote), vote_up(Post.meta.id, Vote), Up).
post1(votes_down, Post, Down) :-
	aggregate_all(sum(Vote), vote_down(Post.meta.id, Vote), Down).


%!	post(+Id:atom, +Options)// is det.
%
%	Generate HTML for apost.  Supported Options:
%
%	  * orientation(+Orientation:oneof([left,right]))
%	  Orientation of the post.  This is used in binary conversations
%         to show the different conversation parties.
%	  * standalone(+Standalone:boolean)
%	  Whether this post is part of multiple posts or not.

post(Id, Options) -->
	{ post(Id, kind, Kind),
	  (   option(orientation(Orient), Options),
	      Orient \== none
	  ->  Extra = [ style('float:'+Orient+';') ]
	  ;   Extra = []
	  )
	},

	html(article([ class([post,Kind]),
		       id(Id)
		     | Extra
		     ],
		     [ \post_header(Id, Options),
		       \post_section(Id),
		       \edit_delete_post(Id)
		     ])),

	(   { option(standalone(true), Options, true) }
	->  html_requires(css('post.css')),
	    (   { site_user_logged_in(_) }
	    ->  {   post(Id, about, Object),
		    object_id(Object, About)
		->  true
		;   About = @(null)
		},
	        html(\write_post_js(Kind, About))
	    ;   login_post(Kind)
	    )
	;   []
	).

%!	post_header(+Id, +Options)// is det.
%
%	When    the    post     appears      in     isolation    (option
%	standalone(true)), the title is not displayed.

post_header(Id, O1) -->
	html(header([],
		    [ \post_title(O1, Id),
		      \post_metadata(Id),
		      span(class='post-links-and-votes',
			   [ \post_votes(Id),
			     \html_receive(edit_delete(Id))
			   ])
		    ])).

post_metadata(Id) -->
	{post(Id, kind, Kind)},
	post_metadata(Kind, Id).

post_metadata(annotation, Id) -->
	{post(Id, author, Author)},
	html(span(class='post-meta',
		  [ \user_profile_link(Author),
		    ' said (',
		    \post_time(Id),
		    '):'
		  ])).
post_metadata(news, Id) -->
	{post(Id, author, Author)},
	html(span(class='post-meta',
		  [ 'By ',
		    \user_profile_link(Author),
		    ' at ',
		    \post_time(Id)
		  ])).

post_section(Id) -->
	{ post(Id, author, Author),
	  post(Id, content, Content),
	  atom_codes(Content, Codes),
	  wiki_file_codes_to_dom(Codes, /, DOM1),
	  clean_dom(DOM1, DOM2)
	},
	html(section([],
		     [ \author_image(Author),
		       div(class='read-post', DOM2)
		     ])).

post_time(Id) -->
	{ post(Id, created, Posted) }, !,
	html(\dateTime(Posted)).
post_time(_) --> [].

post_title(O1, Id) -->
	{ option(standalone(false), O1, true),
	  post(Id, title, Title), !,
	  post_url(Id, HREF)
	},
	html(h2(class('post-title'), a(href(HREF),Title))).
post_title(_, _) --> [].

post_votes(Id) -->
	{ post(Id, votes_down, Down),
	  format(atom(AltDown), '~d downvotes', [Down]),
	  post(Id, votes_up, Up),
	  format(atom(AltUp), '~d upvotes', [Up]),
	  post(Id, votes, Amount),
	  http_absolute_location(icons('vote_up.gif'), UpIMG, []),
	  http_absolute_location(icons('vote_down.gif'), DownIMG, [])
	},
	html([ a([class='post-vote-up',href=''],
		 img([alt(AltUp),src(UpIMG),title(Up)], [])),
	       ' ',
	       span(class='post-vote-amount', Amount),
	       ' ',
	       a([class='post-vote-down',href=''],
		 img([alt(AltDown),src(DownIMG),title(Down)], []))
	     ]).


%%	posts(+Kind, +Object, +Ids:list(atom), +Options)//
%
%	Generate HTML for a list of posts and add a link to add new
%	posts.  Options:
%
%	  * order_by(+Property)
%	  Order posts by Property.  Properties are defined by post/3.
%	  * add_add_link(+Boolean)
%	  Add link to add new posts.  Default is =true=.

posts(Kind, Object, Ids1, Options) -->
	{ atomic_list_concat([Kind,component], '-', Class),
	  default_order(Kind, DefOrder),
	  option(order_by(OrderBy), Options, DefOrder),
	  sort_posts(Ids1, OrderBy, Ids2)
	},
	html_requires(css('post.css')),
	html([ div(class=[posts,Class],
		   \post_list(Ids2, Kind, none))
	     ]),
	(   { option(add_add_link(true), Options, true) }
	->  add_post_link(Kind, Object)
	;   []
	).

default_order(news, created).
default_order(annotation, votes).


post_list([], _Kind, _Orient) --> [].
post_list([Id|Ids], Kind, Orient1) -->
	post(Id, [orientation(Orient1),standalone(false)]),
	{switch_orientation(Orient1, Orient2)},
	post_list(Ids, Kind, Orient2).

switch_orientation(left,  right).
switch_orientation(right, left).
switch_orientation(none,  none).


%%	add_post_link(+Kind, +Object)//
%
%	Emit HTML that allows for adding a new post

add_post_link(Kind, Object) -->
	{ site_user_logged_in(User),
	  post_granted(User, Kind),
	  (   Object == null
	  ->  About = @(null)
	  ;   object_id(Object, About)
	  ),
	  Id = ''			% empty id
	}, !,
	html(div(id='add-post',
		 [ \add_post_link(Kind),
		   form([id='add-post-content',style='display:none;'],
			table([ tr(td(\add_post_title(Id, Kind))),
				tr(td([ \add_post_importance(Id, Kind),
					\add_post_freshnesslifetime(Id, Kind)
				      ])),
				tr(td(\add_post_content(Id))),
				tr(td(\submit_post_links(Kind)))
			      ])),
		   \write_post_js(Kind, About)
		 ])).
add_post_link(Kind, _) -->
	login_post(Kind).

add_post_content(Id) -->
	{   Id \== '', post(Id, content, Content)
	->  true
	;   Content = []
	},
	html(textarea([class(markItUp)], Content)).

%%	add_post_freshnesslifetime(+Kind)
%
%	Add fressness menu if Kind = =news=.  Freshness times are
%	represented as seconds.

add_post_freshnesslifetime(Id, news) --> !,
	{   Id \== '', post(Id, 'freshness-lifetime', Default)
	->  true
	;   menu(freshness, 'One month', Default)
	},
	html([ label([], 'Freshness lifetime: '),
	       select(class='freshness-lifetime',
		      \options(freshness, Default)),
	       br([])
	     ]).
add_post_freshnesslifetime(_, _) --> [].

add_post_importance(Id, news) --> !,
	{   Id \== '', post(Id, importance, Importance)
	->  true
	;   menu(importance, 'Normal', Importance)
	},
	html([ label([], 'Importance: '),
	       select(class=importance,
		      \options(importance, Importance))
	     ]).
add_post_importance(_, _) --> [].

options(Key, Default) -->
	{ findall(Name-Value, menu(Key, Name, Value), Pairs) },
	option_list(Pairs, Default).

option_list([], _) --> [].
option_list([Name-Value|T], Default) -->
	{   Name == Default
	->  Extra = [selected(selected)]
	;   Extra = []
	},
	html(option([value(Value)|Extra], Name)),
	option_list(T, Default).


menu(freshness, 'One year',  Secs) :- Secs is 365*24*3600.
menu(freshness, 'One month', Secs) :- Secs is 31*24*3600.
menu(freshness, 'One week',  Secs) :- Secs is 7*24*3600.
menu(freshness, 'One day',   Secs) :- Secs is 1*24*3600.

menu(importance, 'Very high', 1.00).
menu(importance, 'High',      0.75).
menu(importance, 'Normal',    0.50).
menu(importance, 'Low',	      0.25).
menu(importance, 'Very low',  0.00).


add_post_link(Kind) -->
	html(a([id('add-post-link'),href('')],
	       \add_post_label(Kind))).

add_post_label(news) -->
	html('Post new article').
add_post_label(annotation) -->
	html('Add comment').

add_post_title(Id, news) --> !,
	{   Id \== '', post(Id, title, Title)
	->  Extra = [value(Title)]
	;   Extra = []
	},
	html([ label([], 'Title: '),
	       input([ class(title),
		       size(70),
		       type(text)
		     | Extra
		     ], []),
	       br([])
	     ]).
add_post_title(_, _) --> [].

submit_post_links(Kind) -->
	html(div([ id='add-post-links',style='display:none;'],
		 [ a([id='add-post-submit',href=''], \submit_post_label(Kind)),
		   a([id='add-post-cancel',href=''], 'Cancel')
		 ])).

submit_post_label(news) -->
	html('Submit article').
submit_post_label(annotation) -->
	html('Submit comment').

%%	edit_post_form(+Id)//
%
%	Provide a non-displayed editor for post Id if the author of this
%	post is logged on.

edit_post_form(Id) -->
	{ site_user_logged_in(User),
	  edit_post_granted(Id, User), !,
	  post(Id, kind, Kind)
	},
	html([ form([class='edit-post-content',style='display:none;'],
		    table([ tr(td(\add_post_title(Id, Kind))),
			    tr(td([ \add_post_importance(Id, Kind),
				    \add_post_freshnesslifetime(Id, Kind)
				  ])),
			    tr(td(\add_post_content(Id))),
			    tr(td(\save_post_links(Kind)))
			  ]))
	     ]).
edit_post_form(_) --> [].

edit_delete_post(Id) -->
	{ site_user_logged_in(User),
	  edit_post_granted(Id, User), !
	},
	html([ \html_post(edit_delete(Id), \edit_delete_post_link),
	       \edit_post_form(Id)
	     ]).
edit_delete_post(_) --> [].

edit_delete_post_link -->
	html([ ' ',
	       a([class='edit-post-link',href=''], 'Edit'),
	       '/',
	       a([class='delete-post-link',href=''], 'Delete')
	     ]).

save_post_links(Kind) -->
	html(div([class='save-post-links',style='display:none;'],
		 [ a([class='save-post-submit',href=''],
		     \save_post_title(Kind)),
		   a([class='save-post-cancel',href=''],
		     'Cancel')
		 ])).

save_post_title(news) -->
	html('Save updated article').
save_post_title(annotation) -->
	html('Save updated comment').

edit_post_granted(_Id, User) :-
	site_user_property(User, granted(admin)), !.
edit_post_granted(Id, User) :-
	post(Id, author, Author),
	User == Author.

%!	age(+Id:atom, -Age) is det.
%
%	True when post Id was created Age seconds ago.

age(Id, Age):-
	post(Id, created, Posted),
	get_time(Now),
	Age is Now - Posted.

%!	author_image(+User:atom)// is det.

author_image(User) -->
	{ site_user_property(User, name(Name)),
	  format(atom(Alt), 'Picture of user ~w.', [Name]),
	  user_avatar(User, Avatar),
	  http_link_to_id(view_profile, [user(User)], Link)
	},
	html(a(href(Link),
	       img([ alt(Alt),
		     class('post-avatar'),
		     src(Avatar),
		     title(Name)
		   ]))).

%%	user_avatar(+User, -AvatarImageLink) is det.
%
%	@see https://en.gravatar.com/site/implement/hash/
%	@see https://en.gravatar.com/site/implement/images/

user_avatar(User, URL) :-
	site_user_property(User, email(Email)),
	downcase_atom(Email, CanonicalEmail),
	md5_hash(CanonicalEmail, Hash, []),
	atom_concat('/avatar/', Hash, Path),
	uri_data(scheme,    Components, https),
	uri_data(authority, Components, 'www.gravatar.com'),
	uri_data(path,      Components, Path),
	uri_components(URL, Components).

dateTime(TimeStamp) -->
	{ format_time(atom(Date), '%Y-%m-%dT%H:%M:%S', TimeStamp) },
	html(span([class(date),title(TimeStamp)], Date)).

%!	find_posts(+Kind, :CheckId, -Ids) is det.
%
%	True when Ids  is  a  list  of   all  posts  of  Kind  for which
%	call(CheckId, Id) is true.

find_posts(Kind, CheckId, Ids):-
	findall(Id,
		( post(Id, Post),
		  post(Post, kind, Kind),
		  call(CheckId, Id)
		),
		Ids).

%!	fresh(+Id:atom) is semidet.
%
%	True if post Id is considered _fresh_.

fresh(Id):-
	post(Id, 'freshness-lifetime', FreshnessLifetime),
	nonvar(FreshnessLifetime), !,
	age(Id, Age),
	Age < FreshnessLifetime.
fresh(_).

%!	all(+Id:atom) is det.
%
%	News filter, returning all objects

all(_).

%! relevance(+Id:atom, -Relevance:between(0.0,1.0)) is det.
% - If `Importance` is higher, then the dropoff of `Relevance` is flatter.
% - `Relevance` is 0.0 if `FreshnessLifetime =< Age`.
% - `Relevance` is 1.0 if `Age == 0`.

relevance(Id, Relevance) :-
	fresh(Id),
	post(Id, importance, Importance),
	nonvar(Importance),
	post(Id, 'freshness-lifetime', FreshnessLifetime),
	nonvar(FreshnessLifetime), !,
	age(Id, Age),
	Relevance is Importance * (1 - Age / FreshnessLifetime).
relevance(_, 0.0).

sort_posts(Ids, SortedIds):-
	sort_posts(Ids, created, SortedIds).

sort_posts(Ids, Property, SortedIds):-
	map_list_to_pairs(post_property(Property), Ids, Pairs),
	keysort(Pairs, SortedPairs),
	reverse(SortedPairs, RevSorted),
	pairs_values(RevSorted, SortedIds).

post_property(Property, Id, Value) :-
	post(Id, Property, Value).

%%	login_post(+Kind)//
%
%	Suggest to login or request  permission   to  get  access to the
%	posting facility.

login_post(Kind) -->
	{ site_user_logged_in(_), !,
	  http_link_to_id(register, [for(Kind)], HREF)
	},
	html({|html(HREF, Kind)||
	      <div class="post-login">
	      <a href="HREF">request permission</a> to add a new
	      <span>Kind</span> post.
	      </div>
	     |}).
login_post(Kind) -->
	html(div(class='post-login',
		 [b(\login_link),' to add a new ',Kind,' post.'])).

%%	write_post_js(+Kind, +About)//
%
%	Emit JavaScript to manage posts.

write_post_js(Kind, About) -->
	{ kind_handler(Kind, HandlerId),
	  http_link_to_id(HandlerId, path_postfix(''), URL),
	  http_link_to_id(vote, [], VoteURL)
	},
	html_requires(js('markitup/sets/pldoc/set.js')),
	html_requires(js('post.js')),
	js_script({|javascript(URL,VoteURL,About)||
		   $(document).ready(function() {
		      prepare_post(URL, VoteURL, About);
		   });
		  |}).


		 /*******************************
		 *	      VOTING		*
		 *******************************/

%%	vote(+Request)
%
%	HTTP POST handler for handling a vote.   The  posted object is a
%	JSON object containing the post it and vote.
%
%	Returns a JSON object holding the current number of votes.

vote(Request) :-
	site_user_logged_in(User), !,	% any logged in user can vote
	catch(( memberchk(method(post), Request),
		http_read_json_dict(Request, Dict),
		atom_string(Id, Dict.id),
		vote(Id, User, Dict.vote)
	      ), E,
	      throw(http_reply(bad_request(E)))),
	post(Id, votes, Votes),
	reply_json(_{votes:Votes}).
vote(Request) :-
	memberchk(path(Path), Request),
	throw(http_reply(forbidden(Path))).

%%	vote(+PostId, +User, +Vote) is det.
%
%	Add a vote for PostId.

vote(Post, User, Vote) :-
	must_be(oneof([-1,1]), Vote),
	(   post(Post, _)
	->  true
	;   existence_error(post, Post)
	),
	(   post(Post, author, User)
	->  throw(error(permission_error(vote, post, Post),
			context(_, 'Author cannot vote')))
	;   true
	),
	(   findall(Old, vote(Post, Old, User, _), Votes),
	    sum_list([Vote|Votes], Sum),
	    memberchk(Sum, [-1,0,1])
	->  get_time(NowF),
	    Now is integer(NowF),
	    assert_vote(Post, Vote, User, Now),
	    post(Post, about, About),
	    notify(About, voted(User, Post, Vote))
	;   vote(Post, Vote, User, Time0),
	    get_time(Now),
	    Now - Time0 < 10		% double click or similar
	;   throw(error(permission_error(vote, post, Post),
			context(_, 'Already voted')))
	).


%%	vote(?PostId, ?Vote) is nondet.
%%	vote_up(?PostId, ?Vote) is nondet.
%%	vote_down(?PostId, ?Vote) is nondet.
%
%	True when PostId has been voted with   Vote. Vote is either 1 or
%	-1. The predicates vote_up/2 and vote_down/2   only  count up or
%	down votes.

vote(PostId, Vote) :-
	vote(PostId, Vote, _By, _Time).

vote_up(Post, Vote) :-
	vote(Post, Vote), Vote > 0.

vote_down(Post, Vote) :-
	vote(Post, Vote), Vote < 0.

%%	user_vote_count(+User, -Up, -Down) is det.
%
%	Number of votes issued by this user.

user_vote_count(User, Up, Down) :-
	findall(Vote, vote(_, Vote, User, _), Votes),
	partition(positive, Votes, UpList, DownList),
	sum_list(UpList, Up),
	sum_list(DownList, Down).

positive(Vote) :-
	Vote > 0.


		 /*******************************
		 *	  PROFILE SUPPORT	*
		 *******************************/

%%	user_posts(+User, +Kind)//
%
%	Show posts from a specific user of the specified Kind.

user_posts(User, Kind) -->
	{ find_posts(Kind, user_post(User), Ids),
	  Ids \== [], !,
	  sort_posts(Ids, SortedIds),
	  site_user_property(User, name(Name))
	},
	html([ \html_requires(css('annotation.css')),
	       h2(class(wiki), \posts_title(Kind, Name)),
	       table(class('user-comments'),
		     \list_post_summaries(SortedIds))
	     ]).
user_posts(_, _) -->
	[].

user_post(User, Id) :-
	post(Id, author, User).

posts_title(news, Name) -->
	html(['News articles by ', Name]).
posts_title(annotation, Name) -->
	html(['Comments by ', Name]).


list_post_summaries([]) --> [].
list_post_summaries([H|T]) -->		% annotation
	{ post(H, object, Object), !,
	  post(H, content, Comment)
	},
	html(tr([ td(\object_ref(Object, [])),
		  td(class('comment-summary'),
		     \comment_summary(Comment))
		])),
	list_post_summaries(T).
list_post_summaries([H|T]) -->		% news article
	{ post(H, content, Comment)
	},
	html(tr([ td(class('comment-summary'),
		     [ \post_link(H), ' -- ',
		       \comment_summary(Comment)
		     ] )
		])),
	list_post_summaries(T).

%%	comment_summary(+Comment)//
%
%	Show the first sentence or max first 80 characters of Comment.

comment_summary(Comment) -->
	{ summary_sentence(Comment, Summary) },
	html(Summary).

summary_sentence(Comment, Summary):-
	atom_codes(Comment, Codes),
	phrase(summary(SummaryCodes, 80), Codes, _),
	atom_codes(Summary, SummaryCodes).

summary([C,End], _) -->
	[C,End],
	{ \+ code_type(C, period),
	  code_type(End, period) % ., !, ?
	},
	white, !.
summary([0' |T0], Max) -->
	blank, !,
	blanks,
	{Left is Max-1},
	summary(T0, Left).
summary(Elipsis, 0) --> !,
	{ string_codes(" ...", Elipsis)
	}.
summary([H|T0], Max) -->
	[H], !,
	{Left is Max-1},
	summary(T0, Left).
summary([], _) -->
	[].

%%	user_post_count(+User, +Kind, -Count) is det.
%
%	True when Count is the number of posts of Kind created by User.

user_post_count(User, Kind, Count) :-
	find_posts(Kind, user_post(User), Annotations),
	length(Annotations, Count).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	mail_notify:event_subject//1,		% +Event
	mail_notify:event_message//1.		% +event

mail_notify:event_subject(post_created(Post)) -->
	[ 'Comment by '-[] ],
	msg_user(Post.meta.author).
mail_notify:event_subject(post_deleted(Post)) -->
	[ 'Comment removed by '-[] ],
	msg_user(Post.meta.author).
mail_notify:event_subject(post_updated(_OldPost, NewPost)) -->
	[ 'Comment updated by '-[] ],
	msg_user(NewPost.meta.author).
mail_notify:event_subject(voted(User, _PostId, Vote)) -->
	{ updown(Vote, UpDown) },
	[ 'Voted ~w by '-[UpDown] ],
	msg_user(User).

mail_notify:event_message(post_created(Post)) -->
	[ 'Comment by '-[] ],
	msg_user(Post.meta.author), [nl],
	msg_body(Post.content).
mail_notify:event_message(post_deleted(Post)) -->
	[ 'Comment removed by '-[] ],
	msg_user(Post.meta.author), [nl],
	msg_body(Post.content).
mail_notify:event_message(post_updated(_OldPost, NewPost)) -->
	[ 'Comment updated by '-[] ],
	msg_user(NewPost.meta.author), [nl],
	msg_body(NewPost.content).
mail_notify:event_message(voted(User, PostId, Vote)) -->
	{ updown(Vote, UpDown) },
	[ '~w by '-[UpDown] ],
	msg_user(User),
	[ 'For'-[] ],
	{ post(PostId, content, Content) },
	msg_body(Content).

msg_body(Body) -->
	[ nl,
	  '~w'-[Body],
	  nl
	].

updown(N, Atom) :-
	N > 0, !,
	format(atom(Atom), '+~d', [N]).
updown(Vote, Vote).

