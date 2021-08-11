/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013, VU University Amsterdam

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


:- module(tagit,
	  [ user_tags//2,		% +User, +Options
	    user_tag_count/2,		% +User, -Count
	    tagit_footer//2		% +Object, +Options
	  ]).
:- use_module(generics).
:- use_module(library(debug)).
:- use_module(library(persistency)).
:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(dcg/basics)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(pldoc/doc_search)).
:- use_module(library(pldoc/doc_html)).
:- use_module(notify).
:- use_module(object_support).
:- use_module(openid).

:- html_resource(tagit,
		 [ ordered(true),
		   requires([ jquery_ui,
			      js('tagit/js/tag-it.min.js'),
			      js('tagit/css/jquery.tagit.css'),
			      js('tagit/css/tagit.ui-zendesk.css')
			    ]),
		   virtual(true)
		 ]).
:- html_resource(css('tags.css'), []).


		 /*******************************
		 *	       DATA		*
		 *******************************/

:- persistent
	tagged(tag:atom,			% Name of the tag
	       object:any,			% Object attached to
	       time:integer,			% When was it tagged
	       user:atom),			% User that added the tag
	tag(tag:atom,
	    time:integer,			% When was it created
	    user:atom).

user_tag_count(User, Count) :-
	aggregate_all(count, tagged(_,_,_,User), Count).


:- initialization
	db_attach('tags.db',
		  [ sync(close)
		  ]).

current_tag(Tag) :-
	tag(Tag, _, _).

create_tag(Tag, _User) :-
	tag(Tag, _, _), !.
create_tag(Tag, User) :-
	get_time(NowF),
	Now is round(NowF),
	assert_tag(Tag, Now, User), !.


%%	tagit_user(+Request, -Type, -User) is det.
%
%	User as seen for tagging. This is either the current user or the
%	peer.

tagit_user(_Request, uuid, User) :-
	site_user_logged_in(User), !.
tagit_user(Request, ip, Peer) :-
	http_peer(Request, Peer).

peer(Peer) :-
	atom_codes(Peer, Codes),
	phrase(ip, Codes).

ip -->
	integer(_), ".",
	integer(_), ".",
	integer(_), ".",
	integer(_).


		 /*******************************
		 *	 PROLOG BINDING		*
		 *******************************/

:- http_handler(root('complete-tag'), complete_tag, []).
:- http_handler(root('show-tag'),     show_tag,	    []).
:- http_handler(root('add-tag'),      add_tag,	    []).
:- http_handler(root('remove-tag'),   remove_tag,   []).
:- http_handler(root('list-tags'),    list_tags,    []).
:- http_handler(root('tag-abuse'),    tag_abuse,    []).

%%	tagit_footer(+Obj, +Options)// is det.
%
%	Show tagit widget for adding and deleting tags.

tagit_footer(Obj, _Options) -->
	{ http_link_to_id(complete_tag, [], Complete),
	  http_link_to_id(show_tag, [], OnClick),
	  http_link_to_id(add_tag, [], AddTag),
	  http_link_to_id(remove_tag, [], RemoveTag),
	  object_label(Obj, Label),
	  object_id(Obj, ObjectID),
	  format(atom(PlaceHolder), 'Tag ~w', [Label]),
	  object_tags(Obj, Tags)
	},
	html(div(id='tags-component',
		 [ \tag_notes(ObjectID, Tags),
		   div(id='tags-label', 'Tags:'),
		   div(id='tags-bar', ul(id=tags, \tags_li(Tags))),
		   div(id='tags-warnings', [])
		 ])),
	html_requires(css('tags.css')),
	html_requires(tagit),
	js_script({|javascript(Complete, OnClick, PlaceHolder, ObjectID,
			       AddTag, RemoveTag)||
		    function tagInfo(text) {
		      $("#tags-warnings").text(text);
		      $("#tags-warnings").removeClass("warning");
		      $("#tags-warnings").addClass("informational");
		    }
		    function tagWarning(text) {
		      $("#tags-warnings").text(text);
		      $("#tags-warnings").addClass("warning");
		      $("#tags-warnings").removeClass("informational");
		    }

		    $(document).ready(function() {
		      $("#tags").tagit({
			  autocomplete: { delay: 0.3,
					  minLength: 1,
					  source: Complete
					},
			  onTagClicked: function(event, ui) {
			    window.location.href = OnClick+"?tag="+
			      encodeURIComponent(ui.tagLabel);
			  },
			  beforeTagAdded: function(event, ui) {
			    if ( !ui.duringInitialization ) {
			      var result = false;
			      tagInfo("Submitting ...");
			      $.ajax({ dataType: "json",
				       url: AddTag,
				       data: { tag: ui.tagLabel,
					       obj: ObjectID
					     },
				       async: false,
				       success: function(data) {
					if ( data.status == true ) {
					  tagInfo("Added: "+ui.tagLabel);
					  result = true;
					} else {
					  tagWarning(data.message);
					}
				      }
				     });
			      return result;
			    }
			  },
			  beforeTagRemoved: function(event, ui) {
			    var result = false;
			    if ( !ui.tagLabel ) {
			      return false;
			    }
			    tagInfo("Submitting ...");
			    $.ajax({ dataType: "json",
				     url: RemoveTag,
				     data: { tag: ui.tagLabel,
					     obj: ObjectID
					   },
				     async: false,
				     success: function(data) {
					if ( data.status == true ) {
					  tagInfo("Removed: "+ui.tagLabel);
					  result = true;
					} else {
					  tagWarning(data.message);
					}
				      }
				   });
			    return result;
			  },
			  placeholderText: PlaceHolder
			});
		      });
		  |}).

tags_li([]) --> [].
tags_li([H|T]) --> html(li(H)), tags_li(T).

tag_notes(ObjectID, Tags) -->
	html(div(id='tags-notes',
		 [ \docs_need_work_plea,
		   \why_login,
		   \abuse_link(ObjectID, Tags)
		 ])).

docs_need_work_plea -->
	html(['Tag confusing pages with ', b('doc-needs-help')]).

abuse_link(_, []) --> [].
abuse_link(ObjectID, _) -->
	sep,
	{ http_link_to_id(tag_abuse, [obj=ObjectID], HREF)
	},
	html(a(href(HREF), 'Report abuse')).

why_login -->
	{ site_user_logged_in(_) }, !.
why_login -->
	sep,
	html('Tags are associated to your profile if you are logged in').

sep -->
	html(span(class(separator), '|')).

object_tags(Object, Tags) :-
	findall(Tag, tagged(Tag, Object, _Time, _User), Tags0),
	sort(Tags0, Tags).

%%	complete_tag(+Request)
%
%	Complete.  Currently only uses existing tags for completion.
%
%	@tbd	Provide pre-populated completion (e.g., from FOLDOC)
%	@tbd	Show (as feedback) how often this is used, etc.

complete_tag(Request) :-
	http_parameters(Request,
			[ term(Q, [])
			]),
	debug(tag(autocomplete), 'Autocomplete ~q', [Q]),
	(   setof(A, tag_holding(Q,A), List)
	->  true
	;   List = []
	),
	reply_json(List).

tag_holding(Term, Tag) :-
	current_tag(Tag),
	(   sub_atom(Tag, _, _, _, Term)
	->  true
	).

%%	add_tag(+Request)
%
%	Add tag to the given object

add_tag(Request) :-
	http_parameters(Request,
			[ tag(Tag, []),
			  obj(Hash, [])
			]),
	object_id(Object, Hash),
	tagit_user(Request, UserType, User),
	debug(tagit, 'add_tag: ~q: ~q to ~q', [User, Tag, Object]),
	add_tag_validate(Tag, Object, UserType, Message),
	(   var(Message)
	->  create_tag(Tag, User),
	    get_time(NowF),
	    Now is round(NowF),
	    assert_tagged(Tag, Object, Now, User),
	    notify(Object, tagged(Tag)),
	    reply_json_dict(json{status:true})
	;   reply_json_dict(json{status:false,
			         message:Message})
	).

add_tag_validate(Tag, _Object, _UserType, Message) :-
	tag_not_ok(Tag, Message), !.
add_tag_validate(Tag, Object, _UserType, Message) :-
	object_label(Object, Label),
	sub_atom_icasechk(Label, _, Tag), !,
	Message = 'Rejected: tag is part of object name'.
add_tag_validate(Tag, _Object, UserType, Message) :-
%	\+ tag(Tag, _, _),
	tag_create_not_ok(Tag, UserType, Message), !.
add_tag_validate(_, _, _, _).

tag_not_ok(Tag, Message) :-
	sub_atom(Tag, _, 1, _, Char),
	\+ tag_char_ok(Char), !,
	format(atom(Message), 'Illegal character: ~w', [Char]).

tag_char_ok(Char) :- char_type(Char, alnum).
tag_char_ok('_').
tag_char_ok('-').
tag_char_ok('/').
tag_char_ok('(').
tag_char_ok(')').

%tag_create_not_ok(_, ip, 'Not logged-in users can only use existing tags').
tag_create_not_ok(_, ip, 'Not logged-in users can not add tags').


%%	remove_tag(+Request)
%
%	Remove tag from the given object

remove_tag(Request) :-
	http_parameters(Request,
			[ tag(Tag, []),
			  obj(Hash, [])
			]),
	object_id(Object, Hash),
	tagit_user(Request, _, User),
	debug(tagit, 'remove_tag: ~q: ~q to ~q', [User, Tag, Object]),
	tagged(Tag, Object, _, Creator),
	(   may_remove(User, Creator)
	->  (   retract_tagged(Tag, Object, _, Creator),
	        gc_tag(Tag)
	    ->  notify(Object, untagged(Tag)),
		reply_json(json{status:true})
	    ;   reply_json(json{status:false,
				message:"Unknown error"
			       })
	    )
	;   reply_json(json{status:false,
			    message:"Permission denied"
			   })
	).

%%	may_remove(+CurrentUser, +Creator)

may_remove(User, User) :- !.
may_remove(User, _Anonymous) :-
	site_user_property(User, granted(admin)).

%%	gc_tag(+Tag)
%
%	Remove tag if it is no longer in use.

gc_tag(Tag) :-
	tagged(Tag, _, _, _), !.
gc_tag(Tag) :-
	retract_tag(Tag, _, _).

gc_tags :-
	forall(tag(Tag,_,_),
	       gc_tag(Tag)).

%%	show_tag(+Request)
%
%	Show pages that are tagged with this tag.

show_tag(Request) :-
	http_parameters(Request,
			[ tag(Tag, [])
			]),
	findall(Obj, tagged(Tag, Obj, _, _), Objects0),
	sort(Objects0, Objects),
	reply_html_page(wiki(tags),
			title('Pages tagged "~w"'-[Tag]),
			[ h1(class(wiki), 'Pages tagged "~w"'-[Tag]),
			  \doc_resources([]),
			  \matching_object_table(Objects, [])
			]).

%%	tag_abuse(+Request)
%
%	Some user claims that the tag is abused.

tag_abuse(Request) :-
	site_user_logged_in(_), !,
	http_parameters(Request,
			[ obj(Hash, [])
			]),
	object_id(Object, Hash),
	Link = \object_ref(Object,[]),
	tagit_user(Request, uuid, _User),
	notify(Object, tag_abuse),
	reply_html_page(
	    wiki(tags),
	    title('Notification of abuse'),
	    {|html(Link)||
	     <h1 class="wiki">Notification of abuse sent</h1>
	     <p>
	     Thanks for reporting abuse of tagging on documentation object
	     <span>Link</span>.
	     |}).
tag_abuse(Request) :-
	memberchk(path(Path), Request),
	permission_error(access, http_location, Path).



		 /*******************************
		 *   AUTOCOMPLETE INTEGRATION	*
		 *******************************/

:- multifile
	prolog:ac_object/3,
	prolog:doc_object_href/2,		% +Object, -HREF
	prolog:doc_object_label_class/3,
	prolog:ac_object_attributes/2.

%%	prolog:ac_object(+MatchHow, +Term, -Match) is nondet.
%
%	Provide additional autocompletion matches on tags,
%

prolog:ac_object(name, Term, Tag-tag(Tag)) :-
	current_tag(Tag),
	(   sub_atom_icasechk(Tag, 0, Term),
	    tagged(Tag, _, _, _)
	->  true
	).
prolog:ac_object(token, Term, Tag-tag(Tag)) :-
	current_tag(Tag),
	(   sub_atom_icasechk(Tag, _, Term),
	    tagged(Tag, _, _, _)
	->  true
	).

prolog:doc_object_href(tag(Tag), HREF) :-
	http_link_to_id(show_tag, [tag(Tag)], HREF).

prolog:doc_object_label_class(tag(Tag), Tag, tag).

prolog:ac_object_attributes(tag(Tag), [tag=Info]) :-
	aggregate_all(count, tagged(Tag,_,_,_), Used),
	format(atom(Info), 'tag x~D', [Used]).


		 /*******************************
		 *	     LIST TAGS		*
		 *******************************/

%%	list_tags(+Request)
%
%	HTTP handler that lists the defined tags.

list_tags(Request) :-
	http_parameters(Request,
			[ sort_by(SortBy, [ oneof([ name,
						    popularity,
						    time
						  ]),
					    default(name)
					  ])
			]),
	reply_html_page(
	    tags(list),
	    title('Overview of tags'),
	    \user_tags(_, [sort_by(SortBy)])).


%%	user_tags(?User, +Options)// is det.
%
%	Show all tags created by a given user.

user_tags(User, Options) -->
	{ findall(Tag-tag(Obj,Time), tagged(Tag, Obj, Time, User), Pairs),
	  Pairs \== [], !,
	  keysort(Pairs, Sorted),
	  group_pairs_by_key(Sorted, Keyed),
	  option(sort_by(SortBy), Options, name),
	  sort_tags(Keyed, SortedTags, SortBy)
	},
	html([ \tag_list_header(User, SortBy),
	       table(class('user-tags'),
		     \list_tags(SortedTags))
	     ]).
user_tags(_, _) --> [].

tag_list_header(User, _SortBy) -->
	{ nonvar(User),
	  site_user_property(User, name(Name))
	}, !,
	html(h2(class(wiki), 'Tags by ~w'-[Name])).
tag_list_header(_User, SortBy) -->
	html(h2(class(wiki), 'Tags sorted by ~w'-[SortBy])).

sort_tags(Tags, Tags, name) :- !.
sort_tags(Tags, Sorted, SortBy) :-
	map_list_to_pairs(sort_key_tag(SortBy),	Tags, Keyed),
	keysort(Keyed, KeySorted),
	pairs_values(KeySorted, Sorted).

sort_key_tag(name,       Tag-_, Tag).
sort_key_tag(popularity, _-Tagged, Count) :-
	length(Tagged, Count).
sort_key_tag(time,	 _-Tagged, Last) :-
	maplist(arg(2), Tagged, Times),
	max_list(Times, Last).

%%	list_tags(+Tags)
%
%	List tags and what they are linked to.

list_tags([]) --> [].
list_tags([H|T]) --> list_tag(H), list_tags(T).

list_tag(Tag-Objects) -->
	{ http_link_to_id(show_tag, [tag(Tag)], HREF)
	},
	html(tr([td(a([class(tag),href(HREF)], Tag)),
		 td(\objects(Objects))
		])).

objects([]) --> [].
objects([tag(Obj,_Time)|T]) -->
	object_ref(Obj, []),
	(   { T == [] }
	->  []
	;   html(', '),
	    objects(T)
	).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	mail_notify:event_subject//1,		% +Event
	mail_notify:event_message//1.		% +event

mail_notify:event_subject(tagged(Tag)) -->
	[ 'tagged with ~w'-[Tag] ].
mail_notify:event_subject(untagged(Tag)) -->
	[ 'removed tag ~w'-[Tag] ].
mail_notify:event_subject(tag_abuse) -->
	[ 'tag abuse'-[] ].


mail_notify:event_message(tagged(Tag)) -->
	[ 'tagged with "~w"'-[Tag] ].
mail_notify:event_message(untagged(Tag)) -->
	[ 'removed tag "~w"'-[Tag] ].
mail_notify:event_message(tag_abuse) -->
	[ 'tag abuse'-[] ].
