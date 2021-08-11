/*  Part of SWI-Prolog

    Author:        Wouter Beek & Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013-2014, VU University Amsterdam

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

:- module(news,
	  [ random_news//0
	  ]).
:- use_module(generics).
:- use_module(library(aggregate)).
:- use_module(library(random)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(pldoc/doc_html)).
:- use_module(post).

:- html_resource(css('news.css'), [requires([css('post.css')])]).

:- http_handler(root(news), news_process, [prefix]).
:- http_handler(root(news/archive), news_archive, []).

/** <module> News on the SWI-Prolog Web site

@author Wouter Beek
@tbd Calculate relevance based on freshness lifetime and importance.
@tbd User-specific influencing of relevance. Based on login/based on cookies.
@version 2013/12
*/




%%	news_process(+Request)
%
%	HTTP handler for /news/.  Distinguishes three cases:
%
%	  1. GET to /news/<Kind>/<Id> to render a single news item.
%	  2. GET to /news/ to render the _fresh_ news.
%	  3. POST provides a REST API for managing news.

news_process(Request) :-			% list specific article
	memberchk(method(get), Request),
	request_to_id(Request, news, Post),
	Post \== '', !,
	post(Post, title, Title1),
	post(Post, kind, Kind),
	(   post(Post, object, Object)
	->  true
	;   Object = null
	),
	atomic_list_concat(['News',Title1], ' -- ', Title2),
	reply_html_page(
	    news(Post),
	    title(Title2),
	    [ \post(Post, []),
	      \news_backlink(Kind, Object)
	    ]).
news_process(Request) :-			% list fresh news
	memberchk(method(get), Request), !,
	find_posts(news, fresh, Ids),
	Title = 'News',
	reply_html_page(
	    news(fresh),
	    title(Title),
	    [ \html_requires(css('news.css')),
	      \posts(news, null, Ids,
		     [ order_by(created),
		       add_add_link(false)
		     ]),
	      \news_archive_link(news, Ids),
	      \add_post_link(news, null)
	    ]).
news_process(Request) :-			% handle editing news
	post_process(Request, news).

news_archive_link(Kind, Ids) -->
	{ find_posts(Kind, all, All),
	  length(All, Total)
	},
	(   { length(Ids, Total) }
	->  []
	;   { http_link_to_id(news_archive, [], HREF)
	    },
	    html(div(class('news-archive-link'),
		     a(href(HREF), 'View all ~D news articles'-[Total])))
	).


%%	news_archive(+Request) is det.
%
%	Show all available news.

news_archive(_Request):-
	find_posts(news, all, Ids),

	reply_html_page(
	    news(all),
	    title('News archive'),
	    [ \posts(news, null, Ids,
		     [ order_by(created),
		       add_add_link(false)
		     ]),
	      \news_backlink(news, null),
	      \add_post_link(news, null)
	    ]).

news_backlink(news, _Object) --> !,
	{ http_link_to_id(news_process, [], Link) },
	html(a(href=Link, 'Back to fresh news items')).
news_backlink(_Kind, Object) -->
	html('View annotation in context of '),
	object_ref(Object, [style(title)]).

%!	random_news// is semidet.
%
%	Emit a random news item for the Did You Know place of the page.
%	Fails if there is no news.

random_news -->
	{ random_new_item(Id, Title),
	  http_link_to_id(news_process, path_postfix(Id), Link)
	},
	html([ span(class(lbl), 'News: '),
	       span(id(dyknow), a(href=Link, Title))
	     ]).

%% random_new_item(-Id:atom, -Title:atom) is det.

random_new_item(Id, Title):-
	aggregate_all(
	    sum(Relevance),
	    ( post(Id, kind, news),
	      relevance(Id, Relevance)
	    ),
	    SummedRelevance),
	random(0.0, SummedRelevance, R),
	find_posts(news, fresh, Ids),
	random_new_item(0.0, R, Ids, Id, Title).

random_new_item(_V, _R, [Id], Id, Title):- !,
	post(Id, title, Title).
random_new_item(V1, R, [Id|_], Id, Title):-
	relevance(Id, Relevance),
	V2 is V1 + Relevance,
	R =< V2, !,
	post(Id, title, Title).
random_new_item(V1, R, [Id0|Ids], Id, Title):-
	relevance(Id0, Relevance),
	V2 is V1 + Relevance,
	random_new_item(V2, R, Ids, Id, Title).

