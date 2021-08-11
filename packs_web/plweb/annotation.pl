/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
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

:- module(annotation,
	  [ annotation//1			% +object:compound
	  ]).

/** <module> Annotation

@author Wouter Beek
@tbd Build annotation2post converter.
@version 2014/01
*/

:- use_module(generics).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(pldoc/doc_html), [object_ref//2]).
:- use_module(object_support).
:- use_module(post).

:- html_resource(css('annotation.css'),
		 [ requires([css('post.css')])
		 ]).

:- multifile
	prolog:doc_object_page_footer/2.

:- http_handler(root(annotation), annotation_process, [prefix]).

%%	annotation_process(+Request)
%
%	REST HTTP handler for /annotation/ID
%
%	@tbd	Where is this used for?  This also seems to do a request
%		on ourselves.  We'd like to avoid that.

annotation_process(Request):-
	memberchk(method(get), Request),
	request_to_id(Request, annotation, Post), !,
	post(Post, id, Id),
	post(Post, about, Object),
	object_label(Object, Label),
	atomic_list_concat(['Annotation',Label], '--', Title),
	reply_html_page(
	    wiki(Title),
	    title(Title),
	    \post(Id, [])).
annotation_process(Request):-
	post_process(Request, annotation).

%%	annotation(+Object)//
%
%	Show annotations for Object.

annotation(Object) -->
	{ ground(Object), !,
	  (   prolog:doc_canonical_object(Object, Object2)
	  ->  true
	  ;   Object2 = Object
	  ),
	  find_posts(annotation, object_post(Object2), Ids)
	},
	html([\html_requires(css('annotation.css')),
	      \posts(annotation, Object2, Ids, [])
	     ]).
annotation(_) --> [].

object_post(About, Id) :-
	post(Id, object, About).

