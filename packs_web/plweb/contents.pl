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

:- module(man_contents,
	  []).
:- use_module(library(pldoc/doc_man)).
:- use_module(library(pldoc/doc_html)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(contents),	contents, []).

/** <module> Documentation contents as a hierarchy
*/

contents(_Request) :-
	reply_html_page(
	    pldoc(tree),
	    title('SWI-Prolog manual'),
	    html(\contents([ secref_style(title)
			   ]))).

%%	contents(+Options)//
%
%	Emit a =ul= hierarchy  of  the   entire  manual,  including  the
%	packages.

contents(Options) -->
	html(ul([ \refman_contents(swi('doc/Manual'), Options),
		  \package_contents(Options)
		])).

refman_contents(Dir, Options) -->
	{ man_content_tree(Dir, Tree)
	},
	man_tree(Tree, Options).

package_contents(Options) -->
	{ man_packages_tree(Tree)
	},
	man_tree(Tree, Options).

man_tree(node(Obj, Children), Options) -->
	html(li(\contents_link(Obj, Options))),
	(   { Children == [] }
	->  []
	;   html(ul(\man_children(Children, Options)))
	).
man_tree(Obj, Options) -->
	html(li(\contents_link(Obj, Options))).

man_children([], _) --> [].
man_children([H|T], Options) --> man_tree(H, Options), man_children(T, Options).

contents_link(manual, _) --> !,
	{ http_link_to_id(pldoc_root, [], HREF) },
	html(a(href(HREF), 'Base system')).
contents_link(packages, _) --> !,
	{ http_link_to_id(pldoc_index, [], HREF) }, % wrong link
	html(a(href(HREF), 'Packages')).
contents_link(Obj, Options) -->
	object_ref(Obj, Options).
