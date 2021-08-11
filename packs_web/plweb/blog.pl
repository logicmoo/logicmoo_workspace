/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (C): 2020, SWI-Prolog Solutions b.v.

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
    invalexpandidate any other reasons why the executable file might be
    covered by the GNU General Public License.
*/

:- module(blog,
          []).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(debug)).
:- use_module(library(yaml)).
:- use_module(library(dcg/high_order)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(git)).
:- use_module(library(option)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_host)).
:- use_module(library(http/js_write)).
:- use_module(library(uri)).

:- use_module(wiki).
:- use_module(messages).
:- use_module(fastly).
:- use_module(parms).

:- http_handler(root(blog), blog, [prefix, id(blog)]).

user:file_search_path(blog, blog).

:- html_resource(pldoc_blog,
		 [ ordered(true),
                   requires([ jquery,
                              js('blog.js')
			    ]),
		   virtual(true)
		 ]).
:- html_resource(css('blog.css'), []).


blog(Request) :-
    memberchk(path_info(PathInfo), Request),
    PathInfo \== '/',
    !,
    debug(blog, 'Path info ~p', [PathInfo]),
    atom_concat(/, File, PathInfo),
    safe_file_name(File),
    absolute_file_name(blog(File), Path,
                       [ access(read)
                       ]),
    wiki_file_to_dom(Path, DOM0),
    extract_title(DOM0, Title, DOM1),
    append(DOM1, [\discourse(Request)], DOM),
    title_text(Title, TitleString),
    http_link_to_id(blog, [], HREF),
    reply_html_page(
        blog(Path, [a(href(HREF), 'Blog'), ': ' | Title]),
        [ title(TitleString)
        ],
        DOM).
blog(_Request) :-
    blog_index(Blogs),
    reply_html_page(
        blog(index),
        [ title("SWI-Prolog blog")
        ],
        \blog_index_page(Blogs)).

blog_index_page(Blogs) -->
    html_requires(pldoc_blog),
    html_requires(css('blog.css')),
    blog_index_title,
    blog_tags(Blogs),
    blog_index(Blogs).

blog_index_title -->
    html({|html||
<p>
The SWI-Prolog blog is intended for articles on how to tackle certain problems
using SWI-Prolog, experience using SWI-Prolog for larger projects, etc.  Posts
can be submitted as pull-requests on
<a href="https://github.com/SWI-Prolog/plweb-blog">GitHub</a>.
         |}).


%!  blog_tags(+Blogs)//

blog_tags(Blogs) -->
    { blog_tag_counts(Blogs, Counts) },
    html(div(class('blog-tags'), \sequence(tag, [' '], Counts))).

tag(Tag-Count) -->
    html(span([ class('blog-tag'),
                'data-tag'(Tag)
              ],
              [ span(class('blog-tag-tag'), Tag),
                span(class('blog-tag-cnt'), Count)
              ])).

blog_tag_counts(Blogs, Pairs) :-
    convlist(get_dict(tags), Blogs, Tags),
    flatten(Tags, TagList0),
    msort(TagList0, TagList),
    clumped(TagList, Pairs0),
    sort(2, >=, Pairs0, Pairs).

blog_index(Index) :-
    absolute_file_name(blog('Index.yaml'), File,
                       [ access(read)
                       ]),
    yaml_read(File, Index0),
    sort(date, @>=, Index0.posts, Index).

blog_index(Blogs) -->
    { map_list_to_pairs(key_blog_year, Blogs, Tagged),
      group_pairs_by_key(Tagged, ByYear)
    },
    html(div(class('blog-index'),
             \sequence(blog_year, ByYear))).

key_blog_year(Blog, Year) :-
    split_string(Blog.get(date), "-", "", [YS|_]),
    number_string(Year, YS).

blog_year(Year-Blogs) -->
    html(div(class('blog-year-index'),
             [ div(class('blog-year'), Year),
               div(class('blog-year-entries'),
                   \sequence(blog_index_entry, Blogs))
             ])).

blog_index_entry(Blog) -->
    { atomics_to_string(Blog.get(tags,[]),"|",Tags),
      http_link_to_id(blog, path_postfix(Blog.file), HREF)
    },
    html(a([ class('blog-index-entry'),
             'data-tags'(Tags),
             href(HREF)
           ],
           [ \block_date(Blog),
             \block_title(Blog)
           ])).

block_date(Blog) -->
    optional(html(span(class('blog-index-date'),Blog.get(date))), []).
block_title(Blog) -->
    optional(html(span(class('blog-index-title'),Blog.get(title))), []).


		 /*******************************
		 *            DISCOURSE		*
		 *******************************/

discourse(Request) -->
    { cdn_url(Request, URL) },
    html(div(id('discourse-comments'), [])),
    js_script({|javascript(URL)||
window.DiscourseEmbed = { discourseUrl: 'https://swi-prolog.discourse.group/',
                   discourseEmbedUrl: URL };

(function() {
  var d = document.createElement('script'); d.type = 'text/javascript'; d.async = true;
  d.src = window.DiscourseEmbed.discourseUrl + 'javascripts/embed.js';
  (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(d);
})();
|}).


cdn_url(Request, CDNURL) :-
    memberchk(request_uri(ReqURL), Request),
    server(cdn, CDN, _),
    format(atom(CDNURL), 'https://~w~w', [CDN, ReqURL]).


		 /*******************************
		 *            UPDATE		*
		 *******************************/

%!  pull_blogs
%
%   Do a git pull on the blog repo

pull_blogs :-
    (   absolute_file_name(blog(.), BlogDir,
                           [ file_type(directory),
                             access(write),
                             solutions(all)
                           ]),
        is_git_directory(BlogDir),
        git([pull], [directory(BlogDir)]),
        fail
    ;   purge_location('/blog')
    ).


		 /*******************************
		 *             HTTP		*
		 *******************************/

:- http_handler(root(blog/pull), pull_blogs, []).

pull_blogs(Request) :-
    (   option(method(post), Request)
    ->  http_read_json(Request, JSON),
        print_message(informational, got(JSON))
    ;   true
    ),
    call_showing_messages(pull_blogs, []).
