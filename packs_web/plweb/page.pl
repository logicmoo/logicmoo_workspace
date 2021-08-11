/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2017, VU University Amsterdam

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

:- module(plweb_page, []).
:- use_module(footer).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(pldoc/doc_search)).
:- use_module(library(http/js_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(pldoc/doc_html), [object_name//2]).
:- use_module(library(uri)).
:- use_module(library(option)).

:- use_module(wiki).
:- use_module(post).
:- use_module(openid).
:- use_module(did_you_know).
:- use_module(holidays).

:- html_meta
	outer_container(html, +, ?, ?).

:- http_handler(root(search), plweb_search, []).

%%	user:body(+Style, +Body)//
%
%	Provide the page skin.

:- multifile
	user:body//2,
	plweb:page_title//1.

user:body(homepage, Body) --> !,
	outer_container([ \tag_line_area,
			  \menubar(homepage),
			  \blurb,
			  \cta_area,
			  \enhanced_search_area,
			  Body
			], []).
user:body(Style, Body) -->
	{ page_style(Style, Options), !,
	  functor(Style, ContentClass, _)
	},
	outer_container(
	    [ \title_area(Style),
	      \menubar(Style),
	      div(class(breadcrumb), []),
	      div(class(['inner-contents', ContentClass]),
		  div([id(contents), class([contents, ContentClass])],
		      Body))
	    ],
	    Options).
user:body(Style, Body) -->
	{ Style = forum(_) }, !,
	outer_container(
	    [ \title_area(Style),
	      \menubar(Style),
	      div(class(breadcrumb), []),
	      Body
	    ],
	    []).
user:body(plain, Body) --> !,
	html(body(class(plain), Body)).
user:body(default, Body) --> !,
	html(body(class(plain), Body)).
user:body(Style, _Body) -->
	html(div('Unknown page style ~q'-[Style])).

%%	page_style(+Style, -Options) is semidet.
%
%	True if Style is an `object page' and Obj is the object.

page_style(user(_Action),	   [show_user(false)]).
page_style(download(_Dir, _Title), []).
page_style(dir_index(_Dir, _Title),[]).
page_style(news(_Which),	   []).
page_style(wiki(_Special),	   []).
page_style(wiki(Path, _Title),	   [object(wiki(Path))]).
page_style(blog(_Special),	   []).
page_style(blog(Path, _Title),	   [object(blog(Path))]).
page_style(pack(_Action),	   []).
page_style(tags(_Action),	   []).
page_style(pldoc(object(Obj)),	   [object(Obj)]) :- !.
page_style(pldoc(search(For)),     [for(For)]) :- !.
page_style(pldoc(_),		   []).
page_style(pack(_Type, _Title),	   []).
page_style(git(_),		   []).

%%	outer_container(+Content, +Options)//
%
%	Display a typical page including headers and footers.

outer_container(Content, Options) -->
	html(body(div(class('outer-container'),
		  [ \html_requires(plweb),
		    \html_requires(swipl_css),
		    \shortcut_icons,
		    \upper_header(Options),
		    Content,
		    div([id(dialog),style('display:none;')], []),
		    div(class([footer, newstyle]), \footer(Options)),
		    div(id('tail-end'), &(nbsp))
		  ]))),
	html_receive(script).


%%	prolog:doc_page_header(+File, +Options)// is det.
%%	prolog:doc_links(+Directory, +Options)// is det.
%%	prolog:doc_file_title(+Title, +File, +Options)// is det.
%
%	Called to render the PlDoc page header   and  link menu. We kill
%	both.

:- multifile
	prolog:doc_page_header//2,
	prolog:doc_links//2.

prolog:doc_page_header(_File, _Options) --> [].
prolog:doc_links(_Directory, _Options) --> [].
prolog:doc_file_title(_Title, _File, _Options) --> [].

shortcut_icons -->
	{ http_absolute_location(icons('favicon.ico'), FavIcon, []),
	  http_absolute_location(root('apple-touch-icon.png'), TouchIcon, [])
	},
	html_post(head,
		  [ link([ rel('shortcut icon'), href(FavIcon) ]),
		    link([ rel('apple-touch-icon'), href(TouchIcon) ])
		  ]).

%%	upper_header(+Options)//
%
%	Emit the small blue header with Did You Know? and search box

upper_header(Options) -->
	{ http_link_to_id(plweb_search, [], Action),
	  option(for(Search), Options, '')
	},
	html(div(id('upper-header'),
		 table(id('upper-header-contents'),
		       tr([ td(id('dyknow-container'),
			       \did_you_know_script('dyknow-container')),
			    td(id('search-container'),
			       [ span(class(lbl), 'Search Documentation:'),
				 form([action(Action),id('search-form')],
				      [ input([ name(for),
						id(for),
						value(Search)
					      ], []),
					input([ id('submit-for'),
						type(submit),
						value('Search')
					      ], []),
					\searchbox_script(for)
				      ])
			       ])
			  ])))).

%%	plweb_search(+Request)
%
%	HTTP Handler to search the Prolog website.

plweb_search(Request) :-
	http_parameters(
	    Request,
	    [ for(For,
		  [ default(''),
		    description('String to search for')
		  ]),
	      in(In,
		 [ oneof([all,app,noapp,man,lib,pack,wiki]),
		   default(noapp),
		   description('Search everying, application only \c
		                or manual only')
		 ]),
	      match(Match,
		    [ oneof([name,summary]),
		      default(summary),
		      description('Match only the name or also the summary')
		    ]),
	      resultFormat(Format,
			   [ oneof(long,summary),
			     default(summary),
			     description('Return full documentation \c
			                  or summary-lines')
			   ]),
	      page(Page,
		   [ integer,
		     default(1),
		     description('Page of search results to view')
		   ])

	    ]),
	format(string(Title), 'Prolog search -- ~w', [For]),
	reply_html_page(pldoc(search(For)),
			title(Title),
			\search_reply(For,
				      [ resultFormat(Format),
					search_in(In),
					search_match(Match),
					header(false),
					private(false),
					edit(false),
					page(Page)
				      ])).


%%	searchbox_script(+Tag)//
%
%	Emits the script tag for the searchbox

searchbox_script(Tag) -->
	html([
	    \html_requires(jquery_ui),
	    script(type('text/javascript'), {|javascript(Tag)||
    $(function() {
	function htmlEncode(text) {
	  if ( !text ) return "";
	  return document.createElement('a')
			 .appendChild(document.createTextNode(text))
			 .parentNode
			 .innerHTML;
	}
        $("#"+Tag).autocomplete({
        minLength: 1,
        delay: 0.3,
        source: "/autocomplete/ac_predicate",
        focus: function(event,ui) {
          $("#"+Tag).val(ui.item.label);
          return false;
        },
        select: function(event,ui) {
          $("#"+Tag).val(ui.item.label);
          window.location.href = ui.item.href;
          return false;
        }
        })
        .data("ui-autocomplete")._renderItem = function(ul,item) {
        var label = String(htmlEncode(item.label)).replace(
            htmlEncode(this.term),
            "<span class=\"acmatch\">"+this.term+"</span>");
        var tag = item.tag ? " <i>["+item.tag+"]</i>" : "";
        return $("<li>")
          .append("<a class=\""+item.class+"\">"+label+tag+"</a>")
          .appendTo(ul)
        };
        });
|})]).

%%	tag_line_area//
%
%	Emit the Owl logo and tagline area (Robust, mature, free. Prolog
%	for the real world)

tag_line_area -->
	html(div(id('tag-line-area'),
		 [ \swi_logo,
		   span(class(tagline),
			[ 'Robust, mature, free. ',
			  b('Prolog for the real world.')
			])
		 ])).

%%	title_area(+Style)
%

title_area(pldoc(file(File, Title))) --> !,
	{ file_base_name(File, Base) },
	html([ table(id('header-line-area'),
		     tr([ td(id('logo'), \swi_logo),
			  td(class('primary-header'),
			     \page_title(title(Title)))
			])),
	       div([ class('file-buttons')
		   ],
		   [ \zoom_button(Base, []),
		     \source_button(Base, [])
		   ])
	     ]).
title_area(Style) -->
	html(table(id('header-line-area'),
		   tr([ td(id('logo'), \swi_logo),
			td(class('primary-header'),
			   \page_title(Style))
		      ]))).

page_title(For) -->
	plweb:page_title(For), !.
page_title(pldoc(search(''))) --> !,
	html('How to use the search box').
page_title(pldoc(search(For))) --> !,
	html(['Search results for ', span(class(for), ['"', For, '"'])]).
page_title(pldoc(object(Obj))) -->
	object_name(Obj,
		    [ style(title)
		    ]), !.
page_title(title(Title)) --> !,
	html(Title).
page_title(user(login)) --> !,
	html('Login to www.swi-prolog.org').
page_title(user(logout)) --> !,
	html('Logged out from www.swi-prolog.org').
page_title(user(create_profile)) --> !,
	html('Create user profile').
page_title(user(view_profile(UUID))) --> !,
	{ site_user_property(UUID, name(Name)) },
	html('Profile for user ~w'-[Name]).
page_title(user(list)) --> !,
	html('Registered site users').
page_title(news(fresh)) --> !,
	html('News').
page_title(news(all)) --> !,
	html('News archive').
page_title(news(Id)) -->
	{ post(Id, title, Title) },
	html(Title).
page_title(pack(list)) -->
	html('Packs (add-ons) for SWI-Prolog').
page_title(wiki(sandbox)) -->
	html('PlDoc wiki sandbox').
page_title(wiki(edit(Action, Location))) -->
	html([Action, ' wiki page ', Location]).
page_title(wiki(_Path, Title)) -->
	html(Title).
page_title(blog(index)) -->
	html('SWI-Prolog blog -- index').
page_title(blog(_Path, Title)) -->
	html(Title).
page_title(tags(list)) -->
	html('Tags').
page_title(download(_Dir, Title)) -->
	html(Title).
page_title(dir_index(_Dir, Title)) -->
	html(Title).
page_title(Term) -->
	html('Title for ~q'-[Term]).


%%	todays_logo(-File:atom, -AltText:atom) is det
%
%	succeeds if File is the relative name of the appropriate
%	version of the swi-Prolog logo for the day
%	and AltText is the alt text
%
todays_logo('christmas.png', 'Merry Christmas.') :-
	todays_holiday(christmas).
todays_logo('koningsdag.png', 'Kings day in the Netherlands') :-
	todays_holiday(koningsdag).
todays_logo('santiklaas.png', 'St. Nicholas\' eve in the Netherlands') :-
	todays_holiday(santiklaas).
todays_logo('carnivalswipl.png', 'Carnival in the Netherlands') :-
	todays_holiday(carnival).
todays_logo('halloween.png', 'Hoooo.... on Halloween') :-
	todays_holiday(halloween).
todays_logo('chinesenewyear.png', 'Chinese New Year') :-
	todays_holiday(chinese_new_year).
todays_logo('liberationday.png', 'Liberation Day in the Netherlands') :-
	todays_holiday(liberation_day).
todays_logo('swipl.png', 'SWI-Prolog owl logo') :-
	todays_holiday(_).

%%	swi_logo//
%
%	Embed the SWI-Prolog logo.

swi_logo -->
	{ todays_logo(File, Alt),
	  http_absolute_location(icons(File), Logo, [])
	},
	html(a(href('http://www.swi-prolog.org'),
	       img([ class(owl),
		     src(Logo),
		     alt(Alt),
		     title(Alt)
		   ], []))).


%%	menubar(+Style)// is semidet
%
%	Emits a menubar. Style is the page style

menubar(Style) -->
	{ menu(Style, Menu) },
	html_requires(jquery),
	html_requires(jq('menu.js')),
	html(div(id(menubar),
		 div(class([menubar, 'fixed-width']),
		     ul(class('menubar-container'),
			\menu(Menu, 1))))).

menu([], _) --> !.
menu([H|T], Level) --> !, menu(H, Level), menu(T, Level).
menu(Label = Link + SubMenu, Level) --> !,
	submenu(Label, Level, SubMenu, Link).
menu(Label = SubMenu, Level) -->
	{ is_list(SubMenu)
	}, !,
	submenu(Label, Level, SubMenu, -).
menu(Label = Link, _) -->
	{ atom(Link),
	  uri_is_global(Link), !,
	  http_absolute_location(icons('ext-link.png'), IMG, [])
	}, !,
	html(li([ a(href(Link),
		    [ Label,
		      img([ class('ext-link'),
			    src(IMG),
			    alt('External')
			  ])
		    ])
		])).
menu(_Label = (-), _) --> !,
	[].
menu(Label = Link, 1) -->
	{ upcase_atom(Label, LABEL) },
	html(li(a(href(Link), LABEL))).
menu(Label = Link, _) -->
	html(li(a(href(Link), Label))).

submenu(Label, Level, SubMenu, -) --> !,
	{ SubLevel is Level+1 },
	html(li([ \submenu_label(Label, Level),
		  ul(\menu(SubMenu, SubLevel))
		])).
submenu(Label, Level, SubMenu, HREF) -->
	{ SubLevel is Level+1 },
	html(li([ a(href(HREF), \submenu_label(Label, Level)),
		  ul(\menu(SubMenu, SubLevel))
		])).

submenu_label(Label, Level) -->
	{ Level =< 1,
	  upcase_atom(Label, LABEL)
	}, !,
	html(LABEL).
submenu_label(Label, _) -->
	html([Label, span(class(arrow), &('#x25B6'))]).


menu(Style,
     [ 'Home'                = '/',
       'Download' =
       [ 'SWI-Prolog'	     = '/Download.html',
	 'Sources/building'  = '/build/',
	 'Docker images'     = '/Docker.html',
	 'Add-ons'           = '/pack/list',
	 'Browse GIT'	     = 'https://github.com/SWI-Prolog'
       ],
       'Documentation' =
       [ 'Manual'              = '/pldoc/refman/',
	 'Packages'	       = '/pldoc/package/',
	 'FAQ'                 = '/FAQ/',
	 'Command line'        = '/pldoc/man?section=cmdline',
	 'PlDoc'               = '/pldoc/package/pldoc.html',
	 'Bluffers' =
	 [ 'Prolog syntax'     = '/pldoc/man?section=syntax',
	   'PceEmacs'          = '/pldoc/man?section=emacsbluff',
	   'HTML generation'   = '/pldoc/man?section=htmlwrite'
	 ],
	 'License'             = '/license.html',
	 'Publications'        = '/Publications.html',
	 'Rev 7 Extensions'    = '/pldoc/man?section=extensions'
       ],
       'Tutorials' =
       [ 'Beginner' =
	 [ 'Getting started'   = '/pldoc/man?section=quickstart',
	   'Learn Prolog Now!' = 'http://lpn.swi-prolog.org/',
	   'Simply Logical'    = 'http://book.simply-logical.space/',
	   'Debugger'          = '/pldoc/man?section=debugoverview',
	   'Development tools' = '/IDE.html'
	 ],
	 'Advanced' =
	 [ 'Modules'           = 'http://chiselapp.com/user/ttmrichter/repository/gng/doc/trunk/output/tutorials/swiplmodtut.html',
	   'Grammars (DCGs)'   = 'https://www.github.com/Anniepoo/swipldcgtut/blob/master/dcgcourse.adoc',
	   'clp(fd)'	       = 'https://www.github.com/Anniepoo/swiplclpfd/blob/master/clpfd.adoc',
	   'Printing messages' = 'https://www.github.com/Anniepoo/swiplmessage/blob/master/message.adoc',
	   'PlDoc'             = 'http://chiselapp.com/user/ttmrichter/repository/swipldoctut/doc/tip/doc/tutorial.html'
	 ],
	 'Web applications' =
	 [ 'Web applications'  = 'https://www.github.com/Anniepoo/swiplwebtut/blob/master/web.adoc',
	   'Let\'s Encrypt!'   = 'https://github.com/triska/letswicrypt',
	   'Pengines'	       = '/pengines/'
	 ],
	 'Semantic web' =
	 [ 'ClioPatria'	       = 'https://cliopatria.swi-prolog.org/tutorial/',
	   'RDF namespaces'    = '/howto/UseRdfMeta.html'
	 ],
	 'Graphics' =
	 [ 'XPCE'              = '/download/xpce/doc/coursenotes/coursenotes.pdf',
	   'GUI options'       = '/Graphics.html'
	 ],
	 'Machine learning' =
	 [ 'Probabilistic Logic Programming' =
			         'http://cplint.ml.unife.it/'
	 ],
	 'External collections' =
	 [ 'Meta level tutorials' = 'https://www.metalevel.at/prolog'
	 ],
	 'For packagers' =
	 [ 'Linux packages'    = '/build/guidelines.html'
	 ]
       ],
       'Community' =           '/community.html' +
       [ 'IRC'                 = 'http://webchat.freenode.net/?channels=##prolog',
	 'Forum & mailing list'= 'https://swi-prolog.discourse.group',
	 'Blog'                = '/blog',
	 'News'                = '/news/archive',
	 'Report a bug'	       = '/bug.html',
	 'Submit a patch'      = '/howto/SubmitPatch.html',
	 'Submit an add-on'    = '/howto/Pack.html',
	 'Roadmap (on GitHub)' = 'https://github.com/SWI-Prolog/roadmap',
	 'External links'      = '/Links.html',
	 'Contributing'        = '/contributing.html',
	 'Code of Conduct'     = '/Code-of-Conduct.html',
	 'Contributors'        = '/Contributors.html',
	 'SWI-Prolog items'    = '/loot.html'
       ],
       'Users' =
       [ 'Semantic web'        = '/web/index.html',
	 'Students'            = '/students/index.html',
	 'Researchers'         = '/research/index.html',
	 'Commercial users'    = '/commercial/index.html',
	 'Dog food'            = '/dogfood.html',
	 'Is SWIPL right for me?' = '/pldoc/man?section=swiorother'
       ],
       'Wiki' =
       [ LoginLabel            = LoginURL,
	 'Edit this page'      = EditHREF,
	 'View changes'	       = '/wiki/changes',
	 'Sandbox'             = '/wiki/sandbox',
	 'Wiki help'           = '/wiki/',
	 'All tags'            = '/list-tags'
       ]
     ]) :-
	http_current_request(Request),
	memberchk(request_uri(ReqURL), Request),
	(   functor(Style, wiki, _)
	->  http_link_to_id(wiki_edit,
			    [location(ReqURL)], EditHREF)
	;   EditHREF = (-)
	),
	(   site_user_logged_in(_)
	->  LoginLabel = 'Logout',
	    http_link_to_id(logout, ['openid.return_to'(ReqURL)], LoginURL)
	;   LoginLabel = 'Login',
	    (	http_link_to_id(logout, [], ReqURL)
	    ->	RetURL = '/'		% HOME
	    ;	RetURL = ReqURL
	    ),
	    http_link_to_id(plweb_login_page,
			    ['openid.return_to'(RetURL)], LoginURL)
	).


%%	blurb//
%
%	Emit the blurb
blurb -->
	html({|html||
    <div id="blurb">
      <div>
	 SWI-Prolog offers a comprehensive free Prolog environment.
	 Since its start in 1987, SWI-Prolog development has been driven
	 by the needs of real world applications. SWI-Prolog is widely
	 used in research and education as well as commercial applications.
	 Join over a million users who have downloaded SWI-Prolog.
	 <a href="/features.html">more ...</a>
      </div>
    </div>
	     |}).


%%	cta_area//
%
%	Emit the Call To Action - the 3 big buttons on homepage
cta_area -->
	html({|html(_)||
    <table id='cta-container'>
      <tr>
        <td style="text-align:left">
	      <a href="Download.html">Download SWI-Prolog</a>
        <td style="text-align:center">
	      <a href="GetStarted.html">Get Started</a>
        <td style="text-align:right">
	      <a href="http://swish.swi-prolog.org">Try SWI-Prolog online</a>
      </tr>
    </table>|}).


%%	enhanced_search_area//
%
%	Emit the large size search area at bottom of home page

enhanced_search_area -->
	{ http_link_to_id(plweb_search, [], Action) },
	html({|html(Action)||
	      <div id='enhanced-search-container'>
	        <div>
	          <span class='lbl'>SEARCH DOCUMENTATION:</span>
	          <form  id="search-form-enhanced" action="Action">
	            <input name="for" type='text' id="forenhanced">
	            <input type="image" src="/icons/go.png" alt='Search'>
	          </form>
	        </div>
	      </div>|}),
	searchbox_script(forenhanced).
