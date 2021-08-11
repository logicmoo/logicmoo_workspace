:- module(
  footer,
  [
    footer//1, % +Options
    server_information//0
  ]
).

/** <module> Footer

Footer for SWI-Prolog Web pages.

@author Wouter Beek
@version 2014/01
*/

:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/js_write)).
:- use_module(openid).
:- use_module(tagit).
:- use_module(annotation).

:- html_resource(css('footer.css'), []).


'community-content'(Options) -->
	{ option(object(Object), Options) }, !,
	html(div(id='community-content',
		 [ \tagit_footer(Object, []),
		   \annotation(Object)
		 ])).
'community-content'(_) -->
	[].

%%	footer(+Options)// is det.
%
%	Emit the footer, which contains   the  community content, server
%	address and user information. Options:
%
%	  * object(Object)
%	  Display community content area for Object.
%	  * show_user(+Boolean)
%	  If =false=, omit the user

footer(Options) -->
	html_requires(css('footer.css')),
	html(div(class=footer,
		 [ \'community-content'(Options),
		   \'footer-footer'(Options)
		 ])),
	balance_columns_script.

'footer-footer'(Options) -->
	html(div(id=footer,
		 [ \show_user(Options),
		   \server_information
		 ])).

show_user(Options) -->
	{ option(show_user(false), Options) }, !.
show_user(_) -->
	current_user.

balance_columns_script -->
	js_script({|javascript||
		   $().ready(function()
	           { var $navtree = $(".navwindow");
		     var $navcontent = $(".navcontent");
		     if ( $navtree.length > 0 && $navcontent.length > 0 )
		     { var $window = $(window).on("resize", function()
		       { var ch = $navcontent.height();
			 var nh = $navtree.height();
			 if ( nh > 400 && nh > ch + 200 )
			 { if ( ch < 300 ) ch = 300;
			   $navtree.height(ch);
			   $navtree.css('overflow-y', 'scroll');

			   var current = $navtree.find("li.nav.current");
			   if ( current.position().top > ch-40 )
			   { $navtree.scrollTop(current.position().top - (ch-40));
			   }
			 }
		       }).trigger("resize")
		     }
		   });
		  |}).


prolog_version(Version) :-
  current_prolog_flag(version_git, Version), !.
prolog_version(Version) :-
  current_prolog_flag(version_data, swi(Ma,Mi,Pa,_)),
  format(atom(Version), '~w.~w.~w', [Ma,Mi,Pa]).

%! server_information// is det.
% Emit server information.

server_information -->
  {prolog_version(Version)},
  html(
    a([id=powered,href='http://www.swi-prolog.org'], [
      'Powered by SWI-Prolog ',
      Version
    ])
  ).

