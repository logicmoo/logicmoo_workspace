% Things that need to be defined in the user module, so Swish finds them
% To start clout as daemon:
% sudo swipl user_module_file.pl ../../swish/daemon.pl --port=80 --no-fork --user=www-data --workers=16
% To start as local server:
% swipl -l user_module_file.pl -l ../../swish/server.pl -g server:server

% :- use_module(library(http/http_log)).
:- use_module('../../swish/swish').
:- use_module('../../swish/lib/render').
:- use_module(library(http/http_dispatch)).

% LPS visualizations will appear courtesy of either of two SWISH answer renderers:
:- use_module(lps_2d_renderer,[]). % need not and can not import the rendering predicate into here
:- use_module(lps_timeline_renderer,[]).
:- use_rendering(lps_2d). % this will be the preferred... if available for the current visualization
:- use_rendering(lps_timeline).

:- multifile pengines:prepare_module/3.
pengines:prepare_module(_Module, swish, _Options) :- 
	style_check(-discontiguous), style_check(-singleton).
	
:- use_module('../utils/visualizer.P'). % this loads LPS
:- use_module('../utils/psyntax.P',[
	syntax2p/4,dumploaded,term_colours/2,may_clear_hints/0,timeless_ref/1,set_top_term/1
	]).

:- multifile sandbox:safe_primitive/1.
sandbox:safe_primitive(interpreter:go(_File,Options)) :- \+ member(cycle_hook(_,_,_),Options).
sandbox:safe_primitive(interpreter:go). 
sandbox:safe_primitive(interpreter:lps_welcome_message). 
sandbox:safe_primitive(visualizer:gojson(_JSON)). 
sandbox:safe_primitive(visualizer:gojson(_File,_Options,_Results,_JSON)). 
sandbox:safe_primitive(psyntax:dumploaded). 
 
% For debugging:
% sandbox:safe_primitive(swish_highlight:server_tokens(_)).  % swish_highlight:server_tokens(source).
% sandbox:safe_primitive(swish_highlight:show_mirror(_)).
% can not print output as usual, would interfere with http responses:
% :- open('mylog.txt',write,S), assert(mylogFile(S)).
% mylog(M) :- mylogFile(S), thread_self(T), writeln(S,T:M), flush_output(S).
% :- asserta((prolog:message(A,B,C) :-  mylog(message-A), fail)).

:- multifile prolog_colour:term_colours/2, prolog_colour:goal_colours/2.
prolog_colour:term_colours(T,C) :- term_colours(T,C).

:- multifile swish_highlight:style/3.
% style(Spec_as_in_specified_item, Type_as_in_prolog_server.js/prolog.css, ? )
swish_highlight:style(lps_delimiter,lps_delimiter,[text,base(atom)]).
swish_highlight:style(fluent,fluent,[text,base(atom)]).
swish_highlight:style(event,event,[text,base(atom)]).
swish_highlight:style(time,time,[text,base(atom)]). % atom?

% patch colouring of Prolog clause heads when they're referred only by LPS clauses
:- asserta((swish_highlight:style(head(unreferenced, Head), Type, Attributes) :-
	nonvar(Head),
	functor(Head,F,N),
	timeless_ref(F/N),
	!, % Head is referred as timeless, so it's not unreferenced:
	swish_highlight:style(head(head, Head), Type, Attributes) )).
	
/* This might work for XPCE... different style attributes
% used Mac Digital Color Meter to pick visjs timeline colours:
prolog_colour:style(fluent,[colour('#1A1A1A'), background('#D5DD28')]). 
prolog_colour:style(event,[colour('#FDA428'), background('#FFFFFF')]). 
prolog_colour:style(time,S) :- prolog_colour:style(event,S).
prolog_colour:style(lps_delimiter,[bold(true)]) :- mylog(lps_delimiter). */

dump :- psyntax:dumploaded.
go :- interpreter:lps_welcome_message, writeln('Using rak:'),interpreter:go.
gov :- interpreter:lps_welcome_message, writeln('Using rak:'),interpreter:go(_,[swish,verbose]).
go(Timeline) :- visualizer:gojson(_File,[],[],Timeline).
go(T,Options) :- \+ member(cycle_hook(_,_,_),Options), interpreter:lps_welcome_message, writeln('Using rak:'),visualizer:gojson(_File,Options,[],T).
godc(T) :- visualizer:gojson(_File,[dc],[],T).
godc :- interpreter:lps_welcome_message, writeln('Using dc:'),interpreter:go(_,[swish,dc]).
godcv :- interpreter:lps_welcome_message, writeln('Using dc:'),interpreter:go(_,[swish,verbose,dc]).
	
:- multifile user:file_search_path/2.
user:file_search_path(example, '../examples/CLOUT_workshop').
user:file_search_path(profile, '../swish/profiles').
user:file_search_path(lps_resources, '../swish/web').

:- multifile swish_config:main_title/1.
swish_config:main_title('SWISH -- with LPS').

:- http_handler('/lps', serve_lps_resources, [prefix]). 
serve_lps_resources(Request) :- % http://localhost:3050/lps/foo/Gruntfile.js working :-)
		option(path(Info), Request),  
        http_reply_file(lps_resources(Info), [], Request).

% hack SWISH to inject our CSS and Google Analytics fragment...
:- use_module('../../swish/lib/page',[swish_resources//0, swish_navbar//1]).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_path)).
:- catch(read_file_to_string(googleAnalyticsKey,Key,[]),_,Key=''), 
	format(atom(JS),'
  (function(i,s,o,g,r,a,m){i[\'GoogleAnalyticsObject\']=r;i[r]=i[r]||function(){
  (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
  m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
  })(window,document,\'script\',\'https://www.google-analytics.com/analytics.js\',\'ga\');

  ga(\'create\', \'~w\', \'auto\');
  ga(\'send\', \'pageview\');
',[Key]),
	expand_term( ( 
swish_page:swish_resources --> !,
	swish_css, swish_js, 
	% {http_absolute_location(lps_resources('lps.css'),LPScss,[])},
	html_post(head, link([ type('text/css'),rel('stylesheet'),href('/lps/lps.css') ])),
	html_post(head, script(JS))
), Expanded), swish_page:asserta(Expanded).

% ... and to remove chat notifications widget, and add LPS site link:
:- expand_term( (
swish_page:swish_navbar(Options) --> !,
	swish_resources,
	html(nav([ class([navbar, 'navbar-default']),
		   role(navigation)
		 ],
		 [ div(class('navbar-header'),
		       [ \collapsed_button,
			 \swish_logos(Options), 
			 span('with '), a([href('https://bitbucket.org/lpsmasters/lps_corner'),target('_blank')],'LPS') 
		       ]),
		   div([ class([collapse, 'navbar-collapse']),
			 id(navbar)
		       ],
		       [ ul([class([nav, 'navbar-nav', menubar])], []),
			 ul([class([nav, 'navbar-nav', 'navbar-right'])],
			    [ % remove chat avatars: li(\notifications(Options)),
			      li(\search_box(Options)),
			      \li_login_button(Options)
			    ])
		       ])
		 ]))
), Expanded), swish_page:asserta(Expanded).


:- multifile term_expansion/4. % place this at the end so we don't get this file's terms...:
% on SWISH we'll avoid the file to file translation, by converting on a term by term basis, assuming the transform to be 1-1
% we assume the LPS transform to preserve Prolog 
term_expansion(NiceTerm,ExpandedTerm) :- 
	context_module(user), % LPS programs are in the user module
	%mylog(term_expansion-NiceTerm),
	may_clear_hints, set_top_term(NiceTerm),
	% current_syntax(lps2p,true), In the future we may want to support other syntax conversions
	% variable names probably not available here, but we don't care about lpsp2p syntax anymore:
	syntax2p(NiceTerm,[],lps2p,ExpandedTerm). 
