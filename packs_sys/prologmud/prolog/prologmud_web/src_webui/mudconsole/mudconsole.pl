/*  Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2012, VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA 02110-1301 USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.

    This file was altered by Anne Ogborn to make the logicmoo mud
    console.

     Note -this was an experiment. Decided it wouldn't work well for our
     uses, but wanted to leave the code around.

*/

:- module(mudconsole,
	  [ mc_start/0,
	    mc_start/1,			% +Options

	    mc_format/2,		% +Format, +Args
	    mc_format/3,		% +WCId, +Format, +Args
	    mc_format/4,		% +WCId, +Format, +Args, +Options
	    mc_html/1,			% :HTML
	    mc_html/2,			% +WCId, :HTML
	    mc_html/3,			% +WCId, :HTML, +Options
	    mc_ask/2,			% -Bindings, +Question
	    mc_ask/4,			% +InputId, -Bindings, +Question, +Options

	    mc_output_area//1,		% +Options
	    mc_form_area//1,		% +Options
	    mc_error_area//0
	  ]).

/* * module * Use a browser as HTML console

The  library(mudconsole)  allows  for    writing  classical  query/reply
programs that use a web-browser for I/O.  In the typical user scenarion,
the application calls mc_start/0 to open a   browser. Next, it calls one
of mc_format/2,3,4 or mc_html/1,2 to send output   to the browser and or
calls mc_ask/2,4 to request data from the user.

The home-page can be customized by defining a handler for =mc_home=. See
mc_home/1 for the default page.

Here is an example run:

  ==
  ?- [library(mudconsole)].
  ?- mc_start.				% opens browser
  ?- mc_format('Hello ~w', [world]).
  ?- mc_html(p(['Hello ', b(world)])).
  ?- mc_ask([age(Age)], [p('How old are you'), input([name(age)])]).
  Age = 24.				% type 24 <enter>
  ==

*/

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_path), []).
:- use_module(library(http/http_server_files), []).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_head)).
:- use_module(swi(library/http/html_write)).
:- use_module(library(option)).

% :- style_check(-atom).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(mudconsole, root(mudconsole), []).

:- multifile user:file_search_path/2.
:- dynamic   user:file_search_path/2.

user:file_search_path(js, './http/web/js').
user:file_search_path(css, './http/web/css').

:- http_handler(mudconsole('mc_home'),    mc_home,    [priority(-10)]).
:- http_handler(mudconsole('mc_message'), mc_message, []).
:- http_handler(mudconsole('mc_reply'),   mc_reply,   []).

:- html_resource(jquery,
		 [ virtual(true),
		   requires(js('jquery-1.7.1.js'))
		 ]).
:- html_resource(js('jquery.form.js'),
		 [ requires(jquery)
		 ]).
:- html_resource(js('mudconsole.js'),
		 [ requires(jquery),
		   requires(js('jquery.form.js'))
		 ]).
:- html_resource(mudconsole,
		 [ virtual(true),
		   requires(js('mudconsole.js'))
		 ]).

:- html_meta
	mc_html(html),
	mc_html(+, html),
	mc_html(+, html, +).


		 /*******************************
		 *	  SIMPLE SERVER		*
		 *******************************/

:- dynamic
	mc_option/1.

mc_option(Option, Default) :-
	Option =.. [Name,Value],
	GenOption =.. [Name,Gen],
	(   mc_option(GenOption)
	->  Value = Gen
	;   Value = Default
	).

%%	mc_start is det.
%%	mc_start(+Options) is det.
%
%	Start  the  mudconsole.   This   opens    your   browser   using
%	www_open_url/1.  Options processed:
%
%	  * title(+Title)
%	  Title for window and =h1= header
%	  * allow(+IP)
%	  Only allow connections whose _peer_ unify to IP. IP is a term
%	  IP(A,B,C,D), where A,B,C,D are integers in the range 0..255.
%
%	The user can customize  the  output   page  by  defining an HTTP
%	handler  with  the  id  =mc_home=    (see  http_handler/3).  The
%	predicate mc_home/1 provides the simple default page.

mc_start :-
	mc_start([]).

mc_start(Options) :-
	retractall(mc_option(_)),
	forall(member(Option, Options), assertz(mc_option(Option))),
	mc_server(Port),
	mc_browser(Port).

mc_server(Port) :-
	http_server_property(Port, goal(_)), !.
mc_server(Port) :-
	mc_option(port(Port), _),
	http_server(http_dispatch, [port(Port)]).

mc_browser(Port) :-
	http_link_to_id(mc_home, [], Home),
	fmt(atom(URL), 'http://localhost:~w~w', [Port, Home]),
	www_open_url(URL).

%%	mc_home(+Request) is det.
%
%	HTTP Handler for the default mudconsole console layout

mc_home(Request) :-
	mc_allowed(Request),
	mc_option(title(Title), 'SWI-Prolog mudconsole'),
	reply_html_page(title(Title),
			[ \html_requires(css('mudconsole.css')),
			  h1(Title),
			  \mc_error_area,
			  \mc_output_area([]),
			  \mc_form_area([])
			]).


mc_allowed(Request) :-
	memberchk(peer(Peer), Request),
	debug(wc(authorise), 'Peer = ~q', [Peer]),
	mc_option(allow(Allow), ip(127,0,0,_)),
	Peer = Allow.


		 /*******************************
		 *	     LIBRARY		*
		 *******************************/

%%	mc_output_area(+Options)// is det.
%
%	Creates a mudconsole =div= element. Multiple output areas can be
%	created,  each  with  their  own  _id_.    The   default  id  is
%	=mc_output=.

mc_output_area(Options) -->
	{ option(id(Id), Options, mc_output)
	},
	html_requires(mudconsole),
	html([ div(id(Id), [])
	     ]).


%%	mc_message(+Request)
%
%	HTTP handler that is queried from mudconsole.js, waiting for the
%	next message to execute. Time  out   after  30 seconds, which is
%	indicated with =|X-Timeout: true|= in the header.

mc_message(_Request) :-
	(   thread_get_message(mc_queue,
			       message(QueueId, Message, Options),
			       [timeout(30)])
	->  reply_message(QueueId, Message, Options)
	;   fmt('X-Timeout: true~n', []),
	    fmt('Content-type: text/plain~n~n'),
	    fmt('timeout~n')
	).

reply_message(Id, fmt(Format, Args), Options) :-
	fmt('X-Id: ~w~n', [Id]),
	maplist(x_header, Options),
	fmt('Content-type: text/plain\n\n'),
	fmt(Format, Args).
reply_message(Id, html(HTML), Options) :-
	fmt('X-Id: ~w~n', [Id]),
	maplist(x_header, Options),
	fmt('Content-type: text/html\n\n'),
	phrase(html(HTML), Tokens),
	print_html(Tokens).

x_header(clear(Bool)) :-
	fmt('X-Clear: ~w~n', [Bool]).

%%	mc_format(+Format, +Args) is det.
%%	mc_format(+WCId, +Format, +Args) is det.
%%	mc_format(+WCId, +Format, +Args, +Options) is det.
%
%	Formats a string (like fmt/3) to the web console. For example:
%
%	  ==
%	  ?- mc_format('Hello ~w', [world]).
%	  ==
%
%	Options:
%
%	  * clear(Boolean)
%	  If =true=, clear the output area before adding the new
%	  content.
%
%	@param WCId is the identifier of the output area.  Default is
%	       =mc_output=.
%	@param Format and Args are passed to fmt/3.

mc_format(Format, Args) :-
	mc_format(mc_output, Format, Args).

mc_format(WCId, Format, Args) :-
	mc_format(WCId, Format, Args, []).

mc_format(WCId, Format, Args, Options) :-
	thread_send_message(
	    mc_queue,
	    message(WCId, fmt(Format, Args), Options)).

%%	mc_html(+HTML) is det.
%%	mc_html(+WCId, +HTML) is det.
%%	mc_html(+WCId, +HTML, +Options) is det.
%
%	Adds an HTML element to the  output   area.  HTML  must be valid
%	input for html//1 from library(http/html_write). For example:
%
%	  ==
%	  ?- mc_write([p(['Hello ', b(world)])]).
%	  ==
%
%	Options:
%
%	  * clear(Boolean)
%	  If =true=, clear the output area before adding the new
%	  content.

mc_html(HTML) :-
	mc_html(mc_output, HTML).

mc_html(WCId, HTML) :-
	mc_html(WCId, HTML, []).

mc_html(WCId, HTML, Options) :-
	thread_send_message(
	    mc_queue,
	    message(WCId, html(HTML), Options)).


		 /*******************************
		 *	       ERRORS		*
		 *******************************/

%%	mc_error_area//
%
%	Create an output area for errors and  warnings. This is a normal
%	output area, using the identifier =ic_error=.

mc_error_area -->
	mc_output_area([id(mc_error)]).


		 /*******************************
		 *	      INPUT		*
		 *******************************/

%%	mc_form_area(+Options)//
%
%	Create a form-area. This is a  =div=   holding  a =form= with ID
%	=mc_form=. A form-area is used with mc_ask/3 and mc_ask/4.

mc_form_area(Options) -->
	{ option(id(Id), Options, mc_form),
	  http_link_to_id(mc_reply, [], HREF)
	},
	html_requires(mudconsole),
	form_script(Id),
	html([ div(class(form),
		   [ form([id(Id), action(HREF)], [])
		   ]),
	       div(id(preview), [])
	     ]).

form_script(Id) -->
	html(script(type('text/javascript'),
		    \[ '$("#~w").ajaxForm({\n\c
			   target: "#preview",\n\c
			   success: function(respText, statusText, xhr, el) {\n\c
			     $("#~w").addClass("inactive");\n\c
			     $("#~w input").prop("disabled", true);\n\c
			   },\n\c
			   error: function(xhr, textStatus, errorThrown) {\n\c
                             $("#preview").empty();\n\c
			     $("#preview").addClass("error");\n\c
			     $("#preview").append(xhr.responseText);\n\c
			   }\n\c
		        });'-[Id, Id, Id]
		     ])).

%%	mc_ask(-Result, +Specification) is det.
%%	mc_ask(+InputId, -Result, +Specification, +Options) is det.
%
%	Ask a question. Result is a   list Name(Value). Specification is
%	an HTML specification (as mc_html/1, see  also html//1) which is
%	used as the content for  a  =form=   element.  Each  Name in the
%	Result list must be covered by an equally named input element in
%	the form.
%
%	==
%	?- mc_ask([ age(Age)
%	          ],
%	          [ p('How old are you?'),
%	            input([name(age)])
%	          ]).
%	Age = 24.
%	==
%
%	@see We need a form that doesn't submit.  Generic code I found
%	sofar is http://www.9lessons.info/2011/09/submit-form-without-refreshing-page.html
%	@param Options is currently ignored

:- dynamic
	form_result/2.				% Id, Result

mc_ask(Result, Question) :-
	mc_ask(mc_form, Result, Question, []).
mc_ask(InputId, Result, Question, _Options) :-
	Id is random(1<<63),
	(   is_list(Question)
	->  QuestionList = Question
	;   QuestionList = [Question]
	),
	asserta(form_result(Id, Result)),
	mc_html(InputId,
		[ input([type(hidden), name(id), value(Id)])
		| QuestionList
		],
		[ clear(true)
		]),
	thread_get_message(reply_queue, Id-Result).

%%	mc_reply(+Request)
%
%	HTTP handler than processed the answer  after the user completes
%	an input form.

mc_reply(Request) :-
	http_parameters(Request,
			[ id(Id, [integer])
			],
			[ form_data(Form)
			]),
	form_result(Id, Result),
	bind_form(Result, Form),
	thread_send_message(reply_queue, Id-Result),
	fmt('Content-type: text/plain\n\n'),
	fmt('Thank you\n').

bind_form([], _).
bind_form([H|T], Form) :-
	(   H =.. [Name,Value|Options]
	->  memberchk(Name=Raw, Form),
	    http_convert_parameter(Options, Name, Raw, Value)
	;   true
	),
	bind_form(T, Form).


		 /*******************************
		 *	      RESOURCES		*
		 *******************************/

:- initialization (
       catch(message_queue_create(mc_queue),
	     error(permission_error(_,_,_),_),
	     true),
       catch(message_queue_create(reply_queue),
	     error(permission_error(_,_,_),_),
	     true)
       ).
