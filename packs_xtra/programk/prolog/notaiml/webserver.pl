:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(debug)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_parameters)).
:- use_module(library(url)).
:- use_module(library(sgml)).

% including this turns on sessions
:- use_module(library(http/http_session)).

:- use_module(interpret).

% TODO it's long since time you broke this into two files
% :- use_module(library(porter_stem)).

% The predicate server(?Port) starts the server. It simply creates a
% number of Prolog threads and then returns to the toplevel, so you can
% (re-)load code, debug, etc.
server(Port) :-
        http_server(http_dispatch, [port(Port)]).

% define the root of our web system
web_root(X) :- X = '.'. % '/home/annie/prologclass/fromwindows'.
% convert path names and URIs for files
web_path(Relative,Absolute) :- web_root(Root), atom_concat(Root, Relative, Absolute).
%  handle static pages.
/* See

http://old.nabble.com/How-to-load-an-image-into-a-web-page-with-swi-prolog-td14363488.html
Supposedly this handles everything in ./pages/ but it makes the compiler
freak
*/
:- http_handler( '/pages' , serve_page, [prefix]).

serve_page(Request) :-
        memberchk(path(Path), Request),
	web_path(Path, FilePath),
	http_reply_file(FilePath, [], Request).

% Declare a handler, binding an HTTP path to a predicate.
% The notation root(hello_world) uses an alias-mechanism similar to
% absolute_file_name/3 and allows for moving parts of the server locations
% easily. See http_absolute_location/3. We could also have used '/hello_world'.
:- http_handler(root(hello_world), say_hi, []).
% They say we can use an absolute URI so lets
% try it
:- http_handler('/tacos/of/god' , say_tacos, []).

% handler for the pseudo AIML language
:- http_handler('/aiml' , aiml_page , []).

% handle images
:- http_handler('/screenshot.png',
   http_reply_file('screenshot.png', []), []).


/* The implementation of /hello_world. The single argument provides the request
details, which we ignore for now. Our task is to write a CGI-Document:
a number of name: value -pair lines, followed by two newlines, followed
by the document content, The only obligatory header line is the
Content-type: <mime-type> header.
Printing can be done using any Prolog printing predicate, but the
format-family is the most useful. See format/2.   */

say_hi(_Request) :-
        reply_html_page(title('Hello World'),
                        [ h1('Hello World'),
                          p(['This example demonstrates generating HTML ',
                             'messages from Prolog'
                            ]),
			  img([width(32),height(32),src('screenshot.png')])
                        ]).

say_tacos(_Request) :-
	debug(tacos , 'entered say_tacos', []),
	format('Content-type: text/html~n~n'),
	format('<html><body><h1>Tacos of God</h1></body></html').

aiml_page(Request) :-
	(http_session_data(last_utterance(Last_Utterance));
	    Last_Utterance = '') ,
	(http_session_data(that(That)); That=[star(0)]),
	(http_session_data(topic(Topic)); Topic=[star(0)]),
	http_parameters(Request,
			[intext(Intext , [ default('') ])]),
	(http_session_retractall(last_utterance(_)) ; true),
	http_session_assert(last_utterance(Intext)),
	% TODO - need to use That, Topic, and Intext to find reply
	% from chatterbot
	chatterbot(memory(That, Topic), Intext, Response, NewTopic),
	!,  % cut in case something below fails - if there's an http problem
	% I don't want to retry the bot
	http_session_retractall(that(_)),
	http_session_assert(that(Response)),
	http_session_retractall(topic(_)),
	http_session_assert(topic(NewTopic)),
	reply_html_page(title('chatterbot'),
			[ h1('Talk with the chatterbot'),
			  p(['I\'m a chatterbot, you can talk with me']),
			  p(['You said ', Last_Utterance]),
			  p(['and your last message was ', Intext]),
			  p(['and my response is ', Response]),
			  form([action=location_by_id(aiml_page),
				method='GET'],
			       [label([for=intext], 'Say: '),
			       input([type=text, id=intext, name=intext], []),
			       input([type=submit, value='Send'],[])])
			  ]).














