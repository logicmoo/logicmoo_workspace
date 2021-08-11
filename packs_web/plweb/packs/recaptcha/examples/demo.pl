:- module(recaptcha_demo,
	  [ server/1				% +Port
	  ]).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).

:- use_module(library(http/recaptcha)).

:- multifile recaptcha:key/2.

%%	recaptcha:key(+Which, -Key)
%
%	Get keys from http://www.google.com/recaptcha and update the two
%	lines  below.  Note  that  all  keys  work  if  you  connect  to
%	=localhost=.

recaptcha:key(public,  'Public key goes here').
recaptcha:key(private, 'Private key goes here').

:- http_handler(root(.),	     captcha_form,     []).
:- http_handler(root(test/callback), captcha_callback, []).

/** <module> reCAPTCHA plugin demo

Demo of reCAPTCHA plugin. To use

  1. Go to http://www.google.com/recaptcha, get a key-pair and
     put the keys in the placeholders above.
  2. Open this file in Prolog and start the server using e.g.,

	?- server(5000).

  3. Connect to http://local:5000 and enjoy.
*/

%%	server(?Port) is det.
%
%	Start the HTTP server.

server(Port) :-
	http_server(http_dispatch,
		    [ port(Port)
		    ]).


%%	captcha_form(+Request)
%
%	HTTP handler that creates a form holding a captcha

captcha_form(_Request) :-
	reply_html_page(title('Captcha test'),
			\form).

form -->
	{ http_link_to_id(captcha_callback, [], HREF)
	},
	html(form([method('POST'), action(HREF)],
		  [ \recaptcha([]),
		    div(input([name(name)])),
		    div(input(type(submit)))
		  ])).

%%	captcha_callback(+Request)
%
%	HTTP handlet that handles a form holding a captcha.

captcha_callback(Request) :-
	recaptcha_parameters(RecapthaParams),
	http_parameters(Request,
			[ name(Name, [optional(true), default('')])
			| RecapthaParams
			],
			[]),
	reply_html_page(
	    title('reCAPTCHA demo reply'),
	    [ h1('Form reply'),
	      \form_reply(Request, Name, RecapthaParams)
	    ]).

form_reply(Request, Name, RecapthaParams) -->
	{ http_link_to_id(captcha_form, [], Back) },
	(   { recaptcha_verify(Request, RecapthaParams) }
	->  html(p('Welcome ~w, nice to see a human!'-[Name]))
	;   html(p('Go away, robot ~w!'-[Name]))
	),
	html(p(['Click ', a(href(Back), 'here'), ' to try again'])).
