:- module(test_recaptcha,
	  [
	  ]).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/recaptcha)).

:- http_handler(root(test/recaptcha), captcha_form,     []).
:- http_handler(root(test/callback),  captcha_callback, []).

captcha_form(_Request) :-
	reply_html_page(
	    plain,
	    title('Captcha test'),
	    \form).

form -->
	{ http_link_to_id(captcha_callback, [], HREF)
	},
	html(h1('Test page for recaptcha configuration')),
	html(form([method('POST'), action(HREF)],
		  [ \recaptcha([]),
		    input([name(name)]),
		    input(type(submit))
		  ])).

captcha_callback(Request) :-
	recaptcha_parameters(RecapthaParams),
	http_parameters(Request,
			RecapthaParams,
			[form_data(Form)]),
	format('Content-type: text/plain\n\n'),
	print_term(Form, [output(current_output)]),
	(   recaptcha_verify(Request, RecapthaParams)
	->  format('Welcome human!~n')
	;   format('Go away, alien!~n')
	).
