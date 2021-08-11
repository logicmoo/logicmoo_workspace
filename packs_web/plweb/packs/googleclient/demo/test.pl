:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/html_write)).

:- use_module('../prolog/google_client').

server :-
	server(3040).

server(Port) :-
	http_server(http_dispatch,
		    [ port(Port)
		    ]).

:- http_handler(root(.),     home,  []).
:- http_handler(root(login), login, []).

home(_Request) :-
	http_link_to_id(login, [], Login),
	reply_html_page(
	    title('Test Google login'),
	    [ h1('Test Google login'),
	      button([ type(button),
		       onclick('window.location = "'+Login+'";')
		     ],
		     'Login with google')
	    ]).

login(Request) :-
	oauth_authenticate(Request, 'google.com', []).

:- multifile
	google_client:key/2,
	google_client:create_user/1.

google_client:create_user(Profile) :-
	format('Content-type: text/plain~n~n'),
	format('Create user from Profile~n~n'),
	print_term(Profile, [output(current_output)]).

google_client:key(
    client_id,
    REPLACE_WITH_SERVICE_ACCOUNT_ID_FROM_DEVELOPER_CONSOLE).
google_client:key(
    client_secret,
    REPLACE_WITH_CLIENTSECRET_FROM_DEVELOPERS_CONSOLE).

:- set_setting(http:public_host, localhost).
:- set_setting(http:public_port, 3040).
