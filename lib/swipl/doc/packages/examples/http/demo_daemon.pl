/*  Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  J.Wielemaker@vu.nl
    WWW:     http://www.swi-prolog
    Copying: This example is in the public domain
*/

:- module(http_daemon, []).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(http/http_server)).

:- http_handler(/, hello, []).

/** <module> Demo running SWI-Prolog HTTP services as a Unix daemon process

The first example runs this service as   a  normal user. The server does
not produce any output. It is detached from the controlling terminal and
thus remains running if you logout.   You can use library(http/http_log)
to enable logging of the HTTP requests.

    swipl demo_daemon.pl --port=5000

The second example runs this server on a priviledged user on the default
port 80, avoiding the need for  a   proxy  server. In addition, it sends
errors, warnings and debug (see debug/3) messages to syslog deamon. Note
that the =|-q|= suppresses informational messages such as loading Prolog
files.

    sudo swipl demo_daemon.pl -- --syslog=swi-demo --user=www-data

@see library(http/http_unix_daemon) for more options to http_daemon/0.
*/


hello(_Request) :-
    reply_html_page(title('Hello'),
                    p('Hello from SWI-Prolog!')).
