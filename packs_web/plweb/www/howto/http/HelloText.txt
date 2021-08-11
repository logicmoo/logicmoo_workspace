---++ Hello World!

Below is the source from our first web-server. It serves a single page,
named =|/hello_world|=, which returns a simple plain-text message.  Note
the three elements of the page:

    1. Declare a handler, binding an HTTP path to a predicate.  The
    notation root(hello_world) uses an alias-mechanism similar to
    absolute_file_name/3 and allows for moving parts of the server
    locations easily. See http_absolute_location/3. We could also have
    used =|'/hello_world'|=.

    2. The predicate server(?Port) starts the server. It simply
    creates a number of Prolog _threads_ and then returns to the
    toplevel, so you can (re-)load code, debug, etc.

    3. The implementation of /hello_world.  The single argument provides
    the request details, which we ignore for now.  Our task is
    to write a CGI-Document: a number of _|name: value|_ -pair
    lines, followed by two newlines, followed by the document
    content,  The only obligatory header line is the
    =|Content-type:|= <mime-type> header.  Printing can be
    done using any Prolog printing predicate, but the format-family
    is the most useful.  See format/2.

==
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(hello_world), say_hi, []).		% (1)

server(Port) :-						% (2)
	http_server(http_dispatch, [port(Port)]).

say_hi(_Request) :-					% (3)
	format('Content-type: text/plain~n~n'),
	format('Hello World!~n').
==

To run this server, put the above code in a
[[file][<hello_world.pl>]], load the file into Prolog and run the
goal below. 5000 is the port-number. This can be any number between 1001
and 65000 (depending on your OS). Now direct your browser to
http://localhost:5000/hello_world and enjoy your first Prolog-based
web-server!

==
?- server(5000).
==

---+++ Where is Apache, IIS, Tomcat, ...?

Oops, we do not need that. The [[full
manual][</pldoc/package/http.html>]] gives options for _redirecting_
requests from Apache to our server, so that your server is reachable on
the default port 80. Actually, the SWI-Prolog HTTP server libraries are
very much like Tomcat, but now for Prolog. The [[SWI-Prolog
website][http://www.swi-prolog.org/]] is handled by SWI-Prolog,
redirected from an Apache server.

@see Source: hello_world.pl
@see Next: [[Development support for web-servers][<Developing.html>]]
