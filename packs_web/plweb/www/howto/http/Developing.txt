---++ Development support for web-servers

You can change the program above and/or load additional Prolog sources
into the life server and type this in the toplevel to update the server.
Then simply reload the page in your browser to see the changes. Using
make/0 should be fairly safe in recent Prolog versions.

==
?- make.
==

If a requests ends in failure or an error in running the Prolog query
that handles it, the server responds with a *|500 internal server
error|*.  If it concerns an exception, extra context can be added by
loading the library(http/http_error) as below.

==
:- use_module(library(http/http_error)).
==

If the error context is insufficient, one can use the [[graphical
tracer][</gtrace.html>]] by setting a spy-point on the handler or a
suspect predicate called by the handler. Spy-points for background
threads are set using tspy/1 (_thread_-spy). E.g., see below. After
setting the spy-point, reload the page in the browser and the graphical
tracer should appear.

==
?- tspy(say_hi/1).
==

Progress and *|debug messages|* cannot be printed to the current output,
because =current_output= is rebound for catching output for the server.
It is strongly encouraged to use library(debug) for this purpose.
Statements are added to your code using debug/3:

==
	...,
	debug(hello, 'About to say hello', []),
	...
==

Such statements are activated using =|?- debug(hello).|= and
deactivated using =|?- nodebug(hello).|= and print messages like this:

==
?- debug(hello).
?- debug(hello, 'About to say hello to ~p', ['the world']).
% About to say hello to the world
==

If you forgot what predicates are called by your server, you can use
[[Firefox Developer Tools][https://developer.mozilla.org/en-US/docs/Tools]]
to analyse the traffic.  You can also use debug channel http(request).
Doing this on our first server and loading '/hello_world' gives this
output:

==
?- debug(http(request)).
% [Thread httpd@5000_1] [2] get /hello_world ...
% [Thread httpd@5000_1] [2] 200 OK (0.002 seconds; 0 bytes)
==

The development environment is programmed to be able to locate source
from the HTTP locations, so the following now helps to edit the source:

==
?- edit('/hello_world').
==

@see Next: [[Hello World in HTML][<HelloHTML.html>]]
