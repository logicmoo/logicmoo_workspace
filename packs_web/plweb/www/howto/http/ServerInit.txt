---+ Running the server under init or daemontools

Next to [[VNC][ServerVNC.txt]], another option for deploying servers as
continuously running processes is to use the Unix started scripts. One
option here is to use init, and the other is to use e.g.,
[[daemontools][http://cr.yp.to/daemontools.html]].  At this moment we
will not give a complete scenario here.

---++ How to keep the server running?

When starting SWI-Prolog's multi-threaded HTTP server, the main thread
starts a number of server threads and then presents the normal
interactive Prolog toplevel. This is great for development and under the
[[VNC][ServerVNC.txt]] solution, but of no use for a non-interactive
server, because Prolog terminates if it reads an end-of-file at the
toplevel.  We need to give the toplevel something to do.  The common
solution is to use thread_get_message/1 for that because it provides
a non-polling indefinite sleep that can be stopped by sending a message
to the thread.  For example:

==
run :-
	http_server(http_dispatch, []),
	thread_get_message(stop).
==

We can make thread_get_message/1 return by sending a message to the
=main= thread, either from an HTTP handler or by catching a signal.  For
example:

==
:- on_signal(hup, _, hup).

hup(_Signal) :-
	thread_send_message(main, stop).
==
