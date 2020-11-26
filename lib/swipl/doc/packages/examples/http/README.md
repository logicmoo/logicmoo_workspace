# HTTP example files

This is a simple demo of the  HTTP server facilities, providing a simple
body and the three documented server instantiations.

## The server main programs are:

	$ demo_threads.pl :
	Run threaded server. Requires SWI-Prolog with thread-support.
	The server is started at port 3000 using server/0.  server/2
	allows to specify options. tm/0 provides a graphical display
	of the runing threads.  See source-file.

## Unix services demo:

	$ demo_daemon.pl :
	Demo script to run the SWI-Prolog HTTP server as a Unix
	service.

	$ linux-init-script :
	/etc/init.d script for Debian and Redhat based Unix systems.
	Must be configured.

	$ upstart-script.conf :
	Ubuntu `upstart` script.  Must be configured and placed in
	`/etc/init`

	$ systemd-script.service :
	Linux `systemd` script.  The typical installation sequence
	is:

	    cp systemd-script.service demo.service
	    <edit>
	    cp demo.service /etc/systemd/system
	    systemctl enable demo
	    systemctl start demo
	    journalctl -f -u demo

## Session management demo:

	$ calc.pl :
	Multi-threaded server with session management using the
	html_write.pl library.  See source for usage.

## File serving demo:

	$ demo_files.pl :
	Is a multi-threaded server that serves static files and
	directory indices.

## Client demo

	$ demo_client.pl :
	Simple multi-threaded client to test the server under
	different conditions.  Requires SWI-Prolog with thread-support.
	See source for usage.

## HTTP digest authentication

	$ demo_digest.pl :
	Simple demo showing HTTP digest authentication at work.

## Login and request rewriting

	$ demo_login.pl :
	Demonstrated using request rewriting and redefining status
	pages to provide cookie based login with RBAC authorization.

	$ demo_rest.pl :
	Demonstrates path wildcarts for common REST api routing.

## Performance testing

A very early start of some  routines   to  validate the server platform.
Eventually, stress_server.pl will serve different   tests  from multiple
locations  and  stress_client.pl  will  contain    client  code  to  run
individual tests as well as doing multi-threaded tests.

	$ stress_server.pl :
	Server platform.

	$ stress_client.pl :
	Client.















