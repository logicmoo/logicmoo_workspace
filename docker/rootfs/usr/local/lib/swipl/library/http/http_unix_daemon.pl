/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2022, University of Amsterdam
                              VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(http_unix_daemon,
          [ http_daemon/0,
            http_daemon/1                       % +Options
          ]).
:- use_module(library(error)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(broadcast)).
:- use_module(library(socket)).
:- use_module(library(option)).
:- use_module(library(uid)).
:- use_module(library(unix)).
:- use_module(library(syslog)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_host)).
:- use_module(library(main)).
:- use_module(library(readutil)).

:- if(( exists_source(library(http/http_ssl_plugin)),
        \+ current_prolog_flag(pldoc_to_tex,true))).
:- use_module(library(ssl)).
:- use_module(library(http/http_ssl_plugin)).
:- endif.

:- multifile
    http_server_hook/1,                     % +Options
    http_certificate_hook/3,                % +CertFile, +KeyFile, -Password
    http:sni_options/2.                     % +HostName, +SSLOptions

:- initialization(http_daemon, main).

/** <module> Run SWI-Prolog HTTP server as a Unix system daemon

This module provides the logic that  is   needed  to integrate a process
into the Unix service (daemon) architecture. It deals with the following
aspects,  all  of  which  may  be   used/ignored  and  configured  using
commandline options:

  - Select the port(s) to be used by the server
  - Run the startup of the process as root to perform privileged
    tasks and the server itself as unpriviledged user, for example
    to open ports below 1000.
  - Fork and detach from the controlling terminal
  - Handle console and debug output using a file and/or the syslog
    daemon.
  - Manage a _|pid file|_

The typical use scenario is to  write   a  file that loads the following
components:

  1. The application code, including http handlers (see http_handler/3).
  2. This library

In the code below, =|?- [load].|= loads   the remainder of the webserver
code.  This is often a sequence of use_module/1 directives.

  ==
  :- use_module(library(http/http_unix_daemon)).

  :- [load].
  ==

The   program   entry   point   is     http_daemon/0,   declared   using
initialization/2. This may be overruled using   a  new declaration after
loading  this  library.  The  new  entry    point  will  typically  call
http_daemon/1 to start the server in a preconfigured way.

  ==
  :- use_module(library(http/http_unix_daemon)).
  :- initialization(run, main).

  run :-
      ...
      http_daemon(Options).
  ==

Now,  the  server  may  be  started    using   the  command  below.  See
http_daemon/0 for supported options.

  ==
  % [sudo] swipl mainfile.pl [option ...]
  ==

Below are some examples. Our first example is completely silent, running
on port 80 as user =www=.

  ==
  % swipl mainfile.pl --user=www --pidfile=/var/run/http.pid
  ==

Our second example logs HTTP  interaction   with  the  syslog daemon for
debugging purposes. Note that the argument   to =|--debug|== is a Prolog
term and must often be escaped to   avoid  misinterpretation by the Unix
shell.   The debug option can be repeated to log multiple debug topics.

  ==
  % swipl mainfile.pl --user=www --pidfile=/var/run/http.pid \
          --debug='http(request)' --syslog=http
  ==

*Broadcasting* The library uses  broadcast/1   to  allow hooking certain
events:

  - http(pre_server_start)
  Run _after_ _fork_, just before starting the HTTP server.  Can be used
  to load additional files or perform additional initialisation, such as
  starting additional threads.  Recall that it is not possible to start
  threads _before_ forking.

  - http(post_server_start)
  Run _after_ starting the HTTP server.

@tbd    Cleanup issues wrt. loading and initialization of xpce.
@see    The file <swi-home>/doc/packages/examples/http/linux-init-script
        provides a /etc/init.d script for controlling a server as a normal
        Unix service.
*/

:- debug(daemon).

% Do not run xpce in a thread. This disables forking. The problem here
% is that loading library(pce) starts the event dispatching thread. This
% should be handled lazily.

:- set_prolog_flag(xpce_threaded,   false).
:- set_prolog_flag(message_ide,     false). % cause xpce to trap messages
:- set_prolog_flag(message_context, [thread,time('%F %T.%3f')]).
:- dynamic interactive/0.

%!  http_daemon
%
%   Start the HTTP server  as  a   daemon  process.  This  predicate
%   processes the commandline arguments below. Commandline arguments
%   that specify servers are processed  in   the  order  they appear
%   using the following schema:
%
%     1. Arguments that act as default for all servers.
%     2. =|--http=Spec|= or =|--https=Spec|= is followed by
%        arguments for that server until the next =|--http=Spec|=
%        or =|--https=Spec|= or the end of the options.
%     3. If no =|--http=Spec|= or =|--https=Spec|= appears, one
%        HTTP server is created from the specified parameters.
%
%     Examples:
%
%       ==
%       --workers=10 --http --https
%       --http=8080 --https=8443
%       --http=localhost:8080 --workers=1 --https=8443 --workers=25
%       ==
%
%     $ --port=Port :
%     Start HTTP server at Port. It requires root permission and the
%     option =|--user=User|= to open ports below 1000.  The default
%     port is 80. If =|--https|= is used, the default port is 443.
%
%     $ --ip=IP :
%     Only listen to the given IP address.  Typically used as
%     =|--ip=localhost|= to restrict access to connections from
%     _localhost_ if the server itself is behind an (Apache)
%     proxy server running on the same host.
%
%     $ --debug=Topic :
%     Enable debugging Topic.  See debug/3.
%
%     $ --syslog=Ident :
%     Write debug messages to the syslog daemon using Ident
%
%     $ --user=User :
%     When started as root to open a port below 1000, this option
%     must be provided to switch to the target user for operating
%     the server. The following actions are performed as root, i.e.,
%     _before_ switching to User:
%
%       - open the socket(s)
%       - write the pidfile
%       - setup syslog interaction
%       - Read the certificate, key and password file (=|--pwfile=File|=)
%
%     $ --group=Group :
%     May be used in addition to =|--user|=.  If omitted, the login
%     group of the target user is used.
%
%     $ --pidfile=File :
%     Write the PID of the daemon process to File.
%
%     $ --output=File :
%     Send output of the process to File.  By default, all
%     Prolog console output is discarded.
%
%     $ --fork[=Bool] :
%     If given as =|--no-fork|= or =|--fork=false|=, the process
%     runs in the foreground.
%
%     $ --http[=(Bool|Port|BindTo:Port)] :
%     Create a plain HTTP server.  If the argument is missing or
%     =true=, create at the specified or default address.  Else
%     use the given port and interface.  Thus, =|--http|= creates
%     a server at port 80, =|--http=8080|= creates one at port
%     8080 and =|--http=localhost:8080|= creates one at port
%     8080 that is only accessible from `localhost`.
%
%     $ --https[=(Bool|Port|BindTo:Port)] :
%     As =|--http|=, but creates an HTTPS server.
%     Use =|--certfile|=, =|--keyfile|=, =|-pwfile|=,
%     =|--password|= and =|--cipherlist|= to configure SSL for
%     this server.
%
%     $ --certfile=File :
%     The server certificate for HTTPS.
%
%     $ --keyfile=File :
%     The server private key for HTTPS.
%
%     $ --pwfile=File :
%     File holding the password for accessing  the private key. This
%     is preferred over using =|--password=PW|=   as it allows using
%     file protection to avoid leaking the password.  The file is
%     read _before_ the server drops privileges when started with
%     the =|--user|= option.
%
%     $ --password=PW :
%     The password for accessing the private key. See also `--pwfile`.
%
%     $ --cipherlist=Ciphers :
%     One or more cipher strings separated by colons. See the OpenSSL
%     documentation for more information. Starting with SWI-Prolog
%     7.5.11, the default value is always a set of ciphers that was
%     considered secure enough to prevent all critical attacks at the
%     time of the SWI-Prolog release.
%
%     $ --interactive[=Bool] :
%     If =true= (default =false=) implies =|--no-fork|= and presents
%     the Prolog toplevel after starting the server.
%
%     $ --gtrace=[Bool] :
%     Use the debugger to trace http_daemon/1.
%
%     $ --sighup=Action :
%     Action to perform on =|kill -HUP <pid>|=.  Default is `reload`
%     (running make/0).  Alternative is `quit`, stopping the server.
%
%   Other options are converted  by   argv_options/3  and  passed to
%   http_server/1.  For example, this allows for:
%
%     $ --workers=Count :
%     Set the number of workers for the multi-threaded server.
%
%   http_daemon/0 is defined as below.  The   start  code for a specific
%   server can use this as a starting  point, for example for specifying
%   defaults.
%
%   ```
%   http_daemon :-
%       current_prolog_flag(argv, Argv),
%       argv_options(Argv, _RestArgv, Options),
%       http_daemon(Options).
%   ```
%
%   @see http_daemon/1

http_daemon :-
    current_prolog_flag(argv, Argv),
    argv_options(Argv, _RestArgv, Options),
    http_daemon(Options).

%!  http_daemon(+Options)
%
%   Start the HTTP server as a  daemon process. This predicate processes
%   a Prolog option list. It  is   normally  called  from http_daemon/0,
%   which derives the option list from the command line arguments.
%
%   Error handling depends on whether  or   not  interactive(true) is in
%   effect. If so, the error is printed before entering the toplevel. In
%   non-interactive mode this predicate calls halt(1).

http_daemon(Options) :-
    catch(http_daemon_guarded(Options), Error, start_failed(Error)).

start_failed(Error) :-
    interactive,
    !,
    print_message(warning, Error).
start_failed(Error) :-
    print_message(error, Error),
    halt(1).

%!  http_daemon_guarded(+Options)
%
%   Helper that is started from http_daemon/1. See http_daemon/1 for
%   options that are processed.

http_daemon_guarded(Options) :-
    option(help(true), Options),
    !,
    print_message(information, http_daemon(help)),
    halt.
http_daemon_guarded(Options) :-
    setup_debug(Options),
    kill_x11(Options),
    option_servers(Options, Servers0),
    maplist(make_socket, Servers0, Servers),
    (   option(fork(true), Options, true),
        option(interactive(false), Options, false),
        can_switch_user(Options)
    ->  fork(Who),
        (   Who \== child
        ->  halt
        ;   disable_development_system,
            setup_syslog(Options),
            write_pid(Options),
            setup_output(Options),
            switch_user(Options),
            setup_signals(Options),
            start_servers(Servers),
            wait(Options)
        )
    ;   write_pid(Options),
        switch_user(Options),
        setup_signals(Options),
        start_servers(Servers),
        wait(Options)
    ).

%!  option_servers(+Options, -Sockets:list)
%
%   Find all sockets that must be created according to Options. Each
%   socket is a term server(Scheme, Address, Opts), where Address is
%   either a plain port (integer) or Host:Port. The latter binds the
%   port  to  the  interface  belonging    to   Host.  For  example:
%   socket(http, localhost:8080, Opts) creates an   HTTP socket that
%   binds to the localhost  interface  on   port  80.  Opts  are the
%   options specific for the given server.

option_servers(Options, Sockets) :-
    opt_sockets(Options, [], [], Sockets).

opt_sockets([], Options, [], [Socket]) :-
    !,
    make_server(http(true), Options, Socket).
opt_sockets([], _, Sockets, Sockets).
opt_sockets([H|T], OptsH, Sockets0, Sockets) :-
    server_option(H),
    !,
    append(OptsH, [H], OptsH1),
    opt_sockets(T, OptsH1, Sockets0, Sockets).
opt_sockets([H|T0], Opts, Sockets0, Sockets) :-
    server_start_option(H),
    !,
    server_options(T0, T, Opts, SOpts),
    make_server(H, SOpts, Socket),
    append(Sockets0, [Socket], Sockets1),
    opt_sockets(T, Opts, Sockets1, Sockets).
opt_sockets([_|T], Opts, Sockets0, Sockets) :-
    opt_sockets(T, Opts, Sockets0, Sockets).

server_options([], [], Options, Options).
server_options([H|T], Rest, Options0, Options) :-
    server_option(H),
    !,
    generalise_option(H, G),
    delete(Options0, G, Options1),
    append(Options1, [H], Options2),
    server_options(T, Rest, Options2, Options).
server_options([H|T], [H|T], Options, Options) :-
    server_start_option(H),
    !.
server_options([_|T0], Rest, Options0, Options) :-
    server_options(T0, Rest, Options0, Options).

generalise_option(H, G) :-
    H =.. [Name,_],
    G =.. [Name,_].

server_start_option(http(_)).
server_start_option(https(_)).

server_option(port(_)).
server_option(ip(_)).
server_option(certfile(_)).
server_option(keyfile(_)).
server_option(pwfile(_)).
server_option(password(_)).
server_option(cipherlist(_)).
server_option(workers(_)).
server_option(redirect(_)).
server_option(timeout(_)).
server_option(keep_alive_timeout(_)).

make_server(http(Address0), Options0, server(http, Address, Options)) :-
    make_address(Address0, 80, Address, Options0, Options).
make_server(https(Address0), Options0, server(https, Address, SSLOptions)) :-
    make_address(Address0, 443, Address, Options0, Options),
    merge_https_options(Options, SSLOptions).

make_address(true, DefPort, Address, Options0, Options) :-
    !,
    option(port(Port), Options0, DefPort),
    (   option(ip(Bind), Options0)
    ->  Address = (Bind:Port)
    ;   Address = Port
    ),
    merge_options([port(Port)], Options0, Options).
make_address(Bind:Port, _, Bind:Port, Options0, Options) :-
    !,
    must_be(atom, Bind),
    must_be(integer, Port),
    merge_options([port(Port), ip(Bind)], Options0, Options).
make_address(Port, _, Address, Options0, Options) :-
    integer(Port),
    !,
    (   option(ip(Bind), Options0)
    ->  Address = (Bind:Port)
    ;   Address = Port,
        merge_options([port(Port)], Options0, Options)
    ).
make_address(Spec, _, Address, Options0, Options) :-
    atomic(Spec),
    split_string(Spec, ":", "", [BindString, PortString]),
    number_string(Port, PortString),
    !,
    atom_string(Bind, BindString),
    Address = (Bind:Port),
    merge_options([port(Port), ip(Bind)], Options0, Options).
make_address(Spec, _, _, _, _) :-
    domain_error(address, Spec).

:- dynamic sni/3.

merge_https_options(Options, [SSL|Options]) :-
    (   option(certfile(CertFile), Options),
        option(keyfile(KeyFile), Options)
    ->  prepare_https_certificate(CertFile, KeyFile, Passwd0),
        read_file_to_string(CertFile, Certificate, []),
        read_file_to_string(KeyFile, Key, []),
        Pairs = [Certificate-Key]
    ;   Pairs = []
    ),
    ssl_secure_ciphers(SecureCiphers),
    option(cipherlist(CipherList), Options, SecureCiphers),
    (   string(Passwd0)
    ->  Passwd = Passwd0
    ;   options_password(Options, Passwd)
    ),
    findall(HostName-HostOptions, http:sni_options(HostName, HostOptions), SNIs),
    maplist(sni_contexts, SNIs),
    SSL = ssl([ certificate_key_pairs(Pairs),
                cipher_list(CipherList),
                password(Passwd),
                sni_hook(http_unix_daemon:sni)
              ]).

sni_contexts(Host-Options) :-
    ssl_context(server, SSL, Options),
    assertz(sni(_, Host, SSL)).

%!  http_certificate_hook(+CertFile, +KeyFile, -Password) is semidet.
%
%   Hook called before starting the server  if the --https option is
%   used.  This  hook  may  be  used    to  create  or  refresh  the
%   certificate. If the hook binds Password to a string, this string
%   will be used to  decrypt  the  server   private  key  as  if the
%   --password=Password option was given.

prepare_https_certificate(CertFile, KeyFile, Password) :-
    http_certificate_hook(CertFile, KeyFile, Password),
    !.
prepare_https_certificate(_, _, _).


options_password(Options, Passwd) :-
    option(password(Passwd), Options),
    !.
options_password(Options, Passwd) :-
    option(pwfile(File), Options),
    !,
    read_file_to_string(File, String, []),
    split_string(String, "", "\r\n\t ", [Passwd]).
options_password(_, '').

%!  start_servers(+Servers) is det.
%
%   Start the HTTP server.  It performs the following steps:
%
%     1. Call broadcast(http(pre_server_start))
%     2. Foreach server
%        a. Call broadcast(http(pre_server_start(Port)))
%        b. Call http_server(http_dispatch, Options)
%        c. Call broadcast(http(post_server_start(Port)))
%     3. Call broadcast(http(post_server_start))
%
%   This predicate can be  hooked   using  http_server_hook/1.  This
%   predicate is executed after
%
%     - Forking
%     - Setting I/O (e.g., to talk to the syslog daemon)
%     - Dropping root privileges (--user)
%     - Setting up signal handling

start_servers(Servers) :-
    broadcast(http(pre_server_start)),
    maplist(start_server, Servers),
    broadcast(http(post_server_start)).

start_server(server(_Scheme, Socket, Options)) :-
    option(redirect(To), Options),
    !,
    http_server(server_redirect(To), [tcp_socket(Socket)|Options]).
start_server(server(_Scheme, Socket, Options)) :-
    http_server_hook([tcp_socket(Socket)|Options]),
    !.
start_server(server(_Scheme, Socket, Options)) :-
    option(port(Port), Options),
    broadcast(http(pre_server_start(Port))),
    http_server(http_dispatch, [tcp_socket(Socket)|Options]),
    broadcast(http(post_server_start(Port))).

make_socket(server(Scheme, Address, Options),
            server(Scheme, Socket, Options)) :-
    tcp_socket(Socket),
    catch(bind_socket(Socket, Address), Error,
          make_socket_error(Error, Address)),
    debug(daemon(socket),
          'Created socket ~p, listening on ~p', [Socket, Address]).

bind_socket(Socket, Address) :-
    tcp_setopt(Socket, reuseaddr),
    tcp_bind(Socket, Address),
    tcp_listen(Socket, 5).

make_socket_error(error(socket_error(_,_), _), Address) :-
    address_port(Address, Port),
    integer(Port),
    Port =< 1000,
    !,
    verify_root(open_port(Port)).
make_socket_error(Error, _) :-
    throw(Error).

address_port(_:Port, Port) :- !.
address_port(Port, Port).

%!  disable_development_system
%
%   Disable some development stuff.

disable_development_system :-
    set_prolog_flag(editor, '/bin/false').

%!  enable_development_system
%
%   Re-enable the development environment. Currently  re-enables xpce if
%   this was loaded, but not  initialised   and  causes  the interactive
%   toplevel to be re-enabled.

enable_development_system :-
    assertz(interactive),
    set_prolog_flag(xpce_threaded, true),
    set_prolog_flag(message_ide, true),
    (   current_prolog_flag(xpce_version, _)
    ->  call(pce_dispatch([]))
    ;   true
    ),
    set_prolog_flag(toplevel_goal, prolog).

%!  setup_syslog(+Options) is det.
%
%   Setup syslog interaction.

setup_syslog(Options) :-
    option(syslog(Ident), Options),
    !,
    openlog(Ident, [pid], user).
setup_syslog(_).


%!  setup_output(+Options) is det.
%
%   Setup output from the daemon process. The default is to send all
%   output to a  null-stream  (see   open_null_stream/1).  With  the
%   option output(File), all output is written to File.

setup_output(Options) :-
    option(output(File), Options),
    !,
    open(File, write, Out, [encoding(utf8)]),
    set_stream(Out, buffer(line)),
    detach_IO(Out).
setup_output(_) :-
    open_null_stream(Out),
    detach_IO(Out).


%!  write_pid(+Options) is det.
%
%   If the option pidfile(File) is  present,   write  the PID of the
%   daemon to this file.

write_pid(Options) :-
    option(pidfile(File), Options),
    current_prolog_flag(pid, PID),
    !,
    setup_call_cleanup(
        open(File, write, Out),
        format(Out, '~d~n', [PID]),
        close(Out)),
    at_halt(catch(delete_file(File), _, true)).
write_pid(_).


%!  switch_user(+Options) is det.
%
%   Switch to the target user and group. If the server is started as
%   root, this option *must* be present.

switch_user(Options) :-
    option(user(User), Options),
    !,
    verify_root(switch_user(User)),
    (   option(group(Group), Options)
    ->  set_user_and_group(User, Group)
    ;   set_user_and_group(User)
    ),
    prctl(set_dumpable(true)).      % re-enable core dumps on Linux
switch_user(_Options) :-
    verify_no_root.

%!  can_switch_user(Options) is det.
%
%   Verify the user options before  forking,   so  we  can print the
%   message in time.

can_switch_user(Options) :-
    option(user(User), Options),
    !,
    verify_root(switch_user(User)).
can_switch_user(_Options) :-
    verify_no_root.

verify_root(_Task) :-
    geteuid(0),
    !.
verify_root(Task) :-
    print_message(error, http_daemon(no_root(Task))),
    halt(1).

verify_no_root :-
    geteuid(0),
    !,
    throw(error(permission_error(open, server, http),
                context('Refusing to run HTTP server as root', _))).
verify_no_root.

:- if(\+current_predicate(prctl/1)).
prctl(_).
:- endif.

%!  server_redirect(+To, +Request)
%
%   Redirect al requests for this server to the specified server. To
%   is one of:
%
%     $ A port (integer) :
%     Redirect to the server running on that port in the same
%     Prolog process.
%     $ =true= :
%     Results from just passing =|--redirect|=.  Redirects to
%     an HTTPS server in the same Prolog process.
%     $ A URL :
%     Redirect to the the given URL + the request uri.  This can
%     be used if the server cannot find its public address.  For
%     example:
%
%       ```
%       --http --redirect=https://myhost.org --https
%       ```

server_redirect(Port, Request) :-
    integer(Port),
    http_server_property(Port, scheme(Scheme)),
    http_public_host(Request, Host, _Port, []),
    memberchk(request_uri(Location), Request),
    (   default_port(Scheme, Port)
    ->  format(string(To), '~w://~w~w', [Scheme, Host, Location])
    ;   format(string(To), '~w://~w:~w~w', [Scheme, Host, Port, Location])
    ),
    throw(http_reply(moved_temporary(To))).
server_redirect(true, Request) :-
    !,
    http_server_property(P, scheme(https)),
    server_redirect(P, Request).
server_redirect(URI, Request) :-
    memberchk(request_uri(Location), Request),
    atom_concat(URI, Location, To),
    throw(http_reply(moved_temporary(To))).

default_port(http, 80).
default_port(https, 443).


%!  setup_debug(+Options) is det.
%
%   Initialse debug/3 topics. The  =|--debug|=   option  may be used
%   multiple times.

setup_debug(Options) :-
    setup_trace(Options),
    nodebug(_),
    debug(daemon),
    enable_debug(Options).

enable_debug([]).
enable_debug([debug(Topic)|T]) :-
    !,
    atom_to_term(Topic, Term, _),
    debug(Term),
    enable_debug(T).
enable_debug([_|T]) :-
    enable_debug(T).

setup_trace(Options) :-
    option(gtrace(true), Options),
    !,
    gtrace.
setup_trace(_).


%!  kill_x11(+Options) is det.
%
%   Get rid of X11 access if interactive is false.

kill_x11(Options) :-
    getenv('DISPLAY', Display),
    Display \== '',
    option(interactive(false), Options, false),
    !,
    setenv('DISPLAY', ''),
    set_prolog_flag(gui, false).
kill_x11(_).


%!  setup_signals(+Options)
%
%   Prepare the server for signal handling.   By  default SIGINT and
%   SIGTERM terminate the server. SIGHUP causes   the  server to run
%   make/0.

setup_signals(Options) :-
    option(interactive(true), Options, false),
    !.
setup_signals(Options) :-
    on_signal(int,  _, quit),
    on_signal(term, _, quit),
    option(sighup(Action), Options, reload),
    must_be(oneof([reload,quit]), Action),
    on_signal(usr1, _, logrotate),
    on_signal(hup,  _, Action).

:- public
    quit/1,
    reload/1,
    logrotate/1.

quit(Signal) :-
    debug(daemon, 'Dying on signal ~w', [Signal]),
    thread_send_message(main, quit).

reload(Signal) :-
    debug(daemon, 'Reload on signal ~w', [Signal]),
    thread_send_message(main, reload).

logrotate(Signal) :-
    debug(daemon, 'Closing log files on signal ~w', [Signal]),
    thread_send_message(main, logrotate).

%!  wait(+Options)
%
%   This predicate runs in the  main   thread,  waiting for messages
%   send by signal handlers to control   the server. In addition, it
%   broadcasts  maintenance(Interval,  Deadline)    messages   every
%   Interval seconds. These messages may   be trapped using listen/2
%   for performing scheduled maintenance such as rotating log files,
%   cleaning stale data, etc.

wait(Options) :-
    option(interactive(true), Options, false),
    !,
    enable_development_system.
wait(Options) :-
    thread_self(Me),
    option(maintenance_interval(Interval), Options, 300),
    Interval > 0,
    !,
    first_deadline(Interval, FirstDeadline),
    State = deadline(0),
    repeat,
        State = deadline(Count),
        Deadline is FirstDeadline+Count*Interval,
        (   thread_idle(thread_get_message(Me, Msg, [deadline(Deadline)]),
                        long)
        ->  catch(ignore(handle_message(Msg)), E,
                  print_message(error, E)),
            Msg == quit,
            catch(broadcast(http(shutdown)), E,
                  print_message(error, E)),
            halt(0)
        ;   Count1 is Count + 1,
            nb_setarg(1, State, Count1),
            catch(broadcast(maintenance(Interval, Deadline)), E,
                  print_message(error, E)),
            fail
        ).
wait(_) :-
    thread_self(Me),
    repeat,
        thread_idle(thread_get_message(Me, Msg), long),
        catch(ignore(handle_message(Msg)), E,
              print_message(error, E)),
        Msg == quit,
        !,
        halt(0).

handle_message(reload) :-
    make,
    broadcast(logrotate).
handle_message(logrotate) :-
    broadcast(logrotate).

first_deadline(Interval, Deadline) :-
    get_time(Now),
    Deadline is ((integer(Now) + Interval - 1)//Interval)*Interval.


                 /*******************************
                 *            HOOKS             *
                 *******************************/

%!  http_server_hook(+Options) is semidet.
%
%   Hook that is called to start the  HTTP server. This hook must be
%   compatible to http_server(Handler,  Options).   The  default  is
%   provided by start_server/1.


%!  http:sni_options(-HostName, -SSLOptions) is multi.
%
%   Hook  to   provide  Server  Name  Indication   (SNI)  for  TLS
%   servers. When starting an HTTPS  server, all solutions of this
%   predicate are  collected and a suitable  sni_hook/1 is defined
%   for ssl_context/3  to use different contexts  depending on the
%   host  name  of the  client  request.   This hook  is  executed
%   _before_ privileges are dropped.


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message//1.

prolog:message(http_daemon(help)) -->
    [ 'Usage: <program> option ...'-[], nl,
      'Options:'-[], nl, nl,
      '  --port=port        HTTP port to listen to'-[], nl,
      '  --ip=IP            Only listen to this ip (--ip=localhost)'-[], nl,
      '  --debug=topic      Print debug message for topic'-[], nl,
      '  --syslog=ident     Send output to syslog daemon as ident'-[], nl,
      '  --user=user        Run server under this user'-[], nl,
      '  --group=group      Run server under this group'-[], nl,
      '  --pidfile=path     Write PID to path'-[], nl,
      '  --output=file      Send output to file (instead of syslog)'-[], nl,
      '  --fork=bool        Do/do not fork'-[], nl,
      '  --http[=Address]   Create HTTP server'-[], nl,
      '  --https[=Address]  Create HTTPS server'-[], nl,
      '  --certfile=file    The server certificate'-[], nl,
      '  --keyfile=file     The server private key'-[], nl,
      '  --pwfile=file      File holding password for the private key'-[], nl,
      '  --password=pw      Password for the private key'-[], nl,
      '  --cipherlist=cs    Cipher strings separated by colons'-[], nl,
      '  --redirect=to      Redirect all requests to a URL or port'-[], nl,
      '  --interactive=bool Enter Prolog toplevel after starting server'-[], nl,
      '  --gtrace=bool      Start (graphical) debugger'-[], nl,
      '  --sighup=action    Action on SIGHUP: reload (default) or quit'-[], nl,
      '  --workers=count    Number of HTTP worker threads'-[], nl,
      '  --timeout=sec      Time to wait for client to complete request'-[], nl,
      '  --keep_alive_timeout=sec'-[], nl,
      '                     Time to wait for a new request'-[], nl,
      nl,
      'Boolean options may be written without value (true) or as --no-name (false)'-[], nl,
      'Address is a port number or host:port, e.g., 8080 or localhost:8080'-[], nl,
      'Multiple servers can be started by repeating --http and --https'-[], nl,
      'Each server merges the options before the first --http(s) and up the next'-[]
    ].
prolog:message(http_daemon(no_root(switch_user(User)))) -->
    [ 'Program must be started as root to use --user=~w.'-[User] ].
prolog:message(http_daemon(no_root(open_port(Port)))) -->
    [ 'Cannot open port ~w.  Only root can open ports below 1000.'-[Port] ].
