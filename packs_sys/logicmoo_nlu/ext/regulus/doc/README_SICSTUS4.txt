RUNNING REGULUS APPLICATIONS UNDER SICSTUS 4

Regulus is intended to work under both Sicstus 3 and Sicstus 4. If you are
trying to port a Sicstus 3 Regulus application to Sicstus 4, here are
some initial pointers:

1. Sicstus 4 is NOT downward-compatible with Sicstus 3, so don't expect
your app to work unmodified. It almost certainly won't.

2. Before doing anything else, look at the Sicstus 4 release notes. They
are in doc/pdf/relnotes.pdf in your Sicstus 4 release directory, and list
most of the top-level incompatibilities.

3. Regulus makes heavy use of conditional loading in the code files, and
in particular in $REGULUS/PrologLib/utilities. The predicates required to
do this conditional loading are defined in $REGULUS/PrologLib/compatibility.pl.
You are strongly advised to add the following line at the top of all your
Prolog files:

:- ensure_loaded('$REGULUS/PrologLib/compatibility').

If you want your app to run both under Sicstus 3 and under Sicstus 4,
you are advised to look carefully at $REGULUS/PrologLib/compatibility.pl and
$REGULUS/PrologLib/utilities.

4. A couple of specific Sicstus 3/4 compatibility things that are
particularly relevant to Regulus applications:

- Replace atom_chars/2 with atom_codes/2 everywhere.

- If you are using sockets to connect to other processes, an easy
solution is to use the compatibility predicates in
$REGULUS/PrologLib/compatibility.pl. 

Replace server-side sequences like

	current_host(Host),
	socket('AF_INET', Socket),
	socket_bind(Socket, 'AF_INET'(Host, Port)),
	socket_listen(Socket, 5),
	socket_accept(Socket, Client, Stream)

with 

	safe_socket_server_open(Port, Socket),
	safe_socket_server_accept(Socket, Client, Stream)

You can see an example in $REGULUS/Prolog/dialogue_server.pl.
     
Replace client-side sequences like

	socket('AF_INET', Socket),
	socket_connect(Socket, 'AF_INET'(Host, Port), Stream)

with

	safe_socket('AF_INET', Socket),
	safe_socket_connect(Socket, Host, Port, Stream),

You can see an example in $REGULUS/Examples/Calendar/Prolog/toy1_app_using_server.pl.

5. The Sicstus 4 port of Regulus is still very new. If you see things that look
like problems in the Regulus code, feel free to mail me at Emmanuel.Rayner at issco.unige.ch
or (better) submit a Regulus bug at http://sourceforge.net/projects/regulus/ > Tracker > Bugs