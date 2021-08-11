:- use_module(replay).

r(C) :-
	debug(replay),
	http_replay('httpd-ec.log',
		    [host(localhost), port(3040), concurrent(C)]).
