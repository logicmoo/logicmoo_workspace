
:- use_module(library('linda/server')).
:- use_module(library(system),[pid/1]).
   

start(Host:Port):-
        
        open('/tmp/curtHostPort',write,Stream1),
        format(Stream1,"host_port('~p',~p).~n",[Host,Port]),
        close(Stream1),
	pid(Pid),
        open('/tmp/curtServerPid',write,Stream2),
        format(Stream2,"pid(~p).~n",[Pid]),
        close(Stream2).
	

:- linda((Host:Port)-(user:start(Host:Port))).
