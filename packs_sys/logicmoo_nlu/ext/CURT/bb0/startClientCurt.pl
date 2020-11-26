/*************************************************************************

         name: startClientCurt.pl (Volume 1, Chapter 6)
      version: April 27, 2001
  description: Wrapper that starts all clients and servers for clientCurt
       author: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- use_module(library(system),[exec/3,kill/2,sleep/1]).

:- use_module(library('linda/client')).

:- dynamic startedpid/1.


/*========================================================================
     Start Module
========================================================================*/
   
startModule(E,Sleep):- 
    exec(E,[std,std,std],Pid1),
    assert(startedpid(Pid1)),
    in(pid(Pid2)),
    assert(startedpid(Pid2)),
    sleep(Sleep).


/*========================================================================
     Init Linda Client
========================================================================*/
 
init_client:-

        open('/tmp/curtHostPort',read,Stream),
        read(Stream,host_port(Host,Port)),
        close(Stream),
        linda_client(Host:Port).




/*========================================================================
     Print Proces IDs
========================================================================*/
   
printPIDs:- 
    startedpid(Pid),
    format('~nProces ID: ~p',[Pid]),
    fail.

printPIDs.


/*========================================================================
     Main Predicate
========================================================================*/
  
start:-
	exec('sicstus -l curtServer.pl &',[std,std,std],Pid1),
	sleep(1),
	assert(startedpid(Pid1)),
        open('/tmp/curtServerPid',read,Stream),
        read(Stream,pid(Pid2)),
        close(Stream),
	assert(startedpid(Pid2)),	
	init_client,
	startModule('xterm -T CURT -sb -e sicstus -l clientCurt.pl &',1),
	startModule('xterm -T MACE -sb -e sicstus -l clientMace.pl &',1),
	startModule('xterm -T BLIKSEM -sb -e sicstus -l clientBliksem.pl &',1),
	startModule('xterm -T OTTER -sb -e sicstus -l clientOtter.pl &',1),
	printPIDs,
	nl, write('READY!').


/*========================================================================
     Kill Processes
========================================================================*/
   
kill:-
	retract(startedpid(Pid)),
	format('~nKilling process ~p.',[Pid]),
	on_exception(_,
		     kill(Pid,1),
		     format('~nNot able to kill process Pid ~p.',[Pid])
		    ),
	kill.

kill:-
	close_client.



/*========================================================================
    Info
========================================================================*/
   
info:-
	
   format('~n * CURT (LINDA VERSION), by Patrick Blackburn & Johan Bos *',[]),
   format('~n * Type "start." to launch Linda server and all clients   *',[]),
   format('~n * Type "kill." to kill the server and clients.           *~n~n',[]).


/*========================================================================
       Print info when starting
========================================================================*/

:- info.
