/*************************************************************************

         name: clientMace.pl
      version: August 7, 2001
  description: Linda Client for Model Builder Mace
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- ensure_loaded(comsemOperators).

:- use_module(callInference,[callModelBuilder/4]).

:- use_module(library('linda/client')).

:- use_module(library(system),[pid/1]).


/*========================================================================
    Init
========================================================================*/

start:-
        init_client,
        pid(Pid),
        out(pid(Pid)),
        consumer,
        close_client,
        halt.


init_client:-
        open('/tmp/curtHostPort',read,Stream),
        read(Stream,host_port(Host,Port)),
        close(Stream),
        linda_client(Host:Port).



/*========================================================================
    Consumer
========================================================================*/


consumer:-
	
        format('~nMACE: ready',[]), flush_output,
        rd(callModelBuilder(ID,Formula,Size)),
	format('~nMACE: attacking problem ~p',[ID]),
	callModelBuilder(mace,Formula,Size,Model),
	format('~nMACE: found model ~p',[Model]),
	out(model(ID,mace,Model)),
	(
	    in_noblock(callModelBuilder(ID,_,_)), !,
	    format('~nMACE: cleaning up ~p',[ID])
	;
	    true
	),


        consumer.


/*========================================================================
   Self Starting
========================================================================*/

:- start.



