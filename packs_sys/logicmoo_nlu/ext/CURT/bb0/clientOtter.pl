/*************************************************************************

         name: clientOtter.pl (Chapter 5)
      version: June 18, 1998
  description: Client for Theorem Prover Otter
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- ensure_loaded(comsemOperators).

:- use_module(callInference,[callTheoremProver/3]).
	      
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
	
        format('~nOTTER: ready',[]),
	flush_output,
	rd(callTheoremProver(ID,Formula)),
	format('~nOTTER: attacking problem ~p ',[ID]), 
	callTheoremProver(otter,Formula,Proof),
	format('~nOTTER: proof for problem ~p: ~p',[ID,Proof]), 
	out(proof(ID,otter,Proof)),
	(
	    in_noblock(callTheoremProver(ID,_)), !,
	    format('~nOTTER: cleaning up ~p',[ID])
	;
	    true
	),

        consumer.



/*========================================================================
   Self Starting
========================================================================*/

:- start.
