/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 * to activate debugging for this module, uncomment:
 */
% :- debug(process_observe).

:- module(process_observe,[
	process_observe/3,
	process_observe/2,	
	process_unobserve/2,
	process_notify/2,
	process_dispatch/3
]).

:- use_module(library(debug)).

:-dynamic process_observe_hook/3,process_unobserve_hook/3.
:-multifile process_observe_hook/3,process_unobserve_hook/3.

% process_observe_hook(Thread,Subject,Key),
% A hook predicate that will be called each time an observer registers to 
% a subject, if it is not already registered with this subject.
% Clients that provide observable subjects can add clauses to these predicates
% if they need to take additional actions on registration of an observer
% to a subject. If the call succeeds, it is assumes that these additional actions have 
% been taken.
% 
process_observe_hook(_,_,_):-
	fail.	     	

% process_unobserve_hook(Thread,Subject,Key),
% A hook predicate that will be called each time an observer unregisters to 
% a subject. see process_observe_hook/3
% 
process_unobserve_hook(_,_,_):-
	fail.	     	

call_observe_hook(Thread,Subject,Key):-
	catch(process_observe_hook(Thread,Subject,Key),E,print_message(error,E)),
	!.  
call_observe_hook(_,_,_).
call_unobserve_hook(Thread,Subject,Key):-
	catch(process_unobserve_hook(Thread,Subject,Key),E,print_message(error,E)),
	!.  		
call_unobserve_hook(_,_,_).	


/*
 * backwards compatibility. 
 */
process_observe(Thread,Subject):-
   term_to_atom(Subject,Key),
   process_observe(Thread,Subject,Key). 
%% process_observe(+Thread,+Subject).
%  Add an observer to a subject.
% 
%  
% 
%  @param Thread the observer, i.e. a thread that is running dispatch/3
%  @param Subject the subject to observe. 
% 		This term is unified with the subject given as second argument to notify/2.
%  @param Key should be an atom. During notification, if the Subject terms was successfully unified,
% 		the key is also passed to the observer. The idea of this is to help observers calling from
% 		Java, or otherwise lacking the concept of unification, to recognize the Subject they subscribed 
% 		for. 
% 
process_observe(Thread,Subject,Key) :-
  recorded(process_observer,observation(Thread,OtherSubject,Key), _),
  OtherSubject =@= Subject,
  !.

process_observe(Thread,Subject,Key) :-
%  sync:init_idb(Subject),
  call_observe_hook(Thread,Subject,Key),
  recordz(process_observer,observation(Thread,Subject,Key), _).



%%  process_unobserve(+Thread,+Subject) 
%  Remove an observer from a subject.
% 
%  @param Thread the observer thread to remove.
%  @param Subject the subject from which to remove the observer.
% 

process_unobserve(Thread,Subject) :-
  recorded(process_observer,observation(Thread,OtherSubject,Key),Ref),
  OtherSubject =@= Subject,
  erase(Ref),
  %sync:unregister_observer(Subject). 
  call_unobserve_hook(Thread,Subject,Key).

%% process_notify(+Subject,+Event)
% Notify all active observers.
% If observer's thread is stopped
% it will be removed.
% 
process_notify(Subject,Event) :-
   debug(process_observe,'~w~n',[process_notify(Subject,Event)]),
   forall(
    	( 
    	  recorded(process_observer,observation(Thread,Subject,_),Ref)
    	),
    	(	current_thread(Thread,running)
    	->	(    	      
    	      thread_send_message(Thread,notify(Subject,Event)),
   	      debug(process_observe,'~w~n',[thread_send_message(Thread,notify(Subject,Event))])
    	   
    	);	erase(Ref)
    	)
    ).   

%% process_dispatch(-Subject,-Key,-Event)
% Recieve events.
% This predicate is intended to be called by observer threads. 
% It produces solutions for every recieved event, i.e. every time
% process_notify/2 is called on a subject the observer thread is subscribed for.
% If it recieves an event for the subject '$stop' it will cut and fail.

 
 process_dispatch(Subject,Key,Event):-
     	thread_self(Me),
     	repeat,
	     	thread_get_message(notify(Subject,Event)),
	     	(	Subject='$abort'
	     	->	!
	     	;	recorded(process_observer,observation(Me,Subject,Key), _)
	     	).
	     	
	     	


