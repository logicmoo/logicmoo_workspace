%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : indigolog-vanilla.pl
%
%   IndiGolog vanilla interpreter
%
% First written by Hector Levesque
% Modified and adapted by Sebastian Sardina March 1999.
% This file contains vanilla Prolog code. If you run in a special Prolog
% platform you may want to consult indigolog-vanilla_xxx.pl where xxx is the
% corresponding Prolog implementation (e.g., SWI or ECLIPSE)
%
% This file contains the original IndiGolog interpreter
%
%   The main tool provided in this file is the following predicate:
%
% -- indigolog(E):  E is an IndiGolog program
%
% For more information on Golog and some of its variants, see:
%               http://www.cs.toronto.edu/~cogrobo/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%                             March, 1999
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
%
%        Do not distribute without permission.
%        Include this notice in any copy made.
%
%
%         Copyright (c) 2002-2005 by The University of Toronto,
%                        Toronto, Ontario, Canada.
%
%                          All Rights Reserved
%
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  In addition to a (Con)Golog program, users provide these predicates:  
%
%      prim_fluent(fluent),             for each primitive fluent        
%      prim_action(action),             for each primitive action        
%      exog_action(action),             for each exogenous action        
%      senses(action,fluent),           for each sensing action          
%      poss(action,cond)                when cond, action is executable  
%      initially(fluent,value)          fluent has value in S0           
%      causes_val(action,fluent,value,cond)                              
%            when cond holds, doing act causes fluent to have value      
%
%      execute(action,sensing_result)   do the action, return the result 
%            can use ask_execute
%      exog_occurs(action)              return an exog action            
%            can use ask_exog_occurs (or fail, if none)                  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:-dynamic senses/2.
:-dynamic exog_action/1.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% MAIN LOOP: indigolog and indixeq %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% indigolog(E): E is a high-level program 
% 	The history H is a list of actions (prim or exog), initially [] (empty)
% 	Sensing reports are inserted as actions of  the form e(fluent,value)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
indigolog(E) :- indigo(E,[]).

% (1)- In each single step ask for an exogenous action, check it and 
%	continue execution inserting that exogenous action 
indigo(E,H) :- 	exog_occurs(Act), exog_action(Act), !, indigo(E,[Act|H]).

% (2) - Find a signle step (trans), execute it, commit and continue 
indigo(E,H) :- trans(E,H,E1,H1), indixeq(H,H1,H2), !, indigo(E1,H2).

% (3) - If E is final write the length of history H 
indigo(E,H) :- final(E,H), nl, length(H,N), write(N), write(' actions.'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% indixeq(H1,H2,H3): Implementation of execution of an action.
% 	H1 is the original history, H2 is H1 with the new action to be 
%	executed and H3 is the resulting history after executing such new 
% 	action. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% (1) - No action was performed so we dont execute anything 
indixeq(H,H,H). 
% (2) - The action is not a sensing one: execute and ignore its sensing 
indixeq(H,[Act|H],[Act|H]) :- \+ senses(Act,_), execute(Act,_).
% (3) - The action is a sensing one for fluent F: execute sensing action
indixeq(H,[Act|H],[e(F,Sr),Act|H]) :- senses(Act,F), execute(Act,Sr).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% exog_occurs(Act) and execute(Act,Sr): 
% 	predicates that make contact with the outside world.  
%	Here are two basic versions using read and write that the domain 
%	may use as a simulated environment.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ask_exog_occurs(Act) :- write('Exogenous input:'), read(Act).
ask_execute(Act,Sr) :-  write(Act), senses(Act,_) -> (write(':'),read(Sr)); nl.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% TRANS and FINAL: lang semantics% %%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       /* (a) - CONGOLOG */
final(conc(E1,E2),H) :- final(E1,H), final(E2,H).
final(pconc(E1,E2),H) :- final(E1,H), final(E2,H).
final(iconc(_),_).

trans(conc(E1,E2),H,conc(E,E2),H1) :- trans(E1,H,E,H1).
trans(conc(E1,E2),H,conc(E1,E),H1) :- trans(E2,H,E,H1).
trans(pconc(E1,E2),H,E,H1) :- 
    trans(E1,H,E3,H1) -> E=pconc(E3,E2) ; (trans(E2,H,E3,H1), E=pconc(E1,E3)).
trans(iconc(E),H,conc(E1,iconc(E)),H1) :- trans(E,H,E1,H1).

       /* (b) - GOLOG */
final([],_).
final([E|L],H) :- final(E,H), final(L,H).
final(ndet(E1,E2),H) :- final(E1,H) ; final(E2,H).
final(if(P,E1,E2),H) :- holds(P,H) -> final(E1,H) ; final(E2,H).
final(star(_),_).
final(while(P,E),H) :- \+ holds(P,H) ; final(E,H).
final(pi(V,E),H) :- subv(V,_,E,E2), final(E2,H).
final(E,H) :- proc(E,E2), final(E2,H).

trans([E|L],H,[E1|L],H2) :- trans(E,H,E1,H2).
trans([E|L],H,E1,H2) :- \+ L=[], final(E,H), trans(L,H,E1,H2).
trans(?(P),H,[],H) :- holds(P,H). 
trans(ndet(E1,E2),H,E,H1) :- trans(E1,H,E,H1) ; trans(E2,H,E,H1).
trans(if(P,E1,E2),H,E,H1) :- holds(P,H) -> trans(E1,H,E,H1) ; trans(E2,H,E,H1).
trans(star(E),H,[E1,star(E)],H1) :- trans(E,H,E1,H1).
trans(while(P,E),H,[E1,while(P,E)],H1) :- holds(P,H), trans(E,H,E1,H1).
trans(pi(V,E),H,E1,H1) :- subv(V,_,E,E2), trans(E2,H,E1,H1).
trans(E,H,E1,H1) :- proc(E,E2), trans(E2,H,E1,H1).
trans(E,H,[],[E|H]) :- prim_action(E), poss(E,P), holds(P,H).

       /* (c) -  SEARCH (ignoring exogenous or other concurrent actions) */
/* If (E,H) is a final state then finish. Otherwise, look for a straight
   path (E1,L) without looking at exogenous actions */
final(search(E),H) :- final(E,H).
trans(search(E),H,followpath(E1,L),H1) :- trans(E,H,E1,H1), findpath(E1,H1,L).

/* Look for a good path without looking at exogenous actions */
findpath(E,H,[E,H]) :- final(E,H).
findpath(E,H,[E,H|L]) :- trans(E,H,E1,H1), findpath(E1,H1,L).


/* When we have a followpath(E,L), try to advance using the list L
   in an offline manner.
   If it is not possible to advance any more redo the search to continue */
final(followpath(E,[E,H]),H) :- !.
final(followpath(E,_),H) :- final(E,H).  /* off path; check again */
trans(followpath(E,[E,H,E1,H1|L]),H,followpath(E1,[E1,H1|L]),H1) :- !.
trans(followpath(E,_),H,E1,H1) :- trans(search(E),H,E1,H1).  /* redo search */

       /* (d) -  INTERRUPTS */
prim_action(start_interrupts).
prim_action(stop_interrupts).
prim_fluent(interrupts).
causes_val(start_interrupts, interrupts, running, true).
causes_val(stop_interrupts, interrupts, stopped, true).
poss(start_interrupts, true).
poss(stop_interrupts,  true).

proc(interrupt(V,Trigger,Body),            /* version with variable */
    while(interrupts=running, pi(V,if(Trigger,Body,?(neg(true)))))).
proc(interrupt(Trigger,Body),              /* version without variable */
    while(interrupts=running, if(Trigger,Body,?(neg(true))))).
proc(prioritized_interrupts(L),[start_interrupts,E]) :- expand_interrupts(L,E).
expand_interrupts([],stop_interrupts).
expand_interrupts([X|L],pconc(X,E)) :- expand_interrupts(L,E).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% HOLDS: temporal projector %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% holds(P,H): formula P is true at history H

holds(and(P1,P2),H) :- !, holds(P1,H), holds(P2,H).
holds(or(P1,P2),H) :- !, (holds(P1,H) ; holds(P2,H)).
holds(neg(P),H) :- !, \+ holds(P,H).   /* Negation by failure */
holds(some(V,P),H) :- !, subv(V,_,P,P1), holds(P1,H).
holds(P,H) :- proc(P,P1), holds(P1,H).
holds(P,H) :- \+ proc(P,P1), subf(P,P1,H), call(P1).

       /*  T2 is T1 with X1 replaced by X2  */
subv(_,_,T1,T2) :- (var(T1);integer(T1)), !, T2 = T1.
subv(X1,X2,T1,T2) :- T1 = X1, !, T2 = X2.
subv(X1,X2,T1,T2) :- T1 =..[F|L1], subvl(X1,X2,L1,L2), T2 =..[F|L2].
subvl(_,_,[],[]).
subvl(X1,X2,[T1|L1],[T2|L2]) :- subv(X1,X2,T1,T2), subvl(X1,X2,L1,L2).

       /*  P2 is P1 with all fluents replaced by their values  */
subf(P1,P2,_) :- (var(P1);integer(P1)), !, P2 = P1.
subf(P1,P2,H) :- prim_fluent(P1), has_val(P1,P2,H).
subf(P1,P2,H) :- \+ prim_fluent(P1), P1=..[F|L1], subfl(L1,L2,H), P2=..[F|L2].
subfl([],[],_).
subfl([T1|L1],[T2|L2],H) :- subf(T1,T2,H), subfl(L1,L2,H).


% has_val(F,V,H):  Fluent F has value V in history H.  
has_val(F,V,[]) :- initially(F,V).
has_val(F,V,[Act|H]) :- sets_val(Act,F,V1,H) -> V = V1 ; has_val(F,V,H).


% sets_val(Act,F,V,H): Action Act causes fluent F to be set to V in history H.
%		Act can be either an exogenous action e(F,V) or a standard
%		action with a successor state axiom causes_val(Act,F,V,P).
sets_val(Act,F,V,H) :- Act = e(F,V) ; (causes_val(Act,F,V,P), holds(P,H)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: indigolog-vanilla.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

