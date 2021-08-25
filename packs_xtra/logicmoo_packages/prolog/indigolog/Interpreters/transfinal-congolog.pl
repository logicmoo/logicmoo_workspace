%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : Interpreters/transfinal-congolog.pl
%
%       IndiGolog TRANS & FINAL Implementation for the ConGolog language.
%
%  AUTHOR : Sebastian Sardina 
%           based on the definitions for ConGolog by 
%			Giuseppe De Giaccomo, Yves Lesperance, and Hector Levesque
%  EMAIL  : ssardina@cs.toronto.edu
%  WWW    : www.cs.toronto.edu/~ssardina www.cs.toronto.edu/cogrobo
%  TYPE   : system independent code
%  TESTED : SWI Prolog 5.0.10 http://www.swi-prolog.org
%
%           For more information on Golog and some of its variants, see:
%               http://www.cs.toronto.edu/~cogrobo/
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file provides:
%
% -- trans(P,H,P2,H2)    : configuration (P,H) can perform a single step
%                          to configuration (P2,H2)
% -- final(P,H)          : configuration (P,H) is terminating
%
%  The following special features are also provided:
% 
% -- A special action `wait' that is always possible. It can be used to state 
%         that the program will be waiting for an exogenous action.
% -- A special action `stop_interrupts' that is used to set the fluent
%         interrupts_running to false
% -- A special fluent `interrupts_running' that is always true unless stopped
%
%
%  The following is required for this file:
%
% FROM SYSTEM CODE DEPENDING ON WHERE IT IS USED
% -- report_message(T, M) : report message M of type T
%
% FROM TEMPORAL PROJECTOR:
% -- isTrue(+C, +H) 
%           Conditio C is true at history H
% -- calc_arg(+A, -A2, +H) 
%           calculate the arguments of action A at history H
% -- domain(-V, +D)       
% -- rdomain(-V, +D)       
%           object V is an element of domain D (random)
% -- getdomain(+D, -L) 
%           L is the list of elements in domain D
% -- sensed(+A, ?V, ?H) 
%           action A got sensing result V w.r.t. history H
%
% FROM DOMAIN SPECIFIC CODE:
% -- prim_action(action) : for each primitive action 
% -- exog_action(action) : for each exogenous action 
% -- poss(action,cond)   : precondition axioms
%
%  Code for describing the high-level program:
% -- proc(name,P)           : for each procedure P 
% -- simulator(Id,C,A)      : Under simulator Id, exog action A must happens if C holds
%
% OTHERS (PROLOG SPECIFIC):
% -- false
%            equivalent to fail
% -- random(+L,+U,-R) 
%            randomly returns a number R between L and U
% -- subv(+X1,+X2,+T1,-T2) 
%            T2 is T1 with X1 replaced by X2
% -- catch/3 and throw/1 for handling exceptions 
%            (provide empty implementations of both if there is no support 
%            for exceptions available)
% -- shuffle/2 : shuffle a list into another list in a random way
%
% -- call_with_time_limit(+Sec, +Goal): True if Goal completes within Time. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            TRANS and FINAL                           
% Trans(E,H,E1,H1) ->  One execution step of program E from history H  
%			 leads to program E1 with history H1.           
% Final(E,H)       ->  Program E at history H is final.                
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


    /* CONGOLOG CONSTRUCTS                                           */
    /*    iconc(E)    : iterative concurrent execution of E              */
    /*    conc(E1,E2) : concurrent (interleaved) execution of E1 and E2  */
    /*    pconc(E1,E2): prioritized conc. execution of E1 and E2 (E1>E2) */
    /*    bpconc(E1,E2,H): used to improve the performance of pconc(_,_) */
    /*                                                                   */
final(iconc(_),_).
final(conc(E1,E2),H)  :- final(E1,H), final(E2,H).
final(pconc(E1,E2),H) :- final(E1,H), final(E2,H).
trans(iconc(E),H,conc(E1,iconc(E)),H1) :- trans(E,H,E1,H1).

trans(conc(E1,E2),H,conc(E,E2),H1) :- trans(E1,H,E,H1).
trans(conc(E1,E2),H,conc(E1,E),H1) :- trans(E2,H,E,H1).
trans(pconc(E1,E2),H,E,H1) :-    % bpconc(E1,E2,H) is for when E1 blocked at H
    trans(E1,H,E3,H1) -> E=pconc(E3,E2) ; trans(bpconc(E1,E2,H),H,E,H1).

% bpconc(E1,E2,H) does not reconsider process E1 as long as the history
% remains being H (at H, E1 is already known to be blocked)
trans(bpconc(E1,E2,H),H,E,H1) :- !,
    trans(E2,H,E3,H1),  % blocked history H
    (H1=H -> E=bpconc(E1,E3,H) ; E=pconc(E1,E3)).
trans(bpconc(E1,E2,_),H,E,H1) :- trans(pconc(E1,E2),H,E,H1).


       /* INTERRUPTS */
trans(interrupt(Trigger,Body),H,E1,H1) :-
    trans(while(interrupts_running,if(Trigger,Body,?(neg(true)))),H,E1,H1).

trans(interrupt(V,Trigger,Body),H,E1,H1) :- 
    trans(while(interrupts_running, 
    		pi(V,if(Trigger,Body,?(neg(true))))),H,E1,H1).  

final(interrupt(Trigger,Body),H) :-
    final(while(interrupts_running,if(Trigger,Body,?(neg(true)))), H).

final(interrupt(V,Trigger,Body),H) :- 
    final(while(interrupts_running, pi(V,if(Trigger,Body,?(neg(true))))),H).

% Note these fluents (e.g., halt) and special actions (e.g., halt_exec) need to be defined already.
trans(prioritized_interrupts(L),H,E1,H1) :- 
    expand_interrupts([interrupt(haveExecuted(halt),  halt_exec),
    		       interrupt(haveExecuted(abort), abort_exec),
    		       interrupt(haveExecuted(pause), break_exec),
    		       interrupt(haveExecuted(reset), reset_exec),
		       interrupt(haveExecuted(debug), debug_exec)|L],E), !,
    trans(E,H,E1,H1).

trans(prioritized_interrupts_simple(L),H,E1,H1) :- 
%    expand_interrupts([interrupt(haveExecuted(halt),halt_exec)|L],E), !,
    expand_interrupts(L,E), !,
    trans(E,H,E1,H1).

expand_interrupts([],stop_interrupts).

expand_interrupts([X|L],pconc(X,E)) :-
    expand_interrupts(L,E).

% trans and final for system actions (e.g., show_debug, halt_exec, etc.)    
trans(stop_interrupts,H,[],[stop_interrupts|H]).
final(stop_interrupts,_) :- fail, !.



    /* GOLOG CONSTRUCTS                                           */
    /*                                                                */
    /*  These include primitive action, test, while, pick, if         */
    /*  nondeterministic choice and nondeterministic iteration.	      */
    /*								      */
final([],_).
final(star(_),_).
final(star(_,_),_).
final([E|L],H)       :- final(E,H), final(L,H).
final(ndet(E1,E2),H) :- final(E1,H) ; final(E2,H).
final(if(P,E1,E2),H) :- ground(P), !, (isTrue(P,H) -> final(E1,H) ; final(E2,H)).
final(if(P,E1,_),H)  :- isTrue(P,H), final(E1,H).
final(if(P,_,E2),H)  :- isTrue(neg(P),H), final(E2,H).
final(while(P,E),H)  :- isTrue(neg(P),H) ; final(E,H).

final(pi([],E),H)    :- !, final(E,H).
final(pi([V|L],E),H) :- !, final(pi(L,pi(V,E)),H).
final(pi(V,E),H)     :- !, subv(V,_,E,E2), !, final(E2,H).
final(pi((V,D),E),H) :- !, final(pi(V,D,E),H).
final(pi(V,D,E),H)   :- domain(W,D), subv(V,W,E,E2), !, final(E2,H).

final(E,H)           :- proc(E,E2), !, final(E2,H).



trans([E|L],H,E1,H2)      :- \+ L=[], final(E,H), trans(L,H,E1,H2).
trans([E|L],H,[E1|L],H2)  :- trans(E,H,E1,H2).
trans(?(P),H,[],H)        :- isTrue(P,H).
trans(ndet(E1,E2),H,E,H1) :- trans(E1,H,E,H1) ; trans(E2,H,E,H1).
trans(if(P,E1,E2),H,E,H1) :- ground(P), !,
	(isTrue(P,H) -> trans(E1,H,E,H1) ;  trans(E2,H,E,H1)).
trans(if(P,E1,E2),H,E,H1)  :- !,
	((isTrue(P,H), trans(E1,H,E,H1)) ; (isTrue(neg(P),H), trans(E2,H,E,H1))).
trans(star(E,1),H,E1,H1)  :- !, trans(E,H,E1,H1).
trans(star(E,N),H,[E1,star(E,M)],H1)   :- N>1, trans(E,H,E1,H1), M is N-1.
trans(star(E),H,E1,H1)       :- trans(E,H,E1,H1).
trans(star(E),H,[E1,star(E)],H1)       :- trans(E,H,E1,H1).
trans(while(P,E),H,[E1,while(P,E)],H1) :- isTrue(P,H), trans(E,H,E1,H1).

trans(pi([],E),H,E1,H1)    :- !, trans(E,H,E1,H1).
trans(pi([V|L],E),H,E1,H1) :- !, trans(pi(L,pi(V,E)),H,E1,H1).
trans(pi((V,D),E),H,E1,H1) :- !, trans(pi(V,D,E),H,E1,H1).
trans(pi(r(V),D,E),H,E1,H1):- !, rdomain(W,D), subv(V,W,E,E2), trans(E2,H,E1,H1).
trans(pi(V,D,E),H,E1,H1)   :- !, domain(W,D), subv(V,W,E,E2), trans(E2,H,E1,H1).
trans(pi(V,E),H,E1,H1)     :- subv(V,_,E,E2), !, trans(E2,H,E1,H1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LAST TRANS FOR PROCEDURES AND PRIMITIVE ACTIONS (everything else failed)
% Replace the arguments by their value, check that it is a primitive action
% and finally check for preconditions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trans(E,H,E1,H1)    :- proc(E,E2), !, trans(E2,H,E1,H1).
trans(A,H,[],[A|H]) :- system_action(A), !.
final(A,_) 	    :- system_action(A), !, fail.

trans(E,H,[],[E1|H])    :- 
	calc_arg(E,E1,H),
	prim_action(E1), 
	poss(E1,P), 
	isTrue(P,H).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Interpreters/transfinal-congolog.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%