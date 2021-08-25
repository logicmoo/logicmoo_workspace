%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : Interpreters/transfinal-ext.pl
%
%       IndiGolog TRANS & FINAL Implementation for 
%			some extended constructs to the ConGolog language.
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                            TRANS and FINAL                           
% Trans(E,H,E1,H1) ->  One execution step of program E from history H  
%			 leads to program E1 with history H1.           
% Final(E,H)       ->  Program E at history H is final.                
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    /* (A) EXTENDED CONSTRUCTS                                           	*/
    /*    wndet(E1,E2) : Weak nondeterministic choice of program         	*/
    /*    rndet(E1,E2) : Real nondeterministic choice of program	 		*/
    /*    rconc(E1,E2) : Real concurrency on 2 programs   	    	 		*/
    /*    rconc(L) 	   : Real concurrency on a list of programs L 	 		*/
    /*    rrobin(L)	   : Round-robin concurrency		 					*/	
    /*    rpi(X,D)     : Real nondeterministic choice of argument from D 	*/
    /*    gexec(P,E)   : Guarded execution of program E wrt condition P  	*/
    /*    goal(PSucc,E,PFail,ERec): full guarded execution		 			*/
    /*    abort(P)     : Abort process identified with P                 	*/
    /*    ??(P)        : Like ?(P) but it leaves a test(P) mark in H     	*/
    /*    wait         : Meta action to wait until an exogenous event    	*/
    /*    commit       : Meta action to commit to the plan found so far  	*/
    /*    abort        : Meta action to, suddenly,  abort execution      	*/
    /*	  time(P,Sec)  : Make first step on P in less than Sec seconds	 	*/
    /*	  ttime(P,Sec) : Make every step on P in less than Sec seconds 	 	*/
% Try to execute program E1 first. If impossible, then try program E2 instead
trans(wndet(E1,E2),H,E,H1) :- trans(E1,H,E,H1) -> true ; trans(E2,H,E,H1).
final(wndet(E1,E2),H)      :- final(E1,H), final(E2,H).
%final(wndet(E1,E2),H)      :- final(E1,H) ; (\+ trans(E1,H,_,H), final(E2,H)).

% Simulate random choice in a nondeterministc choice of programs
trans(rndet(E1,E2),H,E,H1):- 
        random(1,10,R), % flip a coin!
	(R>5 -> (trans(E1,H,E,H1) ; trans(E2,H,E,H1)) ;
	        (trans(E2,H,E,H1) ; trans(E1,H,E,H1)) ).
final(rndet(E1,E2),H):- final(E1,H) ; final(E2,H).

% Simulate random choice in a concurrent execution of E1 and E2
trans(rconc(E1,E2),H,rconc(E11,E22),H1) :- 
    (random(1, 3, 1) -> 	% flip a coin!
    	( (trans(E1,H,E11,H1), E22=E2) ; (trans(E2,H,E22,H1), E11=E1) ) 
    	;
        ( (trans(E2,H,E22,H1), E11=E1) ; (trans(E1,H,E11,H1), E22=E2) ) 
    ).
trans(rconc(L),H,rconc([E1|LRest]),H1) :-
	length(L,LL),
	random(0,LL, R),
	nth0(R,L,E),
	trans(E,H,E1,H1),
	select(E,L,LRest).	 
final(rconc(E1,E2),H) :- final(conc(E1,E2),H).
final(rconc([]),_).
final(rconc([E|L]),H) :- final(E,H), final(rconc(L),H).

trans(rrobin(L),H,rrobin(L2),H1) :-
	select(E,L,LRest),
	trans(E,H,E1,H1),
	append(LRest,[E1],L2).
final(rrobin(L),H) :- final(rconc(L),H).


% Execute E atomically (i.e., as a transaction)
trans(atomic(E),H,[],[atomic(E2)|H2]) :-
	trans(E,H,E2,H2).
final(atomic(E),H) :- final(E,H).


% Execute program E as long as condition P holds; finish E if neg(P) holds
final(gexec(_,E), H) :- final(E,H).
trans(gexec(P,E), H, gexec2(P,E1), H1) :- 	% P needs to be a simple fluent
        assume(P, true, H, H2),    % Set P to be TRUE
        trans(E, H2, E1, H1).
final(gexec2(P,E), H) :- isTrue(neg(P),H) ; final(E,H).
trans(gexec2(P,E), H, gexec2(P,E1), H1) :- isTrue(P,H), trans(E,H,E1,H1).


% goal(PSucc,E,PFail,ERec): full guarded execution
%	PSucc 	: finalize successfully if PSucc holds
%	E	: the program to be executed
%	PFail	: Terminate the program E and execute recovery procedure ERec
final(goal(PSucc,E,_,_), H) :- isTrue(PSucc,H) ; final(E,H).
trans(goal(PSucc,_,PFail,ERec), H, E2, H2) :-
	isTrue(neg(PSucc),H),
	isTrue(PFail,H),
	trans(ERec,H, E2, H2).
trans(goal(PSucc,E,PFail,ERec), H, goal(PSucc,E2,PFail,ERec), H2) :-
	isTrue(neg(PSucc),H),
	isTrue(neg(PFail),H),
	trans(E,H,E2,H2).

% Abort process identified with P by setting P to false in H
trans(abort(P), H, [], H1) :- assume(P, false, H, H1).

% Perform program P(V) with all elements in domain D: P(e1);P(e2);...;P(en)
% Domain D can either be a list of elements or a domain identifier from 
% where we get its domain list with getdomain/2
trans(for(V,D,P),H,E1,H1) :- D\=[], atom(D), !, getdomain(D,L), 
                             trans(for(V,L,P),H,E1,H1).
trans(for(V,[F|L],P),H,[E1,for(V,L,P)],H1) :- 
	subv(V,F,P,P1), trans(P1,H,E1,H1).

final(for(V,D,P),H)    :- D\=[], atom(D), !, getdomain(D,L), 
                          final(for(V,L,P),H).
final(for(_,[],_),_).
final(for(V,[F|L],P),H):- subv(V,F,P,P1), final(P1,H), final(for(V,L,P),H).

% A test action that leaves a mark in the history
trans(??(P),H,[],[test(P)|H]):- isTrue(P,H). 

% Simulation of exogenous actions E
trans(sim(E),H,[],[sim(E)|H]):- !, calc_arg(E,E1,H), exog_action(E1).

% Wait and commit are two "meta" actions.
% wait action tells the interpreter to wait until an exogenous action arrives
% commit is used in search and searchc to commit to the plan computed so far
trans(wait,H,[],[wait|H])    :- !. % wait is a no-op but encourages rolling db
trans(commit,S,[],[commit|S]).	   % commit to the plan found so far! 
trans(abort,S,[],[abort|S]).	   % completely abort execution


% Time bounded steps
% time(E,Sec)  : make the *first* step in E before Sec seconds
% ttime(E,Sec) : make every step before in E before Sec seconds
trans(time(E,Sec),H,E2,H2) :- timeout(trans(E,H,E2,H2), Sec, fail).
final(time(E,Sec),H) :-	timeout(final(E,H), Sec, fail).
trans(ttime(E,Sec),H,time(E2,Sec),H2) :- trans(time(E,Sec),H,E2,H2).
final(ttime(E,Sec),H) :- final(time(E,Sec),H).


% Perform a transition on E, aborting though if an exogenous action happens
% meanwhile and Cond holds in H
% requires exog_interruptable/3 from main cycle
final(exogint(E,_Cond),H) :- final(E,H).
trans(exogint(E,Cond),H,exogint(E2,Cond),H2) :- 
	exog_interruptable(trans(E,H,E2,H2), isTrue(Cond,H), Status),
	(Status=ok -> 
		true 
	; 
		report_message('TF', system(3),'Computation of trans/4 aborted due to exog events'),
		E2=E, H2=H
	).


%%% Random pick construct
trans(rpi([],E),H,E1,H1)   :- !, trans(E,H,E1,H1).
trans(rpi([V|L],E),H,E1,H1):- !, trans(rpi(L,rpi(V,E)),H,E1,H1).
trans(rpi((V,D),E),H,E1,H1):- !, trans(rpi(V,D,E),H,E1,H1).
trans(rpi(V,D,E),H,E1,H1)  :- rdomain(W,D), subv(V,W,E,E2), trans(E2,H,E1,H1).

final(rpi([],_,E),H)  :- !, final(E,H).
final(rpi([V|L],E),H) :- !, final(rpi(L,rpi(V,E)),H).
final(rpi((V,D),E),H) :- !, final(rpi(V,D,E),H).
final(rpi(V,D,E),H)   :- rdomain(W,D), subv(V,W,E,E2), !, final(E2,H).


trans(?(printHistory),H,[],H) :- !, 
	write('CURRENT HISTORY: '), 
	writeln(H).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (E) SYNCHRONIZATION CONSTRUCT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Syncronize a set of configuration pairs
%%
%% trans(sync(EL),H,sync(EL2),H2):
%%      programs EL can all perform a syncronized step to EL2 and H2
%% final(sync(EL),H):
%%      programs EL can all terminate at H
%%
%% synctrans/4 and syncfinal/2 are a bit more powerful as they may
%%  use different situations for the different programs:
%%
%%    synctrans([E1,E2,...],[H1,H2,...],[E11,E22,...],[H11,H22,...],A)
%%  configurations (Ei,Hi) can advance to (Eii,Hii) by doing action A
%%    syncfinal([E1,E2,...],[H1,H2,...])
%%  configurations (Ei,Hi) can all terminate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trans(sync(LE),H,sync(EL2),[A|H]) :-
	length(LE,LL),
	buildListRepeat(LL,H,LH),
	synctrans(LE,LH,EL2,_,A).

final(sync(LE),H) :- 
	length(LE,LL),
	buildListRepeat(LL,H,LH),
	syncfinal(LE,LH).

% buildListRepeat(N,E,L) :- L is a list of N repetitions of element E
buildListRepeat(0,_,[])    :- !.
buildListRepeat(N,H,[H|L]) :- N2 is N-1, buildListRepeat(N2,H,L).

synctrans([E],[H],[E2],[A|H],A)   :- !, ttrans(E,H,E2,[A|H]).
synctrans([E|LP],[H|LH],[E2|LP2],[[A|H]|LH2],A) :- 
	ttrans(E,H,E2,[A|H]), 
	synctrans(LP,LH,LP2,LH2,A).

syncfinal([E],[H])       :- !, tfinal(E,H).
syncfinal([E|LP],[H|LH]) :- tfinal(E,H), syncfinal(LP,LH).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Interpreters/transfinal-ext.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%