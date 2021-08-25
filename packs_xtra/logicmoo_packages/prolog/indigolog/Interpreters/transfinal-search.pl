%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% FILE    : Interpreters/transfinal-search.pl
%
%       IndiGolog TRANS & FINAL Implementation for 
%			various search operators (IndiGolog)
%
%  AUTHOR : Sebastian Sardina 
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
% -- wscp(Name,G,Max,IA,SimNo,H,E): WSCP implementation
%        Name : name of the planning problem
%        G    : goal to be achieved
%        IA   : initial list of possible actions
%        SimNo: identification of the exogenous action simulator to be used
%        H    : Initial situation-history
%        E    : SOLUTION: CONDITIONAL PLAN
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




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D) INDIGOLOG SEARCH CONSTRUCTS                               
%%
%% (D.1) search(E,M)  : linear search on E, with message M
%% (D.1) search(E)    : linear search on E	
%% (D.1) searchg(P,E,M) : linear search on E, with message M, replanning condition P
%% (D.1) search(P,E)    : linear search on E, replanning condition P	
%% (D.1) searcho(P,Options)  : linear search on E, with list of Options	
%%
%% (D.2) searchc(E,M) : conditional  search on E, with message M   
%% (D.2) searchc(E)   : conditional  search on E
%%
%% (D.3) achieve(G,Max,IA) : CONDITIONAL PLANNER WSCP (Hector Levesque)
%%
%% (D.4) fullSearch   : INTERRUPTABLE SEARCH (BETA VERSION)
%%			still not attached to any Golog construct
%% (D.5) searchr(E,LGoals,FluentAssum,M): RATIONAL SEARCH (BETA VERSION)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D.1) TRADITIONAL SEARCH (From [De Giacomo & Levesque 99])
%%
%% Linear plans, ignore sensing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% search(E): search on E, using caching and replanning only when
%		situation is not the expected one
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
final(search(E,_),H) :- final(search(E),H).
final(search(E),H) :- final(E,H).

trans(search(E,M),H,E1,H1):- 
        report_message(program, ['Thinking linear plan on:      ', M]),
        (trans(search(E),H,E1,H1) ->
             report_message(program, 'Finished thinking: Plan found!') 
        ;
             report_message(program, 'Finished thinking: No plan found!'), 
              fail
        ).

% if findpath/3 wants to abort everhting it has to throw exception search
% you can obtain the vanilla search version by having Prolog code to
% ignore both catch/3 and throw/1.
trans(search(E),H,followpath(E1,L),H1) :- 
%	store_node(search, E, H),  % For debugging
        catch( (trans(E,H,E1,H1), findpath(E1, H1, L)) , search, fail).
trans(search(E,M),H,E1,H1):- 
        report_message(program, ['Thinking linear plan on:      ', M]),
        (trans(search(E),H,E1,H1) ->
             report_message(program, 'Finished thinking: Plan found!') 
        ;
             report_message(program, 'Finished thinking: No plan found!'), 
             fail
        ).

% findpath(E,H,L): find a solution L for E at H; 
%		   L is the list [E1,H1,E2,H2,...,EN,HN] encoding
%		   each step evolution (Ei,Hi) where final(EN,HN)
%
% If last action was commit, then try to find a plan with what remains
% if no plan is possible then throw exception "search" to abort the
% whole search
findpath(E,[commit|H],L) :- !, (findpath(E,H,L) -> true ; throw(search)).
findpath(E,H,[E,H]) :- final(E,H).
findpath(E,H,[E,H|L]) :- 
%	store_node(search, E, H),  % For debugging only
        trans(E,H,E1,H1), 
        findpath(E1,H1,L).


% followpath(E,L): execute program E wrt expected sequence of
%		   configurations L=[E,HEx,E1,H1,...]
%	if the current history does not match the next expected one
% 	in L (i.e., H\=HEx), then redo the search for E from H
final(followpath(E,[E,H]),H) :- !.
final(followpath(E,_),H) :- final(E,H).  /* off path; check again */
trans(followpath(E,[E,H,E1,H1|L]),H,followpath(E1,[E1,H1|L]),H1) :- !.
trans(followpath(E,_),H,E1,H1) :- trans(search(E),H,E1,H1). /* redo search */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% searchext(P,Opt) : search for E with options Opt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trans(searchn(E,LOptions),H,mnt(E,H,followpath(E1,L),LOptions),H1) :- 
        trans(E,H,E1,H1), 
        findpathn(E1, H1, L, LOptions).
final(search(E,opt(_LOptions)),H) :- final(E,H).

findpathn(E,H,[E,H],_LOpt) :- final(E,H).
findpathn(E,H,[E,H|L],LOpt) :- 
        trans(E,H,E1,H1), 
		(H1=[A|H], member(assumptions(LAssumptions), LOpt), add_assumptions(A,LAssumptions,ExogAss,_TestAss) -> 
			E2 = [ExogAss|E1]
		;
			E2 = E1
		),
        findpathn(E2,H1,L,LOpt).
  
% LAssumption is a list of assumptions of the form [A, Exog]: If A just happend
% then assume Exog will ocurr right away.
%      
% If action A has just been executed in H, then assume exog action Exog
% occurrs. _Test is supposed to encode a test to wait for the exog. action (not used at this point)
add_assumptions(A, LAssumptions, Exog, _Test) :-
	copy_term(LAssumptions,LAssumptionsCopy), % get a copy of the assumptions to avoid binding their orig vars
	member([A, Exog], LAssumptionsCopy).
	

%% Semantics of mnt(EOriginal,HOriginal,EFollow,LPlanningOptions)
%%
final(mnt(_,_,followpath(E,[E,H]),_),H) :- !.
final(mnt(_,_,followpath(E,_),_),H) :- final(E,H).  /* off path; check again */


trans(mnt(EO,HO,followpath(E,[E,_,E1,H1|L]),LOpt),H1,mnt(EO,HO,followpath(E1,[E1,H1|L]),LOpt),H1) :- 
	E = [ExogAction|_],			% An exogenous action was assumed, wait until added to current history H1
	exog_action(ExogAction), !.	
trans(mnt(EO,HO,followpath(E,[E,H,E1,H1|L]),LOpt),H,mnt(EO,HO,followpath(E1,[E1,H1|L]),LOpt),H1) :- 
	\+ (E = [ExogAction|_], exog_action(ExogAction)), !.	% Progress blindly if not an assumed exog action
trans(mnt(EO,HO,EFollow,LOpt),H,ERecovered,HRecovered) :- 
	EFollow=followpath(Ex,[Ex,Hx|_]),						% History is not what expected, recover
	H\=Hx,		% Replan only if the current situation is not the one expected
	recover(EO,HO,LOpt,EFollow,H,ERecovered,HRecovered).	


%% recover(EOriginal,HOriginal,LPlanningOptions,EFollow,HCurrent,ERecoveredPlan)
%%
recover(_EO,_HO,_LOpt,EFollow,H,followpath(Ex,ListRecovered),H) :-
	EFollow=followpath(Ex,[Ex,Hx|L]),
	append(H,HDropped,Hx), !,					% The expected history has been chopped (progressed)
	writeln('======> Surgery recovering plan.........'),
	dropPrefixHistory([Ex,Hx|L],HDropped,ListRecovered).

dropPrefixHistory([],_,[]).
dropPrefixHistory([E,H|L],HDropped,followpath(E,[E,HNew|L2])) :-
	append(HNew,HDropped,H),
	dropPrefixHistory(L,HDropped,L2).

	
recover(EO,HO,LOpt,_,H,EN,HN) :-
	writeln('======> Full recovering......'),		% Full re-planning is required
	transstar(conc(EO,star(pi(a,[?(exog_action(a)),a]))),HO,conc(E1,_),H),	% respect actions already done
	trans(searchn(E1,LOpt),H,EN,HN).	






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% searchg(P,E) : search for E with replanning when P holds only
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
final(searchg(_,E,_),H):- final(search(E),H).
final(searchg(_,E),H)  :- final(E,H).

trans(searchg(P,E),H,followpathg(P,E1,L),H1) :- 
        catch( (trans(E,H,E1,H1), findpath(E1,H1,L)), search,fail).
trans(searchg(P,E,M),H,E1,H1):- 
        report_message(program, ['Thinking linear plan on:      ', M]),
        (trans(searchg(P,E),H,E1,H1) ->
             report_message(program, 'Finished thinking: Plan found!') 
        ;
             report_message(program, 'Finished thinking: No plan found!'), 
             fail
        ).

% followpath(P,E,L): execute program E wrt expected sequence of
%		     configurations L=[E,H,E1,H1,...]
%	if the current history does not match the next expected one
%	in L and re-planning condition P holds, then redo the search for E at H
final(followpathg(_,E,[E,H]),H) :- !.
final(followpathg(P,E,[E,_]),H) :- \+ isTrue(P,H), !. /* _\=H */
final(followpathg(_,E,_),H) :- final(E,H).  /* off path; check again */

trans(followpathg(P,E,[E,H,E1,H1|L]),H,followpathg(P,E1,[E1,H1|L]),H1) :- !.
trans(followpathg(P,E,_),H,EN,HN) :- 
	isTrue(P,H), !,		/* HExp\= H and replanning cond P holds */
	writeln('we need to replan!'),
	trans(searchg(P,E),H,EN,HN).
trans(followpathg(P,E,[E,HExp,E1,H1|L]),H,followpathg(P,E1,[E1,HN|LN]),HN) :- 
	writeln('NO need to replan!'),
	append(HNExp,HExp,H),	/* HExp\= H and replanning cond P does not holds */
	repair_expected([E1,H1|L],HNExp,[E1,HN|LN]).	/* H=HNExp+HExp */

% repair_expected(L,H,LN): L is a list of expected configurations
%			   LN is the new list of expected configurations
%			   where H is added at the front of each history in L
repair_expected([],_,[]).
repair_expected([E1,H1|L],HNExp,[E1,H11|LN]) :-
	append(HNExp,H1,H11),
	repair_expected(L,HNExp,LN).
	





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D.2) CONDITIONAL SEARCH: Conditional plans with sensing  
%%
%% From [Sardina LPAR-01] and based on sGolog [Lakemeyer 99]
%%
%% After a step, the program looks for special "marks" generated by the
%% program when searched:
%%
%% branch(P): CPP should branch w.r.t. rel fluent P 
%% commit   : no backtracking on the already found partial CPP 
%% sim(A)   : A is a simulated exogenous action, don't add it to the CPP
%% test(P)  : a test ?(P) should be left in the CPP (from ??(P))
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
final(searchc(E,_),H):- final(searchc(E),H).
final(searchc(E),H)  :- final(E,H).

trans(searchc(E,M),H,E1,H1):- 
        report_message(program, ['Thinking Conditional plan on:      ', M]),
        (trans(searchc(E),H,E1,H1) ->
             report_message(program, 'Finished thinking: CPP found!') 
         ;
             report_message(program, 'Finished thinking: No CPP found!'), 
              fail
        ).

% if calcCPP/3 wants to abort everything it has to throw exception searchc
trans(searchc(E),S,CPP,S):- 
        catch(calcCPP(E,S,CPP), searchc, fail).

trans(branch(P),S,[],[branch(P)|S]).	/* branching step always succeeds */

calcCPP(E,S,[])      :- final(E,S).
calcCPP([E1|E2],S,C) :- E2\=[], !, calcCPP(E1,S,C1), /* program is a sequence */
                        extendCPP(E2,S,C1,C). 

%calcCPP(branch(P),S,[])            :- isTrue(know(P),S), !. /* no branching */
%calcCPP(branch(P),S,[if(P,[],[])]) :- isTrue(kwhether(P),S), !. /* branching */
calcCPP(branch(P),_,[if(P,[],[])]) :- !. /* branching, do not check */

calcCPP(E,S,C) :- trans(E,S,E1,S1),    /* program is not a sequence */
   (S1=[branch(P)|S] -> calcCPP([branch(P)|E1],S,C) ;     /* branch now wrt P*/
%    S1=[commit|S]    -> (calcCPP(E1,S,C) -> /* commit here */
%	                       true 
%		       ;      
%	                 throw(searchc))  ; /* abort if no plan found for E1 */
    S1=S             -> calcCPP(E1,S1,C) ;                /* normal test     */
    S1=[test(P)|S]   -> (calcCPP(E1,S,C1), C=[?(P)|C1]) ; /* perdurable test */
    S1=[A|S]         -> (calcCPP(E1,S1,C1), C=[A|C1]) ).  /* normal action   */

/* extendCPP(E,S,C,C1) recursively descends the CAT C (first two clauses). */
/* Once a leaf of the CAT is reached (third clauses), "calcCPP" is called  */
/* which then extends this branch accordingly to E 		           */
extendCPP(E,S,[sim(A)|C],[sim(A)|C2]) :- exog_action(A), !,
                                         extendCPP(E,[sim(A)|S],C,C2).
extendCPP(E,S,[if(P,C1,C2)],[if(P,C3,C4)]) :- !,  
                assume(P,true,S,S1),  extendCPP(E,S1,C1,C3), 
                assume(P,false,S,S2), extendCPP(E,S2,C2,C4).
extendCPP(E,S,[commit|C],C2) :- !, (extendCPP(E,S,C,C2) ; throw(searchc)).
extendCPP(E,S,[A|C],[A|C2]) :- prim_action(A), !, extendCPP(E,[A|S],C,C2).
extendCPP(E,S,[],C) :- calcCPP(E,S,C).	/* We are on a leaf of the CPP */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D.3) CONDITIONAL PLANNER WSCP: conditional planner (Hector Levesque)
%%
%% Requires loading the library for WSCP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- ensure_loaded(wscplan).  % Load the WSCP Planner

% Transitions for the case(A,CondPlan) construct
trans(case(A,[if(V,Plan)|_]),H,Plan,H):- sensed(A,V,H), !.
trans(case(A,[if(_,_)|BL]),H,case(A,BL),H).

% Transition for the planning construct achieve
% G     	: the goal to achieve
% Max   	: the maximum depth for the search
% 	---	LOptions  is the list of options:
%			- id(NameId) : planning name id  
% 			- simid(Id) : Id of the exog. simulator to be used (none = no simulator)
% 			- mess(Mess) : description of the planning work to be done (to be printed)
%			- actions(LAct) : legal actions that may be used  

trans(achieve(G,Max,LOptions),H,E,H) :- 
	extract_option(LOptions,id,NameId,G),
	extract_option(LOptions,mess,Mess,none),
	(Mess\=none ->
	    report_message(program,  ['Planning for: ', Mess])
	;
		true
	),      
	wscp(NameId,G,Max,H,E) ->		% Do the actual planning!
		Mess\=none,
        report_message(program, ['Finished planning for ',Mess,' : Plan found!'])
	;    
		Mess\=none,
        report_message(program, ['Finished planning for ',Mess,' : No plan found!']),
		fail. 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D.4) INTERRUPTABLE SEARCH (BETA VERSION)
%%
%% Developed first by Hector Levesque (2003)
%% Fixed and improved by Sebastian Sardina (2003-2004)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  This is code to incrementally go through a search tree 
%%
%% Tree is a list [node_k children_k-1 node_k-1 ... children_1 node_1] 
%% where a node is a pair (E,H), E is a program and H is a history     
%% and where children_k is a list of remaining children of node_k      
%% and where node_1 is the root of the tree */
%%
%% Two predicates defined:                                             
%%   - doneSearch(Tree) succeeds iff the search has found a full path  
%%   - xtndSearch(Tree,Tree1) succeeds iff the search from Tree can    
%%     progress one step to Tree1: either extend a leaf of Tree using  
%%     Trans or pop the tree (as necessary) if the leaf goes nowhere   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fullSearch(T, T)  :- doneSearch(T).
fullSearch(T, T1) :- xtndSearch(T, T2), !, fullSearch(T2, T1).

doneSearch([(E,H)|_]) :- final(E,H).

% Progress the tree T one step (poping is not considered a step)
xtndSearch(T,[First,Rest|T]) :-
        T=[(E,H)|_],
%	store_node(xtnd, E, H),  % For debugging
        findall((E1,H1),trans(E,H,E1,H1),[First|Rest]).
%       setof((E1,H1),trans(E,H,E1,H1),[First|Rest]).
xtndSearch([_,[],_,[]|T], T1)          :- !, xtndSearch([backup,[]|T], T1).
xtndSearch([_,[],_,[First|Rest]|T],T1) :- !, xtndSearch([First, Rest|T], T1).
xtndSearch([_,[First|Rest]|T],T1)      :- !, xtndSearch([First, Rest|T], T1).

% Progress the tree T counting each pop as a step
xtndSearch2(T,[First,Rest|T]) :-
        T=[(E,H)|_], setof((E1,H1),trans(E,H,E1,H1),[First|Rest]).
xtndSearch2([_,[],_,[]|T], [_,[]|T]):- !.
xtndSearch2([_,[],_,[First|Rest]|T],[First, Rest|T]) :- !.
xtndSearch2([_,[First|Rest]|T],[First, Rest|T]).

xtnd(T, T, 0) :- !.
xtnd(T, T1, N):- xtndSearch2(T, TT),!, N2 is N-1, xtnd(TT, T1, N2).

/* With these two predicates, we can get a full search without     */
/* any interruptions if desired by calling xtndSearch repeatedly   */
findpath(E,H,Path,H2) :- 
%	store_node(xtnd, E, H),  % For debugging
        fullSearch([(E,H)], T), !,
        buildPath(T, PathR), 
        PathR=[(_,H2)|_],
        reverse(PathR, Path).

buildPath([C],[C]).
buildPath([C,_|R], [C|RP]) :- buildPath(R, RP).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (D.5) RATIONAL SEARCH (BETA VERSION)
%%
%% From [Sardina & Shapiro AAMAS-2003]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Computes the "best" CPP for a program wrt a set of goals
%%
%% The rational search construct needs
%%
%% - E: a nondeterministic ConGolog program
%% - SetGoals: set of pairs goals-rewards: [[G1,R1],...,[Gn,Rn]]
%% - FluentAssum: set of possible assumptions [...,[fi,[v1,..,v2],...]
%%                this set identifies the set of possible worlds for
%%                which all solutions will be tested/evaluated
%% - M: a message for the user when the search starts (optional)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Version with a message
trans(searchr(E,SetGoals,FluentAssum,M),S,CPP,S):- 
        report_message(program, ['Thinking a rational plan on:      ', M]),
        (trans(searchr(E,SetGoals,FluentAssum),S,CPP,S) ->
             report_message(program, 'Finished thinking: rational CPP found!') 
        ;
	     report_message(program, 'Finished thinking: No rat. CPP found!'), 
             fail
        ).

:- dynamic bestPlan/2.

% trans/5 for rational search
trans(searchr(E,SetGoals,FluentAssum),S,CPP,S):- 
	compute_possible_assumptions(FluentAssum, S, SetAssum),
	retractall(bestPlan(_,_)),
        catch(calcRationalCPP(E,S,SetGoals,SetAssum), searchr, bestPlan(CPP)),
	(\+ var(CPP) ; bestPlan(CPP)).

% Compute all CPPs, evaluate them, and store them in the database
% CPP is computed in situation S in which there may be unknowns. 
%     E itself has to be designed to deal with this unknowns (E JIT in S)
calcRationalCPP(E,S,SetGoals,SetAssum) :-
	calcCPP(E,S,CPP),
	calc_vector_utility(CPP, S, SetAssum, SetGoals, LVectorU),
	assert(bestPlan(CPP,LVectorU)),
	fail.
calcRationalCPP(_,_,_,_).

bestPlan(CPP) :-
	bestPlan(CPP,EvalCPP),
	\+ (bestPlan(CPP2, EvalCPP2), 
	    CPP2 \= CPP,
	    member([N,[Min1,_]], EvalCPP),
	    member([N,[_,Max2]], EvalCPP2),
	    Min1<Max2
           ).

%trans(searchr(mainControl(0), [[safeOpen=true,10],[neg(exploded),5]],[[combNum0,[true,false]]]),[],E,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CODE FOR EVALUATING PLANS WRT A SET OF GOALS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% calc_vector_utility(+CPP, +H, +SetAssumptions, +SetGoal, -LVectorU) 
%    Computes the vector of utilities of a CPP at H wrt the SetGoals
% LVectorU = [ [MI1,MA1],[MI2,MA2],....,[MIN,MAN] ]
%     where MIk = minimum utility of CPP with assumptions k
%     where MAk = maximum utility of CPP with assumptions k
% SetAssumptions is a list of pairs [Id, History]
calc_vector_utility(CPP, H, SetAssumptions, SetGoals, LVectorU) :-
	findall([Id,U],(member([Id,H2],SetAssumptions),
                        append(H2,H,H3),
			evaluate_CPP(CPP,H3,SetGoals,U)), LU),  
	maplist(get_min_max,LU,LVectorU).

% Obtains the pair minimum-maximum of a list L of numbers
get_min_max([Id,L],[Id,[Min,Max]]) :- min(L, Min), max(L, Max).
	
% compute_possible_assumptions(+FluentAssum, +H, -SetAssumptions)
%  FluentAssum is a list of pairs [Fluent, [v1,v2,...,vn]] where
%        v1,..,vn are the possible assumptions for Fluent
%  H is the current history
%  SetAssumptions are all the possible assumptions that are consistent at H
compute_possible_assumptions(FluentAssum, H, SetAssumptions) :-
	findall(HA, multiple_assumptions(FluentAssum, H, HA), LHA),
	identify_list(LHA, 1, SetAssumptions).

% identify_list(L,N,IL) 
%   If L = [e1,e2,e3,...,en], then IL=[ [N,e1], [N+1,e2], ..., [N+n,en] ]
identify_list([], _, []).
identify_list([A|RA], N, [[N,A]|IRA]) :-
	N2 is N+1,
	identify_list(RA, N2, IRA).

% multiple_assumptions(+LAssum, +H, H2) :
%         H2 is a consistent combination of assumptions at H from LAssum
multiple_assumptions([], _, []).
multiple_assumptions([[F,PV]|R], H, HA) :- % F has no value on H
	holds(neg(know(F)), H), !,
	multiple_assumptions(R, H, HR),
	member(X, PV),
	assume(F,X,HR,HA).
multiple_assumptions([[_,_]|R], H, HA) :- % F has a value in H, do not assume
	multiple_assumptions(R, H, HA).


% Evaluate a CPP in a particular history H wrt goals-value SG
% LUtilities is a list of utilities that the CPP may collect
evaluate_CPP(CPP, H, SG, LUtilities) :-
	findall(U,  (extract_trace(CPP, H, H2),
	             append(H2, H, H3), 
		     evaluate_trace(H3, SG, U)), LUtilities).


% Evaluate history H wrt the set of pairs Goal-Value
evaluate_trace(_, [], 0) :- !.
evaluate_trace(H, [[Goal,Value]|RG], Utility) :-
	evaluate_trace(H,RG,UtilityRG),
	(isTrue(Goal,H) -> 
		Utility is UtilityRG + Value 
	; 
	        Utility is UtilityRG
	).
	

% extract_trace(E,H,H2) : H2 is the execution trace of E starting in H
%                         (E is known to be executable at H)
extract_trace([],_,[]).
extract_trace([if(P,E1,E2)],H,H2) :-
	isTrue(P=Y,H), !,
        A = _SingletonInBranches,
	(Y=true -> extract_trace(E1,[A|H], H2) ; extract_trace(E2,[A|H], H2)).
extract_trace([if(P,E1,_)],H,HR) :-
	assume(P,true,H,H2),
	extract_trace(E1,H2, H3),
	assume(P,true,[],H4),
	append(H3,H4,HR).
extract_trace([if(P,_,E2)],H,HR) :- !,
	assume(P,false,H,H2),
	extract_trace(E2,H2, H3),
	assume(P,false,[],H4),
	append(H3,H4,HR).
extract_trace([A|E],H,H2) :-   % It is an action followed by a CPP
	A\=[],
	extract_trace(E,[A|H], HE),
	append(HE,[A],H2).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Interpreters/transfinal-search.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%