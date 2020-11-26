% MODULE interpreter EXPORTS
:- module( interpreter, 
	 [ solve/3, 
           solve_once/3,
           failed_proof/1,
           prooftrees/3,
           proof_close/2,
	   prove1/2,
	   prove3/2,
	   prove4/3,	
   
	   prove5/2,

           proof_path/4,
           set_proof_depth/0,

	   t_interpreter/2,
           ip_part1/2,
	   ip_part2/3 ]).

% IMPORTS
:- use_module(home(kb),
              [get_clause/5,interpretable_predicate/1]).
:- use_module(home(div_utils),
              [insert_unique/3,identical_member/2,append_all/2,mysetof/3]).
:- use_module(home(bu_basics),
              [head/3,body/3,assumption/3]).
:- use_module(home(environment),
              [satisfiable/1]).
:- use_module_if_exists(library(basics),
              [member/2]).
:- use_module_if_exists(library(unify),
              [unify/2]).


% METAPREDICATES
% none


:- dynamic failed_proof/1, tag/1, prooftrees/3, depth_bound/1,depth_exceeded/0,
   depth_exceeded/3. 



%***********************************************************************
%*	
%* module: interpreter.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92	
%*									
%* changed:								
%*
%* description:	different interpreters working on the knowledge base
%*									
%* see also:								
%*									
%***********************************************************************


%***********************************************************************
%*									
%* predicate:	ip_part1/2							
%*									
%* syntax: ip_part1(+Goal,-Proof)
%*									
%* args: Goal: an uncovered positive example
%*       Proof: a failing proof for the positive example
%*									
%* description:	works exactly as the general interpreter solve0/2. The only
%*	difference is that instead of failing when a system goal is failing
%*      or a proof is looping or rules are missing, the interpreter
%*      continues, assuming that the failing goals should be correct
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

ip_part1(Goal,Proof) :-
    retractall(tag(_)),
    assert(tag(Goal)),
    gen_depth(D, Delta),
    ipp1(Goal,D,Delta,Proof,Proof,[]).
 
ipp1(true,_D,_Delta,_Proof,Poi,_) :-
    !,
    Poi = [].

ipp1(no_rule,_,_,_,_,_):- !.

ipp1((A,B),D, Delta,Proof,Poi,Ancestors) :-
    !, Poi = [PoiA|PoiB],
    ipp1(A,D, Delta,Proof,[PoiA],Ancestors),
    ipp1(B,D, Delta,Proof,PoiB,Ancestors).
 
ipp1(A,D, Delta,Proof,Poi,Ancestors) :-
    interpretable_predicate(A),!,
    (   D > 0 ->
        true
    ;   assert(tag(A)),fail
    ),
    (   identical_member(A,Ancestors) ->
        Poi = [[-1,A,looping]]
    ;   D1 is D - 1,
        ipp1_rule(D,Delta,Proof,Poi,I,A,  B),
        Poi = [[I,A,PoiB]],
        ipp1(B,D1, Delta,Proof,PoiB,[A|Ancestors])
    ).

ipp1(A,_D, _Delta,_Proof,Poi,_):-
    (   call(A) ->
        Poi = [[sys,A,[]]]
    ;   Poi = [[sys,A,fail]]
    ).



ipp1_rule(_,_,_,_,I,A,B):- get_clause(I,A,B,_,_).
ipp1_rule(_D,_Delta,_Proof,Poi,_,A,no_rule):- 
     (   get_clause(_,A,_,_,_) ->
         fail
     ;   Poi = [[-1,A,no_rules]]
     ).


%***********************************************************************
%*									
%* predicate: ip_part2/3								
%*									
%* syntax: ip_part2(+Proofs,+Goal,-Uncovered_Atoms)
%*									
%* args: Proofs: failing proofs determined by ip_part1,
%*       Goal: uncovered positive example Uncovered_Atoms: Atoms that make
%*       Goal succeed, if they were covered by the kb
%*									
%* description:	the satisfiability of each subgoal within failing proof is
%*      determined. For that, the oracle might be necessary.
%*
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

ip_part2([P|_],_Goal,UA):-
   ipp2(P,[],[],UA).
ip_part2([_|R],Goal,UA):-
   ip_part2(R,Goal,UA).

ipp2([I,H,looping],_,L,[I:H|L]):- !.
ipp2([sys,_,fail],[I:A|_],L,[I:A|L]):- !.
ipp2([_,_,[]],_,L,L):- !.
ipp2([_,H,no_rules],_,L,[-1:H|L]):- !.
ipp2([I,H,SG],Ancestors,L,L1):-
   (   satisfiable(SG) ->
       ipp2_list(SG,[I:H|Ancestors],L,L1)
   ;   L1 = [I:H|L]
   ).


ipp2_list([],_,L,L).
ipp2_list([G|R],A,L,L2):-
   ipp2(G,A,L,L1),
   ipp2_list(R,A,L1,L2).


%***********************************************************************
%*									
%* predicate:	proof_path/4						
%*									
%* syntax: proof_path(+Ex,+Pred,+Type,-ClauseIDs)
%*									
%* args: Ex: example for p/n
%*       Pred = p(X1,..,Xn): most general term of p/n 
%*       Type = typei(Xi) for an argument of p/n
%*       ClauseIDs: list of clauseIDs that have beed used for proving
%*                  typei(ei) for the ith argument of Ex
%*									
%* description:	simulates the proof of typei(ei) for the ith argument of Ex
%*              and collects the indices of all used clauses
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

proof_path(Ex,P,T,Ts):-
   copy_term((P,T),(Ex,T0)),
   proof_path(T0,[],Ts).

proof_path(true,T,T):- !.
proof_path((A,B),T,T2):- !,
   proof_path(A,T,T1),
   proof_path(B,T1,T2).

proof_path(A,T,T):-
   A =.. [all|_],!.
proof_path(A,T,T):-
   A =.. [T1|_],
   (   T1 = atom; T1 = atomic; T1 = number ),!,
   call(A).

proof_path(A,T,T1):-
   get_clause(I,A,B,_,type),
   proof_path(B,T,T0),
   insert_unique(I,T0,T1).


%***********************************************************************
%*									
%* predicate:	t_interpreter/2							
%*									
%* syntax: t_interpreter(+Goal,+ClauseList)
%*									
%* args: Goal: goal type(Arg), Arg ground
%*       ClauseList: List of clauses defining different types
%*									
%* description:	proves type(Arg) from ClauseList as kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

t_interpreter(true,_):- !.
t_interpreter((A,B),CL):- !,
   t_interpreter(A,CL),
   t_interpreter(B,CL).
t_interpreter(C,_):-
   C =.. [P|_],
   (  P = atom ; P = number ; P = atomic   ),!,
   call(C).
t_interpreter(C,CL):-
   copy_term(CL,CL1),
   member((C:-B),CL1),
   t_interpreter(B,CL).



%************************************************************************
%*
%* predicate: solve/3
%*
%* syntax: solve(+Goal,-Mark,-Proofs)
%*
%* args: Goal: ground atom or rule with ground head
%*       Mark: success or fail
%*       Proofs: all succeeding/failing proofs according to Mark
%*
%* description: format for Proofs: [P1,..,Pn]
%*              where Pi = [ID,Head,PBody] where ID is the ID of the
%*              applied rule (sys for system predicates, -1 if no rule 
%*              is applicable), Head is the instantiation of the rule head,
%*              and PBody is the proof of the rule body. PBody is of the form
%*               - [], if Head is true
%*               - fail, if Head is a failing syspred
%*               - looping if the proof is looping on Head
%*               - no_rules if no rules match Head
%*               - depth_exceeded if the proof fails because of depth bound
%*              Maximum depth for proofs: 50
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

solve( (H :- B), Mark, Proofs) :- 
	ground(H) ->
	solve( B, Mark, Proofs)
	; !, fail.

solve(Goal,Mark,Proofs):-
    (   setof(Proof,Goal^solve0(Goal,Proof),Proofs0) ->  
        Mark = success
    ;   bagof(FProof,failed_proof(FProof),Proofs00),
        Mark = fail,
        (   depth_exceeded ->
            setof(EProof,A^depth_exceeded(A,EProof,[[-1,A,depth_exceeded]]),EProofs0),
            append(EProofs0,Proofs00,Proofs0)
        ;   Proofs0 = Proofs00
        )

    ),
    append_all(Proofs0,Proofs1),
    proof_close(Proofs1,Proofs).


%***********************************************************************
%*									
%* predicate:	solve_once/3							
%*									
%* syntax: solve_once(+Goal,-Mark,-Proof)
%*									
%* args: as solve/3								
%*									
%* description:	proves Goal only once							
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

solve_once( (H :- B), Mark, Proofs) :- 
	ground(H) ->
	solve_once( B, Mark, Proofs)
	; !, fail.

solve_once(Goal,Mark,Proofs):-
    (   solve0(Goal,Proof) -> Proofs0 = [ Proof ],
        Mark = success
    ;   bagof(FProof,failed_proof(FProof),Proofs0),
        Mark = fail
    ),
    append_all(Proofs0,Proofs1),
    proof_close(Proofs1,Proofs).



%***********************************************************************
%*									
%* predicate:	proof_close/2							
%*									
%* syntax: proof_close(+Proofs,-Proofs)
%*									
%* args: Proofs	as for solve/3							
%*									
%* description:	closes the open lists in Proofs
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

proof_close(X,[]):-
    var(X),!.
proof_close([[J,H,B1]|R1],[[J,H,B2]|R2]):-
    proof_close(B1,B2),
    proof_close(R1,R2).
proof_close(X,X):-
    atomic(X),!.


%***********************************************************************
%*									
%* predicate:	solve0/2							
%*									
%* syntax: solve0(+Goal,-Proof)								
%*									
%* args: Goal: ground atom, Proof: one possible proof for Goal
%*									
%* description:								
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

solve0(Goal,Proof) :-
    retractall(tag(_)),retractall(failed_proof(_)),retractall(depth_exceeded(_,_,_)),
    retractall(depth_exceeded),
    gen_depth(D, Delta),
    solve2(Goal,D,Delta,Proof,Proof,[]).


solve2(true,_,_,_,[],_) :- !.

 
solve2((A,B),D,Delta,Proof,Poi,Ancestors) :-
    !,
    Poi = [PoiA|PoiB],
    solve2(A,D,Delta,Proof,[PoiA],Ancestors),
    solve2(B,D,Delta,Proof,PoiB,Ancestors).
    
 
solve2(A,D,Delta,Proof,Poi,Ancestors) :-       % A is in KB 
    interpretable_predicate(A),!,
    ( D = 0 -> assert(tag(A)), assert(depth_exceeded(A,Proof,Poi)), fail
    ; ( identical_member(A,Ancestors) -> 
	    Poi = [[-1,A,looping]],
	    (   D < Delta ->
		    assert(failed_proof(Proof))
	    ;   true
	    ),
	    fail
      ; D1 is D - 1,
	solve_rule(D1,Delta,Proof,Poi,I,A,  B),
	Poi = [[I,A,PoiB]],
	solve2(B,D1,Delta,Proof,PoiB,[A|Ancestors])
      )
    ).

solve2(A,D,Delta,Proof,Poi,_):-            % A is built-in
    (                                      
        predicate_property(A,built_in),
	on_exception(_,call(A),fail) ->    % exception handling
        Poi = [[sys,A,[]]]
    ;   Poi = [[sys,A,fail]],
        (   D < Delta ->
            assert(failed_proof(Proof))
        ;   true
        ),
        fail
    ).


solve2(A,D,D,Proof,Poi,_) :-         % no rules at all for initial goal
    \+(depth_exceeded),
    Poi = [[-1,A,no_rules]],
    assert(failed_proof(Proof)),
    fail.


%***********************************************************************
%*									
%* predicate:	solve_rule/7							
%*									
%* syntax: solve_rule(+D,+Delta,+Proof,+Proof_Poi,-ID,+Goal,-Body)
%*									
%* args: D,Delta: depth bounds for iterative deepening
%*       Proof,Proof_Poi: intermediate Proof of the toplevel goal (open list)
%*       Goal: current goal
%*       ID,Body: id and body of a kb-rule matchin Goal (if any)
%*									
%* description:								
%*
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

solve_rule(_,_,_,_,I,A,B):- 
   functor(A,F,N),
   functor(A1,F,N),
   get_clause(I,A1,B,_,_),
   unify(A,A1).
solve_rule(D,Delta,Proof,Poi,_,A,_):- 
    Poi = [[-1,A,no_more_rules]],
    (   D < Delta ->
        assert(failed_proof(Proof))
    ;   true
    ),
    fail.


%***********************************************************************
%*									
%* predicate: gen_depth/2							
%*									
%* syntax:    gen_depth(D, Delta)							
%*									
%* args: D,Delta: integers
%*									
%* description:	generates depth bound for the interative deepening
%*              theorem prover. Delta is the difference between D and
%*              the former depth (not to create duplicate proofs)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

/***** gen_depth without maximum depth*****
gen_depth(D, Delta) :-
    gen_depth(3, 100, D, Delta).
 
 
gen_depth(D, Delta, D, Delta).
 
gen_depth(D0, _, D, Delta) :-
    Delta1 is D0 div 2 + 1,
    D1 is D0 + Delta1,
    (   tag(_) ->
        retractall(tag(_)),
        gen_depth(D1, Delta1, D, Delta)
    ;   fail
    ).
*******/ 


gen_depth(D, Delta) :-                     % D = new depth for proofs
                                           % Delta = new D - former D
    depth_bound(N),
    (   number(N) ->
        (   N >= 3 ->
            gen_depth(3, D, 3, Delta)
        ;   gen_depth(N,D,N,Delta)
        )
    ;   gen_depth(3, D, 3, Delta)
    ).           

 

gen_depth(D, D, Delta, Delta).
 
gen_depth(D0, D, _, Delta) :-
    (   tag(_) ->
        retractall(tag(_)),
	Delta1 is ( D0 div 2 ) + 1,
	D1 is D0 + Delta1,
        depth_bound(Max),
        (    number(Max) ->
	     (   D1 =< Max ->
                 retractall(depth_exceeded(_,_,_))
             ;   assert(depth_exceeded),fail
             )
        ;    true
        ),
        gen_depth(D1, D, Delta1, Delta)
    ;   fail
    ).

set_proof_depth:-
    nl,nl, 
    write('Speficy maximum depth for theorem prover (number or n for unbound proofs): '),
    read(N),
    retractall(depth_bound(_)),
    assert(depth_bound(N)).


%***********************************************************************
%*                                                                      
%* predicate: prove1/2                                    
%*                                                                      
%* syntax: prove1(+Clause, -Proof)                                      
%*                                                                      
%* args:     Clause: clause in list form ( [H:p,L1:n,L2:n,..])          
%*           Proof:  list of all literals used to prove clause 
%*                                                                      
%* description:                                                         
%*      prove1 tries to match Clause against literals in this kb,       
%*             use for clause reduction wrt theta-subsumption (Plotkin). 
%*
%* example:                                                             
%*                                                                      
%* peculiarities:                                                       
%*                                                                      
%* see also: Buntine,1988. Plotkin,1970.                                
%*                                                                      
%***********************************************************************
/*
prove1([H|T],Proof):- 
	 prove1(H,HProof),
	 prove1(T,TProof),
	 append(HProof,TProof,Proof).
prove1([],[]).
prove1(L:S,[L:S]):- 
	 member(S,[n,r]), 
         body(L,_O,_I).
prove1(L:p,[L:p]):- 
	 head(L,_O,_I).
*/

%***********************************************************************
%*                                                                      
%* predicate: prove1/2                                    
%*                                                                      
%* syntax: prove1(+Clause, -Proof)                                      
%*                                                                      
%* args:     Clause: clause in list form ( [H:p,L1:n,L2:n,..])          
%*           Proof:  list of all literals used to prove clause 
%*                                                                      
%* description:                                                         
%*      prove1 tries to match Clause against literals in this kb,       
%*      use for clause reduction wrt theta-subsumption (Plotkin). 
%*      This is a more efficient version for embedding Clause in the kb: 
%*      (IRENE)
%*      a list CL1 = [Lit:Sign:Litlist|_] is constructed from Clause
%*      where Litlist is the list of literals in the kb (if Sign = p, literals 
%*      head(L,_,_), else body(L,_,_)) unifiable with Lit. CL1 is sorted
%*      ascendingly according to the length of Litlist. If there is an
%*      empty Litlist in CL1, prove1a fails and backtracking occurs.
%*      Else Lit is unified with a literal in Litlist (backtracking point),
%*      and the remaining list CL1 is updated.
%*
%* example:                                                             
%*                                                                      
%* peculiarities:                                                       
%*                                                                      
%* see also: Buntine,1988. Plotkin,1970.                                
%*                                                                      
%***********************************************************************

prove1(CL,SProof):-
   ini_prove1(CL,CL1),
   prove1a(CL1,[],SProof).

prove1a([],SP,SP).
prove1a(L,_,_):- member(_:_:[],L),!,fail.
prove1a([Lit:S:LitL|R],SProof,SP):-
   (   ground(Lit) ->
       prove1a(R,[Lit:S|SProof],SP)
   ;   member(Lit,LitL),
       adapt_prove1(R,Lit,S,R1),
       prove1a(R1,[Lit:S|SProof],SP)
   ).

ini_prove1([],[]).
ini_prove1([Lit:S|R],R2):-
   ini_prove1(R,R1),
   (   S = p ->
       mysetof(H,M^O^(head(H,M,O),\+(\+(Lit = H))),LitL)
   ;   mysetof(H,M^O^(body(H,M,O),\+(\+(Lit = H))),LitL)
   ),
   insert_prove1(Lit:S:LitL,R1,R2).

insert_prove1(L:S:LL,[L1:S1:LL1|R],[L1:S1:LL1|R1]):-
   length(LL,LLN),length(LL1,LL1N),
   LLN > LL1N,!,
   insert_prove1(L:S:LL,R,R1).
insert_prove1(X,L,[X|L]).

adapt_prove1([],_,_,[]).
adapt_prove1([Lit:S:_|R],Lit1,S1,R1):-
   Lit == Lit1, S == S1,!,
   adapt_prove1(R,Lit1,S1,R1).
adapt_prove1([Lit:S:LL|R],Lit1,S1,R2):-
   mysetof(X,(member(X,LL),\+(\+(Lit = X))),LL1),
   adapt_prove1(R,Lit1,S1,R1),
   insert_prove1(Lit:S:LL1,R1,R2).


%***********************************************************************
%*									
%* predicate:   prove3/2						
%*									
%* syntax: prove3(+CL,-CL)								
%*									
%* args: CL: clause body in list notation
%*									
%* description:	embedd CL in skolemized body of an example clause
%*		(body/3 entries in the kb)
%* 		used for absorption, saturation
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

prove3( [A|B],[ProofA|ProofB] ):-
	 prove3(A,ProofA),
	 prove3(B,ProofB).
prove3([],[]).

prove3(A:n,A:n):-
	 body(A,_,_).
prove3(A:r,A:r):-
	 body(A,_,_).


%***********************************************************************
%*									
%* predicate:   prove4/3						
%*									
%* syntax: prove4(+CL,-Uncovered,-Proof)
%*									
%* args: CL: clause in list notation
%*       Uncovered = H/M, where M in {new_head,new_body}
%*                or Uncovered = [] if all literals are covered
%*       Proof = [[Lit,N],...] where N in {head,body}
%*									
%* description:	embeds CL in skolemized example clause (head/3,body/3 entries)
%*              allows 1 uncovered literal (= the resolution literal) & returns it
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

prove4( [H|More], Uncovered, [ProofH|ProofRest]):-
       prove4(H,Uncovered,ProofH),
       prove4(More,Uncovered,ProofRest).


prove4([],[],[]):-!.
prove4([],_,[]):-!.
prove4(H:n,_,[H,body]):- body(H,_,_).
prove4(H:r,_,[H,body]):- body(H,_,_).
prove4(H:p,_,[H,head]):- head(H,_,_).
prove4(H:n,Uncovered,[]):- var(Uncovered), Uncovered = H/new_head.
prove4(H:r,Uncovered,[]):- var(Uncovered), Uncovered = H/new_head.
prove4(H:p,Uncovered,[]):- var(Uncovered), Uncovered = H/new_body.


%***********************************************************************
%*									
%* predicate:   prove5/2						
%*									
%* syntax: prove5(+HS,+RuleIDs)								
%*									
%* args: HS: skolemized clause head, RuleIDs: list of ruleIDs
%*									
%* description:	tries to infer HS from assumptions and the rules in
%*              RuleIDs
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

prove5( H, _):- assumption( H,_,_).
prove5( true, _).
prove5( H, RULES):- 
     get_clause(ID,H,true,_,_), 
     member(ID,RULES).
prove5( (L1,L2), RULES):-
     prove5(L1,RULES),
     prove5(L2,RULES).
prove5( H, RULES):-
     get_clause(ID, H, B, _,_),
     member( ID, RULES),
     prove5(B, RULES).

