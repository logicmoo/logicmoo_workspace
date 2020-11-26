% MODULE filter EXPORTS
:- module( filter,
	   [ is_flat/1,
             is_unflat/1,
	     connected_vars/4,
             truncate_unconnected/2,
             truncate_unconnected/1,
             is_weakly_generative/1,
             is_strongly_generative/1,
             is_connected/1,
             truncate/3,	
             truncate_r/2,
             truncate_r/1,
             truncate_flat_r/1,
             truncate_unconnecting/1,
             truncate_unconnecting/2,
             truncate_strongly_generative/1,
             truncate_strongly_generative/2,
             truncate_neg_based/1,
             truncate_flat_neg_based/1,
             truncate_facts/1,
             truncate_j/2,  
      	     noduplicate_atoms/1,
             noduplicate_atom/2,
	     select_var_sharing_lits/2,
	     already_in/3
	   ]).

 
%IMPORTS
:- use_module_if_exists(library(not),
               [ (once)/1 ]).
:- use_module_if_exists(library(sets),
              [union/2,union/3,intersection/3,list_to_set/2,subtract/3,
               subset/2,select/3]).
:- use_module_if_exists(library(strings),
              [string_append/3,substring/4,midstring/6]).
:- use_module_if_exists(library(basics),[member/2]).
:- use_module_if_exists(library(lists),[subseq/3,rev/2,last/2,nth1/4]).
:- use_module_if_exists(library(occurs),[sub_term/2]).
:- use_module_if_exists(library(arg),[genarg/3]).
:- use_module_if_exists(library(subsumes),
              [subsumes_chk/2]).
:- use_module(home(div_utils),
              [shares_var/2]).
:- use_module(home(var_utils),
              [vars/2,contains_vars/2,skolemize/3,deskolemize/3,skolems/2,
               flagged_contains_vars/3]).
:- use_module(home(kb),
              [get_clause/5,store_clause/4,delete_clause/1,get_fact/4,
               unflatten_kb/0]).
:- use_module(home(flatten),
              [flatten_clause/2,unflatten_clause/2]).
:- use_module(home(evaluation),
              [correct_chk/0]).


% METAPREDICATES
% none

:- dynamic functional_mode/1.

%***********************************************************************
%*	
%* module: filter.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92	
%*									
%* changed:								
%*									
%* description: various filters useful for td- and bu-induction
%*
%* see also:								
%*									
%***********************************************************************


%***********************************************************************
%* predicate: already_in/3 (optional)
%*
%* syntax: already_in(+N,+P,+Y)                     					
%*		 	
%* args:  N: arity of a literal P 
%*        P: literal
%*        Y: term
%*	
%* description: succeeds if Y is(==) already an argument of P  
%*
%* example:       
%*	
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

already_in(0,_,_):- !,fail.
already_in(N,P,Y):-
   arg(N,P,Pn),
   (   Pn == Y ->
       true
   ;   N1 is N - 1,
       already_in(N1,P,Y)
   ).


%***********************************************************************
%*									
%* predicate:	select_var_sharing_lits/2
%*									
%* syntax: select_var_sharing_lits(+List_of_Clauses,-List_of_Clauses)
%*									
%* args:								
%*									
%* description:	removes all clauses from List_of_Clauses the last
%*    literal of which does not share a variable with
%*    the rest of the clause -> to be used during td-refinement
%*    (not to add unconnected body literals)
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

select_var_sharing_lits([],[]).
select_var_sharing_lits([C|R],R2):-
   select_var_sharing_lits(R,R1),
   (   var_sharing_lit(C,[]) ->
       R2 = [C|R1]
   ;   R2 = R1
   ).


%***********************************************************************
%*									
%* predicate:	var_sharing_lit/2
%*									
%* syntax: var_sharing_lit(+Clause,+CAccu)
%*									
%* args: CAccu contains all literals of clause except the last one
%*									
%* description:	succeeds if the last literal of clause shares (at least)
%*	a variable with the rest of the clause.
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

var_sharing_lit((H:-B),[]):-
   !,var_sharing_lit(B,[H]).
var_sharing_lit((A,B),C):-
   !, var_sharing_lit(B,[A|C]).
var_sharing_lit(A,C):-
   shares_var(A,C),!.


%***********************************************************************
%* predicate: noduplicate_atom/2	
%*
%* syntax: noduplicate_atom(+P,+B) 
%*		 	
%* args:  P: literal  
%*        B: clause body 
%*	
%* description: tests if P already occurs(==) in B 
%*
%* example:                 
%*	
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

noduplicate_atom(P,(A,B)):-
   !, P \== A,
   noduplicate_atom(P,B).
noduplicate_atom(P,A):-
   P \== A.


%***********************************************************************
%* predicate: noduplicate_atoms/1	
%*
%* syntax: noduplicate_atoms(+Clause) 
%*
%* args:
%*	
%* description:  tests whether  Clause  contains duplicate literals 
%*
%* example:               
%*	
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

noduplicate_atoms((A:-B)):-
  !, noduplicate_atom(A,B),
  noduplicate_atoms(B).
noduplicate_atoms((A,B)):-
  !,noduplicate_atom(A,B),
  noduplicate_atoms(B).
noduplicate_atoms(_).
   

%***********************************************************************
%*									
%* predicate: connected_vars/4						
%*									
%* syntax: connected_vars(+ClauseIn,-ClauseOut,-Connected,-Unconnected) 
%*									
%* args: ClauseIn,ClauseOut: clauses in list notation			
%*	 Connected,Unconnected: list of variables          		
%*									
%* description:	returns connected & unconnected vars 			
%*    A variable is connected ( to the head literal ) iff   
%*	  - it appears in the head, or	
%*	  - it appears in a literal with  a connected variable
%*    A literal is connected iff it's vars are.	
%*									
%* example:								
%*									
%* peculiarities: we make a copy of the input clause	                
%*									
%* see also:								
%*									
%***********************************************************************

connected_vars(ClauseIn,ClauseOut,Connected,Unconnected):-
        copy_term(ClauseIn,ClauseIn1),
        skolemize( ClauseIn1, S, Clause1),
        connected_skolems( Clause1, C, U),
        deskolemize( (Clause1:C:U) , S, (ClauseOut:Connected:Unconnected) ).

connected_skolems([Head:p|Body],Connected,Unconnected):-
        skolems(Head, Con),
        Uncon = [],
        find_connected_skolems_in_body(Body, Con, Connected, Uncon, Unconnected_tupels),
        union(Unconnected_tupels,Unconnected).


%***********************************************************************
%*									
%* predicate:	find_connected_skolems_in_body/5
%*									
%* syntax: find_connected_skolems_in_body(+Body_list,+Connected,-Connected,
%*                                        +Unconnected_tuples,-Unconnected_tuples)
%*									
%* args: Body_list .. clause body in list notation,
%*       Connected .. connected skolem atoms
%*       Unconnected_tuples .. lists of lists of unconnected skolem atoms
%*									
%* description:	separates connected and unconnected skolem atoms
%*	(each corresponds to a variable) within the body
%*									
%* example:								
%*									
%* peculiarities:							
%*									
%* see also:								
%*									
%***********************************************************************

find_connected_skolems_in_body( [L:_|Rest], C1,C2,U1,U2):-
        skolems(L,V1),
        ( intersection(C1,V1,I), I \== []
             -> union(C1,V1,C4) , connect(V1,C4,C3,U1,U3)
        ;
             C3 = C1, U3 = [ V1 | U1 ]
         ),
        find_connected_skolems_in_body(Rest, C3,C2,U3,U2).
find_connected_skolems_in_body( [],C,C,U,U).

connect(_V,C,C,[],[]).
connect(Vars,C1,C2,[Tupel|More],U2):-
        member(Var,Vars),
        member(Var,Tupel),!,
        union(C1,Tupel,C3),
        connect(Vars,C3,C2,More,U2).
connect(Vars,C1,C2,[Tupel|More],[Tupel|U2]):-
        connect(Vars,C1,C2,More,U2).


%***********************************************************************
%*	
%* predicate: is_weakly_generative/1
%*								
%* syntax: is_weakly_generative(+Clause)	
%*			
%* args:      Clause: clause in list notation				
%*									
%* description:	a clause is weakly generative if all vars of its head   
%*		appear also in another literal
%*
%* example:       
%*
%* peculiarities:       				
%* 									
%* see also: Muggleton,90						
%*									
%***********************************************************************

is_weakly_generative([H:p|B]):- 
          vars(H,Vars),
          contains_vars(Vars,B).


%***********************************************************************
%*	
%* predicate: is_stronly_generative/1
%*        								
%* syntax: is_strongly_generative(+Clause)	
%*			
%* args:      Clause: clausew in list notation				
%*									
%* description:	a clause is strongly generative if every variable       
%*		appears in at least 2 literals
%*
%* example:       
%*
%* peculiarities:       				
%* 									
%* see also: Muggleton,90						
%*									
%***********************************************************************

is_strongly_generative(Clause):-
           findall(Flag, 
                       ( subseq( Clause, [L], Rest),   
                         vars(L,Vars),
                         flagged_contains_vars(Vars,Rest,Flag) ),
                 Flags),
           \+ member( false, Flags), !.


%***********************************************************************
%*	
%* predicate: is_connected/1
%*								
%* syntax: is_connected(+Clause)	
%*	        		
%* args:      Clause: clause in list notation				
%*									
%* description:	a clause is connected if every variable is connected    
%*		to the head.  
%*
%* example:       
%*
%* peculiarities:                    				
%* 									
%* see also: Rouveirol,91 
%*									
%***********************************************************************

is_connected(Clause):- connected_vars(Clause,_,_,[]).


%***********************************************************************
%*	
%* predicate: is_flat/1, is_unflat/1
%*								
%* syntax: is_flat(+Clause), is_unflat(Clause)			
%*									
%* args: clause in list notation [ head:p, b1:n, .. ]			
%*									
%* description:	succeeds/fails if Clause contains no function symbols	
%*
%* example:       
%*									
%* peculiarities: if input is not a clause in list form, is_flat/1	
%*		  always succeeds.	
%*
%* see also:				
%*									
%***********************************************************************

is_flat(Clause):- nonvar(Clause), \+ is_unflat(Clause).

is_unflat([L:_|_Rest]):-is_unflat_literal(L).
is_unflat([_|Rest]):- is_unflat(Rest).                  

is_unflat_literal(L):-
         sub_term(Subterm,L),
         Subterm \== L,
         nonvar(Subterm),!.


%***********************************************************************
%*									
%* predicate:	truncate/3         					
%*									
%* syntax: truncate(+Strategy,+ClauseIn,-ClauseOut)			
%*									
%* args: Strategy: one of { r, unconnected, unconnecting,               
%*                                           strongly_generative}       
%*	 ClauseIn,ClauseOut: integers	(kb references)			
%*									
%* description:	performs truncation operator on ClauseIn			
%*		using Strategy.		                                
%* 									
%*		The list of possible strategies is to be completed.	
%*
%* example:       
%*									
%* peculiarities:							
%*									
%* see also: 
%*									
%***********************************************************************

truncate( Strategy, In, Out):-
      get_clause( In,_,_,C1,_),
      do_truncate( Strategy, C1, C2),
      store_clause( _,C2,trunc,Out).

do_truncate( r, C1, C2):- truncate_r(C1,C2).
do_truncate( unconnected,C1,C2):- truncate_unconnected(C1,C2).
do_truncate( strongly_generative, C1,C2):- truncate_strongly_generative(C1,C2).
do_truncate( unconnecting,C1,C2):- truncate_unconnecting(C1,C2). 


%***********************************************************************
%*									
%* predicate: truncate_r/2/1                 				
%*									
%* syntax: truncate_r(ClauseIn,ClauseOut)				
%*         truncate_r(ID)                  				
%*									
%* args: clauses in list notation					
%*									
%* description:	drop all literals with label ':r',			
%*		i.e. drop all literals that were used in saturation.
%*
%* example:
%*
%* peculiarities:
%*
%* see also:	
%* 									
%***********************************************************************

truncate_r(ID):-
         get_clause(ID,_,_,ClauseIn,_),
         truncate_r(ClauseIn,ClauseOut),
         delete_clause(ID),
         store_clause( _,ClauseOut, trc, ID).

truncate_r(ClauseIn,ClauseOut):-
         copy_term(ClauseIn,C),
         do_truncate_r(C,ClauseOut).

do_truncate_r([],[]).
do_truncate_r([_L:r|Rest],Rest1):-!, do_truncate_r(Rest,Rest1).
do_truncate_r([L:S|Rest],[L:S|Rest1]):-!, do_truncate_r(Rest,Rest1).


%***********************************************************************
%*									
%* predicate: truncate_flat_r/2/1                 			
%*									
%* syntax: truncate_flat_r(ClauseIn,ClauseOut)				
%*         truncate_flat_r(ID)                  			
%*									
%* args: clauses in list notation					
%*									
%* description:	drop all true literals with label ':r',			
%*		i.e. drop all literals that were used in saturation,	
%* 	        but no literals with type information ( suffix '_p').
%*
%* example:
%*
%* peculiarities:
%*
%* see also:	
%* 									
%***********************************************************************

truncate_flat_r(ID):-
         get_clause(ID,_,_,ClauseIn,_),
         truncate_flat_r(ClauseIn,ClauseOut),
         delete_clause(ID),
         store_clause( _,ClauseOut, trc, ID).

truncate_flat_r(ClauseIn,ClauseOut):-
         copy_term(ClauseIn,C),
         do_truncate_flat_r(C,ClauseOut).

do_truncate_flat_r([],[]).
do_truncate_flat_r([L:r|Rest],Rest1):-
     functor(L,F,N),
     functor(LC,F,N),                     % L is bg predicate
     get_clause(_,LC,_,_,usr), 
     !, do_truncate_flat_r(Rest,Rest1).
do_truncate_flat_r([L:S|Rest],[L:S|Rest1]):-!, do_truncate_flat_r(Rest,Rest1).


%***********************************************************************
%*									
%* predicate: truncate_unconnected/2       				
%*									
%* syntax: truncate_unconnected(+ClauseIn, -ClauseOut)			
%*									
%* args: ClauseIn,ClauseOut: clauses in list notation 			
%*									
%* description:	truncate unconnected body literals (see above)		
%*									
%* example: let ClauseIn = [ min(A,[A|B]):p, min(C,B):n, ge(E,F):n ]    
%*	    then ClauseOut = [ min(A,[A|B]):p, min(C,B):n ]
%*          let ClauseIn = [p(X):n,q(X,V1):n,r(V1,V2):n,q(V3):n,s(V3,V1):n],
%*          then ClauseOut = [p(X):n,q(X,V1):n,r(V1,V2):n,q(V3):n,s(V3,V1):n]
%*          (in contrast to truncate_unconnecting)
%*									
%* peculiarities:							
%*									
%* see also:								
%*									
%***********************************************************************

truncate_unconnected(ID):-
        get_clause(ID, _,_,C,_),
        truncate_unconnected(C,D),
        delete_clause(ID),
        store_clause(_,D,trc_unconn,ID),!.



truncate_unconnected( ClauseIn, ClauseOut):-
        skolemize(ClauseIn,S,ClauseInS ),
        connected_skolems( ClauseInS, _Con, Uncon),
        ClauseInS = [ Head:p | Body ],
        truncate_unconnected1(Body, Uncon, BodyOut),
        ClauseOutS = [ Head:p | BodyOut ],
        deskolemize( ClauseOutS,S, ClauseOut).

truncate_unconnected1([],_,[]).  % no literals to drop
truncate_unconnected1(B,[],B):-!.   % all vars connected
truncate_unconnected1([L:_ | More], Uncon, BodyOut):-
        ( skolems( L, SKs),
          member(A,SKs)                  % either all or no vars in L are connected
          ->   member(A,Uncon),          % local cut 
               truncate_unconnected1(More,Uncon,BodyOut)      
        ; fail
        ),!.
truncate_unconnected1([L:S | More], Uncon, [L:S|BodyOut]):-
        truncate_unconnected1(More,Uncon,BodyOut),!.


%***********************************************************************
%*									
%* predicate: truncate_unconnecting/1/2					
%*									
%* syntax: truncate_unconnecting(ClauseIn,ClauseOut)			
%*									
%* args: clauses in list notation          				
%*									
%* description:	connectivity heuristics for truncation:			
%* 	Rouveirol: Drop a body literal if all other literals remain	
%*		   connected.                             		
%*	We added another constraint: the resulting clause must be  	
%*				     weakly generative.  		
%*	Do not confuse the connectivity with the connectedness		
%*	heuristics !!	
%*
%* example: truncate_unconnecting([p(X):n,q(X,V1):n,r(V1,V2):n,q(V3):n,s(V3,V1):n],
%*                                [p(X):n,q(X,V1):n,r(V1,V2):n,        s(V3,V1):n],
%*
%* peculiarities:						
%*									
%* see also: Rouveirol '90; module connectedness.pl			
%*	
%***********************************************************************

truncate_unconnecting(ID):-
        get_clause(ID, _,_,C,_),
        truncate_unconnecting(C,D),
        delete_clause(ID),
        store_clause(_,D,trc_unconn,ID),!.

truncate_unconnecting( ClauseIn, ClauseOut):-
           copy_term( ClauseIn, [H:p|Body]),
           subseq(Body,[_L],BodyOut),
           ClauseOut1 = [ H:p | BodyOut ],
           connected_vars( ClauseOut1, ClauseOut, _Con, []),   % no unconnected vars
           is_weakly_generative(ClauseOut).


%***********************************************************************
%*									
%* predicate: truncate_strongly_generative/2				
%*									
%* syntax: truncate_strongly_generative(+ClauseIn,-ClauseOut)		
%*									
%* args: clauses in list notation   					
%*									
%* description:	drop one body literal from a strongly generative	
%*		clause s.t. the resulting clause is also strongly	
%* 		generative.						
%*
%* example:
%*
%* peculiarities:
%*
%* see also:									
%*									
%***********************************************************************

truncate_strongly_generative(ID):-
        get_clause(ID, _,_,C,_),
        truncate_strongly_generative(C,D),
        delete_clause(ID),
        store_clause(_,D,trc_unconn,ID),!.

truncate_strongly_generative(ClauseIn, ClauseOut):-
            copy_term(ClauseIn,[H:p|Body]),
            subseq(Body,[_L],BodyOut),
            ClauseOut = [ H:p | BodyOut ],
            is_strongly_generative(ClauseOut).


%***********************************************************************
%*									
%* predicate: truncate_neg_based/1	           			
%*									
%* syntax: truncate_neg_based(+ID)                        		
%*									
%* args: integer, kb reference   					
%*									
%* description:	truncate as many literals as possible, s.t. the 	
%*		resulting clause covers no  negative examples.     	
%* 						                        
%*              This only works on unflat clauses;                      
%*              i.e. it only accounts for dropping condition.           
%*
%* example:
%*
%* peculiarities:
%* 						                        
%* see also : Muggleton's negative based reduction                      
%*									
%***********************************************************************

truncate_neg_based(ID):-
        ( get_clause( ID, _,_,C, _Label) 
          ;
          store_clause(_,C, _Label,ID), fail
        ),
        delete_clause(ID),
        once(truncate_unconnected(C,D)),
        store_clause(_,D,trc,ID),
        correct_chk,
        D = [ H | Body ],
        length(Body,N),
        truncate_neg_based1(N,ID, H, Body). 

truncate_neg_based1(0,_,_,_):-!.

truncate_neg_based1(N, ID, H, Body):-
        nth1(N,Body,_L,NewBody),
        delete_clause(ID),
        store_clause( _,[H|NewBody],trc,ID),
        M is N - 1,
        ( is_weakly_generative([H|NewBody]),
          correct_chk 
               -> truncate_neg_based1(M,ID,H,NewBody)
         ; 
                delete_clause(ID),
                store_clause( _,[H|Body],trc,ID),
               truncate_neg_based1( M, ID, H, Body)
        ).


%***********************************************************************
%*									
%* predicate: truncate_flat_neg_based/1	           			
%*									
%* syntax: truncate_flat_neg_based(+ID)                        		
%*									
%* args: integer, kb reference   					
%*									
%* description:	truncate as many literals as possible, s.t. the 	
%*		resulting clause covers no  negative examples.     	
%* 						                        
%*              As initial condition, the kb must be unflat.            
%*              The truncation is done on the flattened clause, so that 
%*              this accounts for the dropping rule & inverse subst.    
%* 	        On exiting, the kb is unflat.
%*
%* example:
%*
%* peculiarities:                           
%* 						                        
%* see also : Muggleton's negative based reduction                      
%*									
%*********************************************************************** 

truncate_flat_neg_based(ID):-
       get_clause(ID,_,_,C,Label),
       flatten_clause(C,D),
       truncate_unconnected(D,E),
       delete_clause(ID),
       store_clause(_,E,_Trc,ID),!,
       ( once(correct_chk),
         D = [ H|Body],
         truncate_flat_neg_based(ID,H,[],Body)
        ;
         delete_clause(ID),
         store_clause(_,C,Label,ID)
        ).

truncate_flat_neg_based(ID,H,Nec,[]):-
        delete_clause(ID),
        truncate_unconnected([H|Nec],D),
        unflatten_clause(D,E),
        store_clause(_,E,trc,ID),
        !,
        correct_chk.


truncate_flat_neg_based(ID,H,Nec,[L|Maybe]):-
        append(Nec,Maybe,C),
        truncate_unconnected([H|C],D),
        unflatten_clause(D,E),
        delete_clause(ID),
        store_clause(_,E,trc,ID),
        ( correct_chk   ->  Nec1 = Nec
         ;
          append(Nec,[L],Nec1)
        ),
        truncate_flat_neg_based(ID,H,Nec1,Maybe).


%***********************************************************************
%*									
%* predicate: truncate_facts/1     	           			
%*									
%* syntax: truncate_facts(+ID)                            		
%*									
%* args: integer, kb reference   					
%*									
%* description:	truncate all body literals unifying with a kb fact    	
%*		labeled 'usr'.	
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%*********************************************************************** 

truncate_facts(ID):-
        get_clause(ID,_,_,[H|B],_),
        truncate_facts1(B,BodyOut),
        delete_clause(ID),
        store_clause(_,[H|BodyOut],trc,ID).

truncate_facts1([],[]).
truncate_facts1([L:S|Rest], BodyOut):-
        truncate_facts1(Rest,Rest1),
        ( get_fact( _,L1,_,usr),
          subsumes_chk(L1,L)  -> BodyOut = Rest1
          ;                      BodyOut = [ L:S|Rest1]
        ).


%***********************************************************************
%*									
%* predicate: truncate_j/2						
%*									
%* syntax: truncate_j(+ID,J)						
%*									
%* args: ID: kb reference						
%*	 J : integer = 	number of allowed new variables per literal	
%*									
%* description:	truncate all literals containing more than J variables	
%*		not appearing in the head of kb clause ID.             	
%* 
%* example:
%*
%* peculiarities:									
%*									
%* see also: truncate_j is remotely related to Muggleton's      	
%*	     ij-determination. We take i = 1.               		
%*	     More importantly, we cannot tell in our system, if a	
%*	     literal is determinate or not, since we have no model.     
%*
%***********************************************************************

truncate_j(ID,J):-
      get_clause(ID,_,_,C,_),
      skolemize( C, S, [Head:p|BodyIn] ) ,
      skolems(Head,Vars),
      do_truncate_j( J, Vars, BodyIn, BodyOutS),
      deskolemize( [Head:p|BodyOutS], S, D),
      truncate_unconnected( D,E),
      delete_clause(ID),
      store_clause( _,E,trc_j,ID).

do_truncate_j(J,Vars,BodyIn,BodyOut):-
      findall( L, ( member(L,BodyIn),
                    once(   ( skolems(L,VarsL),
                             subtract(VarsL,Vars,NewVars),
                             length(NewVars, J1),
                             J1 =< J
                           ))
                  ),
               BodyOut).
                    
