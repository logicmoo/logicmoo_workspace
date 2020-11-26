% MODULE g1_ops EXPORTS
:- module( g1_ops, 
        [ saturate/2    ,        % saturate with default depth
          saturate/3,            % saturate with given maximum depth
          elem_saturate/3, 	
	  inv_derivate/2,
          inv_derivate/3,
          inv_derivate1/2,
	  most_spec_v/3,
	  absorb/3,
          identify/3,
	  g1_op/3, 
	  g1_op/4,
	  apply_g1/2
	]).

%IMPORTS
:- use_module(home(lgg),
              [headed_lgg/3,headed_lgg/4]).
:- use_module(home(kb),
              [get_clause/5,store_clause/4,delete_clause/1,
               get_example/3]).
:- use_module(home(var_utils),
              [skolemize/3,skolemize/4,deskolemize/3]).
:- use_module(home(div_utils),
              [neg/2, buildpar2/3,efface/3]).
:- use_module(home(bu_basics),
              [addtolist/1,getlist/1,clear_mngr/0,assert_body/1,
               abs_process_proofs/2,abs_build_body/1,assert_clause/1,
               ident_process_proofs/2,g1_process_proofs/2,g1_build_clause/2,
               idev_build_clause/2,process_new_literals/2,sat_build_clause/3,
               assert_absorptions/2,body/3,ident_build_body/1]).
:- use_module(home(show_utils),
              [show_bodies/0]).
:- use_module(home(interpreter),
              [prove3/2,prove4/3]).
:- use_module_if_exists(library(basics),
              [member/2]).

% METAPREDICATES
% none


%***********************************************************************
%*	
%* module: g1_ops.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92	
%*									
%* changed:								
%*									
%*									
%* description:	- g1-Operator
%*                Implementation of Ruediger Wirth's G1-operator for inverse
%*                resolution corresponding to his 1989 PhD thesis.
%*              - Absorption              				
%* 		  All clauses induced by absorption are labelled "abs" in kb	
%*              - Rouveirol's saturation, with functions allowed as terms.
%*                *Saturation: Maximum depth: given as input                           
%*                             default:  100 inverse resolution steps             
%*                *elementary saturation                                   
%*              - inverse derivate 
%*                Muggleton's inverse linear derivation, i.e. the 
%*                repeated application of the most specific v 
%*                (most specific absorption &most specific identification)
%*                Induced clauses are marked invd
%*              - identification
%*	          clauses induced by identification are labelled "idn" in kb
%*									
%* see also:								
%*									
%***********************************************************************


%********************************************************************************%
%*                                                                     
%* predicate: g1_op/3, g1_op/4                                                   
%*                                                                     
%* syntax: g1_op ( +ResolventID, +Parent1ID, -Parent2ID ) 
%*         g1_op ( +ResolventID, +Parent1ID, -Parent2ID, + Label )
%*
%* args: ResolventID, Parent1ID, Parent2ID .. clauseIDs
%*       Label for Parent2ID (default: g11)           
%*                                                                   
%* description: given a resolvent and one parent clause, the second parent clause
%*              is constructed
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*									
%********************************************************************************

g1_op( Res, Par1, Par2) :-
	g1_op( Res, Par1, Par2, g11).
g1_op( Res, Par1, Par2, Label) :-
 	(var(Label) -> Label = g11;true),
        get_clause(Res,_,_,Lres,_), 
        get_clause(Par1,_,_,Lpar1,_),                     % clauses in list representation
        Res \== Par1,                                     % not a clause with itself
        clear_mngr,
        skolemize(Lres,SS,LresSko),                       % skolemize resolvent
        assert_clause(LresSko),
        findall(Uncovered:Proof,prove4(Lpar1,Uncovered,Proof),Proofs),
        g1_process_proofs(Proofs,Reslit),
        g1_build_clause(Reslit,Lpar2Sko),                 % build parent2
        deskolemize(Lpar2Sko,SS,Lpar2),                   % deskolemize
        store_clause(_,Lpar2,Label,Par2).                 % and store 


%********************************************************************************%
%*                                                                     
%* predicate: extend_g1/2
%*
%* syntax: extend_g1(+Ai_ID,-A_Id)
%*
%* args: clauseIDs
%*
%* description: locates suitable V's that are already available in the kb
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*									
%********************************************************************************%

extend_g1(Ai_id, A_id) :-
	get_clause(Ai_id,_,_,Ai,_),
	length(Ai,Li),
        get_clause(Aj_id,_,_,Aj,g11),
        Ai_id \== Aj_id,
	length(Aj,Li),
        headed_lgg(Ai_id,Aj_id,A_id,g1),
        get_clause(A_id,_,_,A,_),
	length(A,L),
	(L >= Li ->                 % heuristic
	    delete_clause(Aj_id);
	    delete_clause(A_id),
	    fail).


%***********************************************************************
%*									
%* predicate:	apply_g1/2							
%*									
%* syntax:	apply_g1( + NewClauseId, - List_of_ResultIds )
%*									
%* args:								
%*									
%* description:	One might want to use apply_g1/2 if a new clause of background
%*              knowledge is added to the kb and the G1-operator is to be applied.
%*              If there already is a suitable "V", it will be extended and the
%*              lgg of two A's will be built.
%*
%*                        A
%*                Bi     / \     Bj
%*                  \   Ai  Aj  /
%*                   \ /     \ /
%*                    Ci      Cj
%*              
%*              If a clause A can be built as lgg of Ai and Aj (extend_g1/2),
%*              Ai and Aj will be deleted. 
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

apply_g1(Clause,_) :-           % use new clause as parent1
        g1_op(_,Clause,Par2),
        findall(GenPar2, (extend_g1(Par2,GenPar2)), Bag),
	(Bag == [] -> addtolist(Par2);
	delete_clause(Par2), addtolist(Bag)),
        fail, !.
apply_g1(Clause,_) :-           % use new clause as resolvent
        g1_op(Clause,_,Par2),
        findall(GenPar2, (extend_g1(Par2,GenPar2)), Bag),
	(Bag == [] -> addtolist(Par2);
	delete_clause(Par2), addtolist(Bag)),
        fail.
apply_g1(_,List) :- getlist(List).


%***********************************************************************
%*									
%* predicate: absorb/3           					
%*									
%* syntax: absorb(+ExID, ?Parent1ID, -NewID)				
%*									
%* args: ExID: ID of example clause				
%*	 Parent1ID: id of known parent clause                      
%*	 NewID: ID of absorption of example clause			
%*									
%* description: apply one absorption step on input clause ExID; 	
%*		if Parent1ID is given, it is tried to perform the       
%* 	        absorption step with it as known parent.                
%*		Otherwise absorption will be performed with the first   
%*		applicable background clause.              		
%*              It is made sure that no 2 literals of a parent 	        
%*		clause abs_match the same literal in the resolvent.	
%*
%* example:
%*									
%* peculiarities: no inverse subsitution yet               		
%*									
%* see also:								
%*									
%***********************************************************************


% parent given
absorb(ExID,PID,NewID):-
	nonvar(PID),
	clear_mngr,
	get_clause(ExID,_,_,Ex,_),
	skolemize(Ex,S,[SHead:p|SBody]),
        assert_body(SBody),
	!,
	abs_match1(PID,success,Proofs),
	abs_process_proofs(Proofs,PHead),
	abs_build_body(Body1),
	append(Body1,[PHead:n],Body),
	NewClauseS = [ SHead:p | Body ],
        deskolemize(NewClauseS,S, NewClause),
        store_clause(_,NewClause,abs,NewID).



% parent not given
absorb(ExID,PID,NewID):-
	var(PID),
	clear_mngr,
	get_clause(ExID,_,_,Ex,_),
	skolemize(Ex,S,[SHead:p|SBody]),
        assert_body(SBody),
	!,
	abs_match(ExID,success,Proofs),
	abs_process_proofs(Proofs,PHead),
	abs_build_body(Body1),
	append(Body1,[PHead:n],Body),
	NewClauseS = [ SHead:p | Body ],
        deskolemize(NewClauseS,S, NewClause),
        store_clause(_,NewClause,abs,NewID).


%***********************************************************************
%*									
%* predicate:	abs_match/3							
%*									
%* syntax: abs_match(+ExID,-Mark,-Proofs)
%*									
%* args: ExID: Id of the resolvent
%*       Mark in {success,fail}
%*       Proofs = [CL,...] where CL is a clause in list notation
%*									
%* description:	returns all (instantiated) clauses that can be embedded in the 
%*              skolemized example clause (stored in kb with head/3,body/3)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

abs_match( ExID, Mark,Proofs):-
	( findall(Proof, abs_match0(ExID,Proof), Proofs) 
                 -> Mark = success
         ;
          Proofs = [], Mark = fail
         ).
 
abs_match0( ExId, [Goal:p|Proof]):-
        get_clause(I,_,_,[Goal:p|Body],usr),
        I \== ExId,
        prove3(Body,Proof).


%***********************************************************************
%*									
%* predicate:	abs_match1/3							
%*									
%* syntax: abs_match1(+PID,-Mark,-Proofs)
%*									
%* args: PID: ID of a parent clause
%*									
%* description:	as abs_match, except for the fixed parent clause that
%*              is embedded in the resolvent
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

abs_match1(PID, Mark, Proofs):-
	( findall(Proof, abs_match1a(PID,Proof), Proofs) 
                 -> Mark = success
         ;
          Proofs = [], Mark = fail
         ).

abs_match1a( PID, [Goal:p|Proof]):-
        get_clause(PID,_,_,[Goal:p|Body],_),
        prove3(Body,Proof).
 

%***********************************************************************
%*									
%* predicate: identify/3           					
%*									
%* syntax: identify(+ExID, ?Parent1ID, -NewID)				
%*									
%* args: ExID: ID of example clause				
%*	 Parent1ID: id of known parent clause                      
%*	 NewID: ID of identification of example clause			
%*									
%* description: apply one identification step on input clause ExID; 	
%*		if Parent1ID is given, it is tried to perform the       
%* 	        identification step with it as known parent.            
%*		Otherwise identification will be performed with the     
%*		first  applicable background clause.           		
%*              It is made sure that no 2 literals of a parent 	        
%*		clause ident_match the same literal in the resolvent.
%*
%* example:	
%*									
%* peculiarities: no inverse subsitution yet               		
%*		  no backtraking           				
%*									
%* see also:								
%*									
%***********************************************************************

% parent given
identify(ExID,PID,NewID):-
	nonvar(PID),
	clear_mngr,
	get_clause(ExID,_,_,Ex,_),
	skolemize(Ex,S,SClause),
        assert_clause(SClause),
	!,
	ident_match1(PID,success,Proofs),
        ident_process_proofs(Proofs,PHead),
        ident_build_body(Body1),
	NewClauseS = [ PHead:p | Body1 ],
        deskolemize(NewClauseS,S, NewClause),
        store_clause(_,NewClause,idn,NewID).



% parent not given
identify(ExID,PID,NewID):-
	var(PID),
	get_clause(ExID,_,_,Ex,_),    
	clear_mngr,
	skolemize(Ex,S,SClause),
        assert_clause(SClause),
%	show_bodies,
	ident_match(ExID,success,Proofs),
	ident_process_proofs(Proofs,PHead),
	ident_build_body(Body1),
	NewClauseS = [ PHead:p | Body1 ],
        deskolemize(NewClauseS,S, NewClause),
        store_clause(_,NewClause,idn,NewID).



%***********************************************************************
%*									
%* predicate:	ident_match/3							
%*									
%* syntax: ident_match(+ExID,-Mark,-Proofs)
%*									
%* args: ExID: ID of the resolvent
%*       Mark in {success,fail}
%*       Proofs = [P1,..,Pm], where Pi=[Uncovered:Proof] and
%*       Uncovered = Lit/M (M in {new_head,new_body}), Proof = [[Lit,N],...]
%*       (N in {head,body}
%*									
%* description:	matches clauses in kb against skolemized resolvent (stored
%*              in kb with head/3,body/3), Uncovered is the resolution
%*              literal that might be positive (new_head) or negative
%*              (new_body)
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

ident_match( ExID, Mark,Proofs):-
	( findall(Proof, ident_match0(ExID,Proof), Proofs) 
                 -> Mark = success
         ;
          Proofs = [], Mark = fail
         ).
 

ident_match0( ExId, [Uncovered:Proof]):-
        get_clause(I,_,_,Clause,usr),
        I \== ExId,
        prove4(Clause,Uncovered,Proof).


%***********************************************************************
%*									
%* predicate:	ident_match1/3							
%*									
%* syntax: ident_match1(+PID,-Mark,-Proofs)
%*									
%* args: PID ...parentID, Mark,Proofs as for ident_match
%*									
%* description:	as ident_match, except for the parent clause to be matched
%*              against the resolvent is given
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

ident_match1(PID, Mark, Proofs):-
	( findall(Proof, ident_match1a(PID,Proof), Proofs) 
                 -> Mark = success
         ;
          Proofs = [], Mark = fail
         ).


ident_match1a( PID, [Uncovered:Proof]):-
        get_clause(PID,_,_,Clause,_),
        prove4(Clause,Uncovered,Proof).


%***********************************************************************
%*                                                                      
%* predicates: inv_derivate/2/3                                         
%*                                                                      
%* syntax: inv_derivate(+ExID,-NewID)                                   
%*         inv_derivate(+ExID,+PrefHead,-NewID)                                    
%*                                                                      
%* args: ExID : id of example                                           
%*       NewID: id of expanded example                                  
%*       PrefHead: a prolog literal  
%*                                   
%* description: Muggleton's inverse linear derivation                   
%*              But:                                                    
%*              while in intermediate stages several head literals      
%*              might appear simultanously, the result will always      
%*              be a Horn clause. As head literal we choose the         
%*              latest one derived in inv_derivate/2.                   
%*              inv_derivate/3 takes as additional argument             
%*              a literal, which is interpreted as a preferred           
%*              head. If it is possible, inv_derivate/3 results         
%*              in a Horn clause where the head matches this            
%*              literal.                                                
%*              The operator is restricted to finding clauses at most   
%*              100 inverse resolution steps away.                      
%*                                                                      
%* example:     inv_derivate(1, member(A,B), ID) 
%*
%* peculiarities:                       
%*                                                                      
%* see also:                                                            
%*									
%***********************************************************************

inv_derivate(ExID,NewID):-
	clear_mngr,
	( get_clause(ExID,_,_,CList,_)
          ;
          get_example(ExID,Ex,'+'), CList = [Ex:p]
        ),
	skolemize(CList,S,CListS),
	assert_clause(CListS),
	inv_derivate1(ExID,1),
	idev_build_clause(_,Clause1),
	deskolemize(Clause1,S,Clause),
	store_clause(_,Clause,invd,NewID),!.


inv_derivate(ExID,PrefHead,NewID):-
	clear_mngr,
	get_clause(ExID,_,_,CList,_),
	skolemize(CList,S,CListS),
	assert_clause(CListS),
	inv_derivate1(ExID,1),
	idev_build_clause(PrefHead,Clause1),
	deskolemize(Clause1,S,Clause),
	store_clause(_,Clause,invd,NewID),!.

inv_derivate1(ExID,I):-
	setof(U:P,ExID^Clause^idev_match0(ExID,Clause,U,P),Proofs),
	process_new_literals(Proofs,Flag),
	( nonvar(Flag), I<100
	  -> J is I + 1,inv_derivate1(ExID,J)
	  ; true
	).


%***********************************************************************
%*									
%* predicate:	 idev_match0/4						
%*									
%* syntax: idev_match0(+ExID,-Clause,-Uncovered,-Proof)
%*									
%* args: ExID: ID of the resolvent
%*       Clause: clause in list notation
%*       Uncovered: Lit/M, where M in {new_head,new_body}
%*       Proof: [[Lit,N],...] where N in {head,body}
%*									
%* description:	matches clause on skolemized resolvent (stored in kb
%*              with head/3, body/3), and returns the instantiation
%*              of clause and the resolution literal (Uncovered) 
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

idev_match0( ExID,Clause, Uncovered, Proof):-
	get_clause(ID,_,_,Clause,_),
	ExID \== ID,
	prove4(Clause,Uncovered,Proof).


%***********************************************************************
%*                                                                      
%* predicate: most_spec_v/3                                             
%*                                                                      
%* syntax: most_spec_v(+ExID, ?PID, -NewID)                             
%*                                                                      
%* args: ExID:  id of example (resolvent)                               
%*       PID:   id of known parent                                      
%*       NewID: id of new clause                                        
%*                                                                      
%* description:                                                         
%*       Apply one most-spec-v operation to example with parent PID;    
%*       If PID is not given, take the first applicable clause          
%*       of bg as parent.                                               
%*       The most specific v comprises the most specific absorption     
%*       and the most specific identification.                          
%*       Since we always want Horn clauses as a result, this operator   
%*       does not implement the most specific identification as         
%*       described by Muggleton: Instead of adding a second head        
%*       literal to the old clause, we replace the original head.       
%*       I.e. our most specific identification operator is destructive. 
%*       The most specific absorption remains nondestructive            
%*                                                                      
%* example:
%*
%* peculiarites:                                                                     
%*                                                                      
%* see also: inv_derivate/2 where multiple head literals are            
%*           allowed om intermediate stages.                            
%*									
%***********************************************************************

most_spec_v(ExID,PID,NewID):-
	( get_clause(ExID,_,_,CList,_)
        ; 
          get_example(ExID,Ex,'+'), CList = [Ex:p]
        ) ,
	clear_mngr,
	skolemize(CList,S,CListS),
	assert_clause(CListS),
	idev_match1(ExID,_,Uncovered,Proof,PID),
	% Uncovered \== [],
	process_new_literals([Uncovered:Proof],Flag),
	nonvar(Flag),
	idev_build_clause(_,Clause1),
	deskolemize(Clause1,S,Clause),
	store_clause(_,Clause,msv,NewID).



%***********************************************************************
%*									
%* predicate:	 idev_match1/5							
%*									
%* syntax: idev_match1(+ExID,-Clause,-Uncovered,-Proof,-ID)
%*									
%* args: ExID: ID of the resolvent, ID: ID of matched clause
%*       Clause: clause in list notation
%*       Uncovered: Lit/M, where M in {new_head,new_body}
%*       Proof: [[Lit,N],...] where N in {head,body}
%*									
%* description:	 is like idev_match0/4, but returns id of absorbed clause
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

idev_match1( ExID,Clause, Uncovered, Proof,ID):-
	get_clause(ID,_,_,Clause,_),
	ExID \== ID,
	prove4(Clause,Uncovered,Proof).


%***********************************************************************
%*									
%* predicate: saturate/2,saturate/3           					
%*									
%* syntax: saturate(+ExID, -NewID), saturate(+ExID,-NewID,+Bound)
%*									
%* args: ExID: ID of example clause				
%*	 NewID: ID of saturation of example clause			
%*									
%* description: apply elementary saturation w.r.t. background		
%*		clauses.                                                
%* 		It is bounded by at most 100 iterations, if bound is not given		
%*		When checking the preconditions for firing one          
%*		absorption step,                                    	
%*              it is made sure that no 2 literals of a parent 	        
%*		clause subsume the same literal in the resolvent.
%*
%* example:
%*
%* peculiarities:	
%*									
%* see also:								
%*									
%***********************************************************************

saturate(ExID,GenID):-
        saturate(ExID,GenID,100).

saturate(ExID,GenID,Bound):-
	saturate1(ExID,NewClause,Bound),         % Rouveirol's theorem proving alg.
	                                  
        % write(NewClause),nl,
	store_clause(_,NewClause,sat,GenID),
        !.      % no backtracking
    


%***********************************************************************
%*									
%* predicate:	saturate1/3							
%*									
%* syntax: saturate1(+ExID,-NewClause,+Bound)
%*									
%* args: ExID .. ID of example clause, NewClause .. Prolog clause in list notation, 
%*       Bound .. bound for interations
%*									
%* description:	saturates example clause w.r.t. background knowledge.
%*		It is bounded by at most Bound interations.
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

saturate1(ExID,NewClause,Bound):-
        clear_mngr,
        get_clause(ExID,H,_B,T,_),
        skolemize( T,S,[HS:p|U]),
        assert_body(U),
        saturate1a(HS,1,Bound,S,S1),
        bagof(A, M^I^body(A,I,M), NewBody1),
        sat_build_clause( H, NewBody1, Clause1),     % Clause in list notation
        deskolemize( Clause1,S1,NewClause).
        

%***********************************************************************
%*									
%* predicate:	saturate1a/5							
%*									
%* syntax: saturate1a(+HS,+Count,+Bound,+Subst,-Subst)
%*									
%* args: HS: skolemized head of the example clause,	
%*       Count,Bound: integers
%*       Subst: skolem subtitutions							
%*									
%* description:	while Count < Bound, all heads following from the saturated 
%*              clause so far (stored as body(Lit,_,_)) are asserted as
%*              additional body-literals (via body/3)
%*
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

saturate1a(HS,I,Bound,S1,S2):- 
        sat_match(HS,success,Proofs),
	!,
        skolemize(Proofs,S1,S3,Proofs1),
        assert_absorptions(Proofs1,Flag),
        ( nonvar(Flag), I < Bound  
             -> J is I +1, saturate1a(HS,J,Bound,S3,S2) 
         ; 
        S2 = S3
        ).


saturate1a(_,_,_,S,S):- !.


%***********************************************************************
%*									
%* predicate:	sat_match/3							
%*									
%* syntax: sat_match(+HS,-M,-Proofs)
%*									
%* args: HS: skolemized head of the example clause
%*       Proofs = [CL,...] where each CL is a clause in list notation
%*									
%* description:	finds all possible proofs for all possible absorptions		
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

sat_match(HS,Mark,Proofs):-
        ( setof(Proof,HS^sat_match0(HS,Proof), Proofs )
                  -> Mark = success
        ;
          Proofs = [],
          Mark = fail
        ).

sat_match0(HS,[Goal:p|ProofBody]):-
        get_clause(_I,_,_,[Goal:p|Body],_),
        prove3(Body,ProofBody),
        Goal \== HS.


%***********************************************************************
%*									
%* predicate: elem_saturate/3       					
%*									
%* syntax: elem_saturate( +ExID, ?PID, -NewID)				
%*									
%* args: ExID: id of resolvent						
%*	 PID : id of parent in bg					
%*	 NewID: id of new parent					
%*									
%* description:								
%*	 Add head of parent from bg to body of resolvent.		
%*	 The Operator is identical to Muggleton's 			
%*	 most-specific-absorption.					
%*       When checking the preconditions for firing one          
%*       absorption step,                                    	
%*       it is made sure that no 2 literals of a parent 	        
%*       clause subsume the same literal in the resolvent.	
%*									
%* example:
%*
%* peculiarities:
%*									
%* see also:								
%*								
%***********************************************************************

% parent given
elem_saturate(ExID,PID,NewID):-
       nonvar(PID),
       clear_mngr,
       get_clause(ExID,_,_,[H:p|B],_),
       skolemize(B,S,BS),
       assert_body(BS),
       get_clause(PID,_,_,[Goal:p|Body],_),
       prove3(Body,ProofBody),
       assert_absorptions([ [Goal:p|ProofBody]],Flag),
       nonvar(Flag),
       findall( L, body(L,_I,_M) ,NewBody),
       sat_build_clause(H,NewBody,Clause1),
       deskolemize(Clause1,S,NewClause),
       store_clause(_,NewClause,esat,NewID).


% parent not given
elem_saturate(ExID,PID,NewID):-
       var(PID),
       clear_mngr,
       get_clause(ExID,_,_,[H:p|B],_),
       skolemize(B,S,BS),
       assert_body(BS),
       sat_match1(ExID, [Goal:p|ProofBody],PID),
       assert_absorptions([ [Goal:p|ProofBody]],Flag),
       nonvar(Flag),
       findall( L, body(L,_I,_M) ,NewBody),
       sat_build_clause(H,NewBody,Clause1),
       deskolemize(Clause1,S,NewClause),
       store_clause(_,NewClause,esat,NewID).


%***********************************************************************
%*									
%* predicate:	 sat_match1/3							
%*									
%* syntax: sat_match1(+ExID,-Proof,-ID)
%*									
%* args: ExID,ID: clauseIDs, Proofs: clause in list notation
%*									
%* description:	 is like sat_match0/2, but returns id of absorbed clause
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

sat_match1(Ex,[Goal:p|ProofBody],I):- 
        get_clause(I,_,_,[Goal:p|Body],_),
        I \== Ex,
        prove3(Body,ProofBody).

