% MODULE lgg EXPORTS
:- module( lgg,
	   [ subsumes_list/2,
             gen_msg/3,      % Buntine's most specific generalization
	     gen_msg/4,      %     "  with given bound for saturation
	     rlgg/3,         % rllg                 
	     rlgg/4,         % rllg with given head literal of preference
	     covered_clause/2,      
	     covered_clauses/4,
             reduce_complete/1,
	     reduce_complete/2,
	     reduce_approx/2,
             buildlgg/4,
             lgg/3, lgg/4, lgg/5,                    
             headed_lgg/3, headed_lgg/4, headed_lgg/5,   
             nr_lgg/3, nr_lgg/4, nr_lgg/5,               
             hnr_lgg/3, hnr_lgg/4, hnr_lgg/5,            
             lgg_terms/3, lgg_terms/5,                   
             lgg_terms/7, 
             set_lgg/2 ,                    
             gti/3,
             gti/5,
             lgti/3,
             lgti/5,
             lgti/6      
           ]).

% IMPORTS
:- use_module(home(kb),
              [get_clause/5,delete_clause/1,store_clause/4,get_example/3]).
:- use_module(home(bu_basics),
              [assert_body_randomly/1,clear_mngr/0,subs_build_clause/1,
               reset_counts/0,assert_clause/1,assert_body/1,body/3,head/3,
               cover_assert_assumptions/1,assumption/3,msg_build_long_clause/1,
               msg_build_heads/1,msg_build_body/1]).
:- use_module(home(var_utils),
              [skolemize/3,skolemize/4,deskolemize/3,clean_subst/3]).
:- use_module(home(div_utils),
              [extract_body/2,convert_to_horn_clause/3,effaceall/3,subset_chk/2,
               maximum/2,identical_member/2,identical_make_unique/2]).
:- use_module(home(interpreter),
              [prove1/2,prove5/2]).
:- use_module(home(g1_ops),
              [saturate/2,saturate/3,inv_derivate1/2]).
:- use_module(home(filter),
              [truncate_unconnected/2]).
:- use_module(home(evaluation),
              [complexity/2]).
:- use_module_if_exists(library(sets),
              [list_to_set/2,subtract/3]).
:- use_module_if_exists(library(lists),
              [is_list/1,subseq0/2,nth1/4]).
:- use_module_if_exists(library(basics),
              [member/2]).
:- use_module_if_exists(library(random),
              [maybe/0]).
:- use_module_if_exists(library(not),
              [(once)/1]).
:- use_module_if_exists(library(occurs),
              [contains_var/2,free_of_var/2]).
:- use_module_if_exists(library(subsumes),
              [subsumes_chk/2,variant/2]).


% METAPREDICATES
% none


:- dynamic counter/1, shortsubst/2, iterate/1.



%***********************************************************************
%*	
%* module: lgg.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92	
%*									
%* changed:								
%*									
%* description: - clause reduction 
%*                reduce clause with no repect to background           
%*                knowledge (Plotkin,1970)                             
%*                reduce w.r.t. background knowledge (Buntine,1988)    
%*                Since reduction is in both cases NP-complete,           
%*                we provide 2 versions for each case:                    
%*                a correct, but higly inefficient solution               
%*                & an approximation.
%*              - clause_subsumption 
%*		- Buntine's most specific generalization under 
%*                generalized subsumption 
%*              - Plotkin's RLGG, with Muggleton's algorithm  
%*	        - Plotkins least general generalisation (lgg)
%*                under theta subsumption    
%*              - least general intersection lgti           
%*
%* see also: Buntine,88; Muggleton,90; Plotkin, 70;          
%*           module lgg.pl for msg 
%*                                                           
%***********************************************************************



%***********************************************************************
%*                                                                      
%* predicates: reduce_complete/2
%*                                                                      
%* syntax: reduce_complete(+Clause,-ReducedClause)                      
%*                                                                      
%* args: Clause:        input clause in list form                       
%*       ReducedClause: reduced, minimal clause equivalent to Clause    
%*                                                                      
%* description: do not consider bg, i.e. reduction wrt theta subsumption
%*                                                                      
%* example:                                                             
%*                                                                      
%* peculiarities:                                                       
%*                                                                      
%* see also:                                                            
%*									
%***********************************************************************


reduce_complete(ID):-
        get_clause(ID, _,_,C,_),
        reduce_complete(C,D),
        delete_clause(ID),
        store_clause(_,D,reduce,ID),!.


reduce_complete(Clause,Reduced):-
	clear_mngr,
	skolemize(Clause,S,SClause),
	assert_clause(SClause),
	reduce_complete1(Clause,S,SReduced0),
        list_to_set(SReduced0,SReduced),
	deskolemize(SReduced,S,Reduced),
        !.        % No backtracking allowed (solution is unique)


%***********************************************************************
%*									
%* predicate:	reduce_complete1/3							
%*									
%* syntax: reduce_complete1(+CL,+Subst,-ReducedCL)
%*									
%* args: CL,ReducedCL: clauses in list notation
%*       Subst: skolem substitution
%*									
%* description:	reduces CL by matching it on the skolemized head and
%*		body literals of the reduced clause in the kb
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

reduce_complete1(C,S,RC):-
	copy_term(C,ToShow),
	prove1(ToShow,SProof),
	(   reduce_complete_test(SProof) ->
            clear_mngr,
            list_to_set(SProof, SProof1), 
            assert_clause( SProof1),                 
            copy_term( S, S1),
            deskolemize( SProof1,S1,Proof),
            reduce_complete1(Proof,S,RC)
	;   reset_counts,
            fail   % backtrack, find another proof
	).
reduce_complete1(_,_S,RC):-
	!,    % there is no shorter proof, the minimal clause is found.
	subs_build_clause(RC).


reduce_complete_test(P):-
   findall(H:p,head(H,_,_),HL),
   setof(B:_,body(B,_,_),BL),
   append(HL,BL,L),
   list_to_set(L,L0),
   list_to_set(P,P0),
   subtract(L0,P0,D),!,
   D \== [].



%***********************************************************************
%*									
%* predicate:	reduce_approx/2							
%*									
%* syntax: reduce_approx(+Clause,-ReducedClause)                      
%*                                                                      
%* args: Clause:        input clause in list form                       
%*       ReducedClause: reduced, minimal clause equivalent to Clause    
%*                                                                      
%* description: as reduce_complete except that the number of single 
%*              reduction steps is bound 
%*
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

reduce_approx(Clause,Reduced):-
	clear_mngr,
	copy_term(Clause,Copy),
	skolemize(Copy,S,SClause),
	assert_clause(SClause),
	reduce_approx1(S,1,50),
        reduce_get_current_clause(SReduced0),
        list_to_set(SReduced0,SReduced),
	deskolemize(SReduced,S,Reduced).

reduce_approx1(S,Counter,Bound):-
	Counter =< Bound,
        copy_term( S, S1),
        reduce_get_current_clause(C),
        deskolemize(C,S1,ToShow),
        
        prove1(ToShow,SProof),
        
        clear_mngr,
        assert_body_randomly( SProof),
        J is Counter + 1,                 
        reduce_approx1(S,J,Bound).

	
reduce_approx1(_,_,_).




%***********************************************************************
%*									
%* predicate:	reduce_get_current_clause/1
%*									
%* syntax: reduce_get_current_clause(-CL)
%*									
%* args: CL: clause in list notation
%*									
%* description:	returns current skolemized clause that is stored via 
%*              head/3 and body/3 in the kb						
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

reduce_get_current_clause(CL):-
      findall(H:p,head( H,_,_),Heads),
      findall(L:n, body(L,_,_), Body),
      append(Heads,Body,CL).


%*********************************************************************
%*                                                              
%* predicate: covered_clause/2                                 
%*                                                              
%* syntax: covered_clause(+RULES, +ID)                          
%*                                                              
%* args:  RULES: list of kb references (integers)               
%*        ID:    id of clause to be tested                      
%*                                                              
%* description:  Test, if a clause is a specialization of RULES.
%*                                                              
%* example: let RULES refer to clauses                          
%*          member(A,[A|B]).                                    
%*          member(A,[B|C]):- member(A,C).                      
%*       then                                                   
%*          member(3,[1,2,3]):- member(3,[2,3])               
%*       is covered by RULES                                   
%*                                                            
%* see also:                                                  
%*									
%********************************************************************

covered_clause(RULES,ID):-
       nonvar(RULES),
       member(ID,RULES).


covered_clause(RULES,ID):-
       nonvar(RULES),
       retractall( assumption(_,_,_) ),
       get_clause(ID,_,_,Clause,_),
       skolemize(Clause,_,[H:p|Body]),
       cover_assert_assumptions(Body),
       prove5( H, RULES),     
       !.


%****************************************************************
%*                                                              
%* predicate: covered_clauses/4                                
%*                                                             
%* syntax: covered_clauses(+RULES,+ToTest,-Covered,-Uncovered) 
%*                                                             
%* args: RULES: list of kb references (integers)               
%*       ToTest: either label of kb entries (lgg, sat, ex ..)  
%*               or a list of kb references                    
%* description: test if RULES explain all example clauses      
%*              denoted by ToTest.                             
%*              Or: RULES |= clauses(ToTest)                   
%*                                                             
%* example:                                                    
%*                                                             
%* peculiarities:                                              
%*                                                             
%* see also:                                                   
%*									
%****************************************************************

covered_clauses(RULES,LABEL,Covered,Uncovered):-
        nonvar(RULES),
        atom(LABEL),
        findall( ID, get_clause( ID,_,_,_,LABEL), ToTest),
        covering( RULES, ToTest, [],Covered, [], Uncovered).

covered_clauses(RULES,ToTest,Covered,Uncovered):-
        nonvar(RULES),
        is_list(ToTest),
        covering( RULES, ToTest, [],Covered, [], Uncovered).


covering( RULES, [ID|ToTest], Covered1, Covered2, Uncovered1, Uncovered2):-
        retractall( assumption(_,_,_) ),
        get_clause( ID, _,_,Clause,_),     
        \+ ( member(ID, Covered1) ; member(ID, Uncovered1) ),
        skolemize( Clause, _, [H:p | Body ]),
        cover_assert_assumptions(Body), 
        !,
        ( member(ID,RULES)  -> Covered3 = [ ID | Covered1 ],
                               Uncovered3 = Uncovered1

        ;  
          prove5( H, RULES) -> Covered3 = [ ID | Covered1 ],
                               Uncovered3 = Uncovered1
        
        | otherwise         -> Covered3 = Covered1,
                               Uncovered3 = [ ID | Uncovered1 ]
        ),
        covering(RULES, ToTest, Covered3,Covered2,Uncovered3,Uncovered2).

covering( _,[],Covered,Covered,Uncovered,Uncovered):-!.


%***************************************************************
%*                                                              
%* predicates: gen_msg/3/4                                      
%*                                                              
%* syntax: gen_msg(+ID1,+ID2,-ID)                               
%*         gen_msg(+ID1,+ID2,-ID,+Bound)                        
%*                                                              
%* args: ID1,ID2,ID:integers, references to clauses in kb       
%*           Bound :integer, depth bound for saturation         
%*                                                              
%* description: Approximation of Buntine's most specific        
%*              generalization. We saturate the 2 input clauses 
%*              & build the lgg over them.                      
%*              Our procedure differs from Buntine's in that    
%*              saturation does not construct all generalizing  
%*              clauses under generalized subsumption: if some  
%*              head literal entailed by the body contains      
%*              unbound variables, saturation adds it to the    
%*              body as it is, whereas Buntine instead adds all 
%*              of its ground instances. 
%*                       
%* example:                                                     
%*                                                              
%* peculiarities: the resulting clause is not yet reduced       
%*                                                              
%* see also:                                                    
%*									
%***************************************************************

gen_msg(ID1,ID2,ID):-
       get_clause(ID1,H1,_,_,_),
       get_clause(ID2,H2,_,_,_),
       functor(H1,F,N),
       functor(H2,F,N),
       saturate(ID1,A),
       saturate(ID2,B),
       % lgg(A,B,ID),
       lgti(A,B,ID),                % changed !
       delete_clause(A),
       delete_clause(B).

gen_msg(ID1,ID2,ID,Bound):-
       get_clause(ID1,H1,_,_C1,_),
       get_clause(ID2,H2,_,C2,_),
       functor(H1,F,N),
       functor(H2,F,N),
       delete_clause(ID2),
       
       saturate(ID1,A,Bound),
       delete_clause(ID1),
       get_clause(A,_,_,C1sat,_),
       delete_clause(A),
       store_clause(_,C2,genmsg,ID2),
       
       saturate(ID2,B,Bound),
       store_clause(_,C1sat,sat,A),
       delete_clause(ID2),

       nr_lgg(A,B,ID),
       % lgti(A,B,ID).               % changed !
       delete_clause(A),
       delete_clause(B).


%***************************************************************
%*                                                              
%* predicate: rllg/3/4                                          
%*                                                              
%* syntax: rllg(+ID1,+ID2,-ID)                                  
%*         rllg(+ID1,+ID2,+PrefHead,-ID)                        
%*                                                              
%* args: ID1,ID2,ID: integers, references to clauses in kb      
%*       PrefHead:   prolog literal                             
%*                                                              
%* description: Plotkin's relative leat general generalization. 
%*              Implementation thru Muggleton's alg. :          
%*              Construct 2 inverse linear derivations,         
%*              then build lgg over them.                       
%*                                                              
%*              If PrefHead is given & it is possible to find   
%*              a rllg who's head matches PrefHead, rllg will   
%*              construct this clause.
%*                          
%* example:                                                     
%*                                                              
%* peculiarities: no reduction yet                              
%*                                                              
%* see also:                                                    
%*									
%***************************************************************

rlgg(ID1,ID2,ID):-
      rlgg(ID1,ID2,_,ID).

rlgg(ID1,ID2,PrefHead,ID):-
      ( get_clause(ID1,_,_,Clause1,_) 
      ; get_example(ID1,Ex1,'+'), Clause1 = [Ex1:p] ),
      clear_mngr,
      skolemize(Clause1,S1,C1),
      assert_clause(C1),
      inv_derivate1(ID1,1),
      msg_build_long_clause(D1),
      deskolemize(D1,S1,A1),

      ( get_clause(ID2,_,_,Clause2,_)  
      ; get_example(ID2,Ex2,'+'), Clause2 = [Ex2:p]  ),
      clear_mngr,
      skolemize(Clause2,S2,C2),
      assert_clause(C2),
      inv_derivate1(ID2,1),
      msg_build_long_clause(D2),
      deskolemize(D2,S2,A2),


      lgg_gen_clause(A1,A2,D,_,[],[],_,_),
      !,                             % thru backtracking different
      convert_to_horn_clause(PrefHead,D,E),% heads may be obtained
      truncate_unconnected(E,F),
      store_clause(_,F,rlgg,ID).


%***********************************************************************
%*									
%* predicate: subsumes_list/2
%*
%* syntax: subsumes_list(+General,+Specific)
%*       			
%* args:  clauses in list notation					
%*									
%* description:	checks for theta subsumption by list matching.		
%*		No proofs, no substitutions are returned.		
%* 		General will not be instantiated.	
%*
%* example:
%*
%* peculiarities:
%*
%* see also:		
%*									
%***********************************************************************

subsumes_list( Gen, Spec):-
      copy_term(Gen, Gen1),
      copy_term(Spec, Spec1),
      numbervars(Spec1,1,_),
      subsumes_list1(Gen1,Spec1).

subsumes_list1([],_).
subsumes_list1([L|Rest], Spec):-
      member(L,Spec),
      subsumes_list1(Rest,Spec).


%********************************************************************************
%*                                                                       
%* predicates: lgg_terms/3, lgg_terms/5, lgg_terms/7
%*                                                                       
%* syntax: lgg_terms( + Term1, + Term2, - GenTerm )                  
%*         lgg_terms( + Term1, + Term2, - GenTerm,                   
%*                    - Subst_Term1, - Subst_Term2 )
%*         lgg_terms( + Term1, + Term2, - GenTerm,                   
%*                    - Subst1, - Subst2,                         
%*                    + Init_Subst1, + Init_Subst2 )                                   
%*
%* args: Term1,Term2,GenTerm: prolog terms
%*       Subst_termi: [Var/Fi,..] substitution such that Termi = GenTerm Subst_termi
%*
%* description: Plotkins least general generalisation wrt theta-subsumption
%*              on terms
%*
%* example:
%*
%* peculiarities:
%*
%************************************************************************

lgg_terms(Term1, Term2, GTerm) :-
        lgg_terms(Term1, Term2, GTerm, _, _, [], []).

lgg_terms(Term1, Term2, GTerm, Subst1, Subst2) :-
        lgg_terms(Term1, Term2, GTerm, Subst1, Subst2, [], []).

lgg_terms(Term1, Term2, GTerm, S1, S2, Accu1, Accu2) :-
    % if same functor, same arity
        nonvar(Term1), nonvar(Term2),
        functor(Term1,F,N), functor(Term2,F,N)
    ->  
    % then: build new term by generalizing arguments
        functor(GTerm,F,N),
        generalize_arguments(Term1,Term2,N,GTerm,Accu1,Accu2,S1,S2)
    ;
    % else if same instantiation
        Term1 == Term2
    ->
    % then: no problem
        S1 = Accu1, S2 = Accu2, GTerm = Term1
    ;
    % else if substitution has already been applied
        substituted(Accu1, Accu2, Term1, Term2, Variable)
    ->
    % then: again, no problem
        S1 = Accu1, S2 = Accu2, GTerm = Variable
    ;
    % else if neither is true
    % then: substitute with a new variable
        S1 = [NewVar/Term1 | Accu1],
        S2 = [NewVar/Term2 | Accu2],
        GTerm = NewVar.

% generalize argument-terms (loop from position n to position 0)
generalize_arguments(_,_,0,_,Ac1,Ac2,Ac1,Ac2) :- !.
generalize_arguments(Term1,Term2,N,GTerm,Ac1,Ac2,S1,S2) :-
        arg(N, Term1, ArgN1),
        arg(N, Term2, ArgN2),
        arg(N, GTerm, ArgNG),
        lgg_terms(ArgN1,ArgN2,ArgNG,Ac1new,Ac2new,Ac1,Ac2),
        K is N-1,
        generalize_arguments(Term1,Term2,K,GTerm,Ac1new,Ac2new,S1,S2).

% test whether two terms at the same position have already a common generalised term
substituted([X/T1|_], [X/T2|_], Term1, Term2, X) :-
        T1 == Term1, T2 == Term2, !.
substituted([_|Accu1], [_|Accu2], Term1, Term2, X) :-
        substituted(Accu1, Accu2, Term1, Term2, X).


%************************************************************************
%*
%* predicate: set_lgg/2
%*
%* syntax: set_lgg(+List_of_Terms,-GenTerm)   
%*
%* args:
%*
%* description: lgg of a list of terms
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

set_lgg([L],L):- !.
set_lgg([X,Y|R],Lgg):-
   lgg_terms(X,Y,Lgg0),
   set_lgg([Lgg0|R],Lgg).



%************************************************************************
%*
%* predicate: headed_lgg/3, headed_lgg/4
%*
%* syntax: headed_lgg(+ID1,+ID2,-IDG)
%*         headed_lgg(+ID1,_ID2,-IDG,?Label)
%*
%* args: ID1,ID2,IDG: clauseIDs, Label: atom
%*
%* description: returns lgg of clauses ID1 ID2 in IDG, if both clauses
%*              have a compatible head literal (i.e. same pred, same
%*              arity). Fails else.
%*              Default label is hlgg
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

headed_lgg(Id1, Id2, IdG) :- headed_lgg(Id1, Id2, IdG, 'hlgg').

headed_lgg(Id1, Id2, IdG, Label) :-
        ((var(Label) -> Label = hlgg);true),
        hlgg1(Id1, Id2, HG, BGlist),
%       reduce_irene([HG:p|BGlist],[HGr:p|BGlistred]), 
        reduce_complete([HG:p|BGlist],[HGr:p|BGlistred]),  % finally reduce lgg-body
%       reduce_approx([HG:p|BGlist],[HGr:p|BGlistred]),% alternatively (more efficient)
        store_clause(_, [HGr:p|BGlistred], Label, IdG).

hlgg1(Id1, Id2, HG, BGlist) :-
        get_clause(Id1, H1, _, [_|B1list], _),      % if name + arity match for both heads
        get_clause(Id2, H2, _, [_|B2list], _),      % generalize heads first,
        functor(H1,F,N), functor(H2,F,N),           % then generalize bodies. 
        lgg_terms(H1,H2,HG,Subst1,Subst2),          
        lgg_body(B1list,B2list,BGlist,[],Subst1,Subst2,_,_).



%************************************************************************
%*
%* predicate: hnr_lgg/3, hnr_lgg/4
%*
%* syntax: hnr_lgg(+ID1,+ID2,-IDG)
%*         hnr_lgg(+ID1,_ID2,-IDG,?Label)
%*
%* args: ID1,ID2,IDG: clauseIDs, Label: atom
%*
%* description: same as headed_lgg, except that the resulting generalised 
%*              clause is NOT reduced. Default label is hnrlgg
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

hnr_lgg(Id1, Id2, IdG) :- hnr_lgg(Id1, Id2, IdG, 'hnrlgg').

hnr_lgg(Id1, Id2, IdG, Label) :-
        ((var(Label) -> Label = hnrlgg);true),
        hlgg1(Id1, Id2, HG, BGlist),
        store_clause(_, [HG:p|BGlist], Label, IdG).


%************************************************************************
%*
%* predicate: lgg/3, lgg/4
%*
%* syntax: lgg(+ID1,+ID2,-IDG)
%*         lgg(+ID1,_ID2,-IDG,?Label)
%*
%* args: ID1,ID2,IDG: clauseIDs, Label: atom
%*
%* description: returns lgg of clauses ID1 ID2 in IDG. If both clauses
%*              have no compatible head literal, the head literal of IDG
%*              is set to 'true'.
%*              Default label is lgg
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

lgg(Id1, Id2, IdG) :- lgg(Id1, Id2, IdG, 'lgg').

lgg(Id1, Id2, IdG, Label) :-
        ((var(Label) -> Label = lgg);true),
        lgg1(Id1, Id2, HG, BGlist),
%       reduce_irene([HG:p|BGlist],[HGr:p|BGlistred]), 
        reduce_complete([HG:p|BGlist],[HGr:p|BGlistred]), % finally reduce lgg-body
%       reduce_approx([HG:p|BGlist],[HGr:p|BGlistred]),  % alternatively (more efficient)
        store_clause(_, [HGr:p|BGlistred], Label, IdG).


lgg1(Id1, Id2, HG, BGlist) :-
        get_clause(Id1, H1, _, [_|B1list], _),      % if name + arity match for both heads
        get_clause(Id2, H2, _, [_|B2list], _),      % then generalize heads,
        (functor(H1,F,N), functor(H2,F,N) ->        % else use 'true' as head;
        lgg_terms(H1,H2,HG,Subst1,Subst2);          % then generalize bodies.       
        HG = true, Subst1 = [], Subst2 = []),             
        lgg_body(B1list,B2list,BGlist,[],Subst1,Subst2,_,_).



%************************************************************************
%*
%* predicate: nr_lgg/3, nr_lgg/4
%*
%* syntax: nr_lgg(+ID1,+ID2,-IDG)
%*         nr_lgg(+ID1,_ID2,-IDG,?Label)
%*
%* args: ID1,ID2,IDG: clauseIDs, Label: atom
%*
%* description: same as lgg, except that the resulting generalised 
%*              clause is NOT reduced. Default label is nrlgg
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

nr_lgg(Id1, Id2, IdG) :- nr_lgg(Id1, Id2, IdG, 'nrlgg').

nr_lgg(Id1, Id2, IdG, Label) :-
        ((var(Label) -> Label = nrlgg);true),
        lgg1(Id1, Id2, HG, BGlist),
        store_clause(_, [HG:p|BGlist], Label, IdG).


%***********************************************************************
%*									
%* predicate:	buildlgg/4							
%*									
%* syntax: build_lgg(+IDs,+IID,-GID,+Label)
%*									
%* args: IDs: list of clauseIDs,
%*       IID,GID: clauseIDs
%*       Label: atom								
%*									
%* description:	returns in GID the ID of the lgg of all clauses in IDs. IID is
%*              the ID of the intermediate result
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

buildlgg([C1],C2,Clgg,L) :- lgg(C1,C2,Clgg,L), !.

buildlgg([C1|CRest],Clgg_old,Clgg_new2,L) :-
        lgg(C1,Clgg_old,Clgg_new1,tmp), !,
        (buildlgg(CRest,Clgg_new1,Clgg_new2,L) ->
        delete_clause(Clgg_new1);
        delete_clause(Clgg_new1),fail).

buildlgg([],O,N,L) :-
	get_clause(O,_,_,Cl,_),
	store_clause(_,Cl,L,N), !.


%************************************************************************
%*
%* predicate: lgg/5, nr_lgg/5
%*
%* syntax: (nr_)lgg(+CL1,+CL2,-CLG,-Subst1,-Subst2)
%*
%* args: CL1,CL2,CLG: clauses in list notation
%*       Substi: Substitutions such that CLG Substi \subseteq CLi
%*
%* description: CLG is (non-reduced) lgg of clauses CL1 and CL2
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

lgg(Clause1list, Clause2list, [HGr|BGlistred], S1,S2):-    %Theta1, Theta2) :-
	nr_lgg(Clause1list, Clause2list, [HG|BGlist], SS1, SS2),
%       reduce_irene([HG|BGlist],[HGr|BGlistred]).
       reduce_complete([HG|BGlist],[HGr|BGlistred]),
        clean_subst([HGr|BGlistred],SS1,S1),
        clean_subst([HGr|BGlistred],SS2,S2).
%       reduce_approx([HG|BGlist],[HGr|BGlistred]), %%alternatively (more efficient)

	

nr_lgg([H1:p|B1list], [H2:p|B2list], [HG:p|BGlist], Theta1, Theta2) :-
        (functor(H1,F,N), functor(H2,F,N) ->
        lgg_terms(H1,H2,HG,Subst1,Subst2);
        HG = true, Subst1 = [], Subst2 = []),             
        lgg_body(B1list,B2list,BGlist,[],Subst1,Subst2,Theta1,Theta2).


%************************************************************************
%*
%* predicate: headed_lgg/5, hnr_lgg/5
%*
%* syntax: headed_lgg(+CL1,+CL2,-CLG,-Subst1,-Subst2)
%*         hnr_lgg(+CL1,+CL2,-CLG,-Subst1,-Subst2)
%*
%* args: CL1,CL2,CLG: clauses in list notation
%*       Substi: Substitutions such that CLG Substi \subseteq CLi
%*
%* description: CLG is (non-reduced) headed lgg of clauses CL1 and CL2
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

headed_lgg(Clause1list, Clause2list, [HGr|BGlistred], S1,S2):- %%%Theta1, Theta2) :-
	hnr_lgg(Clause1list, Clause2list, [HG|BGlist], SS1, SS2),
%       reduce_irene([HG|BGlist],[HGr|BGlistred]).
        reduce_complete([HG|BGlist],[HGr|BGlistred]),
        clean_subst([HGr|BGlistred],SS1,S1),
        clean_subst([HGr|BGlistred],SS2,S2).
%       reduce_approx([HG|BGlist],[HGr|BGlistred]), %%alternatively (more efficient)

hnr_lgg([H1:p|B1list], [H2:p|B2list], [HG:p|BGlist], Theta1, Theta2) :-
        functor(H1,F,N), functor(H2,F,N),
        lgg_terms(H1,H2,HG,Subst1,Subst2),
        lgg_body(B1list,B2list,BGlist,[],Subst1,Subst2,Theta1,Theta2).


%************************************************************************
%*
%* predicate: lgg_body/8
%*
%* syntax: lgg_body(+CL1,+CL2,+CLAccu,-CLAccu,+Subst1,+Subst2,-Subst1,-Subst2)
%*        
%* args: CL1,CL2,CLAccu: clauses bodiesin list representation 
%*       Subst..:Substitutions for the variables in CLAccu
%*
%* description: CLAccu is the non-reduced lgg of the clause bodies CL1 and
%*              CL2. The generalisation of two redundant literals L1:r, L2:r is
%*              marked as redundant LG:r.
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

lgg_body([],_,Accu,Accu,S1,S2,S1,S2) :- !.
lgg_body(_,[],Accu,Accu,S1,S2,S1,S2) :- !.
        
lgg_body([Elem1|R1], List2, Glist, AcGL, S1old, S2old, Theta1, Theta2) :-
        generalize_elem(List2, Elem1, GLelem1, S1old, S2old, S1new, S2new),
        append(AcGL, GLelem1, AcGLnew),
        lgg_body(R1, List2, Glist, AcGLnew, S1new, S2new, Theta1, Theta2).


%***************************************************************
%*                                                              
%* predicate: lgg_gen_clause/8  
%*                                                              
%* syntax: lgg_gen_clause(+CL1,+CL2,+CLAccu,-CLAccu,+Subst1,
%*                           +Subst2,-Subst1,-Subst2)
%*        
%* args: CL1,CL2,CLAccu: general clauses in list representation 
%*       Subst..:Substitutions for the variables in CLAccu
%*                                                              
%* description: CLAccu is the lgg of CL1 and CL2 (non-reduced)
%*              CL1, CL2 might be non Horn
%*                                                              
%* example:                                                     
%*                                                              
%* peculiarities: 
%*                                                              
%* see also:                                                    
%*									
%***************************************************************

lgg_gen_clause([],_,Accu,Accu,S1,S2,S1,S2) :- !.
lgg_gen_clause(_,[],Accu,Accu,S1,S2,S1,S2) :- !.
        
lgg_gen_clause([Elem1:Sign1|R1], List2, Glist, AcGL, S1old, S2old, Theta1, Theta2) :-
        generalize_elem(List2, Elem1:Sign1, GLelem1, S1old, S2old, S1new, S2new),
        append(AcGL, GLelem1, AcGLnew),
        lgg_gen_clause(R1, List2, Glist, AcGLnew, S1new, S2new, Theta1, Theta2).


%***************************************************************
%*                                                              
%* predicate: generalize_elem/7  
%*                                                              
%* syntax: generalize_elem(+CL,+Lit:Sign,-GCL,+Subst1,+Subst2,-Subst1,-Subst2)
%*                                                              
%* args: CL,GCL: clauses in list notation
%*       GCL: clause in list notation
%*       Lit:Sign: literal and sign (in {p,n,r})
%*       Subst..: Sustitutions for the variables in GCL 
%*                                                              
%* description: for each literal L in CL matching Lit:Sign (i.e. same functor, arity
%*       and compatible sign), the lgg of L and Lit is added to GCL, and the
%*       subtitutions are extended accordingly
%*                                                              
%* example:                                                     
%*                                                              
%* peculiarities: 
%*                                                              
%* see also:                                                    
%*									
%***************************************************************

generalize_elem([],_,[],S1,S2,S1,S2) :- !.

generalize_elem([Elem1:Sign1|R1],Literal:Sign2,[GTerm:Sign|RestGL],S10,S20,S12,S22) :-
        functor(Literal,F,N), functor(Elem1,F,N),
        lgg_terms(Literal,Elem1,GTerm,S11,S21,S10,S20),
        ( Sign1 == p, Sign2 == p   -> Sign = p
        ;
          Sign1 == r, Sign2 == r   -> Sign = r
        ;
          Sign1 == n, member(Sign2,[n,r]) -> Sign = n
        ;
          Sign1 == r, Sign2 == n   -> Sign = n
        ),
        generalize_elem(R1,Literal:Sign2,RestGL,S11,S21,S12,S22), !.

generalize_elem([_|R1],Literal,RestGL,S1old,S2old,S1new,S2new) :-
        generalize_elem(R1,Literal,RestGL,S1old,S2old,S1new,S2new).


%************************************************************************
%*                                                                     
%* predicate: gti/3, gti/5                                             
%*                                                                     
%* syntax: gti(+C1,+C2,-C), gti(+C1,+C2,-C,-S1,-S2)                    
%*                                                                     
%* args: C1,C2,C: clauses in list notation                             
%*       S1,S2:   substitutions  [ V1/Term1 , .... ]                 
%*                                                                     
%* description: generalization thru intersection
%*              least general intersection                       
%*                                                                     
%* example:                                                            
%*                                                                     
%* peculiarities: the resulting clause might be unconnected            
%*                ( see connectedness.pl )                             
%*                                                                     
%* see also:                                                           
%*                                                                     
%************************************************************************


gti(C1,C2,C):- gti(C1,C2,C,_S1,_S2).


gti(Id1,Id2,ID,S1,S2):- 
     integer(Id1), integer(Id2), !,
     get_clause(Id1,_,_,C1,_),
     get_clause(Id2,_,_,C2,_),
     gti(C1,C2,C,S1,S2),
     store_clause(_,C,gti,ID).


gti([H1:p|C1],[H2:p|C2],[H:p|C],S1,S2):- 
     lgg_terms(H1,H2,H,Phi1,Phi2),
     gti(C1,C2,C,Phi1,Phi2,S1,S2).




gti([], _, [], S1, S2, S1, S2).

gti( [L1:Sign1|Rest1], C2, [L:Sign|Rest], Phi1, Phi2, S1, S2):-
      nth1( _N, C2, L2:Sign2, Rest2),          % subtract L2 from C2
      lgg_terms( L1, L2, L,Theta1, Theta2, Phi1, Phi2),
      nonvar(L),
      ( Sign1 = r, Sign2 = r -> Sign = r
        | otherwise          -> Sign = n),
      gti( Rest1, Rest2, Rest, Theta1, Theta2, S1, S2).

gti( [_|Rest1], C2, Rest, Phi1, Phi2, S1, S2):-
      gti( Rest1, C2, Rest, Phi1, Phi2, S1, S2). 




%************************************************************************
%*                                                                     
%* predicate:   lgti/3, lgti/5, lgti/6                                 
%*                                                                     
%* syntax: lgti(+C1,+C2,-C,-S1,-S2),lgti(+C1,+C2,-C,-S1,-S2,+Bound)    
%*                                                                     
%* args: C1,C2,C,S1,S2: see gti above                                  
%*              Bound : pos. integer                                   
%*                                                                     
%* description: apply gti-operator Bound times ( default: Bound = 10 ).
%*              Return the longest resulting clause & substitutions.   
%*                                                                     
%* example:                                                            
%*                                                                     
%* peculiarities:                                                      
%*                                                                     
%* see also:                                                           
%*                                                                      
%***********************************************************************

lgti(ID1,ID2,ID):-
     integer(ID1), integer(ID2),!,
     lgti(ID1, ID2,C, _,_,10),
     ( store_clause(_,C,lgti,ID); delete_clause(ID),fail ).

lgti(C1,C2,C,S1,S2):- lgti(C1,C2,C,S1,S2,10).



lgti(Id1,Id2,C,S1,S2,Bound):- 
     integer(Id1), integer(Id2), !,
     get_clause(Id1,_,_,C1,_),
     get_clause(Id2,_,_,C2,_),
     lgti(C1,C2,C,S1,S2,Bound).


lgti(C1,C2,C,S1,S2,Bound):-
        integer(Bound), Bound > 0,
        init_chart(C1,C2,Bound),
        !,
        chart(_,_,_,_,_,_),
        findall(Comp, chart(Comp,_,_,_,_,_), Bag),
        once(maximum(Bag, MaxComp)),
        once(retract( chart(MaxComp, C, C1,C2,S1, S2) )).

:- dynamic(chart/6).


init_chart(C1,C2,Bound):-
        retractall( chart_count(_) ),
        assert(chart_count(1) ),
        retractall( chart(_,_,_,_,_,_) ),
        !,
        init_chart1(C1,C2,Bound).


init_chart1(C1,C2,Bound):-
        gti( C1,C2,C,S1,S2 ),
        once((complexity( C, Comp),              % no backtracking thru this
              assertz( chart(Comp, C, C1,C2,S1, S2) ),
              retract(chart_count(I) ),
              J is I + 1,
              assert( chart_count(J) )
        )), 
        J > Bound.

init_chart1(_,_,_):-!.  % if there are less than Bound solutions.


