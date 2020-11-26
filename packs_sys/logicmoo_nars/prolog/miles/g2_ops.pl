% MODULE g2_ops EXPORTS
:- module( g2_ops,
	[ intra_construct1/5,         % ITOU-like
          intra_construct1/6,         % give name of new pred
          intra_construct1/7,         % give name &  bound for common generalizations
          intra_construct2/5,         % CIGOL-like
          intra_construct2/6,         % give name of new pred
          intra_construct2/7,         % give name &  bound for common generalizations
          g2_op/5, 
          apply_g2/5, 
          apply_g2/3,
          apply_g2/2
	]).

% IMPORTS
:- use_module(home(lgg),
              [lgti/6,lgg/5,buildlgg/4,
               gti/5,lgti/5]). %%%diese f"ur lgti/6 ersetzen (ohne Bound)
:- use_module(home(kb),
              [get_clause/5,store_clause/4,delete_clause/1,delete_all/1]).
:- use_module(home(var_utils),
              [relevant_vars2/6,relevant_vars3/6,
               skolemize/3,deskolemize/3,replace/4,inv_replace/4,
               exists_intersect/3,findargs/3,allarg/4,buildrelterms/6]).
:- use_module(home(div_utils),
              [effaceall/3,genterm_test/2]).
:- use_module(home(g1_ops),
              [g1_op/4]).
:- use_module(home(environment),
              [oracle/2,confirm/2,get_ci/2]).
:- use_module(home(evaluation),
              [complexity/2]).
:- use_module_if_exists(library(strings),
              [gensym/2]).
:- use_module_if_exists(library(basics),
              [member/2]).
:- use_module_if_exists(library(sets),
              [subtract/3]).
:- use_module_if_exists(library(not),
              [(once)/1]).


% METAPREDICATES
% none


%***********************************************************************
%*	
%* module: g2_ops.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92	
%*									
%* changed:								
%*
%* description:	Intra-Construction, G2					
%*                                                           
%* see also: 
%*           
%***********************************************************************


%***********************************************************************
%*									
%* predicate: intra_construct1/5/6/7					
%*									
%* syntax: intra_construct1(+C1,+C2,-A,-B1,-B2)	           		
%*	   intra_construct1(+C1,+C2,-A,-B1,-B2,PName)			
%*	   intra_construct1(+C1,+C2,-A,-B1,-B2,PName,Bound)			
%*									
%* args: C1,C2,A,B1,B2: references to clauses in kb			
%*       PName:  atom - name of invented predicate
%*       Bound: integer		
%*									
%* description:	intra-construction where C1,C2 are at bottom of W,	
%*		A at the center top, B1,B2 at outside top positions.	
%* 		S1(A) in C1, S2(A) in C2; the substitutions 		
%*		between Bi & Ci are empty.
%*              Uses an ITOU-like heursitics for determining relevant
%*              variables for the new predicate
%*              Our intra-construction will only work, if the two 	
%*		input clauses require the same number of arguments	
%*		for the newly invented predicate.        		
%*		This restriction is not part of the original          
%*		definition of intra-construction, but its at least	
%*		a very useful heuristics. If the restriction does not	
%*		fit your needs, change it in relevant_vars/3,    	
%*		module "var_utils.pl".
%*									
%* example:								
%*									
%* peculiarities: backtracking over lgti/6 until the same number	
%*		  of arguments is reached.                        	
%*									
%* see also:								
%*									
%***********************************************************************

intra_construct1(IDC1,IDC2,IDA,IDB1,IDB2):-
         gensym( new_pred, NewPred),
         intra_construct1(IDC1,IDC2,IDA,IDB1,IDB2,NewPred).


intra_construct1(IDC1,IDC2,IDA,IDB1,IDB2,NewPred):-
         intra_construct1(IDC1,IDC2,IDA,IDB1,IDB2,NewPred,10). 

intra_construct1(IDC1,IDC2,IDA,IDB1,IDB2,NewPred,Bound):-
         atom(NewPred),
         get_clause( IDC1,_,_,C1,_),
         get_clause( IDC2,_,_,C2,_),
         !,
         lgti(C1,C2,G,S1,S2,Bound),   % G is common generalization

      once((
         relevant_vars3(C1,C2,G,S1,S2,Vars),   %%%  ITOU  - like
         NewLit =.. [NewPred | Vars],
         % build clause in center top 
         append(G,[NewLit:n],A),
         
         
         % build clause at top left
         copy_term( (C1,G,S1,NewLit), (C11,G11,S11,NewLit11) ),  
         skolemize( (C11,G11,S11,NewLit11), Phi1, (C12,G12,S12,NewLit12) ),
         replace( G12,S12,G13,S12 ),
         replace( [NewLit12],S12,[NewLit13],S12),
         subtract( C12,G13,B1BodyS),      % 
         B1S = [ NewLit13:p | B1BodyS ],
         deskolemize(B1S,Phi1,B1),


         % build clause at top right
         copy_term( (C2,G,S2,NewLit), (C21,G21,S21,NewLit21) ), 
         skolemize( (C21,G21,S21,NewLit21), Phi2, (C22,G22,S22,NewLit22) ),
         replace( G22,S22,G23,S22 ),
         replace( [NewLit22],S22,[NewLit23],S22),
         subtract( C22,G23,B2BodyS),
         B2S = [ NewLit23:p | B2BodyS ],
         deskolemize(B2S,Phi2,B2),
         
         store_clause(_,A,ic,IDA),
         store_clause(_,B1,ic,IDB1),
         store_clause(_,B2,ic,IDB2)
          )).        



%***********************************************************************
%*									
%* predicate: intra_construct2/5/6/7					
%*									
%* syntax: intra_construct2(+C1,+C2,-A,-B1,-B2)	           		
%*	   intra_construct2(+C1,+C2,-A,-B1,-B2,PName)			
%*	   intra_construct2(+C1,+C2,-A,-B1,-B2,PName,Bound)			
%*									
%* args: C1,C2,A,B1,B2: references to clauses in kb			
%*       PName:  atom - name of invented predicate
%*       Bound: integer		
%*									
%* description:	intra-construction where C1,C2 are at bottom of W,	
%*		A at the center top, B1,B2 at outside top positions.	
%* 		S1(A) in C1, S2(A) in C2; the substitutions 		
%*		between Bi & Ci are empty.
%*              Uses a CIGOL-like heursitics for determining relevant
%*              variables for the new predicate
%*									
%* example:								
%*									
%* peculiarities: backtracking over lgti/6 until the same number	
%*		  of arguments is reached.                        	
%*									
%* see also:								
%*									
%***********************************************************************

intra_construct2(IDC1,IDC2,IDA,IDB1,IDB2):-
         gensym( new_pred, NewPred),
         intra_construct2(IDC1,IDC2,IDA,IDB1,IDB2,NewPred).


intra_construct2(IDC1,IDC2,IDA,IDB1,IDB2,NewPred):-
         intra_construct2(IDC1,IDC2,IDA,IDB1,IDB2,NewPred,10). 

intra_construct2(IDC1,IDC2,IDA,IDB1,IDB2,NewPred,Bound):-
         atom(NewPred),
         get_clause( IDC1,_,_,C1,_),
         get_clause( IDC2,_,_,C2,_),
         !,
         lgti(C1,C2,G,S1,S2,Bound),   % G is common generalization

      once(  (
         relevant_vars2(C1,C2,G,S1,S2,Vars),   %%%  CIGOL - like
         NewLit =.. [NewPred | Vars],
         % build clause in center top 
         append(G,[NewLit:n],A),
         
         
         % build clause at top left
         copy_term( (C1,G,S1,NewLit), (C11,G11,S11,NewLit11) ),  
         skolemize( (C11,G11,S11,NewLit11), Phi1, (C12,G12,S12,NewLit12) ),
         replace( G12,S12,G13,S12 ),
         replace( [NewLit12],S12,[NewLit13],S12),
         subtract( C12,G13,B1BodyS),      % 
         B1S = [ NewLit13:p | B1BodyS ],
         deskolemize(B1S,Phi1,B1),


         % build clause at top right
         copy_term( (C2,G,S2,NewLit), (C21,G21,S21,NewLit21) ), 
         skolemize( (C21,G21,S21,NewLit21), Phi2, (C22,G22,S22,NewLit22) ),
         replace( G22,S22,G23,S22 ),
         replace( [NewLit22],S22,[NewLit23],S22),
         subtract( C22,G23,B2BodyS),
         B2S = [ NewLit23:p | B2BodyS ],
         deskolemize(B2S,Phi2,B2),
         
         store_clause(_,A,ic,IDA),
         store_clause(_,B1,ic,IDB1),
         store_clause(_,B2,ic,IDB2)
          )).        


%********************************************************************************
%* 
%* predicate: g2_op/5                                                  
%*                                                                     
%* syntax: g2_op ( + C1_ID, + C2_ID, - A_ID, - B1_ID, - B2_ID)
%*                                                                   
%* args:   Ci_ID ... IDs of resolvent clauses C1 and C2 to be generalized
%*         A_ID ...  ID of common parent clause A
%*         Bi_ID ... IDs of corresponding parent clauses B1 and B2
%*       
%* description: Implementation of Ruediger Wirth's G2-operator for inverse 
%*              resolution corresponding to his 1989 PhD thesis.     
%*              We generalize the Ci using Plotkin's LGG, then build a new
%*              predicate as resolution literal, find the argument terms for the
%*              new predicate (in a heuristic manner) and finally build the Bi
%*              using our well-known G1-operator.
%*              The compression achieved is evaluated thru a simple, though quite
%*              sophisticated complexity heuristic (cf. module 'complexity').
%*              If the resulting clauses show some compression, the are passed 
%*              to the oracle for confirmation and the user gets a chance to 
%*              rename the new predicate.
%*              Clauses which become obsolete during the process will be deleted.
%* 
%* example:
%* 
%* peculiarities: The procedure 'inv_replace' might yield unsatisfying results,
%*                due to the possible ambiguity of inverse substitution.
%*
%* see also:
%*                                                                   
%********************************************************************************

g2_op(C1, C2, A, B1, B2) :-
	get_clause(C1,_,_,C1list,_),
	get_clause(C2,_,_,C2list,_),
	lgg(C1list,C2list,Clgg,S1,S2),                   % Clgg = A\{L}
	not_unary(Clgg),                                 % heuristic proc.
	buildrelterms(C1list,C2list,Clgg,S1,S2,Terms),   % Terms = List of Args for L
	buildreslit(Terms,L),                            % L = resolution literal
	buildparentA(Clgg,L,Alist),                      % Alist = parentclause A
	store_clause(_,Alist,g2,A),
	(g1_op(C1,A,B1,g2g1),                             % Bi = parentclauses
	 g1_op(C2,A,B2,g2g1) ->
	 ( compression_heuristic([A,B1,B2], [C1,C2]) ->
	    confirm([A,B1,B2], L),
	    delete_all([C1,C2]),
	    nl, write('Resolvent clauses deleted.');
	    nl, write('G2: No compression achieved.'), nl, fail
	 );
	 delete_clause(A) ).


%********************************************************************************
%*                                                                     
%* predicate: apply_g2/3  - tries to apply the G2-operator to a set of clauses Ci.
%*                          The output will be a kb reference of the common parent
%*                          clause A and a list of id's for parent clauses Bi.
%*
%*                             Bi     A     Bj
%*                               \   / \   /
%*                                \ /   \ /
%*                                Ci    Cj
%*             apply_g2/2  - ORACLE is asked to enter the Id's of resolvent clauses
%*                          Ci one by one. This continues until oracle says 'stop'.
%*                          Doubles and answers which are not a number are ignored.
%*                          Finally apply_g2/3 is called.
%*             apply_g2/5  - simply calls g2_op/5
%*                                                                   
%* syntax: apply_g2( + CC, - A, -BB), apply_g2( - A, -BB),
%*         apply_g2 ( + C1_ID, + C2_ID, - A_ID, - B1_ID, - B2_ID)
%*                                                                   
%* args:   CC ... Id-list of resolvent clauses Ci to be generalized
%*         A ...  Id of common parent clause A
%*         BB ... Id-list of corresponding parent clauses Bi
%*         C_ID, A_ID, B_ID .. as for g2_op/5
%* 
%* description:
%*
%* example:
%*
%* peculiarities:
%* 
%* see also:
%*
%********************************************************************************%

apply_g2(C1, C2, A, B1, B2) :-
	g2_op(C1, C2, A, B1, B2), !.


apply_g2(A, BB) :-
	get_ci([],CC),
	apply_g2(CC, A, BB), !.


apply_g2(CC, A, BB) :-
	sort(CC,CCsort),
	gensym(new_p,N),
	findall(Aij, ( member(Ci,CCsort), member(Cj,CCsort),
	               Ci < Cj, g2_op_A(Ci,Cj,N,Aij) ), AA),
        AA = [A1|An],
	buildlgg(An,A1,A,g2),
	delete_all(AA),
	findall(Bi, ( member(Ci,CC),
	              g1_op(Ci,A,Bi,g2g1) ), BB),
	length(CC,NoC),
	( length(BB,NoC) ->
	    ( compression_heuristic([A|BB], CC) ->
		confirm([A|BB], N),
		delete_all(CC),
		nl, write('Resolvent clauses deleted.');
		nl, write('G2: No compression achieved.'), nl, fail
	    );
	    delete_all(BB),
	    delete_clause(A),
	    fail
	).

	
g2_op_A(C1, C2, Name, A) :-
	get_clause(C1,_,_,C1list,_),
	get_clause(C2,_,_,C2list,_),
	lgg(C1list,C2list,Clgg,S1,S2),               % Clgg = A\{L}
	buildrelterms(C1list,C2list,Clgg,S1,S2,T),   % T = List of Args for L
	not_unary(Clgg),                             % heuristic proc.
	length(T,N),
	functor(L,Name,N),                           % Name = common for all L's
	setargs(N,T,L),                              % L = resolution literal
	buildparentA(Clgg,L,Alist),                  % parentclause A
	store_clause(_,Alist,g2,A).



%************************************************************************
%*
%* predicate: not_unary/1
%*
%* syntax: not_unary(+CL)
%*
%* args: CL .. clause in list representation
%*
%* description: fails, if CL is a unary clause or a unary goal 
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

not_unary([_:p]) :- nl, write('No compression achievable.'), !,fail.
not_unary([true:p,_]) :- nl, write('No compression achievable.'), !,fail.
not_unary(_).
	

%************************************************************************
%*
%* predicate: buildreslit/2
%*
%* syntax: buildreslit(+TermList,-Lit)
%*
%* args: TermList is the list of relevant argument terms, Lit is the resolution
%*       literal (with a new predicate symbol)
%*
%* description: constructs the resolution literal
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

buildreslit(T,L) :-
	length(T,N),
	gensym(new_p,F),
	functor(L,F,N),
	setargs(N,T,L).

setargs(0,[],_) :- !.
setargs(N,[Arg1|Rest],L) :-
	arg(N,L,Arg1),
	M is N-1,
	setargs(M,Rest,L).


%************************************************************************
%*
%* predicate: buildparentA/3
%*
%* syntax: buildparentA(+A_L,+Lit,-AL)
%*
%* args: A_L ... A\{Lit} the lgg of C1 and C2
%*       Lit ... the new predicate literal
%*       AL ... A\{Lit} + {Lit}
%*
%* description: adds the new predicate literal Lit either as head or
%*        as body literal to A_L
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

buildparentA([true:p|Rest],L,[L:p|Rest]) :- !.
buildparentA(List,L,Alist) :- append(List,[L:n],Alist), !.


%************************************************************************
%*
%* predicate:  compression_heuristic/2
%*
%* syntax: compression_heuristic(+NewIDs,+OldIDs)
%*
%* args: NewIDs, OldIDs ... clauseIDs
%*
%* description: succeeds if the size of the clauses NewIDs is smaller
%*        than that of the clauses OldIDs
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

compression_heuristic( New_cl, Old_cl) :-
	complexity(Old_cl, Cold),
	complexity(New_cl, Cnew),
	( Cnew < Cold ->
	    true;
	    delete_all(New_cl), fail
	).
