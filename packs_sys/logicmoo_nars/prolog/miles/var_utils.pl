% MODULE var_utils EXPORTS
:- module( var_utils,
	[ inverse_substitute/2,  
	  inverse_substitute1/2,
	  skolems/2,

	  skolemize/3,
	  skolemize/4,
	  deskolemize/3,

          relevant_vars2/6,
          relevant_vars3/6,
          buildrelterms/6,

	  contains_vars/2,
          flagged_contains_vars/3,
          vars/2,

	  term_size/2,

	  replace/4,
          inv_replace/4,

	  
	  terms/3, 
	  terms/4, 
	  only_vars/2,
	  clause_terms/2,
	  only_vars1/2, 
	  typed_only_vars1/2,
          exists_intersect/3,
          clean_subst/3,
          findargs/3,
          allarg/4]).

% IMPORTS
:- use_module(home(div_utils),
                   [effaceall/3,genterm_test/2,identical_member/2,mysetof/3,
                    clist_to_prolog/2,best/2,subterm_at_position/4,part_of_clause/2]).
:- use_module(home(flatten),
                   [flatten_clause/2,unflatten_clause/2]).
:- use_module(home(filter),
                  [truncate_unconnected/2]).
:- use_module(home(lgg),
              [lgg_terms/7]). 
:- use_module_if_exists(library(basics),
              [member/2,nonmember/2,memberchk/2]).
:- use_module_if_exists(library(sets),
              [union/3,subtract/3,list_to_set/2,intersection/3]).
:- use_module_if_exists(library(strings),
              [gensym/2,string_append/3,substring/4]).
:- use_module_if_exists(library(occurs),
              [sub_term/2,contains_var/2]).
:- use_module_if_exists(library(subsumes),
              [variant/2]).


% METAPREDICATES
% none



%***********************************************************************
%*	
%* module: var_utils.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92
%*									
%* changed:								
%*									
%* description:	- utilities for variable and clause handling
%*              - determine relevant variables for predicate invention.	
%*              - inverse_substitution
%*              - skolemization is a special substitution 
%*              - replacement                                                        
%*
%* see also:	 
%*									
%***********************************************************************



%***********************************************************************
%*									
%* predicate:	vars/2
%*
%* syntax: vars(+Term,-Vars)	
%*				
%* args:        Term: any prolog term					
%*		Vars: list of variables in Term		
%*
%* description:
%*
%* example:
%*
%* peculiarities: setof changed to mysetof (IS)
%*
%*
%* see also:		
%*									
%***********************************************************************
 
vars(Term,Vars):-
	 mysetof(V, ( sub_term(V,Term), var(V) ), Vars).


%***********************************************************************
%*									
%* predicate: clause_terms/2								
%*									
%* syntax: clause_terms(+Clause,-Termlist)
%*									
%* args:								
%*									
%* description:	returns list of all non-ground terms in Clause
%*									
%* example: clause_terms((p(f(X),a,g(Y,b)):- r(f(X),Y)),
%*                       [f(X),X,g(Y,b),Y])
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

clause_terms((H:-B),L):- !,
   functor(H,_,N),
   terms(N,H,[],L0),
   clause_terms(B,L0,L).
clause_terms(H,L):-
   functor(H,_,N),
   terms(N,H,[],L).   

clause_terms((A,B),L,L2):- !,
   clause_terms(A,L,L1),
   clause_terms(B,L1,L2).
clause_terms(A,L,L1):-
   functor(A,_,N),
   terms(N,A,L,L1).


%***********************************************************************
%*									
%* predicate: terms/3,4
%*									
%* syntax: terms(+Term,+Accu,-Accu)
%*	   terms(+Count,+Term,+Accu,-Accu)
%*								
%* args: 								
%*									
%* description:	returns all non-ground subterms within Term
%*									
%* example:								
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

terms(V,L,L1):- 
   var(V),!,
   (   identical_member(V,L) ->
       L1 = L
   ;   L1 = [V|L]
   ).
terms(T,L,L1):-
   functor(T,_,N),
   (   (ground(T); identical_member(T,L)) ->
       L1 = L
   ;   terms(N,T,[T|L],L1)
   ).

terms(0,_,L,L):- !.
terms(N,T,L,L2):-
   N1 is N - 1,
   terms(N1,T,L,L1),
   arg(N,T,Tn),
   terms(Tn,L1,L2).


%***********************************************************************
%*									
%* predicate:	only_vars/2							
%*									
%* syntax:	only_vars(+Term,-Varlist)
%*									
%* args:	
%*		
%* description:	returns all variables within Term
%*									
%* example:	
%*									
%* peculiarities: 
%*									
%* see also:								
%*									
%***********************************************************************

only_vars(T,L):-
   terms(T,[],L1),
   only_vars1(L1,L).

only_vars1([],[]).
only_vars1([X|R],[X|R1]):-
   var(X),!,
   only_vars1(R,R1).
only_vars1([_|R],R1):-
   only_vars1(R,R1).


%***********************************************************************
%*									
%* predicate:	typed_only_vars1/2							
%*									
%* syntax:	typed_only_vars1(+TypedTermlist,-TypedVarlist)
%*									
%* args:	TypedTermlist: [T:typeT,...]
%*		Vars: [Var:typeVar
%*									
%* description:	extracts each term T that is a variable
%*		from a list TypedTermlist of terms with type definition
%*									
%* example:	only_vars2([X:type16,Y:type14,f(Z):type23],[X:type16,Y:type14])
%*									
%* peculiarities:	none						
%*									
%* see also:								
%*									
%***********************************************************************

typed_only_vars1([],[]).
typed_only_vars1([X:T|R],[X:T|R1]):-
   var(X),!,
   typed_only_vars1(R,R1).
typed_only_vars1([_|R],R1):-
   typed_only_vars1(R,R1).



%***********************************************************************
%*									
%* predicate: replace/3				         		
%*									
%* syntax: replace(+C1,+S1,-C2,-S2)   					
%*									
%* args: C1, C2: clauses in list notation				
%*	 S1, S2: replacements [ X / Term, .. ] 
%*       If all X's are variables, this is actually a      	
%*       substitution, but we also allow other terms.
%*									
%* description:	C2 is a copy of C1 with S1 applied.			
%*		S2 is a copy of S1.                         		
%*									
%* example: replace( [p(A,B):p], [A/a], [p(a,D):p], [C/a]).        	
%*									
%* peculiarities:							
%*									
%* see also:								
%*									
%***********************************************************************

replace(C,S1,D,S2):-
      copy_term( (C,S1) , (E,S2) ),
      do_replace( E, S2, D).


do_replace( [], _, []).

do_replace( [ L|More], S, [L1|More1]):-
     do_replace1( L, S, L1),!,
     do_replace(More, S, More1).

do_replace1( T1,S,T2 ):-
     ( member( X/T, S), X==T1  -> T2 = T
       ;
       var(T1) -> T1 = T2
       ;
       functor( T1, F, N) -> functor(T2,F,N), do_replace1(N,T1,T2,S)
     ).

do_replace1(0,_,_,_).

do_replace1(N,T1,T2,S):-
      arg(N,T1,A),
      arg(N,T2,B),
      do_replace1(A,S,B),
      M is N - 1,
      do_replace1(M,T1,T2,S).




%***********************************************************************
%*									
%* predicate: inv_replace/4						
%*									
%* syntax: inv_replace(+C1,+S1,-C2,-S2)				        
%*									
%* args: C1, C2: clauses in list notation				
%*	 S1, S2: replacements [ X / Term, .. ] 		                
%*									
%* description:	C2 is a copy of C1 with term of S1 replaced by ass. vars
%*		S2 is a copy of S1,s.t. vars(S2) in vars(C2).
%*									
%* example: inv_replace( [p(a,B):p], [A/a], [p(C,D):p], [C/a]).         
%*									
%* peculiarities: this is not the inverse operation for replacement :   
%*		  We don't distinguish the between places of terms.	
%*									
%* see also:								
%*									
%***********************************************************************


inv_replace(C,S1,D,S2):-
      copy_term( (C,S1) , (E,S2) ),
      do_inv_replace( E, S2, D).


do_inv_replace( [], _, []).

do_inv_replace( [ L|More], S, [L1|More1]):-
     do_inv_replace1( L, S, L1),
     do_inv_replace(More, S, More1).

do_inv_replace1( T1,S,T2 ):-
     ( var(T1) -> T1 = T2
       ;
       member( X / T, S), T == T1 -> T2 = X 
       ;
       functor( T1, F, N) -> functor(T2,F,N), do_inv_replace1(N,T1,T2,S)
     ).

do_inv_replace1(0,_,_,_).

do_inv_replace1(N,T1,T2,S):-
      arg(N,T1,A),
      arg(N,T2,B),
      do_inv_replace1(A,S,B),
      M is N - 1,
      do_inv_replace1(M,T1,T2,S).


do_inv_replace1(2,[T],T2,S) :-
	arg(2,T2,[]),
	do_inv_replace1(1,[T],T2,S).


%***********************************************************************
%*									
%* predicate:	term_size/2
%*									
%* syntax: term_size(+Term,-Size)
%*									
%* args:								
%*									
%* description:	 the folllowing code is a debugged copy from the Quintus library 
%*               'termdepth'
%*	         term_size(+Term, ?Size) calculates the Size of a Term, defined 
%*               to be the number of constant and function symbol occurrences in it.
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

term_size(Term, Size) :-
    (	var(Term) -> Size = 0
    ;/* nonvar(Term) */
	functor(Term, _F, Arity),            % Here was the bug
	term_size(Arity, Term, 1, Size)      %       " 
    ).


term_size(N, NonVar, SoFar, Size) :-
    (	N =:= 0 ->
	Size is SoFar
    ;   arg(N, NonVar, Arg),
	term_size(Arg, ArgSize),
	Accum is SoFar+ArgSize,
	M is N-1,
	term_size(M, NonVar, Accum, Size)
    ).


%***************************************************************************
%*									
%* predicate:	contains_vars/2
%*
%* syntax: contains_vars(+Term,+Terms)				
%*
%* args:        Term: any prolog term					
%*		Vars: list of prolog terms (also variables)		
%*									
%* description: succeeds if all terms in Terms occur in Term              	
%*	
%* example:
%*
%* peculiarities:
%*
%*
%* see also:								
%*									
%***********************************************************************

contains_vars([],_).
contains_vars([V|Vars],Term):- 
          contains_var(V,Term),
          contains_vars(Vars,Term).


%***************************************************************************
%*									
%* predicate:	flagged_contains_vars/3
%*
%* syntax: flagged_contains_vars(+Term,+Terms,-Flag)				
%*
%* args:        Term: any prolog term, Flag in {true,false}
%*		Vars: list of prolog terms (also variables)		
%*									
%* description: returns true  if all terms in Terms occur in Term, else false
%*	
%* example:
%*
%* peculiarities:
%*
%*
%* see also:								
%*									
%***********************************************************************

flagged_contains_vars(Vars,Term,true):- contains_vars(Vars,Term),!.
flagged_contains_vars(_Vars,_Term,false).


%***********************************************************************
%*                                                                      
%* predicate: inverse_substitute/2                                      
%*                                                                      
%* syntax: inverse_substitute(+ClauseIn,-ClauseOut)                     
%*                                                                      
%* args: clauses in list notation, i.e. [ head(A):p, b1(A):n, .. ]      
%*                                                                      
%* description: replace one term in ClauseIn by a variable.             
%*              Thru  backtracking all solutions can be obtained.       
%*              Implementation: flatten Clause,                         
%*                              truncate one literal,                   
%*                              truncate unconnected literals,          
%*                              unflatten Clause.                       
%*                                                                      
%* example:                                                             
%*                                                                      
%* peculiarities: Since identical terms are represented only once in    
%*                our flattening, we cannot tell between different      
%*                places the terms appear at.                           
%*                                                                      
%* see also: Muggleton,1988                                             
%*									
%***********************************************************************

inverse_substitute(Clause,Clause).       % empty inverse substitution

inverse_substitute(ClauseIn,ClauseOut):-
	flatten_clause(ClauseIn,C1),
	remove_type_literal(C1,C2),
	truncate_unconnected(C2,C3),
	unflatten_clause(C3,ClauseOut).


%***********************************************************************
%*									
%* predicate:	remove_type_literal/2							
%*									
%* syntax: remove_type_literal(+CL,-CL1)
%*									
%* args: CL,CL1: clause in list notation
%*									
%* description:	drop a "type literal" functor_p(...)
%*    the next rules allow to perform inverse substitutions on several terms,
%*    at the cost of an exploding search space.
%*    A better strategy is to trunacte literals one by one and 
%*    only to truncate the promising clauses further.
%* 									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

remove_type_literal([ L_p:n | More ] , More):-    % drop this literal
	functor( L_p, F, _),
	string_append( _ , '_p', F).


  
remove_type_literal([ L:S | More ] , [L:S|More1] ):- % drop another literal
	remove_type_literal(More,More1). 




%***********************************************************************
%*									
%* predicate: 	inverse_substitute1/2							
%*									
%* syntax: inverse_substitute1(+CL,-CL)
%*									
%* args: CL,CL1: clauses in list notation
%*									
%* description:	this is an alternative approach without flattening
%*              it replaces terms by variables.            
%*              (This does of course not work on flat clauses)          
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

inverse_substitute1(CLin,CLout):-
    copy_term(CLin,CLin1),
    clist_to_prolog(CLin1,Clause),
    mysetof(Sub:Pos,( subterm_at_position(Clause,Sub,[],Pos),
                      \+(part_of_clause(Sub,Clause)),
                      nonvar(Sub) % this rule disallows variable renaming to constrain
		                  % search space at the cost of incompleteness
                    ), Sublist),
    isub1_list(Sublist,Sublist1),
    best(Sublist1,T:Positions),
    do_inverse_sub1(T,Positions,_,Clause,Clause1),
    clist_to_prolog(CLout,Clause1),
    \+(variant(CLout,CLin)).


isub1_list([],[]).
isub1_list([T:Pos|R],[T:[Pos|Pos1]|R2]):-
   isub1_l(T,R,R1,Pos1),
   isub1_list(R1,R2).

isub1_l(_,[],[],[]).
isub1_l(T,[T1:Pos|R],R2,Pos1):-
   isub1_l(T,R,R1,Pos0),
   (   T == T1 ->
       R2 = R1,Pos1 = [Pos|Pos0]
   ;   R2 = [T1:Pos|R1], Pos1 = Pos0
   ).


%***********************************************************************
%*									
%* predicate: 	do_inverse_substitute1/5
%*									
%* syntax: do_inverse_substitute(+Term,+Positions,+Var,+Clause,-Clause)
%*									
%* args: Clause: Prolog clause
%*       Term: the term in Clause to be replaced with variable Var
%*       Positions: list of positions of Term within Clause where it might
%*                  be replaced. A position is a list of numbers
%*									
%* description:	replaces Term by a Var
%*              preference is to replace all occurrences of Term by Var;
%*              thru backtracking, clauses may be obtained where
%*              only some occurences of term are replaced.
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

do_inverse_sub1(_,[],_,Clause,Clause).
do_inverse_sub1(T,[P|R],Var,Clause,Clause2):-
   do_inverse_sub1(T,R,Var,Clause,Clause1),
   do_isub1(T,P,Var,Clause1,Clause2).
do_inverse_sub1(T,[_|R],Var,Clause,Clause1):-
   do_inverse_sub1(T,R,Var,Clause,Clause1).

do_isub1(_,[],Var,_,Var).
do_isub1(T,[P|R],V,C,C1):-
   functor(C,F,N),functor(C1,F,N),
   do_isub_copy(N,P,C,C1),
   arg(P,C1,C1p),arg(P,C,Cp),
   do_isub1(T,R,V,Cp,C1p).

do_isub_copy(0,_,_,_):- !.
do_isub_copy(N,P,C,C1):-
   N1 is N - 1,
   do_isub_copy(N1,P,C,C1),
   (   N == P ->
       true
   ;   arg(N,C,Cn),arg(N,C1,Cn)
   ).


%***********************************************************************
%*									
%* predicate: skolemize/3, deskolemize/3				
%*									
%* syntax: skolemize(+Term1,-Subst,-Term2) 				
%*									
%* args: Term1,Term2: arbiraty prolog terms				
%*	 Subst : substitution  [ V1/t1, V2/t2, .. ]			
%*		 where Vi are variables, ti skolem atoms		
%*									
%* description:	skolemization is a special substitution: all variables	
%*	        of Term1 are substituted by atoms. One keeps track of	
%* 		the substitution thru Subst.            		
%*
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:  Rouveirol,1991: ITOU.       				
%*									
%***********************************************************************

skolemize(T1,S,T2):-
        skolemize(T1,[],S,T2).

skolemize(T1,S,S,Sk_Atom):-
        var(T1),
        already_skolem_covered(T1,S,Sk_Atom),
        !.
skolemize(Var,S,[Var/Sk_Atom|S],Sk_Atom):-
        var(Var),
        !,
        gensym(sk_atom,Sk_Atom).
skolemize(T1,S1,S2,T2):-
        functor(T1,F,N),
        functor(T2,F,N),
        skolemize(N,T1,S1,S2,T2).
skolemize(0,_,S,S,_).
skolemize(N,T,S1,S2,U):-
        arg(N,T,Tn),
        arg(N,U,Un),
        skolemize(Tn,S1,S3,Un),
        M is N - 1,
        skolemize(M,T,S3,S2,U).


%***********************************************************************
%*									
%* predicate:	already_skolem_covered/3
%*									
%* syntax: already_skolem_covered(+Var,+Subst,-Skolem_atom)
%*									
%* args:								
%*									
%* description:	if Var has already been skolemized, i.e. Var:Skolem_atom in Subst,
%*               the corresponding Skolem_atom is returned 
%*									
%* example:								
%*									
%* peculiarities:	alter Name: already_covered
%*									
%* see also:								
%*									
%***********************************************************************

already_skolem_covered( Var, [ Var1/Sk_Atom | _ ], Sk_Atom):- 
	Var == Var1,!.

already_skolem_covered( Var, [ _| S ], Sk_Atom):- 
	already_skolem_covered( Var,S, Sk_Atom). 


%***********************************************************************
%*									
%* predicate: deskolemize/3				
%*									
%* syntax:  deskolemize(+Term1,+Subst,-Term2) 				
%*                                                                      
%*									
%* args: Term1,Term2: arbiraty prolog terms				
%*	 Subst : substitution  [ V1/t1, V2/t2, .. ]			
%*		 where Vi are variables, ti skolem atoms		
%* description:	Deskolemization reverses skolemization, if the		
%*		skolem substitution is given as input.	                
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:	
%*									
%***********************************************************************

deskolemize(Sk_Atom,S,Var):- 
	atom(Sk_Atom),
	skolem_covered(Sk_Atom,S,Var),
	!.
deskolemize(Atom,_,Atom):-
	atom(Atom),
	!.
deskolemize(Var,_S,Var):-
	var(Var),
	!.
deskolemize(T1,S,T2):-
	functor(T1,F,N),
	functor(T2,F,N),
	deskolemize(N,T1,S,T2).
deskolemize(0,_,_,_).
deskolemize(N,T,S,U):-
	arg(N,T,Tn),
	arg(N,U,Un),
	deskolemize(Tn,S,Un),
	M is N - 1,
	deskolemize(M,T,S,U).


%***********************************************************************
%*									
%* predicate: skolem_covered/3						
%*									
%* syntax: skolem_covered(+Skolem_atom,+Subst,-Var)
%*									
%* args:								
%*									
%* description:	returns the variable that has been skolemized with Skolem_atom,
%*		i.e. Var/Skolem_atom in Subst
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:	alter Name: covered							
%*									
%***********************************************************************

skolem_covered(Sk_Atom, [ Var/Sk_Atom | _ ], Var):- !.
skolem_covered(Sk_Atom, [ _| S ], Var):-
	skolem_covered(Sk_Atom,S,Var).


%***********************************************************************
%*									
%* predicate: skolems/2						
%*									
%* syntax: skolems(+Term,-Skolems)
%*									
%* args: Term: skolemized term, Skolems: all skolem atoms in Term
%*									
%* description:   returns skolem atoms occuring in Term
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%* 
%***********************************************************************              

skolems(Term,Skolems):-
        setof( Skolem, Len^( sub_term(Skolem,Term),atom(Skolem),
                             substring(Skolem,sk_atom,0,Len)
                         ), 
                Skolems),!.
skolems(_,[]).


%***********************************************************************
%*									
%* predicate: relevant_vars2/6             				
%*									
%* syntax: relevant_vars2(+C1,+C2,+Gen,+S1,+S2,-RelVars)		
%*									
%* args: C1,C2,Gen: clauses in list notation. C1,C2 at bottom of W.     
%*	 S1,S2: substitutions [V1=T1, .. ].				
%*	 RelVars: list of vars [ V1, V2, .. ]				
%*									
%* description:	determine relevant vars with CIGOL heuristics.	
%*		A variable V in Gen is relevant if                   	
%* 		one of the terms T1, T2 it is substituted by in S1, S2	
%*		contains a variable that also appears elsewhere		
%*		in S1 or S2.       					
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

relevant_vars2(_,_,C,S1,S2,RelVars):-
        vars(C,AllVars),
        relevant_vars2(AllVars,S1,S2,RelVars).


relevant_vars2([],_,_,[]).

relevant_vars2([V|MoreVars],S1,S2,[V|RelVars]):-
        ( member( W/T1, S1), V == W, member( X/T1a, S1), V \== X, 
          sub_term(Subterm1,T1), var(Subterm1), contains_var(Subterm1,T1a)
        ;
          member( W/T2, S2), V == W, member( Y/T2a, S2), V \== Y, 
          sub_term(Subterm2,T2), var(Subterm2), contains_var(Subterm2,T2a)
        ),!,
        relevant_vars2(MoreVars, S1,S2,RelVars).

relevant_vars2([_V|MoreVars],S1,S2,RelVars):-
        relevant_vars2(MoreVars, S1,S2,RelVars).



%***********************************************************************
%*									
%* predicate: relevant_vars3/6				           	
%*									
%* syntax: relevant_vars3(+C1,+C2,+Gen,+S1,+S2,-RelVars)		
%*									
%* args: C1,C2,Gen: clauses in list notation			        
%*	     S1,S2: substititions					
%*	  RelVars : set of relevant vars			        
%*									
%* description:	Gen is a common generaliztion of C1,C2 ,                
%*		s.t. S1(Gen) is a subsetof C1, and analogously for C2.  
%* 									
%*		A variable is relevant if                  		
%*		it appears in both Gen and ( C1 - Gen )			
%*				or Gen and ( C2 - Gen ).        	
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also: IRES,ITOU								
%* 
%***********************************************************************

relevant_vars3(C1,C2,Gen,S1,S2,Vars):-
      skolemize( (C1,C2,Gen,S1,S2), Phi, (D1,D2,Gen1,R1,R2) ),
      relevant_vars3a(D1,Gen1,R1,Vars1),
      relevant_vars3a(D2,Gen1,R2,Vars2),
      length(Vars1,Len1),
      length(Vars2,Len2),
      Len1 == Len2,
      union(Vars1,Vars2,Vars0),
      deskolemize(Vars0,Phi,Vars).

relevant_vars3a(Spec,Gen,S,Skolems):-     %%changed Irene
      replace(Gen,S,Gen1,S),
      subtract( Spec,Gen1, Rest),
      inv_replace(Rest,S,Rest1,S),
      skolems(Rest1, Skolems1),
      skolems(Gen, Skolems2),
      intersection(Skolems1,Skolems2,Skolems).



%***********************************************************************
%*									
%* predicate: findargs/3 
%*									
%* syntax: findargs(+CL,+Accu,-Accu)
%*									
%* args: CL: clause in list notation, Accu: arguments of the literals in CL
%*									
%* description:	find all arguments of the literals of a given clause
%*									
%* example:								
%*									
%* peculiarities:	
%*									
%* see also:								
%* 
%***********************************************************************

findargs([],Result,Result) :- !.
findargs([Lit1:_|Rest],Accu,Result) :-
	functor(Lit1,_,N),
	allarg(N,Lit1,[],Args),
	union(Accu,Args,Newaccu),      % set operator
	findargs(Rest,Newaccu,Result).

allarg(0,_,Accu,Accu) :- !.
allarg(N,Lit,Args,Result) :-
	arg(N,Lit,Arg1),
	M is N - 1,
	nonmember(Arg1,Args) ->
	allarg(M,Lit,[Arg1|Args],Result);
	allarg(M,Lit,Args,Result).


%************************************************************************
%*
%* predicate: buildrelterms/6
%*
%* syntax: buildrelterms(+CL1,+CL2,+Clgg,+Subst1,+Subst2,-TermList) 
%*
%* args: CL1, CL2, Clgg .. clauses in list representation
%*       Subst1,Subst2 ... substitutions such that Clgg Subst1 = CL1
%*                         and Clgg Subst2 = CL2
%*       TermList ... list of relevant terms for the new predicate
%*
%* description: determines the relevant terms for the new predicate
%*       as described in R. Wirth's 1989 PhD thesis
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

buildrelterms(SpecC1,SpecC2,Gen,S1,S2,Terms) :-
	skolemize((Gen,SpecC1,SpecC2,S1,S2),SS,(Gen1,Spec1,Spec2,SS1,SS2)),
	findterms(Gen1,Spec1,SS1,Terms1),      % Terms1 = RArgs1 != {}
	findterms(Gen1,Spec2,SS2,Terms2),      % Terms2 = RArgs2 != {}
%        union(Terms1,Terms2,TermsS),
%        deskolemize(TermsS,SS,Terms).          % changed Irene
	deskolemize((Terms1,Terms2),SS,(T1,T2)),
	general_terms(T1,T2,Terms,S1,S2).


findterms(Gen,Spec,SS1,RArgsG) :-
	replace(Gen,SS1,Gen2,_),
	subtract(Spec,Gen2,RestSpec),         % RestSpec = Ci^r ( Spec - Gen )
	subtract(Spec,RestSpec,SpecG),        % SpecG = Ci^g 
	findargs(SpecG,[],ArgsG),
	findargs(RestSpec,[],ArgsR),
	exists_intersect(ArgsG,ArgsR,RArgsG). % RArgs = relevant argument terms (not [])
%        inv_replace(RArgsG0,SS1,RArgsG,SS1).   % changed (Irene)
	
%************************************************************************
%*
%* predicate: general_terms/5
%*
%* syntax: general_terms(+T1,+T2,-TG,+Subst1,+Subst2)
%*
%* args: T1, T2 .. relevant terms in CL1, CL2 (cf. above)
%*       Subst1, Subst2 .. substitutions (cf. above)
%*       TG .. relevant terms in Clgg
%*
%* description: determines the relevant terms in Clgg that 
%*              correspond to the relevant terms in CL1 and CL2
%*              here, inv_replace is used!!
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

%* general_terms (like a shotgun wedding between lgg for terms and inverse replacement...)
general_terms([],[],[],_,_) :- !.

general_terms([T1|R1],[],[T|R3],S1,_) :-
	( genterm_test(T/T1,S1) -> true;
	    inv_replace(T1,S1,T,_)),
	!, general_terms(R1,[],R3,S1,_).

general_terms([],[T2|R2],[T|R3],_,S2) :-
	(genterm_test(T/T2,S2) -> true;
	    inv_replace(T2,S2,T,_)),
	!, general_terms([],R2,R3,_,S2).

general_terms([T1|R1],L2,[T|R3],S1,S2) :-
	gen_term(T1,L2,L2Rest,T,S1,S2), !,
	general_terms(R1,L2Rest,R3,S1,S2).

gen_term(T1,L2,L2new,T,S1,S2) :-
	nonvar(T1),
	functor(T1,F,N),
	effaceall(T2,L2,L2new),
	functor(T2,F,N),
	lgg_terms(T1,T2,T,_,_,S1,S2).

gen_term(T1,L2,L2new,X,S1,S2) :-
	effaceall(T2,L2,L2new),
	genterm_test(X/T1,S1),
	genterm_test(Y/T2,S2),
	X == Y.

gen_term(T1,L2,L2,T,S1,_) :-
	genterm_test(T/T1,S1) -> true;
	inv_replace(T1,S1,T,_).


%************************************************************************
%*
%* predicate: exists_intersect/3
%*
%* syntax: exists_intersect(+L1,+L2,-L)
%*
%* args: L1,L2,L: lists
%*
%* description: if nonempty intersection exists, succeeds and returns
%*              intersection, fails else
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

exists_intersect(X,Y,Z) :- exi(X,Y,Z,_),!.
exi([],_,[],Flag) :- Flag == yes.
exi([],_,[],_) :- !,fail.
exi([X|R],Y,[X|Z],yes) :- 
	memberchk(X,Y),!,
	exi(R,Y,Z,yes).
exi([_|R],Y,Z,Flag) :- 
	exi(R,Y,Z,Flag).



%************************************************************************
%*
%* predicate: clean_subst/3
%*
%* syntax: clean_subst(+CL,+Subst,-Subst)
%*
%* args: CL: clause in list notation, Subst: a substitution [X/Term,...]
%*
%* description: removes all X/T from Subst such that X does not occur in CL
%*
%* example:
%*
%* peculiarities:
%*
%* see also:
%*
%************************************************************************

clean_subst(_,[],[]).
clean_subst(CL,[X/T|R],R2):-
   clean_subst(CL,R,R1),
   (   contains_var(X,CL) ->
       R2 = [X/T|R1]
   ;   R2 = R1
   ).