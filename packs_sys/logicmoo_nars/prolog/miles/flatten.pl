% MODULE flatten EXPORTS
:- module( flatten,
	[ flatten_term/7,
          flatten_term/2,
	  flatten_literal/6,
          flatten_literal/2,
	  flatten_clause/6,
          unflatten_clause/2,
          flatten_clause/2,    
	  unflatten_clause/3 ]).

% IMPORTS
:- use_module(home(div_utils),
              [clist_to_prolog/2,
               list_to_struct/2]).
:- use_module_if_exists(library(basics), 
              [member/2]).
:- use_module_if_exists(library(strings), 
              [concat_atom/3, 
               midstring/6, 
               substring/5]).
:- use_module_if_exists(library(occurs), 
              [sub_term/2, 
               contains_var/2]).

% METAPREDICATES
% none


%***********************************************************************
%*	
%* module: flatten.pl        					
%*									
%* author: B.Jung, M.Mueller, I.Stahl, B.Tausend              date:12/92	
%*									
%* changed:								
%*									
%* description: Rouveirol's representation change to  function free
%*		Horn clauses.						
%* 		Shared variables are deteced.				
%*		Following the later versions of flattening('90,'91)	
%*		identical terms are only represented once thru a  	
%*		new body literal. The older version (1989) introduced	
%*		for each occurence of a term a unique new body literal. 
%*		( the newer approach might not always be more adequate)	
%*									
%* peculiarities: In the process of flattening all literals that are	
%*		  introduced for functions end with the suffix "_p".	
%*		  In return, when unflattening a clause it is assumed   
%*		  that every predicate symbol ending in "_p" stems from 
%*		  a function. This assumption is made because the names 
%*		  for functions and predicates need to be distinct.	
%*									
%*    DON'T FLATTEN ANY CLAUSE CONTAINING LITERALS ENDING IN "_p" !!! 
%* 
%* see also:								
%*									
%***********************************************************************


%***********************************************************************
%*									
%* predicate:	flatten_term/7						
%*									
%* syntax:	flatten_term(+Term, +NewVar , +OldSubstituion,          
%*                           -NewSubstitution,+OldBackground,           
%*                           -NewBackground, -Literals)			
%*									
%* args:	Term:	term to be replace by NewVar, e.g. [a,b]        
%*		NewVar: new variable                                    
%* 		OldSubstitution: list of substitutions that have already
%*			been performed while flattening a clause.       
%*			This way shared variables / terms are detected.	
%*			e.g. [], [ X/a , Y/[b] ]			
%* 		NewSubstitution: = OldSubstitution + [ NewVar/Term ]	
%*		OldBackground: old list of predicate definitions 	
%*		NewBackground: new ...                           	
%*			motivation: e.g. let term be "red".     	
%* 			the resulting literal is " red(X) " which is 	
%*			true iff X=red. Therefore	                
%*			NewBackground = OldBackground + [ red(red) ]	
%*		Literals: list of literals to replace function		
%*									
%* description:		
%*
%* example:
%*								
%* peculiarities: can't flatten integers	
%* 
%* see also:			
%*									
%***********************************************************************

flatten_term(Lin,Lout):-
        flatten_term(Lin,_,[],_,[],_,Lout).

% flatten_term(+,-,+,-,+,-,-)

% known terms              % change: represent only vars once
flatten_term(Term,Var,S,S,Bg,Bg,[]):-
        % var(Term),               %        new !!!
	member( (Var/Term1),S ),
	Term == Term1,!.

% Variables
%flatten_term( X, V, S,[(V/X)|S],[]):- var(X),!.
flatten_term( X, X, S, S,Bg,Bg,[]):- var(X),!.

% empty list
flatten_term([],V,S, [(V/[])|S] , Bg, [ nil_p([]) | Bg], [ nil_p(V) ]):-!.

% other atoms
flatten_term( A, V, S,[V/A|S],Bg,[ B|Bg],[L]):-
	atom(A),!,
	concat_atom([A,p],'_',Functor),
	L =.. [Functor,V],
	B =.. [Functor,A].

% integers
flatten_term( Int,V,S,[V/Int|S],Bg,[B|Bg],[L]):-
        integer(Int),!,
        map_function_to_pred(Int,PredName),
        L =.. [PredName,V],
        B =.. [PredName,Int].
        

% list
flatten_term([A|B],V,S,Snew,Bg, [ cons_p(A,B,[A|B]) | Bg2],Literals):-
	!,
	flatten_term(A,V1,S,S1,Bg,Bg1,Literals1),
	flatten_term(B,V2,S1,Snew1,Bg1,Bg2,Literals2),
	Snew = [ (V/[A|B]) | Snew1],
	append(Literals1,Literals2,Literals3),
	Literals = [ cons_p(V1,V2,V) | Literals3 ].


% other functions
flatten_term( Function, V, S,Snew,Bg, [ BgPredicate|Bg1 ],Literals):-
	Function =.. [ Functor|Args ],
	flatten_args(Args,Vs,S,Snew1,Bg,Bg1,Literals1),
	Snew = [ V/Function | Snew1],
	append(Vs,[V],NewArgs),
	concat_atom([Functor,p],'_',NewFunctor),
	Predicate =.. [ NewFunctor|NewArgs],    % build new predicate of arity n+1
	append( Args,[Function],BgArgs),
	BgPredicate =.. [NewFunctor|BgArgs],
	Literals = [Predicate|Literals1].


flatten_args([],[],S,S,Bg,Bg,[]).
flatten_args([A|Args],[V|Vars],S,Snew,Bg,Bg1,Literals):-
	flatten_term(A,V,S,Snew1,Bg,Bg2,L1),
	flatten_args(Args,Vars,Snew1,Snew,Bg2,Bg1,L2),
	append(L1,L2,Literals).


%***********************************************************************
%*									
%* predicate: flatten_literal/2								
%*									
%* syntax: flatten_literal(+Lit,-Lit_list)
%*									
%* args: Lit .. Literal, Lit_list .. list of literals
%*									
%* description:	returns the list of literals Lit has to be replaced with
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

flatten_literal(In,Out):-
         flatten_literal( In,[],_,[],_,Out).


% flatten_literal(+,+,-,+,-,-)

flatten_literal(true,S,S,Bg,Bg,[]):- !.

flatten_literal( Predicate,S,Snew,Bg,Bg1,Literals):-
	 Predicate =.. [ Functor|Args ],
	 flatten_args( Args,Vars,S,Snew,Bg,Bg1,Literals1),
	 NewPredicate =.. [Functor|Vars],
	 Literals = [NewPredicate|Literals1].


%***********************************************************************
%*									
%* predicate: flatten_literals/2
%*									
%* syntax: flatten_literals(+Body,+OldSubst,-NewSubst,
%*                          +OldBackground,-NewBackground,-Literals)
%*									
%* args: Body.. clause body
%*       OldSubst: list of substitutions that have already
%*                 been performed.       
%* 	 NewSubst: = OldSubst + additional substitutions for Body
%*	 OldBackground: old list of predicate definitions 	
%*	 NewBackground: new ...                           	
%*	 Literals: list of literals to replace Body	
%*									
%* description:	flattens clause body
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

flatten_literals( (A,B),S,Snew,Bg,Bg1,Literals):-
	!,                                     % cut, to prevent 2nd clause
	flatten_literal( A,S,Snew1,Bg,Bg2,Literals1),
	flatten_literals( B,Snew1,Snew,Bg2,Bg1,Literals2),
	append(Literals1,Literals2,Literals).

flatten_literals(A,S,Snew,Bg,Bg1,Literals):-
	flatten_literal(A,S,Snew,Bg,Bg1,Literals).


%***********************************************************************
%*									
%* predicate: flatten_clause/2              				
%*									
%* syntax: flatten_clause(+ClauseIn,-ClauseOut)  			
%*									
%* args: clauses in prolog notation, i.e. ( head :- body )		
%*	 or list notation, i.e. [ head:p , b1:n, b2:n, ... ]		
%*									
%* description:	flatten a clause            				
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

flatten_clause(In,Out):-
        In = [ _:p | _ ],!,    % list notation
        clist_to_prolog(In, F),
        flatten_clause(F,G),
        clist_to_prolog(Out,G),!.

flatten_clause(In,Out):-
	flatten_clause(In,[],_,[],_,Out),!.

flatten_clause( Clause,S,Snew,Bg,Bg1,ClauseOut):-
	Clause =.. [':-',Head,Body],
	
	% flatten head
	Head =.. [Functor|Args],
	flatten_args(Args,Vars,S,Snew1,Bg,Bg2,Literals1),
	NewHead =.. [Functor|Vars],

	% flatten body
	flatten_literals(Body,Snew1,Snew,Bg2,Bg1,Literals2),

	append(Literals1,Literals2,Literals),
	list_to_struct(Literals,StrucLits),
	ClauseOut =.. [':-',NewHead,StrucLits].


%************************************************************************
%*
%* predicates: substitute_in_literals/4
%*	       substitute_in_literal/4
%*             substitute_args/4
%* syntax: substitute_in_literals(+Var,+Term,+OldLiterals,-NewLiterals)	
%*	   substitute_in_literal(+Var,+Term,+OldLiteral,-NewLiteral) 
%*         substitute_args(+Var,+Term,+OldArgs,-NewArgs) 
%*
%* args: 
%*									
%* description: replaces all occurences of Var in OldLiterals with Term	
%*	        and outputs NewLiterals.                               	
%*	        Note that also occurences of Var in subterms of args are	
%*	        detected.	                                           
%*
%* example:
%*
%* peculiarities:
%*
%*
%* see also:
%*									
%***********************************************************************

% substitute all occurences of Var in LiteralIn by Term
substitute_in_literals(_Var,_Term, [],[]).
substitute_in_literals(Var,Term, [Lit1|Lits],[Lit1new|Litsnew]):-
	!,
	substitute_in_literal(Var,Term,Lit1,Lit1new),
	substitute_in_literals(Var,Term,Lits,Litsnew).

substitute_in_literal(Var,Term,LiteralIn,LiteralOut):-
	LiteralIn =.. [Functor|Vars],
	substitute_args(Var,Term,Vars,Args),
	LiteralOut =.. [Functor|Args].

% substitute variables Vars in argument positions by Term if identical to Var
substitute_args( Var, Term, [ V|Vs ], [ Term|Args]):- 
	Var == V,!,
	substitute_args( Var, Term, Vs, Args).

substitute_args( Var, Term, [ V|Vs ], [ Arg|Args]):-
	contains_var(Var,V),     % Var is subterm of V
	!,
	V =.. [ Functor | SubVars ],
	substitute_args(Var,Term,SubVars,SubArgs),
	Arg =.. [ Functor | SubArgs ],
	substitute_args( Var, Term, Vs, Args).

substitute_args( Var, Term, [ V|Vs ], [ V |Args ]):-
	substitute_args( Var, Term, Vs, Args).

substitute_args(_Var, _Term, [],[]).

 
%*******************************************************************************
%*
%* predicate: unflatten_clause/2  
%*
%* syntax: unflatten_clause(+FlatClause,-UnFlatClause)
%*
%* args: FlatClause : flattened clause (either in list or prolog notation) 
%*       UnFlatClause : unflattened clause                                      
%*
%* description: Algorithm for unflattening: (Rouveirol,91.p131)                               
%*      for each flattened predicate f_p(t1,..,tn,X) in the body of clause C 
%*          substitute all occurences of X by the functional term f(t1,..tn)     
%*          & drop f_p(t1,...,tn,X)
%*
%* example:
%*
%* peculiarities:
%*
%*
%* see also:                                              
%*									
%*******************************************************************************

unflatten_clause((Head:-Body) ,(Head1:-Body1)):-
	list_to_struct(BodyListIn,Body),
	unflatten_clause1( Head,[],BodyListIn,
	                   Head1, BodyListOut,[], []),
	list_to_struct(BodyListOut,Body1),
       !.


unflatten_clause(In,Out):-
        In = [ _:p | _ ],!,    % list notation
        clist_to_prolog(In, F),
        unflatten_clause(F,G),
        clist_to_prolog(Out,G),!.


%*******************************************************************************
%*
%* predicate: unflatten_clause/3 
%* 
%* syntax: unflatten_clause(+FlatClause,?Bg,-UnFlatClause)
%*
%* args: FlatClause = ( Head:-Body) : flattened clause                          
%*       Bg : optional background facts - not used yet                          
%*       UnFlatClause : unflattened clause                                      
%*                                                                               
%* description: Algorithm for unflattening: (Rouveirol,91.p131)                               
%*      for each flattened predicate f_p(t1,..,tn,X) in the body of clause C 
%*          substitute all occurences of X by the functional term f(t1,..tn)     
%*          & drop f_p(t1,...,tn,X)  
%*
%* example:
%*
%* peculiarities:
%*
%*
%* see also:                                            
%*
%*******************************************************************************

unflatten_clause((Head:-Body) , Bg, (Head1:-Body1)):-
	list_to_struct(BodyListIn,Body),
	unflatten_clause1( Head,[],BodyListIn,
	                   Head1, BodyListOut,[], Bg),
	list_to_struct(BodyListOut,Body1).


%****************************************************************
%*                                                                
%* predicate: unflatten_clause1/7   
%* 
%* syntax: unflatten_clause1(+HeadIn,+BodyIn1,+BodyIn2,-HeadOut,-BodyOut1,
%*                          -BodyOut2,?Bg)
%*
%* args: +HeadIn     (function free) head of flattened clause     
%*       +BodyIn1                                                 
%*       +BodyIn2    difference lists of body literals (flattened)
%*       -HeadOut    head of unflattened clause                   
%*       -BodyOut1                                                
%*       -BodyOut2   difference lists of body literals (unflattened)
%*       ?Bg         optional background knowledge - not used yet 
%*                                                                
%* description: unflattens a clause  ;                            
%*              some variables are replaced by functions &        
%*              certain literals are dumped
%*
%* example:
%*
%* peculiarities:
%*
%*
%* see also:                       
%*
%****************************************************************

unflatten_clause1( HeadIn,BodyIn1,[Literal|Rest],HeadOut,BodyOut1,BodyOut2,Bg ):-
	Literal  =.. [ PredFunctor | Args],
	map_function_to_pred(Functor,PredFunctor) , 
				% Literal was introduced by flattening
	!,
	append( Fargs,[Var],Args),    % get first n args (Fargs)
	Function =.. [Functor|Fargs],
	% substitute Var by Function in whole clause
	substitute_in_literal(Var,Function,HeadIn, HeadInt),
	substitute_in_literals(Var,Function,BodyIn1, BodyInt1),
        substitute_in_literals(Var,Function,Rest, BodyInt2),
	unflatten_clause1(HeadInt,BodyInt1,BodyInt2,
	                  HeadOut,BodyOut1,BodyOut2,Bg).


unflatten_clause1( HeadIn,BodyIn1,[Literal|Rest],HeadOut,BodyOut1,BodyOut2,Bg):-
	!,
	append(BodyIn1,[Literal],BodyInt1),
	unflatten_clause1(HeadIn,BodyInt1,Rest,HeadOut,BodyOut1,BodyOut2,Bg).



unflatten_clause1(Head,Body,[],Head,Body,[],_Bg).


%***********************************************************************
%*									
%* predicate:	map_function_to_pred/2							
%*									
%* syntax: map_function_to_pred(+Function_symbol,-PredName)
%*									
%* args: 								
%*									
%* description:	constructs a PredName Function_symbol_p for flattening
%*									
%* example:								
%*									
%* peculiarities:	none				
%*									
%* see also:								
%*									
%***********************************************************************

map_function_to_pred([],nil_p):-!.                 % [] -> nil
map_function_to_pred('.',cons_p):-!.               % lists 
map_function_to_pred(Integer,PredName):-           % integers , e.g. 15 -> integer_15_p
        integer(Integer),var(PredName),
        % spypoint,
        number_chars(Integer,String),atom_chars(Atom,String),
        concat_atom([integer, Atom,p],'_',PredName),
        !.
map_function_to_pred(Integer,PredName):-          % integer_15_p -> 15
        var(Integer),nonvar(PredName),
        midstring(PredName,S,'integer__p',8,_,2),
        name(S,List),
        number_chars(Integer,List),
        integer(Integer),!.
map_function_to_pred(FunctionName,PredName):-       % function symbols
	atom(FunctionName),var(PredName),
	concat_atom([FunctionName,'_p'],PredName),
	!.
map_function_to_pred(FunctionName,PredName):-
	atom(PredName),var(FunctionName),
	midstring(PredName,'_p',FunctionName,_,2,0),
	!.
