/*************************************************************************

         name: freeVarTabl.pl (Volume 1, Chapter 5)
      version: Dec 10, 1998
  description: Free Variable Semantic Tableaux
               Uses a number of ideas from Melvin Fitting's 
               implementation of an unsigned tableaux theorem prover 
               for first-order logic, in "First-Order Logic and 
               Automated Theorem Proving", Second Edition (1996),
               Graduate Texts in Computer Science, Springer.
               For more details, see the Notes to Chapter 5.
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(freeVarTabl,[valid/2,
		       saturate/2,
		       notatedFormula/3,
		       freeVarTablTestSuite/0]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[memberList/2,
				removeFirst/3,
				unify/2,
				appendLists/3,
                                compose/3,
				newFunctionCounter/1,
                                substitute/4]).

:- use_module(folTestSuite,[formula/2]).


/*========================================================================
   Expand Tableau until it is closed, allowing Qdepth 
   applications of the universal rule.
========================================================================*/

saturate(Tableau,_Q):-
   closed(Tableau), !.

saturate(OldTableau,Qdepth):-
   expand(OldTableau,Qdepth,NewTableau,NewQdepth), !,
   saturate(NewTableau,NewQdepth).


/*========================================================================
   Every branch of Tableau can be made to contain a contradiction, 
   after a suitable free variable substitution.
========================================================================*/

closed([]).

closed([Branch|Rest]):-
   memberList(NotatedOne,Branch),
   notatedFormula(NotatedOne,_,t(X)),
   memberList(NotatedTwo,Branch),
   notatedFormula(NotatedTwo,_,f(Y)),
   unify(X,Y),
   closed(Rest).


/*========================================================================
   VarList is a list of free variables, and SkolemTerm is a previously 
   unused Skolem function symbol fun(N) applied to those free variables.
========================================================================*/

skolemFunction(VarList,SkolemTerm):-
   newFunctionCounter(N),
   compose(SkolemTerm,fun,[N|VarList]).


/*========================================================================
   Try to create a tableau expansion for f(X) that is closed allowing 
   Qdepth applications of the universal rule.
========================================================================*/

valid(X,Qdepth):-
   notatedFormula(NotatedFormula,[],f(X)),
   saturate([[NotatedFormula]],Qdepth).


/*========================================================================
   Notate the free variables of a formula
========================================================================*/

notatedFormula(n(Free,Formula),Free,Formula).


/*========================================================================
   Prove all formulas from the test suite 
========================================================================*/

freeVarTablTestSuite:-
        format('~n~n>>>>> FREE VARIABLE TABLEAUX ON TEST SUITE <<<<<',[]),
        formula(Formula,Status),
        format('~n~nInput formula: ~p~nStatus: ~p',[Formula,Status]),
        format('~nProver says: ',[]),
	prove(Formula),
        fail.

freeVarTablTestSuite.


/*========================================================================
   Prove a formula with Qdept 10
========================================================================*/

prove(Formula):-
   valid(Formula,10), !,
   write('Theorem').

prove(_Formula):-
   write('Unknown').


/*========================================================================
   Newtableaux with Q-depth NewQdepth is the result of applying  
   a tableaux expansion rule to Oldtableaux with a Q-depth of OldQdepth.
========================================================================*/

expand([Branch|Tableau],QD,[NewBranch|Tableau],QD):-
   unaryExpansion(Branch,NewBranch).

expand([Branch|Tableau],QD,[NewBranch|Tableau],QD):-
   conjunctiveExpansion(Branch,NewBranch).

expand([Branch|Tableau],QD,[NewBranch|Tableau],QD):-
   existentialExpansion(Branch,NewBranch).

expand([Branch|Tableau],QD,[NewBranch1,NewBranch2|Tableau],QD):-
   disjunctiveExpansion(Branch,NewBranch1,NewBranch2).

expand([Branch|Tableau],OldQD,NewTableau,NewQD):-
   universalExpansion(Branch,OldQD,NewBranch,NewQD),
   appendLists(Tableau,[NewBranch],NewTableau).

expand([Branch|Rest],OldQD,[Branch|Newrest],NewQD):-
   expand(Rest,OldQD,Newrest,NewQD).


/*========================================================================
   Take Branch as input, and return NewBranches if a tableau rule
   allows unary expansion.
========================================================================*/

unaryExpansion(Branch,[NotatedComponent|Temp]) :-
   unary(SignedFormula,Component),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   notatedFormula(NotatedComponent,Free,Component).


/*========================================================================
   Take Branch as input, and return the NewBranch if a tableau rule 
   allows conjunctive expansion. 
========================================================================*/

conjunctiveExpansion(Branch,[NotatedComp1,NotatedComp2|Temp]):-
   conjunctive(SignedFormula,Comp1,Comp2),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   notatedFormula(NotatedComp1,Free,Comp1),
   notatedFormula(NotatedComp2,Free,Comp2).


/*========================================================================
   Take Branch as input, and return the NewNranch1 and NewBranch2 
   if a tableau rule allows disjunctive expansion. 
========================================================================*/

disjunctiveExpansion(Branch,[NotComp1|Temp],[NotComp2|Temp]):-
   disjunctive(SignedFormula,Comp1,Comp2),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   notatedFormula(NotComp1,Free,Comp1),
   notatedFormula(NotComp2,Free,Comp2).


/*========================================================================
   Take Branch as input, and return the NewBranch if a tableau rule 
   allows existential expansion.
========================================================================*/

existentialExpansion(Branch,[NotatedInstance|Temp]):-
   notatedFormula(NotatedFormula,Free,SignedFormula),
   existential(SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   skolemFunction(Free,Term),
   instance(SignedFormula,Term,Instance),
   notatedFormula(NotatedInstance,Free,Instance).


/*========================================================================
   Take Branch and OldQD as input, and return the NewBranch and 
   NewQDepthif a tableau rule allow universal expansion.
========================================================================*/

universalExpansion(Branch,OldQD,NewBranch,NewQD):-
   OldQD > 0, NewQD is OldQD - 1,
   memberList(NotatedFormula,Branch),
   notatedFormula(NotatedFormula,Free,SignedFormula),
   universal(SignedFormula),
   removeFirst(NotatedFormula,Branch,Temp),
   instance(SignedFormula,V,Instance),
   notatedFormula(NotatedInstance,[V|Free],Instance),
   appendLists([NotatedInstance|Temp],[NotatedFormula],NewBranch).


/*========================================================================
   Decompose conjunctive signed formula
========================================================================*/

conjunctive(t(X & Y),t(X),t(Y)).
conjunctive(f(X v Y),f(X),f(Y)).
conjunctive(f(X > Y),t(X),f(Y)).


/*========================================================================
   Decompose disjunctive signed formula
========================================================================*/

disjunctive(f(X & Y),f(X),f(Y)).
disjunctive(t(X v Y),t(X),t(Y)).
disjunctive(t(X > Y),f(X),t(Y)).


/*========================================================================
   Decompose unary signed formula
========================================================================*/

unary(t(~X),f(X)).
unary(f(~X),t(X)). 


/*========================================================================
   Universal Signed Formulas
========================================================================*/

universal(t(forall(_,_))).
universal(f(exists(_,_))).


/*========================================================================
   Existential Signed Formulas
========================================================================*/

existential(t(exists(_,_))).
existential(f(forall(_,_))).


/*========================================================================
   Remove quantifier from signed quantified formula, and replacing all
   free occurrences of the quantified variable by occurrences of Term.
========================================================================*/

instance(t(forall(X,F)),Term,t(NewF)):- substitute(Term,X,F,NewF).
instance(f(exists(X,F)),Term,f(NewF)):- substitute(Term,X,F,NewF).
instance(t(exists(X,F)),Term,t(NewF)):- substitute(Term,X,F,NewF).
instance(f(forall(X,F)),Term,f(NewF)):- substitute(Term,X,F,NewF).

