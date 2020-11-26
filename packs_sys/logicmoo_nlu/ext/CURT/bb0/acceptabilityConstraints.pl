/*************************************************************************

         name: acceptabilityConstraints.pl (Chapter 11)
      version: June 18, 1998
  description: Consistency and Informativity
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(acceptabilityConstraints,[consistent/2,
				    informative/2,
				    localConsistent/1,
				    localInformative/1,
				    noFreeVars/1]).

:- use_module(drs2fol,[drs2fol/2]).

:- ensure_loaded(comsemOperators).

:- use_module(drsPredicates,[mergeDrs/2,
			     alphaConvertDrs/2,
			     superSubDrs/3]).

:- use_module(comsemPredicates,[memberList/2,
				variablesInTerm/2]).

:- use_module(callInference,[callModelBuilder/3,
			     callTheoremProver/2]).

:- use_module(semOntology,[generateOntology/2]).


/*========================================================================
   Informativity
========================================================================*/

informative(drs([],[]),_):- !.

informative(OldDrs,NewDrs):-
	DomainSize = 15,
	drs2fol(OldDrs,Phi),
	drs2fol(NewDrs,Psi),
	backgroundKnowledge(Psi,Chi),
	(
      	    Formula1 = (~(Chi & Phi & ~ Psi)),
	    callTheoremProver(Formula1,Proof),
	    Proof=proof, !,
	    fail
	;
            Formula2 = (Chi & Phi & ~ Psi),
	    callModelBuilder(Formula2,DomainSize,Model),
	    Model=model(_,_)
	).


/*========================================================================
   Consistency
========================================================================*/

consistent(Drs,Model):-
	drs2fol(Drs,Formula),
	backgroundKnowledge(Formula,Knowledge),
	callTheoremProver(~(Knowledge & Formula),Proof),
	(
	    Proof=proof, !,
	    fail
	;
	    DomainSize=15,
	    callModelBuilder(Knowledge & Formula,DomainSize,Model),
	    Model=model(_,_)
	).




/*========================================================================
   Local Informativity
========================================================================*/

localInformative(Drs):-
   findall((Super,Sub),superSubDrs(Drs,drs([],[])-Super,Sub),List),
   allLocalInformative(List,0-Number),
   Number=0.

allLocalInformative([],N-N).

allLocalInformative([(drs([],[]),_)|Others],N1-N2):- !,
   allLocalInformative(Others,N1-N2).

allLocalInformative([(Super,Sub)|Others],N1-N3):-
   drs2fol(drs([],[Super>Sub]),Phi),
   backgroundKnowledge(Phi,Chi),
   callTheoremProver(~(Chi & ~Phi),Proof),
   (
       Proof=proof,
       !,
       N2 is N1 + 1
   ;
       N2 is N1
   ),
   allLocalInformative(Others,N2-N3).



/*========================================================================
   Local Consistency
========================================================================*/

localConsistent(Drs):-
   findall((Super,Sub),superSubDrs(Drs,drs([],[])-Super,Sub),List),
   allLocalConsistent(List,0-Number),
   Number=0.

allLocalConsistent([],N-N).

allLocalConsistent([(drs([],[]),_)|Others],N1-N2):- !,
   allLocalConsistent(Others,N1-N2).

allLocalConsistent([(Super,Sub)|Others],N1-N3):-
   drs2fol(drs([],[Super>drs([],[~Sub])]),Phi),
   backgroundKnowledge(Phi,Chi),
   callTheoremProver(~(Chi & ~Phi),Proof),
   (
       Proof=proof,
       !,
       N2 is N1 + 1
      ;
       N2 is N1
   ),
   allLocalConsistent(Others,N2-N3).


/*========================================================================
   Background Knowledge
========================================================================*/

backgroundKnowledge(Formula1,Formula2 & Formula3):-
   knowledge(Formula2),
   generateOntology(Formula1 & Formula2,Formula3).

knowledge(
           forall(X,forall(Y,have(X,Y)>of(Y,X))) &
           forall(X,forall(Y,of(Y,X)>have(X,Y))) &
           forall(X,female(X)&married(X)>exists(Y,husband(Y)&have(X,Y))) &
           forall(X,forall(Y,husband(Y)&have(X,Y)>married(X)&female(X)))
	  ).


/*========================================================================
   Checking for free variables
========================================================================*/

noFreeVars(B1):-
	alphaConvertDrs(B1,B2),
	variablesInTerm(B1,[]-Vars1),
	variablesInTerm(B2,[]-Vars2),
	\+ (
	       memberList(X1,Vars1),
	       memberList(X2,Vars2),
	       X1==X2
	   ).

