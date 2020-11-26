/*************************************************************************

         name: semOntology.pl (Chapter 6)
      version: July 10, 1999
  description: Predicates for working with the semantic ontology
      authors: Patrick Blackburn & Johan Bos

*************************************************************************/

:- module(semOntology,[generateOntology/1,
		       generateOntology/2,
		       consistent/2]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[memberList/2,
				appendLists/3,
				basicFormula/1,
				selectFromList/3,
				compose/3]).

:- use_module(englishLexicon,[lexicon/4]).


/*========================================================================
   Generating Ontology in First-Order Formulas
========================================================================*/

generateOntology(Formula1 & Formula2):-
	isaRelations(Isa),
	disjointRelations(Disjoint),
	relations2fol(Isa,Formula1),
	relations2fol(Disjoint,Formula2).

generateOntology(Input,Formula):-
	isaRelations(Isa),
        disjointRelations(Disjoint),
	selectRelations(Input,Isa-_,Disjoint-_,[]-Relations),
	relations2fol(Relations,Formula).
	

/*========================================================================
   Generating isa/2 and disjoint/2 relations from english lexicon
========================================================================*/

isaRelations(Isa):-
   setof(isa(Concept,SuperConcept),
	 SuperConcepts^Words^(
			      lexicon(noun,Concept,Words,SuperConcepts),
                              memberList(SuperConcept,SuperConcepts)
			     ),
         Isa).


disjointRelations(Disjoint):-
   setof(disjoint(Concept1,Concept2),
	 DisjointConcepts^Words^(
				 lexicon(adj,Concept1,Words,DisjointConcepts),
				 memberList(Concept2,DisjointConcepts)
				),
	 Disjoint).


/*========================================================================
   Translating ISA-relations to first-order formulas
========================================================================*/

relations2fol([],p v ~p).

relations2fol([isa(S1,S2)],forall(X,F1 > F2)):-
	compose(F1,S1,[X]),
	compose(F2,S2,[X]).

relations2fol([disjoint(S1,S2)],forall(X,F1 > ~ F2)):-
	compose(F1,S1,[X]),
	compose(F2,S2,[X]).

relations2fol([A,B|L],Formula1 & Formula2):-
	relations2fol([A],Formula1),
	relations2fol([B|L],Formula2).


/*========================================================================
    Select isa/disjoint relations from a Formula
========================================================================*/

selectRelations(forall(_,F),I1-I2,D1-D2,R1-R2):- 
	selectRelations(F,I1-I2,D1-D2,R1-R2).

selectRelations(exists(_,F),I1-I2,D1-D2,R1-R2):- 
	selectRelations(F,I1-I2,D1-D2,R1-R2).

selectRelations(lambda(_,F),I1-I2,D1-D2,R1-R2):- 
	selectRelations(F,I1-I2,D1-D2,R1-R2).

selectRelations(~ F,I1-I2,D1-D2,R1-R2):- 
	selectRelations(F,I1-I2,D1-D2,R1-R2).

selectRelations(F1 & F2,I1-I3,D1-D3,R1-R3):- 
	selectRelations(F1,I1-I2,D1-D2,R1-R2),
	selectRelations(F2,I2-I3,D2-D3,R2-R3).

selectRelations(F1 v F2,I1-I3,D1-D3,R1-R3):- 
	selectRelations(F1,I1-I2,D1-D2,R1-R2),
	selectRelations(F2,I2-I3,D2-D3,R2-R3).

selectRelations(F1 > F2,I1-I3,D1-D3,R1-R3):-
	selectRelations(F1,I1-I2,D1-D2,R1-R2),
	selectRelations(F2,I2-I3,D2-D3,R2-R3).

selectRelations(Basic,I1-I2,D1-D2,R1-R2):-
	basicFormula(Basic),
	compose(Basic,Symbol,_),
	(
	    selectFromList(isa(Symbol,Hyper),I1,I3), !,
	    selectRelations(Hyper,I3-I4,D1-D3,R1-R3),
	    selectRelations(Symbol,I4-I2,D3-D2,R3-R4),
	    R2=[isa(Symbol,Hyper)|R4]
	;
	    selectFromList(disjoint(Symbol,Concept),D1,D2), !,
	    I2=I1,
	    R2=[disjoint(Symbol,Concept)|R1]
	;   
	    I2=I1,
	    D2=D1,
	    R2=R1
	).


/*========================================================================
   Consistency Check
========================================================================*/

consistent(X,Y):-
   isaRelations(Isa),
   disjointRelations(Disjoint),
   \+ inconsistent(X,Y,Isa,Disjoint).

inconsistent(X,Y,_,Disjoint):-
	memberList(disjoint(X,Y),Disjoint).

inconsistent(X,Y,_,Disjoint):-
	memberList(disjoint(Y,X),Disjoint).

inconsistent(X,Y,Isa,Disjoint):-
	memberList(isa(X,Z),Isa),
	inconsistent(Z,Y,Isa,Disjoint).

inconsistent(X,Y,Isa,Disjoint):-
	memberList(isa(Y,Z),Isa),
	inconsistent(X,Z,Isa,Disjoint).

