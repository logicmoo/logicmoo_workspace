/**********************************************************************

         name: bindingDRT.pl (Chapter 9)
      version: Feb 3, 1999
  description: Check Binding Constraints
      authors: Patrick Blackburn & Johan Bos
 
**********************************************************************/

:- module(bindingDRT,[potentialAntecedent/3,
		      properBinding/3]).

:- use_module(comsemOperators),
   use_module(semOntology,[consistent/2]),
   use_module(comsemPredicates,[compose/3,memberList/2]).


/*=====================================================================
     Potential Antecedent (Ordinary DRSs)
=====================================================================*/

potentialAntecedent(A,X,Gender):-
   memberList(drs(Dom,Conds),A),
   memberList(X,Dom),
   compose(Gender,Symbol1,_),
   \+ (
          memberList(Cond,Conds),
	  compose(Cond,Symbol2,[Y]),
	  Y==X,
          \+ consistent(Symbol1,Symbol2)
      ).

/*=====================================================================
     Potential Antecedent (Focus DRSs)
=====================================================================*/

potentialAntecedent(A,X,Gender):-
   memberList(drs(Dom,_,_,Conds),A),
   memberList(X,Dom),
   compose(Gender,Symbol1,_),
   \+ (
          memberList(Cond,Conds),
	  compose(Cond,Symbol2,[Y]),
	  Y==X,
          \+ consistent(Symbol1,Symbol2)
      ).


/*=====================================================================
   Check Binding violation.
=====================================================================*/
	      
properBinding(Type,X,Drs):-
	Type=refl, 
	reflexiveBinding(X,Drs).

properBinding(Type,X,Drs):-
	\+ Type=refl,
	(
	    reflexiveBinding(X,Drs),
	    !, fail
	;
	    true
	).

reflexiveBinding(_,[]):- fail.
reflexiveBinding(_,alfa(_,_,_,_)):- fail.
reflexiveBinding(_,merge(_,_)):- fail.
reflexiveBinding(_,~_):- !, fail.
reflexiveBinding(X,drs(_,Conds)):-
   reflexiveBinding(X,Conds).

reflexiveBinding(X,[Basic|Conds]):- !,
	(
	    compose(Basic,_Sym,[Subj,Obj]),
	    Subj==Obj,
	    X==Obj, !
	;
	    reflexiveBinding(X,Conds)
	).
