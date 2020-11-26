/*************************************************************************

         name: fol2bliksem.pl (Volume 1, Chapter 5)
      version: June 18, 1998
  description: Translates a formula in bliksem syntax 
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(fol2bliksem,[fol2bliksem/2]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[basicFormula/1]).


/*========================================================================
   Translates formula to otter syntax on Stream
========================================================================*/

fol2bliksem(Formula,Stream):-
	nonvar(Formula),
	printBliksemFormula(Stream,Formula).


/*========================================================================
   Print an Bliksem formula (introducing tab)
========================================================================*/

printBliksemFormula(Stream,F):-
	   format(Stream,'~nAuto.~n~n',[]),
	   \+ \+ (
		     numbervars(F,0,_),
		     printBliksem(Stream,F,5)
		 ),
	   format(Stream,'.~n',[]).


/*========================================================================
   Print Bliksem formulas
========================================================================*/

printBliksem(Stream,exists(X,Formula),Tab):- 
   format(Stream,'(< ~p >',[X]),
   printBliksem(Stream,Formula,Tab),
   write(Stream,')').

printBliksem(Stream,forall(X,Formula),Tab):- 
   format(Stream,'([ ~p ]',[X]),
   printBliksem(Stream,Formula,Tab),
   write(Stream,')').

printBliksem(Stream,lambda(X,Formula),Tab):- 
   format(Stream,'(< ~p >',[X]),
   printBliksem(Stream,Formula,Tab),
   write(Stream,')').

printBliksem(Stream,Phi & Psi,Tab):- 
   write(Stream,'('),
   printBliksem(Stream,Phi,Tab), 
   format(Stream,' & ~n',[]),
   tab(Stream,Tab),
   NewTab is Tab + 5,
   printBliksem(Stream,Psi,NewTab),
   write(Stream,')').

printBliksem(Stream,Phi v Psi,Tab):- 
   write(Stream,'('),
   printBliksem(Stream,Phi,Tab),
   write(Stream,' | '),
   printBliksem(Stream,Psi,Tab),
   write(Stream,')').

printBliksem(Stream,Phi <> Psi,Tab):- 
   write(Stream,'('),
   printBliksem(Stream,Phi,Tab),
   write(Stream,' <-> '),
   printBliksem(Stream,Psi,Tab),
   write(Stream,')').

printBliksem(Stream,Phi > Psi,Tab):- 
   write(Stream,'('),  
   printBliksem(Stream,Phi,Tab),
   write(Stream,' -> '),
   printBliksem(Stream,Psi,Tab),
   write(Stream,')').

printBliksem(Stream,~ Phi,Tab):-
   write(Stream,'!'),
   printBliksem(Stream,Phi,Tab).

printBliksem(Stream,Phi,_):-
   basicFormula(Phi),
   write(Stream,Phi).
