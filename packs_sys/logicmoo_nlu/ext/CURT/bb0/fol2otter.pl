/*************************************************************************

         name: fol2otter.pl (Volume 1, Chapter 5)
      version: June 18, 1998
  description: Translates a formula in otter syntax 
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(fol2otter,[fol2otter/2]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[basicFormula/1]).


/*========================================================================
   Translates formula to Otter syntax on Stream
========================================================================*/

fol2otter(Formula,Stream):- 
	format(Stream,'set(auto).~n~n',[]),
	format(Stream,'assign(max_seconds,30).~n~n',[]),
	format(Stream,'clear(print_proofs).~n~n',[]),
	format(Stream,'set(prolog_style_variables).~n~n',[]),
	format(Stream,'formula_list(usable).~n~n',[]),
	printOtterFormula(Stream,Formula),
	format(Stream,'~nend_of_list.~n',[]).


/*========================================================================
   Print an Otter formula (introducing tab)
========================================================================*/

printOtterFormula(Stream,F):-
	   \+ \+ (
		     numbervars(F,0,_),
		     printOtter(Stream,F,5)
		 ),
	   format(Stream,'.~n',[]).


/*========================================================================
   Print Otter formulas
========================================================================*/

printOtter(Stream,exists(X,Formula),Tab):- 
   format(Stream,'(exists ~p ',[X]),
   printOtter(Stream,Formula,Tab),
   write(Stream,')').

printOtter(Stream,forall(X,Formula),Tab):- 
   format(Stream,'(all ~p ',[X]),
   printOtter(Stream,Formula,Tab),
   write(Stream,')').

printOtter(Stream,lambda(X,Formula),Tab):- 
   format(Stream,'(exists ~p ',[X]),
   printOtter(Stream,Formula,Tab),
   write(Stream,')').

printOtter(Stream,Phi & Psi,Tab):- 
   write(Stream,'('),
   printOtter(Stream,Phi,Tab), 
   format(Stream,' & ~n',[]),
   tab(Stream,Tab),
   NewTab is Tab + 5,
   printOtter(Stream,Psi,NewTab),
   write(Stream,')').

printOtter(Stream,Phi v Psi,Tab):- 
   write(Stream,'('),
   printOtter(Stream,Phi,Tab),
   write(Stream,' | '),
   printOtter(Stream,Psi,Tab),
   write(Stream,')').

printOtter(Stream,Phi <> Psi,Tab):- 
   write(Stream,'('),
   printOtter(Stream,Phi,Tab),
   write(Stream,' <-> '),
   printOtter(Stream,Psi,Tab),
   write(Stream,')').

printOtter(Stream,Phi > Psi,Tab):- 
   write(Stream,'('),  
   printOtter(Stream,Phi,Tab),
   write(Stream,' -> '),
   printOtter(Stream,Psi,Tab),
   write(Stream,')').

printOtter(Stream,~ Phi,Tab):- 
   write(Stream,'-('),
   printOtter(Stream,Phi,Tab),
   write(Stream,')').

printOtter(Stream,Phi,_):-
   basicFormula(Phi),
   write(Stream,Phi).
