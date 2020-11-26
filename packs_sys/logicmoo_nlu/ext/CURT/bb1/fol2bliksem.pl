/*************************************************************************

    File: fol2bliksem.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB1, version 1.2 (August 2005).

    BB1 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB1 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB1; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(fol2bliksem,[fol2bliksem/2]).

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

printBliksem(Stream,some(X,Formula),Tab):- 
   write(Stream,'(< '),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,' >'),
   printBliksem(Stream,Formula,Tab),
   write(Stream,')').

printBliksem(Stream,all(X,Formula),Tab):- 
   write(Stream,'([ '),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,' ]'),
   printBliksem(Stream,Formula,Tab),
   write(Stream,')').

printBliksem(Stream,que(X,Formula1,Formula2),Tab):- 
   write(Stream,'(< '),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,' >'),
   printBliksem(Stream,and(Formula1,Formula2),Tab),
   write(Stream,')').

printBliksem(Stream,and(Phi,Psi),Tab):- 
   write(Stream,'('),
   printBliksem(Stream,Phi,Tab), 
   format(Stream,' & ~n',[]),
   tab(Stream,Tab),
   NewTab is Tab + 5,
   printBliksem(Stream,Psi,NewTab),
   write(Stream,')').

printBliksem(Stream,or(Phi,Psi),Tab):- 
   write(Stream,'('),
   printBliksem(Stream,Phi,Tab),
   write(Stream,' | '),
   printBliksem(Stream,Psi,Tab),
   write(Stream,')').

printBliksem(Stream,imp(Phi,Psi),Tab):- 
   write(Stream,'('),  
   printBliksem(Stream,Phi,Tab),
   write(Stream,' -> '),
   printBliksem(Stream,Psi,Tab),
   write(Stream,')').

printBliksem(Stream,not(Phi),Tab):-
   write(Stream,'!'),
   printBliksem(Stream,Phi,Tab).

printBliksem(Stream,eq(X,Y),_):- 
   write(Stream,'( '),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,' = '),
   write_term(Stream,Y,[numbervars(true)]),
   write(Stream,' )').

printBliksem(Stream,Phi,_):-
   basicFormula(Phi),
   write_term(Stream,Phi,[numbervars(true)]).
