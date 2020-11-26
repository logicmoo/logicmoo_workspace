/*************************************************************************

    File: fol2otter.pl
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

:- module(fol2otter,[fol2otter/2,fol2mace/2]).

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
   Translates formula to MACE syntax on Stream
========================================================================*/

fol2mace(Formula,Stream):- 
	format(Stream,'set(auto).~n~n',[]),
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

printOtter(Stream,some(X,Formula),Tab):- 
   write(Stream,'(exists '),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,' '),
   printOtter(Stream,Formula,Tab),
   write(Stream,')').

printOtter(Stream,all(X,Formula),Tab):- 
   write(Stream,'(all '),
   write_term(Stream,X,[numbervars(true)]),   
   write(Stream,' '),
   printOtter(Stream,Formula,Tab),
   write(Stream,')').

printOtter(Stream,que(X,Formula1,Formula2),Tab):- 
   write(Stream,'(exists '),
   write_term(Stream,X,[numbervars(true)]),   
   write(Stream,' '),
   printOtter(Stream,and(Formula1,Formula2),Tab),
   write(Stream,')').

printOtter(Stream,and(Phi,Psi),Tab):- 
   write(Stream,'('),
   printOtter(Stream,Phi,Tab), 
   format(Stream,' & ~n',[]),
   tab(Stream,Tab),
   NewTab is Tab + 5,
   printOtter(Stream,Psi,NewTab),
   write(Stream,')').

printOtter(Stream,or(Phi,Psi),Tab):- 
   write(Stream,'('),
   printOtter(Stream,Phi,Tab),
   write(Stream,' | '),
   printOtter(Stream,Psi,Tab),
   write(Stream,')').

printOtter(Stream,imp(Phi,Psi),Tab):- 
   write(Stream,'('),  
   printOtter(Stream,Phi,Tab),
   write(Stream,' -> '),
   printOtter(Stream,Psi,Tab),
   write(Stream,')').

printOtter(Stream,not(Phi),Tab):- 
   write(Stream,'-('),
   printOtter(Stream,Phi,Tab),
   write(Stream,')').

printOtter(Stream,eq(X,Y),_):- 
   write(Stream,'('),  
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,' = '),
   write_term(Stream,Y,[numbervars(true)]),
   write(Stream,')').

printOtter(Stream,Phi,_):-
   basicFormula(Phi),
   write_term(Stream,Phi,[numbervars(true)]).
