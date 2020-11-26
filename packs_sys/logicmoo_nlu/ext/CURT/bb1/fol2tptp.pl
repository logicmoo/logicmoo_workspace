/*************************************************************************

    File: fol2tptp.pl
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

:- module(fol2tptp,[fol2tptp/2]).

:- use_module(comsemPredicates,[basicFormula/1]).

/*========================================================================
   Translates formula to TPTP syntax on Stream
========================================================================*/

fol2tptp(Formula,Stream):- 
   write(Stream,'input_formula(comsem,conjecture,'),
   \+ \+ ( numbervars(Formula,0,_),printTptp(Formula,Stream) ),
   write(Stream,').'),
   nl(Stream).


/*========================================================================
   Print Tptp formulas
========================================================================*/

printTptp(some(X,Formula),Stream):- !,
   write(Stream,'(? ['),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,']: '),
   printTptp(Formula,Stream),
   write(Stream,')').

printTptp(que(X,Formula),Stream):- !,
   write(Stream,'(? ['),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,']: '),
   printTptp(Formula,Stream),
   write(Stream,')').

printTptp(all(X,Formula),Stream):- !,
   write(Stream,'(! ['),
   write_term(Stream,X,[numbervars(true)]),
   write(Stream,']: '),
   printTptp(Formula,Stream),
   write(Stream,')').

printTptp(and(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTptp(Phi,Stream), 
   write(Stream,' & '), 
   printTptp(Psi,Stream), 
   write(Stream,')').

printTptp(or(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTptp(Phi,Stream), 
   write(Stream,' | '),
   printTptp(Psi,Stream), 
   write(Stream,')').

printTptp(imp(Phi,Psi),Stream):- !,
   write(Stream,'('),
   printTptp(Phi,Stream), 
   write(Stream,' => '),
   printTptp(Psi,Stream), 
   write(Stream,')').

printTptp(not(Phi),Stream):- !,
   write(Stream,'~ '),
   printTptp(Phi,Stream).

printTptp(eq(X,Y),Stream):- !,
   write_term(Stream,equal(X,Y),[numbervars(true)]).

printTptp(Phi,Stream):-
   basicFormula(Phi),
   write_term(Stream,Phi,[numbervars(true)]).
