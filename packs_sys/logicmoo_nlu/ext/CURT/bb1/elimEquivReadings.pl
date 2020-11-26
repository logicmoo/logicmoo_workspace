/*************************************************************************

    File: elimEquivReadings.pl
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

:- module(elimEquivReadings,[elimEquivReadings/2]).

:- use_module(callInference,[callTP/3]).

:- use_module(comsemPredicates,[memberList/2,
				selectFromList/3,
				printRepresentations/1]).


/*========================================================================
   Eliminate Equivalent Readings
========================================================================*/

elimEquivReadings([],[]).

elimEquivReadings([Reading],[Reading]).

elimEquivReadings(Readings,Unique):-
   numberReadings(Readings,0,N,Numbered),
   format('~nMessage (eliminating equivalent readings): there are ~p readings:',[N]),
   printRepresentations(Readings),
   elimEquivReadings(Numbered,[],Unique).


/*========================================================================
   Number the readings
========================================================================*/

numberReadings([],N,N,[]):-
   N > 1.

numberReadings([X|L1],N1,N3,[n(N2,X)|L2]):-
   N2 is N1 + 1,
   numberReadings(L1,N2,N3,L2).


/*========================================================================
   Check equivalence by calling a theorem prover
========================================================================*/

elimEquivReadings(Numbered,Diff,Unique):-
   selectFromList(n(N1,R1),Numbered,Readings),
   memberList(n(N2,R2),Readings),
   \+ memberList(diff(N1,N2),Diff), !,
   Formula=and(imp(R1,R2),imp(R2,R1)),
   callTP(Formula,Result,Engine),
   (
      Result=proof, !,
      format('Readings ~p and ~p are equivalent (~p).~n',[N1,N2,Engine]),
      elimEquivReadings(Readings,Diff,Unique)
   ;
      format('Readings ~p and ~p are probably not equivalent.~n',[N1,N2]),
      elimEquivReadings([n(N1,R1)|Readings],[diff(N1,N2),diff(N2,N1)|Diff],Unique)
   ).

elimEquivReadings(Numbered,_,Unique):-
   findall(Reading,memberList(n(_,Reading),Numbered),Unique).


