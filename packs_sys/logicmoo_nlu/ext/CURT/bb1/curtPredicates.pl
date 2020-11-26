/*************************************************************************

    File: curtPredicates.pl
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

:- module(curtPredicates,[curtHelp/0,
                          curtOutput/1,
                          updateReadings/1,
                          updateModels/1,
                          updateHistory/1,
                          clearHistory/0,
                          list2string/2,
                          selectReadings/3]).

:- use_module(comsemPredicates,[appendLists/3]).


/*========================================================================
   Curt Help
========================================================================*/

curtHelp:-
   nl, write('readings: prints current readings'),
   nl, write('select N: select a reading (N should be a number)'),
   nl, write('new: starts a new discourse'),
   nl, write('history: shows history of discourse'),
   nl, write('models: prints current models'),
   nl, write('summary: eliminate equivalent readings'),
   nl, write('knowledge: calculate and show background knowledge'),
   nl, write('infix: display formulas in infix notation'),
   nl, write('prefix: display formulas in prefix notation'),
   nl, write('bye: no more talking'),
   nl.


/*========================================================================
   Curt's output
========================================================================*/

curtOutput([]).

curtOutput([Move|Moves]):-
   realiseMove(Move,Output),
   format('~nCurt: ~p~n',[Output]),
   curtOutput(Moves).


/*========================================================================
   Curt's Moves
========================================================================*/

realiseMove(clarify,'Want to tell me something?').
realiseMove(bye,'Goodbye!').
realiseMove(accept,'OK.').
realiseMove(noparse,'What?').
realiseMove(contradiction,'No! I do not believe that!').
realiseMove(obvious,'Well, that is obvious!').
realiseMove(unknown_answer,'I have no idea.').
realiseMove(sensible_question,'This question makes sense!').
realiseMove(answer(String),String).


/*========================================================================
   Select Readings
========================================================================*/

selectReadings(X,R1,R2):-
   selectReadings(1,X,R1,R2).

selectReadings(X,X,[R|_],[R]).

selectReadings(X,Y,[_|L],R):-
   X < Y,
   Z is X + 1,
   selectReadings(Z,Y,L,R).
 

/*========================================================================
   Update History
========================================================================*/

updateHistory(Input):-
   retract(curt:history(His1)),
   appendLists(His1,[Input],His2),
   assert(curt:history(His2)).


/*========================================================================
   Clear History
========================================================================*/

clearHistory:-
   retract(curt:history(_)),
   assert(curt:history([])).


/*========================================================================
   Update Readings
========================================================================*/

updateReadings(R):-
   retract(curt:readings(_)),
   assert(curt:readings(R)).


/*========================================================================
   Update Models
========================================================================*/

updateModels(R):-
   retract(curt:models(_)),
   assert(curt:models(R)).


/*========================================================================
    Covert a list of words to a string
========================================================================*/

list2string([Word],Word).

list2string([Word|L],String2):-
   list2string(L,String1),
   name(Word,Codes1),
   name(String1,Codes2),
   appendLists(Codes1,[32|Codes2],Codes3),
   name(String2,Codes3).
