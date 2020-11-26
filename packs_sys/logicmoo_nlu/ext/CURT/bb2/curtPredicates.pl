/*************************************************************************

    File: curtPredicates.pl
    Copyright (C) 2004 Patrick Blackburn & Johan Bos

    This file is part of BB2, version 1.0 (June 2004).

    BB2 is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    BB2 is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with BB2; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(curtPredicates,[curtHelp/0,
                          curtOutput/1,
                          updateInterpretations/1,
                          updateHistory/1,
                          clearHistory/0,
                          list2string/2,
                          printInterpretations/1,
                          selectInterpretations/3]).

:- use_module(comsemPredicates,[appendLists/3]).


/*========================================================================
   Curt Help
========================================================================*/

curtHelp:-
   nl, write('interpretions: display current interpretations'),
   nl, write('select N: select a reading (N should be a number)'),
   nl, write('new: starts a new discourse'),
   nl, write('history: shows history of discourse'),
   nl, write('knowledge: calculate and show background knowledge'),
   nl, write('infix: print information in infix notation'),
   nl, write('prefix: print information in prefix notation (default)'),
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

selectInterpretations(X,R1,R2):-
   selectInterpretations(1,X,R1,R2).

selectInterpretations(X,X,[R|_],[R]).

selectInterpretations(X,Y,[_|L],R):-
   X < Y,
   Z is X + 1,
   selectInterpretations(Z,Y,L,R).
 

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
   Update Interpretations
========================================================================*/

updateInterpretations(R):-
   retract(curt:interpretations(_)),
   assert(curt:interpretations(R)).


/*========================================================================
   Print Interpretations
========================================================================*/

printInterpretations(L):-
   printInterpretations(L,1).

printInterpretations([],_):- nl.

printInterpretations([rank(Cons,Info,Drs,Model)|L],I):-
   format('~nInterpretation ~p: ',[I]),
   ( 
      Cons = 1, write('inconsistent.'), !, nl, print(Drs)
   ;
      write('consistent, '),
      ( Info = 1, write('uninformative. '), !; write('informative. ') ),
      nl, print(Drs), nl,
      print(Model)
   ),
   J is I + 1,
   printInterpretations(L,J).

printInterpretations([rank(Cons,Info,Local,_,Drs,Model)|L],I):-
   format('~nInterpretation ~p: ',[I]),
   ( 
      Cons = 1, write('inconsistent.'), !, nl, print(Drs)
   ;
      write('consistent, '),
      ( Info = 1, write('uninformative, '), !; write('informative, ') ),
      format('~p local violations.~n',[Local]),
      print(Drs), nl,
      print(Model)
   ),
   J is I + 1,
   printInterpretations(L,J).


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
