/*************************************************************************

    File: rugratCurt.pl
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

:- module(curt,[curt/0,infix/0,prefix/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                memberList/2,
                                appendLists/3,
				printRepresentations/1]).

:- use_module(kellerStorage,[kellerStorage/2]).

:- use_module(curtPredicates,[curtHelp/0,
                              curtOutput/1,
                              updateReadings/1,
                              updateHistory/1,
                              clearHistory/0,
                              selectReadings/3]).

:- use_module(foResolution,[rprove/1]).


/*========================================================================
   Dynamic Predicates
========================================================================*/

:- dynamic history/1, readings/1.

history([]).
readings([]).


/*========================================================================
   Start Curt
========================================================================*/

curt:- 
   curtTalk(run).


/*========================================================================
   Control
========================================================================*/

curtTalk(quit).

curtTalk(run):-
   readLine(Input),
   curtUpdate(Input,CurtsMoves,State), 
   curtOutput(CurtsMoves),
   curtTalk(State).


/*========================================================================
   Update Curt's Information State
========================================================================*/

curtUpdate([],[clarify],run):- !.

curtUpdate([bye],[bye],quit):- !,
   updateReadings([]),
   clearHistory.

curtUpdate([new],[],run):- !,
   updateReadings([]),
   clearHistory.

curtUpdate([help],[],run):- !,
   curtHelp.

curtUpdate([infix],[],run):- !,
   infix.

curtUpdate([prefix],[],run):- !,
   prefix.

curtUpdate([select,X],[],run):-
   number(X),
   readings(R1),
   selectReadings(X,R1,R2), !,
   updateReadings(R2).

curtUpdate([readings],[],run):- !,
   readings(R),
   printRepresentations(R).

curtUpdate([history],[],run):- !,
   history(H),
   printRepresentations(H).

curtUpdate(Input,Moves,run):-
   kellerStorage(Input,Readings), !,
   updateHistory(Input),
   consistentReadings(Readings,[]-ConsReadings),
   (
      ConsReadings=[],
      Moves=[contradiction]
   ;  
      \+ ConsReadings=[],
      Moves=[accept],
      combine(ConsReadings,CombinedReadings), 
      updateReadings(CombinedReadings)
   ).

curtUpdate(_,[noparse],run).


/*========================================================================
   Select Consistent Readings
========================================================================*/

consistentReadings([],C-C).

consistentReadings([New|Readings],C1-C2):-
   readings(Old),
   (
      consistent(Old,New), !,
      consistentReadings(Readings,[New|C1]-C2) 
   ;
      consistentReadings(Readings,C1-C2) 
   ).


/*========================================================================
   Consistency Checking calling Theorem Prover 
========================================================================*/

consistent([Old|_],New):-
   rprove(not(and(Old,New))), !,
   nl, write('Message (consistency checking): proof found.'),
   fail.

consistent([],New):-
   rprove(not(New)), !,
   nl, write('Message (consistency checking): proof found.'),
   fail.

consistent(_,_).

            
/*========================================================================
   Combine New Utterances with History
========================================================================*/

combine(New,New):- 
   readings([]).

combine(Readings,Updated):-
   readings([Old|_]),
   findall(and(Old,New),memberList(New,Readings),Updated).


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n> rugratCurt.pl, by Patrick Blackburn and Johan Bos                   <',[]),
   format('~n>                                                                     <',[]),
   format('~n> ?- curt.                - start a dialogue with Curt                <',[]),
   format('~n>                                                                     <',[]),
   format('~n> Type "help" to get more information about features                  <',[]),
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info.

