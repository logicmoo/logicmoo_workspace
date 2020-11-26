/*************************************************************************

    File: curtPDRT.pl
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

:- module(curt,[curt/0,infix/0,prefix/0]).

:- use_module(callInference,[callTPandMB/5]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                memberList/2,
				printRepresentations/1]).

:- use_module(pronounDRT,[pronounDRT/3]).

:- use_module(backgroundKnowledge,[backgroundKnowledge/2]).

:- use_module(drs2fol,[drs2fol/2]).

:- use_module(curtPredicates,[curtHelp/0,
                              curtOutput/1,
                              updateInterpretations/1,
                              printInterpretations/1,
                              updateHistory/1,
                              clearHistory/0,
                              selectInterpretations/3]).


/*========================================================================
   Dynamic Predicates
========================================================================*/

:- dynamic history/1, interpretations/1.

history([]).
interpretations([]).


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
   updateInterpretations([]),
   clearHistory.

curtUpdate([new],[],run):- !,
   updateInterpretations([]),
   clearHistory.

curtUpdate([help],[],run):- !,
   curtHelp.

curtUpdate([infix],[],run):- !,
   infix.

curtUpdate([prefix],[],run):- !,
   prefix.

curtUpdate([select,X],[],run):-
   number(X),
   interpretations(R1),
   selectInterpretations(X,R1,R2), !,
   updateInterpretations(R2).

curtUpdate([knowledge],[],run):-
   readings(R), 
   findall(K,(memberList(D,R),drs2fol(D,F),backgroundKnowledge(F,K)),L),
   printRepresentations(L).
   
curtUpdate([interpretations],[],run):- !,
   interpretations(I),
   printInterpretations(I).

curtUpdate([history],[],run):- !,
   history(H),
   printRepresentations(H).

curtUpdate(Input,Moves,run):-
   old(Old),
   pronounDRT(Input,Old,Readings), !,
   updateHistory(Input),
   consistentReadings(Readings,Consistent),
   informativeReadings(Consistent,Old,Ranking),   
   sort(Ranking,SortedRanking),
   updateInterpretations(SortedRanking),
   ( SortedRanking=[rank(1,_,_,_)|_], !, Moves=[contradiction];
     SortedRanking=[rank(0,1,_,_)|_], !, Moves=[obvious];
     Moves=[accept] ).

curtUpdate(_,[noparse],run).


/*========================================================================
   Old DRS
========================================================================*/

old(drs([],[])):-
   interpretations([]).

old(Old):-
   interpretations([rank(0,_,Old,_)|_]).

old(drs([],[])):-
   interpretations([rank(1,_,_,_)|_]).


/*========================================================================
   Select Consistent Readings
========================================================================*/

consistentReadings([],[]).

consistentReadings([B|DRSs],Rank):-
   (
      consistent(B,Model), !,
      Rank=[rank(0,B,Model)|R] 
   ;
      Rank=[rank(1,B,model([],[]))|R] 
   ),
   consistentReadings(DRSs,R).


/*========================================================================
   Consistency Checking calling Theorem Prover and Model Builder
========================================================================*/

consistent(Drs,Model):-
   drs2fol(Drs,F),
   backgroundKnowledge(F,BK),
   DomainSize=15,
   callTPandMB(not(and(BK,F)),and(BK,F),DomainSize,Model,Engine),
   format('~nMessage (consistency checking): ~p found a result.',[Engine]),
   \+ Model=model([],[]).


/*========================================================================
   Select Informative Readings
========================================================================*/

informativeReadings([],_,[]).

informativeReadings([rank(Cons,New,Model)|R1],Old,R2):-
   (    
      Cons=0,
      informative(Old,New), !,
      R2=[rank(Cons,0,New,Model)|R3] 
   ;
      Cons=0,
      R2=[rank(Cons,1,New,Model)|R3] 
   ;
      Cons=1,
      R2=[rank(Cons,0,New,Model)|R3] 
   ),
   informativeReadings(R1,Old,R3).


/*========================================================================
   Informativeness Checking calling Theorem Prover
========================================================================*/

informative(OldDrs,NewDrs):-
   \+ OldDrs=drs([],[]),
   drs2fol(OldDrs,Old),
   drs2fol(NewDrs,New),
   backgroundKnowledge(New,BK),
   DomainSize=15,
   callTPandMB(not(and(and(BK,Old),not(New))),and(and(BK,Old),not(New)),DomainSize,Model,Engine),
   format('~nMessage (informativeness checking): ~p found a result.',[Engine]),
   \+ Model=model([],[]).

informative(OldDrs,NewDrs):-
   OldDrs=drs([],[]),
   drs2fol(NewDrs,F),
   backgroundKnowledge(F,BK),
   DomainSize=15,
   callTPandMB(not(and(BK,not(F))),and(BK,not(F)),DomainSize,Model,Engine),
   format('~nMessage (informativeness checking): ~p found a result.',[Engine]),
   \+ Model=model([],[]).


/*========================================================================
   Info
========================================================================*/

info:-
   format('~n> ---------------------------------------------------------- <',[]),
   format('~n> curtPDRT.pl, by Patrick Blackburn and Johan Bos            <',[]),
   format('~n>                                                            <',[]),
   format('~n> ?- curt.                - start a dialogue with Curt       <',[]),
   format('~n>                                                            <',[]),
   format('~n> Type "help" to get more information about features         <',[]),
   format('~n> ---------------------------------------------------------- <',[]),
   format('~n~n',[]).



/*========================================================================
   Display info at start
========================================================================*/

:- info.

 
