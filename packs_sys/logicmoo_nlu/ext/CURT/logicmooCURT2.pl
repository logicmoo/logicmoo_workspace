/*************************************************************************

    File: curtPPDRT.pl
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
%:- module(curt,[curt/0,infix/0,prefix/0]).

% :- ensure_loaded('printDrs.pl'). %  printDrs 0.00 sec, 42 clauses
% :- ensure_loaded('fol2otter.pl'). %  fol2otter 0.02 sec, 236 clauses
% :- ensure_loaded('fol2bliksem.pl'). %  fol2bliksem 0.00 sec, 13 clauses
% :- ensure_loaded('fol2tptp.pl'). %  fol2tptp 0.00 sec, 12 clauses
% :- ensure_loaded('callInference.pl'). %  callInference 0.03 sec, 310 clauses
% :- ensure_loaded('readLine.pl'). %  readLine 0.00 sec, 17 clauses
% :- ensure_loaded('alphaConversionDRT.pl'). %  alphaConversionDRT 0.00 sec, 20 clauses
% :- ensure_loaded('betaConversionDRT.pl'). %  betaConversionDRT 0.00 sec, 32 clauses
% :- ensure_loaded('mergeDRT.pl'). %  mergeDRT 0.00 sec, 11 clauses
% :- ensure_loaded('presupTestSuite.pl'). %  presupTestSuite 0.00 sec, 18 clauses
% :- ensure_loaded('bindingViolation.pl'). %  bindingViolation 0.00 sec, 8 clauses
% :- ensure_loaded('freeVarCheck.pl'). %  freeVarCheck 0.00 sec, 17 clauses
% :- ensure_loaded('lexicalKnowledge.pl'). %  lexicalKnowledge 0.01 sec, 108 clauses
% :- ensure_loaded('sortalCheck.pl'). %  sortalCheck 0.01 sec, 137 clauses
% :- ensure_loaded('englishGrammar'). %  presupDRT 0.01 sec, 77 clauses
% :- ensure_loaded('englishLexicon'). %  presupDRT 0.01 sec, 172 clauses
% :- ensure_loaded('semLexPresupDRT.pl'). %  presupDRT 0.00 sec, 18 clauses
% :- ensure_loaded('semRulesDRT.pl'). %  presupDRT 0.00 sec, 33 clauses
% :- ensure_loaded('presupDRT.pl'). %  presupDRT 0.05 sec, 580 clauses
% :- ensure_loaded('worldKnowledge.pl'). %  worldKnowledge 0.00 sec, 5 clauses
% :- ensure_loaded('situationalKnowledge.pl'). %  situationalKnowledge 0.00 sec, 1 clauses
% :- ensure_loaded('backgroundKnowledge.pl'). %  backgroundKnowledge 0.00 sec, 31 clauses
% :- ensure_loaded('drs2fol.pl'). %  drs2fol 0.00 sec, 14 clauses
% :- ensure_loaded('curtPredicates.pl'). %  curtPredicates 0.00 sec, 26 clauses
% :- ensure_loaded('superSubDRT.pl'). %  superSubDRT 0.00 sec, 13 clauses
% :- ensure_loaded('curtPPDRT.pl').

swi_module(_,_).
swi_use_module(_,_).

:- swi_use_module(callInference,[callTPandMB/5]).

:- swi_use_module(readLine,[readLine/1]).

:- swi_use_module(comsemPredicates,[infix/0,
                                prefix/0,
                                memberList/2,
				printRepresentations/1]).

:- swi_use_module(presupDRT,[presupDRT/3]).

:- swi_use_module(backgroundKnowledge,[backgroundKnowledge/2]).

:- swi_use_module(drs2fol,[drs2fol/2]).

:- swi_use_module(curtPredicates,[curtHelp/0,
                              curtOutput/1,
                              updateInterpretations/1,
                              printInterpretations/1,
                              updateHistory/1,
                              clearHistory/0,
                              selectInterpretations/3]).

:- swi_use_module(superSubDRT,[superSubDrs/3]).


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
   interpretations(I), 
   findall(K,(memberList(rank(_,_,_,_,D,_),I),drs2fol(D,F),backgroundKnowledge(F,K)),L),
   printRepresentations(L).
   
curtUpdate([interpretations],[],run):- !,
   interpretations(I),
   printInterpretations(I).

curtUpdate([history],[],run):- !,
   history(H),
   printRepresentations(H).

curtUpdate(Input,Moves,run):-
   old(Old),
   presupDRT(Input,Old,Readings), !,
   updateHistory(Input),
   consistentReadings(Readings,0,Consistent),
   informativeReadings(Consistent,Old,Ranking),   
   localConstraints(Ranking,FinalRanking),
   sort(FinalRanking,SortedRanking),
   updateInterpretations(SortedRanking),
   ( SortedRanking=[rank(1,_,_,_,_,_)|_], !, Moves=[contradiction];
     SortedRanking=[rank(0,1,_,_,_,_)|_], !, Moves=[obvious];
     Moves=[accept] ).

curtUpdate(_,[noparse],run).


/*========================================================================
   Old DRS
========================================================================*/

old(drs([],[])):-
   interpretations([]).

old(Old):-
   interpretations([rank(0,_,_,_,Old,_)|_]).

old(drs([],[])):-
   interpretations([rank(1,_,_,_,_,_)|_]).


/*========================================================================
   Select Consistent Readings
========================================================================*/

consistentReadings([],_,[]).

consistentReadings([B|DRSs],ParseRank,Rank):-
   (
      consistent(B,Model), !,
      Rank=[rank(0,ParseRank,B,Model)|R] 
   ;
      Rank=[rank(1,ParseRank,B,model([],[]))|R] 
   ),
   Parse is ParseRank + 1,
   consistentReadings(DRSs,Parse,R).


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

informativeReadings([rank(Cons,Parse,New,Model)|R1],Old,R2):-
   (    
      Cons=0,
      informative(Old,New), !,
      R2=[rank(Cons,0,Parse,New,Model)|R3] 
   ;
      Cons=0,
      R2=[rank(Cons,1,Parse,New,Model)|R3] 
   ;
      Cons=1,
      R2=[rank(Cons,0,Parse,New,Model)|R3] 
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
   Local Constraints
========================================================================*/

localConstraints([],[]).

localConstraints([rank(Cons,Info,Parse,B,M)|L1],[rank(Cons,Info,Score,Parse,B,M)|L2]):-
   Cons=1,
   Score=0,
   localConstraints(L1,L2).

localConstraints([rank(Cons,Info,Parse,B,M)|L1],[rank(Cons,Info,Score,Parse,B,M)|L2]):-
   Cons=0,
   findall((Super,Sub),superSubDrs(B,drs([],[])-Super,Sub),List),
   allLocal(List,0-Score),
   localConstraints(L1,L2).


/*========================================================================
   Local Constraints call theorem prover and model builder
========================================================================*/

allLocal([],Score-Score).

allLocal([(drs([],[]),_Sub)|Others],Score1-Score2):-!,
   allLocal(Others,Score1-Score2).

allLocal([(Super,Sub)|Others],Score1-Score4):-
   drs2fol(drs([],[imp(Super,Sub)]),Psi),
   drs2fol(drs([],[imp(Super,drs([],[not(Sub)]))]),Phi),
   backgroundKnowledge(Psi,BK),
   DomainSize=15,
   callTPandMB(imp(BK,Psi),not(imp(BK,Psi)),DomainSize,Model1,Engine1),
   format('~nMessage (local informativeness checking): ~p found a result.',[Engine1]),
   callTPandMB(imp(BK,Phi),not(imp(BK,Phi)),DomainSize,Model2,Engine2),
   format('~nMessage (local consistency checking): ~p found a result.',[Engine2]),
   (  Model1=model([],[]), Score2 is Score1 + 1, !; Score2 is Score1 ),
   (  Model2=model([],[]), Score3 is Score2 + 1, !; Score3 is Score2 ),
   allLocal(Others,Score3-Score4).


/*========================================================================
   Info
========================================================================*/

info4:-
   format('~n> ---------------------------------------------------------- <',[]),
   format('~n> curtPPDRT.pl, by Patrick Blackburn and Johan Bos            <',[]),
   format('~n>                                                            <',[]),
   format('~n> ?- curt.                - start a dialogue with Curt       <',[]),
   format('~n>                                                            <',[]),
   format('~n> Type "help" to get more information about features         <',[]),
   format('~n> ---------------------------------------------------------- <',[]),
   format('~n~n',[]).



/*========================================================================
   Display info at start
========================================================================*/

% :- info4.

 



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


:- swi_module(curtPredicates,[curtHelp/0,
                          curtOutput/1,
                          updateInterpretations/1,
                          updateHistory/1,
                          clearHistory/0,
                          list2string/2,
                          printInterpretations/1,
                          selectInterpretations/3]).

:- swi_use_module(comsemPredicates,[appendLists/3]).


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

/*************************************************************************

    File: sortalCheck.pl
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

:- swi_module(sortalCheck,[sortalCheckDrs/1]).

:- swi_use_module(comsemPredicates,[compose/3,
                                memberList/2,
                                basicCondition/3]).

:- swi_use_module(lexicalKnowledge,[lexicalKnowledge/3]).


/*========================================================================
   Sortal Check (DRSs)
========================================================================*/

sortalCheckDrs(drs([],C)):-
   sortalCheckConds(C).

sortalCheckDrs(drs([X|D],C)):-
   sortalCheckRef(C,X,[]),
   sortalCheckDrs(drs(D,C)).

sortalCheckDrs(merge(B1,B2)):-
   sortalCheckDrs(B1),
   sortalCheckDrs(B2).

sortalCheckDrs(alfa(_,B1,B2)):-
   sortalCheckDrs(B1),
   sortalCheckDrs(B2).


/*========================================================================
   Sortal Check (DRS-Conditions)
========================================================================*/

sortalCheckConds([]).

sortalCheckConds([Cond:_|C]):-
   sortalCheckConds([Cond|C]).

sortalCheckConds([not(B)|C]):-
   sortalCheckDrs(B),
   sortalCheckConds(C).

sortalCheckConds([imp(B1,B2)|C]):-
   sortalCheckDrs(B1),
   sortalCheckDrs(B2),
   sortalCheckConds(C).

sortalCheckConds([or(B1,B2)|C]):-
   sortalCheckDrs(B1),
   sortalCheckDrs(B2),
   sortalCheckConds(C).

sortalCheckConds([Basic|C]):-
   basicCondition(Basic,_,_),
   sortalCheckConds(C).


/*========================================================================
   Sortal Check (referents)
========================================================================*/

sortalCheckRef([],_,L):-
   consistent(L).

sortalCheckRef([Cond:_|C],Ref,L):-
   sortalCheckRef([Cond|C],Ref,L).

sortalCheckRef([not(_)|C],Ref,L):-
   sortalCheckRef(C,Ref,L).

sortalCheckRef([or(_,_)|C],Ref,L):-
   sortalCheckRef(C,Ref,L).

sortalCheckRef([imp(_,_)|C],Ref,L):-
   sortalCheckRef(C,Ref,L).

sortalCheckRef([Basic|C],Ref,L):-
   basicCondition(Basic,Symbol,[X]),
   (
      X==Ref, 
      sortalCheckRef(C,Ref,[Symbol:1|L])
   ;
      \+ X==Ref,
      sortalCheckRef(C,Ref,L)
   ).

sortalCheckRef([eq(X,Y)|C],Ref,L):-
   (
      var(X), 
      var(Y),
      sortalCheckRef(C,Ref,L)
   ;  
      var(X),
      atom(Y),
      X==Ref, 
      sortalCheckRef(C,Ref,[Y:0|L])
   ;  
      var(X),
      atom(Y),
      \+ X==Ref, 
      sortalCheckRef(C,Ref,L)
   ;
      atom(X),
      var(Y),
      Y==Ref, 
      sortalCheckRef(C,Ref,[X:0|L])
   ;
      atom(X),
      var(Y),
      \+ Y==Ref, 
      sortalCheckRef(C,Ref,L)
   ;
      atom(X),
      atom(Y),
      sortalCheckRef(C,Ref,L)
   ).

sortalCheckRef([Basic|C],Ref,L):-
   basicCondition(Basic,Sym,Args),
   \+ Sym = eq,
   \+ Args = [_],
   sortalCheckRef(C,Ref,L).



/*========================================================================
   Consistency Check
========================================================================*/

consistent(L1):- 
   addSuperConcepts(L1,L2),
   \+ conflict(L2).


addSuperConcepts(C1,C2):-
   addSuperConcepts(C1,[],C3),
   (
      length(C1,Len),
      length(C3,Len), !,
      C3=C2
   ;
      addSuperConcepts(C3,C2)
   ).
   

addSuperConcepts([],L,L).

addSuperConcepts([X:1|L1],Accu,L2):-
   lexicalKnowledge(X,1,Axiom),
   Axiom = all(A,imp(Concept,SuperConcept)),
   basicCondition(Concept,X,[A]),
   basicCondition(SuperConcept,Y,[A]), 
   \+ memberList(Y:1,L1),
   \+ memberList(Y:1,Accu),
   !,
   addSuperConcepts(L1,[X:1,Y:1|Accu],L2).

addSuperConcepts([X:0|L1],Accu,L2):-
   lexicalKnowledge(X,0,Axiom),
   Axiom = all(A,imp(eq(A,X),SuperConcept)),
   basicCondition(SuperConcept,Y,[A]), 
   \+ memberList(Y:1,L1),
   \+ memberList(Y:1,Accu),
   !,
   addSuperConcepts(L1,[X:0,Y:1|Accu],L2).

addSuperConcepts([X|L1],Accu,L2):-
   addSuperConcepts(L1,[X|Accu],L2).


conflict(L):-
   memberList(X:1,L),
   lexicalKnowledge(X,1,Axiom),
   Axiom = all(A,imp(Concept,not(SuperConcept))),
   basicCondition(Concept,X,[A]),
   basicCondition(SuperConcept,Y,[A]),
   memberList(Y:1,L). 
   


/*************************************************************************

    File: printDrs.pl
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

/*========================================================================
     Module Declaration
========================================================================*/

:- swi_module(printDrs,[printDrs/1]).

:- swi_use_module(comsemPredicates,[appendLists/3]).


/*========================================================================
     Counter for discourse referents
========================================================================*/

:- dynamic counter/1.

counter(0).
   

/*========================================================================
     Print Predicates
========================================================================*/

printDrs(Drs):- 
   retract(counter(_)), 
   assert(counter(1)),
   \+ \+ (formatDrs(Drs,Lines,_), 
          printDrsLines(Lines)).


/*========================================================================
     Print DRS Lines
========================================================================*/

printDrsLines([]):- nl.

printDrsLines([Line|Rest]):-
   name(L,Line), 
   nl, write(L),
   printDrsLines(Rest).


/*========================================================================
     Format DRSs
========================================================================*/

formatDrs(drs(Dom,Cond),[[32|Top],Refs2,[124|Line]|CondLines2],Length):-
   formatConds(Cond,[]-CondLines1,0-CondLength),
   formatRefs(Dom,Refs),
   length([_,_|Refs],RefLength),
   (RefLength > CondLength, !, Length = RefLength ; Length = CondLength), 
   closeConds(CondLines1,CondLines2,Length),
   Dif is (Length - RefLength) + 1,
   closeLine([124|Refs],Refs2,Dif,[124]),
   formatLine(95,Length,[32]-Top),
   formatLine(45,Length,[124]-Line).

formatDrs(merge(Drs1,Drs2),Lines3,Length):-
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M1 is N1 + 3,
   M2 is N2 + 3,
   combLinesDrs(Lines1,Lines2,Lines3,';',M1,M2),
   Length is N1 + N2 + 4.

formatDrs(alfa(_,Drs1,Drs2),Lines3,Length):-
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M1 is N1 + 3,
   M2 is N2 + 3,
   combLinesDrs(Lines1,Lines2,Lines3,'A',M1,M2),
   Length is N1 + N2 + 4.


/*========================================================================
     Format Discourse Referents
========================================================================*/

formatRefs([],[]).

formatRefs([Ref|Rest],Out):-
   makeConstant(Ref,Code), 
   formatRefs(Rest,Temp),
   appendLists([32|Code],Temp,Out).


/*========================================================================
   Turn a discourse referent into a Prolog constant
========================================================================*/

makeConstant(X,Code):- 
   atomic(X),
   name(X,Code).

makeConstant(X,[120|Codes]):-
   nonvar(X),
   X =.. ['$VAR',Number],
   atomic(Number),
   name(Number,Codes).

makeConstant(X,[C|Odes]):-
   nonvar(X),
   X =.. ['$VAR',[C|Odes]].

makeConstant(X,[120|Number]):- 
   var(X),
   retract(counter(N)),
   name(N,Number), 
   name(X,[120|Number]),
   M is N+1,
   assert(counter(M)).


/*========================================================================
     Format a Line
========================================================================*/

formatLine(_,1,L-L):- !.

formatLine(Code,N,In-[Code|Out]):-
   M is N - 1, 
   formatLine(Code,M,In-Out).


/*========================================================================
     Formatting Conditions
========================================================================*/

formatConds([],L-L,N-N).

formatConds([X:_|Rest],L1-L2,N1-N2):- !,
   formatConds([X|Rest],L1-L2,N1-N2).

formatConds([imp(Drs1,Drs2)|Rest],L1-L2,N0-N4):-!,
   formatConds(Rest,L1-Lines,N0-N3),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 7,
   combLinesConds(Lines1,Lines2,Lines3,' ==> ',M),
   appendLists(Lines3,Lines,L2),
   Length is N1 + N2 + 10,
   (Length > N3, !, N4 = Length; N4 = N3).

formatConds([or(Drs1,Drs2)|Rest],L1-L2,N0-N4):-!,
   formatConds(Rest,L1-Lines,N0-N3),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 5,
   combLinesConds(Lines1,Lines2,Lines3,' V ',M),
   appendLists(Lines3,Lines,L2),
   Length is N1 + N2 + 8,
   (Length > N3, !, N4 = Length; N4 = N3).

formatConds([not(Drs)|Rest],L1-L2,N0-N3):-!,
   formatConds(Rest,L1-Lines,N0-N1),
   formatDrs(Drs,[A,B,C,D|Lines1],N2),
   combLinesConds2([],Lines1,Lines2,5,''),
   appendLists([[124,32,32,32,32,32|A],
                [124,32,32,32,32,32|B],
                [124,32,95,95,32,32|C],
                [124,32,32,32,124,32|D]|Lines2],Lines,L2),
   Length is N2 + 8,
   (Length > N1, !, N3 = Length; N3 = N1).

formatConds([eq(A,B)|Rest],In-[[124,32|Line]|Out],N0-N2):-!,
   formatConds(Rest,In-Out,N0-N1),
   makeConstant(A,L1),
   makeConstant(B,L2),
   appendLists(L1,[32,61,32|L2],Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([Basic|Rest],In-[[124,32|Line]|Out],N0-N2):-
   formatConds(Rest,In-Out,N0-N1),
   formatBasic(Basic,Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).


/*========================================================================
     Formatting Basic Conditions
========================================================================*/

formatBasic(Basic,Line):-
   Basic =.. [Functor,Arg],
   name(Functor,F),
   makeConstant(Arg,A),   
   appendLists(F,[40|A],T),
   appendLists(T,[41],Line).
   
formatBasic(Basic,Line):-
   Basic =.. [Functor,Arg1,Arg2],
   name(Functor,F),
   makeConstant(Arg1,A1),
   makeConstant(Arg2,A2),
   appendLists(F,[40|A1],T1),
   appendLists(T1,[44|A2],T2),
   appendLists(T2,[41],Line).
 
formatBasic(Basic,Line):-
   Basic =.. [Functor,Arg1,Arg2,Arg3],
   name(Functor,F),
   makeConstant(Arg1,A1),
   makeConstant(Arg2,A2),
   makeConstant(Arg3,A3),
   appendLists(F,[40|A1],T1),
   appendLists(T1,[44|A2],T2),
   appendLists(T2,[44|A3],T3),
   appendLists(T3,[41],Line).


/*========================================================================
   Combining Lines of Characters (Complex DRS-Conditions)
========================================================================*/
    
combLinesConds([A1,B1,C1,D1|Rest1],[A2,B2,C2,D2|Rest2],Result,Op,N):-
   combLinesConds2([A1,B1,C1],[A2,B2,C2],Firsts,N,Op),
   name(Op,Code),
   appendLists(Code,D2,T),
   appendLists([124,32|D1],T,D),
   combLinesConds2(Rest1,Rest2,Rest,N,Op),
   appendLists(Firsts,[D|Rest],Result).


/*========================================================================
   Combining Lines of Characters (Complex DRS-Conditions)
========================================================================*/

combLinesConds2([],[],[],_,_).

combLinesConds2([],[A2|Rest2],[A|Rest],N,Op):-
   closeLine([124],A1,N,[]),
   appendLists(A1,A2,A),
   combLinesConds2([],Rest2,Rest,N,Op).

combLinesConds2([A1|Rest1],[],[[124,32|A1]|Rest],N,Op):-
   combLinesConds2(Rest1,[],Rest,N,Op).

combLinesConds2([A1|Rest1],[A2|Rest2],[A|Rest],N,' ==> '):-
   appendLists([124,32|A1],[32,32,32,32,32|A2],A),
   combLinesConds2(Rest1,Rest2,Rest,N,' ==> ').

combLinesConds2([A1|Rest1],[A2|Rest2],[A|Rest],N,' V '):-
   appendLists([124,32|A1],[32,32,32|A2],A),
   combLinesConds2(Rest1,Rest2,Rest,N,' V ').


/*========================================================================
   Combining Lines of Characters (Complex DRSs)
========================================================================*/
    
combLinesDrs([A1,B1,C1,D1|Rest1],[A2,B2,C2,D2|Rest2],Result,Op,N1,N2):-
   combLinesDrs([A1,B1,C1],[A2,B2,C2],Firsts,N1,N2),
   name(Op,Code),
   appendLists(Code,D2,T1),
   appendLists(T1,[41],T2),
   appendLists([40|D1],T2,D),
   combLinesDrs(Rest1,Rest2,Rest,N1,N2),
   appendLists(Firsts,[D|Rest],Result).

combLinesDrs([],[],[],_,_).

combLinesDrs([],[A2|Rest2],[A3|Rest],N1,N2):-
   closeLine([],A1,N1,[]),
   appendLists(A1,A2,A0),
   appendLists(A0,[32],A3),
   combLinesDrs([],Rest2,Rest,N1,N2).

combLinesDrs([A1|Rest1],[],[Closed|Rest],N1,N2):-
   combLinesDrs(Rest1,[],Rest,N1,N2),
   closeLine([32|A1],Closed,N2,[]).

combLinesDrs([A1|Rest1],[A2|Rest2],[A3|Rest],N1,N2):-
   appendLists([32|A1],[32|A2],A0),
   appendLists(A0,[32],A3),
   combLinesDrs(Rest1,Rest2,Rest,N1,N2).


/*========================================================================
     Close Conditions (add '|')
========================================================================*/

closeConds([],[[124|Bottom]],Length):-
   formatLine(95,Length,[124]-Bottom).

closeConds([Line|Rest1],[New|Rest2],Length):-
   length(Line,L),
   R is Length - L,
   closeLine(Line,New,R,[124]),
   closeConds(Rest1,Rest2,Length).


/*========================================================================
     Close Line
========================================================================*/

closeLine(Line,New,N,Accu):- 
   N < 1, !, 
   appendLists(Line,Accu,New).

closeLine(Line,New,N,Accu):- 
   M is N - 1, 
   closeLine(Line,New,M,[32|Accu]).



% :- ensure_loaded('comsemPredicates.pl'). %  comsemPredicates 0.02 sec, 221 clauses

/*************************************************************************

    File: comsemPredicates.pl
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

:- swi_module(comsemPredicates,
          [appendLists/3,
	   basicCondition/3,
	   basicFormula/1,
           compose/3,
	   concatStrings/2,
	   executeCommand/1,
           infix/0,
           memberList/2,
	   newFunctionCounter/1,
           prefix/0,
           printRepresentations/1,
	   proveOnce/1,
	   removeFirst/3,
	   removeDuplicates/2,
           reverseList/2,
	   selectFromList/3,
	   simpleTerms/1,
           substitute/4,
	   unify/2, 
	   unionSets/3,
	   variablesInTerm/2]).

:- swi_use_module(printDrs,[printDrs/1]).


/*========================================================================
   Appending two lists
========================================================================*/

appendLists([],List,List).
appendLists([X|Tail1],List,[X|Tail2]):- 
   appendLists(Tail1,List,Tail2).


/*========================================================================
   List membership
========================================================================*/

memberList(X,[X|_]).

memberList(X,[_|Tail]):- 
   memberList(X,Tail).

 
/*========================================================================
   Reversing Items in a List
========================================================================*/
 
reverseList(A,B):-
   reverseList(A,[],B).
 
reverseList([],L,L).
 
reverseList([X|L1],L2,L3):-
   reverseList(L1,[X|L2],L3).


/*========================================================================
   Selecting (i.e. removing) a member of a list
========================================================================*/

selectFromList(X,[X|L],L).

selectFromList(X,[Y|L1],[Y|L2]):-
   selectFromList(X,L1,L2).


/*========================================================================
   Removing first member of a list
========================================================================*/

removeFirst(X,[X|Tail],Tail) :- !.

removeFirst(X,[Head|Tail],[Head|NewTail]):-
   removeFirst(X,Tail,NewTail).


/*========================================================================
   Remove Duplicates
========================================================================*/

removeDuplicates([],[]).

removeDuplicates([X|L],Pruned):-
	memberList(Y,L), X==Y, !,
	removeDuplicates(L,Pruned).

removeDuplicates([X|L],[X|Pruned]):-
	removeDuplicates(L,Pruned).


/*========================================================================
   Union of two sets (disallowing unification of list elements)
========================================================================*/

unionSets([],L,L).

unionSets([X|L1],L2,L3):-
   memberList(Y,L2), 
   X==Y, !,
   unionSets(L1,L2,L3).

unionSets([X|L1],L2,[X|L3]):-
   unionSets(L1,L2,L3).


/*========================================================================
   Simple Terms
========================================================================*/

simpleTerms([]).

simpleTerms([X|Rest]):-
   simpleTerm(X),
   simpleTerms(Rest).

simpleTerm(T):-
   (
    var(T)
   ;   
    atomic(T)
   ;
    nonvar(T),
    functor(T,'$VAR',1) 
   ;
    nonvar(T),
    functor(T,fun,_)
   ).


/*========================================================================
   Compose predicate argument structure
========================================================================*/

compose(Term,Symbol,ArgList):-
    Term =.. [Symbol|ArgList].


/*========================================================================
   Basic Formula Syntax
========================================================================*/

basicFormula(F):-
   var(F), !, fail.

basicFormula(F):-
   compose(F,Symbol,Args),
   \+ memberList(Symbol,[not,and,imp,app,or,some,all,lam]),
   simpleTerms(Args).
		

/*========================================================================
   Basic DRT Condition Syntax
========================================================================*/

basicCondition(C,_,_):-
   var(C), !, fail.

basicCondition(C,Symbol,Args):-
   compose(C,Symbol,Args),
   \+ memberList(Symbol,[not,imp,app,or,lam,drs,merge]),
   simpleTerms(Args).
		

/*========================================================================
   Collect all occurrences of variables in Term to a difference list
========================================================================*/

variablesInTerm(Term,Var1-Var2):-
   compose(Term,_,Args),
   countVar(Args,Var1-Var2).

countVar([],Var-Var).
countVar([X|Rest],Var1-Var2):-
   var(X),!,
   countVar(Rest,[X|Var1]-Var2).
countVar([X|Rest],Var1-Var3):-
   variablesInTerm(X,Var1-Var2),
   countVar(Rest,Var2-Var3).


/*========================================================================
   Unify with Occurs Check
========================================================================*/

unify(X,Y):-
   var(X), 
   var(Y), !, 
   X=Y.

unify(X,Y):-
   var(X), 
   nonvar(Y), !,
   notOccursIn(X,Y), 
   X=Y.

unify(X,Y):-
   var(Y), 
   nonvar(X), !,
   notOccursIn(Y,X), 
   X=Y.

unify(X,Y):-
   nonvar(X), 
   nonvar(Y), 
   atomic(X),   
   atomic(Y), !,
   X=Y.

unify(X,Y):-
   nonvar(X), 
   nonvar(Y), 
   compound(X), 
   compound(Y), !,
   termUnify(X,Y).


/*========================================================================
   The Occurs Check
========================================================================*/

notOccursIn(X,Term):-
   var(Term), X \== Term.
notOccursIn(_,Term):-
   nonvar(Term), atomic(Term).
notOccursIn(X,Term):-
   nonvar(Term), compound(Term),
   functor(Term,_,Arity), notOccursInComplexTerm(Arity,X,Term).

notOccursInComplexTerm(N,X,Y):-
   N > 0, arg(N,Y,Arg), notOccursIn(X,Arg),
   M is N - 1, notOccursInComplexTerm(M,X,Y).
notOccursInComplexTerm(0,_,_).


/*========================================================================
   Unify Terms
========================================================================*/

termUnify(X,Y):-
   functor(X,Functor,Arity), functor(Y,Functor,Arity),
   unifyArgs(Arity,X,Y).

unifyArgs(N,X,Y):-
   N > 0, M is N - 1,
   arg(N,X,ArgX), arg(N,Y,ArgY), 
   unify(ArgX,ArgY), unifyArgs(M,X,Y).
unifyArgs(0,_,_).


/*========================================================================
   Substitution Predicates
========================================================================*/

substitute(Term,Var,Exp,Result):- 
   Exp==Var, !, Result=Term.

substitute(_Term,_Var,Exp,Result):- 
   \+ compound(Exp), !, Result=Exp.

substitute(Term,Var,Formula,Result):-
   compose(Formula,Functor,[Exp,F]),
   memberList(Functor,[lam,all,some]), !, 
   (
    Exp==Var, !, 
    Result=Formula
   ; 
    substitute(Term,Var,F,R),
    compose(Result,Functor,[Exp,R])
   ).

substitute(Term,Var,Formula,Result):-
   compose(Formula,Functor,ArgList),
   substituteList(Term,Var,ArgList,ResultList),
   compose(Result,Functor,ResultList).

substituteList(_Term,_Var,[],[]).
substituteList(Term,Var,[Exp|Others],[Result|ResultOthers]):-
   substitute(Term,Var,Exp,Result),
   substituteList(Term,Var,Others,ResultOthers).


/*========================================================================
   Skolem Function Counter
========================================================================*/

:- dynamic(functionCounter/1).

functionCounter(1).

newFunctionCounter(N):-
   functionCounter(N), M is N+1,
   retract(functionCounter(N)),
   asserta(functionCounter(M)).


/*========================================================================
   Printing a set of representations
========================================================================*/

printRepresentations(Readings):-
   printRep(Readings,0).

printRep([],_):- nl.
printRep([Reading|OtherReadings],M):-
   N is M + 1, nl, write(N), tab(1), 
   \+ \+ (numbervars(Reading,0,_), print(Reading)),
   printRep(OtherReadings,N).


/*========================================================================
   Concatenate Strings
========================================================================*/

concatStrings(L,S):-
   concatStrings(L,[],S).

concatStrings([],Codes,String):- 
   name(String,Codes).

concatStrings([X|L],Codes1,String):-
   name(X,Codes2),
   appendLists(Codes1,Codes2,Codes3),
   concatStrings(L,Codes3,String).
   

/*========================================================================
   Prove a goal only once
========================================================================*/

proveOnce(Goal):- call(Goal), !.


/*========================================================================
   Prolog Dialect Detection
========================================================================*/

prologDialect(Dialect):-
   (
    predicate_property(version,Property),
    Property=built_in, !,
    Dialect=sicstus            % Probably Sicstus Prolog
   ;
    predicate_property(shell(_,_),Property),
    Property=interpreted, !,
    Dialect=swi                % Probably SWI Prolog
   ;
    Dialect=unknown
   ).


/*========================================================================
   Execute Operating System Command
========================================================================*/

executeCommand(A):-
   shell(A,_).
   

/*========================================================================
   Load Sicstus library if required
========================================================================*/

load_shell:-
   prologDialect(sicstus), !,
   swi_use_module(library(system),[shell/2]).

load_shell.


/*========================================================================
   Load Shell
========================================================================*/

:- load_shell.


/*========================================================================
   Prefix/Infix Switch
========================================================================*/
  
:- dynamic bbmode/1.
 
bbmode(prefix).
 
infix:- retractall(bbmode(_)), assert(bbmode(infix)).
prefix:- retractall(bbmode(_)), assert(bbmode(prefix)).

 
/*========================================================================
   Portray B&B Syntax
========================================================================*/
:- thread_local(t_l:inCURT).

user:portray(E):- \+ atomic(E), t_l:inCURT,  bb_portray(E).

bb_portray(E):- bbmode(prefix), !, write(E).
bb_portray(not(F)):- write('~ '), print(F).
bb_portray(and(F1,F2)):- write('('), print(F1), write(' & '), print(F2), write(')').
bb_portray(imp(F1,F2)):- write('('), print(F1), write(' > '), print(F2), write(')').
bb_portray(app(F1,F2)):- write('('), print(F1), write(' @ '), print(F2), write(')').
bb_portray(or(F1,F2)):- write('('), print(F1), write(' v '), print(F2), write(')').
bb_portray(some(X,F)):- write('some '), write(X), tab(1), print(F).
bb_portray(all(X,F)):- write('all '), write(X), tab(1), print(F).
bb_portray(lam(X,F)):- write('lam '), write(X), tab(1), print(F).
bb_portray(eq(X,Y)):- print(X), write(' = '), print(Y).
bb_portray(que(X,R,B)):- write('?'), print(X), write('['), print(R), write(','), print(B), write(']').
bb_portray(drs(D,C)):- printDrs(drs(D,C)).
bb_portray(merge(B,C)):- printDrs(merge(B,C)).
bb_portray(alfa(A,B,C)):- printDrs(alfa(A,B,C)).
bb_portray(model(A,B)):- printModel(model(A,B)).


/*========================================================================
   Print Model
========================================================================*/

printModel(model(Dom,Ext)):-
   write('D='),write(Dom),nl,
   printExtensions(Ext).

printExtensions([]).

printExtensions([X|L]):-
   tab(2), write(X), nl,
   printExtensions(L).


/*************************************************************************

    File: fol2otter.pl
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

:- swi_module(fol2otter,[fol2otter/2,fol2mace/2]).

:- swi_use_module(comsemPredicates,[basicFormula/1]).


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
   format(Stream,'(exists ~p ',[X]),
   printOtter(Stream,Formula,Tab),
   write(Stream,')').

printOtter(Stream,all(X,Formula),Tab):- 
   format(Stream,'(all ~p ',[X]),
   printOtter(Stream,Formula,Tab),
   write(Stream,')').

printOtter(Stream,que(X,Formula1,Formula2),Tab):- 
   format(Stream,'(exists ~p ',[X]),
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

printOtter(Stream,eq(X,Y),Tab):- 
   write(Stream,'('),  
   printOtter(Stream,X,Tab),
   write(Stream,' = '),
   printOtter(Stream,Y,Tab),
   write(Stream,')').

printOtter(Stream,Phi,_):-
   basicFormula(Phi),
   write(Stream,Phi).


/*************************************************************************

    File: fol2bliksem.pl
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

:- swi_module(fol2bliksem,[fol2bliksem/2]).

:- swi_use_module(comsemPredicates,[basicFormula/1]).


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
   format(Stream,'(< ~p >',[X]),
   printBliksem(Stream,Formula,Tab),
   write(Stream,')').

printBliksem(Stream,all(X,Formula),Tab):- 
   format(Stream,'([ ~p ]',[X]),
   printBliksem(Stream,Formula,Tab),
   write(Stream,')').

printBliksem(Stream,que(X,Formula1,Formula2),Tab):- 
   format(Stream,'(< ~p >',[X]),
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
   format(Stream,'( ~p = ~p )',[X,Y]).  

printBliksem(Stream,Phi,_):-
   basicFormula(Phi),
   write(Stream,Phi).

/*************************************************************************

    File: fol2tptp.pl
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

:- swi_module(fol2tptp,[fol2tptp/2]).

:- swi_use_module(comsemPredicates,[basicFormula/1]).

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
   write(Stream,X),
   write(Stream,']: '),
   printTptp(Formula,Stream),
   write(Stream,')').

printTptp(que(X,Formula),Stream):- !,
   write(Stream,'(? ['),
   write(Stream,X),
   write(Stream,']: '),
   printTptp(Formula,Stream),
   write(Stream,')').

printTptp(all(X,Formula),Stream):- !,
   write(Stream,'(! ['),
   write(Stream,X),
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
   write(Stream,equal(X,Y)).

printTptp(Phi,Stream):-
   basicFormula(Phi),
   write(Stream,Phi).

/*************************************************************************

    File: callInference.pl
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

:- swi_module(callInference,[callTPandMB/5]).

:- swi_use_module(fol2otter,[fol2otter/2,fol2mace/2]).

:- swi_use_module(fol2bliksem,[fol2bliksem/2]).

:- swi_use_module(fol2tptp,[fol2tptp/2]).

:- swi_use_module(comsemPredicates,[appendLists/3,
     			        memberList/2,
                                selectFromList/3,
                                executeCommand/1,
				concatStrings/2]).


/*========================================================================
   Choose Inference Engines
========================================================================*/

inferenceEngines([mace,bliksem]).
%inferenceEngines([paradox,bliksem]).
%inferenceEngines([paradox,otter]).
%inferenceEngines([mace,otter]).


/*========================================================================
   Initialise Theorem Provers
========================================================================*/

initTheoremProvers([],_).

initTheoremProvers([otter|L],Formula):- !,
   open('otter.in',write,Stream),
   fol2otter(not(Formula),Stream),
   close(Stream),
   initTheoremProvers(L,Formula).

initTheoremProvers([bliksem|L],Formula):- !,
   open('bliksem.in',write,Stream),
   fol2bliksem(not(Formula),Stream),
   close(Stream),
   executeCommand('dos2unix bliksem.in'), %%% use for MS-DOS/Windows
   initTheoremProvers(L,Formula).

initTheoremProvers([_|L],Formula):-
   initTheoremProvers(L,Formula).


/*========================================================================
   Initialise Model Builders
========================================================================*/

initModelBuilders([],_).

initModelBuilders([mace|L],Formula):- !,
   open('mace.in',write,Stream),
   fol2mace(Formula,Stream),
   close(Stream),
   initModelBuilders(L,Formula).

initModelBuilders([paradox|L],Formula):- !,
   open('paradox.in',write,Stream),
   fol2tptp(not(Formula),Stream),
   close(Stream),
   initModelBuilders(L,Formula).

initModelBuilders([_|L],Formula):-
   initModelBuilders(L,Formula).


/*========================================================================
   Calls to Theorem Provers and Model Builders
========================================================================*/

callTPandMB(TPProblem,MBProblem,DomainSize,Model,Engine):-
   executeCommand('chmod 700 interfaceTPandMB.perl'),
   number(DomainSize),
   inferenceEngines(Engines),
   initTheoremProvers(Engines,TPProblem),
   initModelBuilders(Engines,MBProblem),
   concatStrings(['perl ./interfaceTPandMB.perl ',DomainSize,' '|Engines],Shell),        
   executeCommand(Shell),
   open('tpmb.out',read,Out),
   read(Out,Result),
   (
      Result=proof, !, 
      read(Out,engine(Engine)),
      Model=model([],[])
   ;
      Result=interpretation(_,_),
      read(Out,engine(Engine)),
      mace2blackburnbos(Result,Model), !
   ;
      Result=paradox(_),
      read(Out,engine(Engine)),
      paradox2blackburnbos(Result,Model), !
   ;
      Model=unknown,
      Engine=unknown
   ),       
   close(Out).


/*========================================================================
   Translate Paradox-type Model into Blackburn & Bos Models
========================================================================*/

paradox2blackburnbos(Paradox,model(D,F)):-
   Paradox = paradox(Terms),
   paradox2d(Terms,[d1]-D),
   paradox2f(Terms,[]-F).

paradox2blackburnbos(Paradox,unknown):-
   \+ Paradox = paradox(_).


/*========================================================================
   Translate Paradox Terms to Domain
========================================================================*/

paradox2d([],D-D).

paradox2d([_Constant=Entity|L],D1-D2):-
   \+ memberList(Entity,D1), !,
   paradox2d(L,[Entity|D1]-D2).

paradox2d([Symbol:1|L],D1-D2):-
   functor(Symbol,_Functor,1),
   arg(1,Symbol,Entity),
   \+ memberList(Entity,D1), !,
   paradox2d(L,[Entity|D1]-D2).

paradox2d([Symbol:1|L],D1-D2):-
   functor(Symbol,_Functor,2),
   arg(1,Symbol,Entity1),
   arg(2,Symbol,Entity2),
   (
      \+ memberList(Entity1,D1), !,
      (
         \+ memberList(Entity2,D2), !,
         paradox2d(L,[Entity1,Entity2|D1]-D2)
      ;
         paradox2d(L,[Entity1|D1]-D2) 
      )
   ;
      \+ memberList(Entity2,D2), 
      paradox2d(L,[Entity2|D1]-D2) 
   ), !.

paradox2d([_|L],D1-D2):-
   paradox2d(L,D1-D2).


/*========================================================================
   Translate Paradox Terms to Interpretation Function
========================================================================*/

paradox2f([],F-F).

paradox2f([Constant=Entity|L],D1-D2):-
   Term = f(0,Constant,Entity),
   \+ memberList(Term,D1), !,
   paradox2f(L,[Term|D1]-D2).

paradox2f([Symbol:1|L],D1-D2):-
   functor(Symbol,Functor,1), !,
   arg(1,Symbol,Arg),
   (
      selectFromList(f(1,Functor,E),D1,D3), !,
      paradox2f(L,[f(1,Functor,[Arg|E])|D3]-D2)
   ;
      paradox2f(L,[f(1,Functor,[Arg])|D1]-D2)
   ).

paradox2f([Symbol:0|L],D1-D2):-
   functor(Symbol,Functor,1), !,
   (
      memberList(f(1,Functor,_),D1), !,
      paradox2f(L,D1-D2)
   ;
      paradox2f(L,[f(1,Functor,[])|D1]-D2)
   ).

paradox2f([Symbol:1|L],D1-D2):-
   functor(Symbol,Functor,2), !,
   arg(1,Symbol,Arg1),
   arg(2,Symbol,Arg2),
   (
      selectFromList(f(2,Functor,E),D1,D3), !,
      paradox2f(L,[f(2,Functor,[(Arg1,Arg2)|E])|D3]-D2)
   ;
      paradox2f(L,[f(2,Functor,[(Arg1,Arg2)])|D1]-D2)
   ).

paradox2f([Symbol:0|L],D1-D2):-
   functor(Symbol,Functor,2), !,
   (
      memberList(f(2,Functor,_),D1), !,
      paradox2f(L,D1-D2)
   ;
      paradox2f(L,[f(2,Functor,[])|D1]-D2)
   ).

paradox2f([_|L],D1-D2):-
   paradox2f(L,D1-D2).


/*========================================================================
   Translate Mace-type Model into Blackburn & Bos Models
========================================================================*/

mace2blackburnbos(Mace,model(D,F)):-
   Mace = interpretation(Size,Terms),
   mace2d(1,Size,D),
   mace2f(Terms,D,F).

mace2blackburnbos(Mace,unknown):-
   \+ Mace = interpretation(_Size,_Terms).


/*========================================================================
   Translate Mace Model to Domain
========================================================================*/

mace2d(N,N,[V]):-
	name(N,Codes),
	name(V,[100|Codes]).

mace2d(I,N,[V|D]):-
	I < N,
	name(I,Codes),
	name(V,[100|Codes]),
	J is I + 1,
	mace2d(J,N,D).


/*========================================================================
   Translate Mace Model to Interpretation Function
========================================================================*/

mace2f([],_,[]):- !.

mace2f([function(Skolem,_)|Terms],D,F):-
	\+ atom(Skolem), !,
	mace2f(Terms,D,F).

mace2f([function(Constant,[V])|Terms],D,[f(0,Constant,X)|F]):-
	atom(Constant), !,
	Index is V + 1,
	name(Index,Codes),
	name(X,[100|Codes]),
	mace2f(Terms,D,F).

mace2f([predicate(Relation,V)|Terms],D,[f(1,Functor,X)|F]):-
	Relation =.. [Functor,_], !,
	positiveValues(V,1,X),
	mace2f(Terms,D,F).

mace2f([predicate(Relation,V)|Terms],D,[f(2,Functor,X)|F]):-
	Relation =.. [Functor,_,_], !,
	length(D,Size),
	positivePairValues(V,Size,1,1,X),
	mace2f(Terms,D,F).

mace2f([_|Terms],D,F):-
	mace2f(Terms,D,F).


/*========================================================================
   Take positive values of one-place predicates
========================================================================*/

positiveValues([],_,[]).

positiveValues([1|Values],I1,[X|Rest]):-
	name(I1,Codes),
	name(X,[100|Codes]),
	I2 is I1 + 1,
	positiveValues(Values,I2,Rest).
		
positiveValues([0|Values],I1,Rest):-
	I2 is I1 + 1,
	positiveValues(Values,I2,Rest).
		

/*========================================================================
   Take positive values of two-place predicates
========================================================================*/

positivePairValues([],_,_,_,[]).

positivePairValues([1|Values],Size,I1,J1,[(X2,X1)|Rest]):-
	name(I1,Codes1),
	name(X1,[100|Codes1]),
	name(J1,Codes2),
	name(X2,[100|Codes2]),
	(
	    I1 < Size,
	    I2 is I1 + 1,
	    J2 is J1
	;   
	    I1 = Size,
	    I2 = 1,
	    J2 is J1 + 1
	),
	positivePairValues(Values,Size,I2,J2,Rest).

positivePairValues([0|Values],Size,I1,J1,Rest):-
	(
	    I1 < Size, 
	    I2 is I1 + 1,
	    J2 is J1
	;
	    I1 = Size,
	    I2 = 1,
	    J2 is J1 + 1
	),
	positivePairValues(Values,Size,I2,J2,Rest).


/*========================================================================
   Info
========================================================================*/

info1:-
   inferenceEngines(Engines),
   format('~n> ---------------------------------------------------------- <',[]),
   format('~n> callInference.pl, by Patrick Blackburn and Johan Bos       <',[]),
   format('~n>                                                            <',[]),
   format('~n> ?- callTP(Problem,Proof,Engine).                           <',[]),
   format('~n> ?- callMB(Problem,DomainSize,Model,Engine)                 <',[]),
   format('~n> ?- callTPandMB(TPProblem,MBProblem,Size,Model,Engine).     <',[]),
   format('~n>                                                            <',[]),
   format('~n> Selected Inference Engines (inferenceEngines/1)            <',[]),
   format('~n> ~p',[Engines]),
   format('~n> ---------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

% :- info1.

/*************************************************************************

    File: readLine.pl
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

:- swi_module(readLine,[readLine/1]).


/*========================================================================
   Output prompt, read from standard input and convert into list
========================================================================*/

readLine(WordList):-
   nl, write('> '),
   readWords(Words),
   checkWords(Words,WordList).


/*========================================================================
   Read in a sequence of characters, until a return is registered
========================================================================*/

readWords([Word|Rest]):-
   get0(Char),
   readWord(Char,Chars,State),
   name(Word,Chars),
   readRest(Rest,State).

readRest([],ended).

readRest(Rest,notended):-
   readWords(Rest).


/*========================================================================
   Read a word coded as Chars (a list of ascii values), starting 
   with with ascii value Char, and determine the State of input
   (`ended' = end of line, `notended' = not end of line).
   Blanks and full stops split words, a return ends a line.
========================================================================*/

readWord(32,[],notended):-!.     %%% blank

readWord(46,[],notended):-!.     %%% full stop

readWord(10,[],ended):-!.        %%% return

readWord(Code,[Code|Rest],State):-
   get0(Char),
   readWord(Char,Rest,State).


/*========================================================================
   Check if all words are unquoted atoms, if not convert them into atoms.
========================================================================*/

checkWords([],[]):- !.

checkWords([''|Rest1],Rest2):-
   checkWords(Rest1,Rest2).

checkWords([Atom|Rest1],[Atom2|Rest2]):-
   name(Atom,Word1),
   convertWord(Word1,Word2),
   name(Atom2,Word2),
   checkWords(Rest1,Rest2).


/*========================================================================
   Convert upper into lower case characters, and eliminate
   non-alphabetic characters.
========================================================================*/

convertWord([],[]):- !.

convertWord([Capital|Rest1],[Small|Rest2]):-
   Capital > 64, Capital < 91, !,
   Small is Capital + 32,
   convertWord(Rest1,Rest2).

convertWord([Number|Rest1],[Number|Rest2]):-
   Number > 47, Number < 58, !,
   convertWord(Rest1,Rest2).

convertWord([Weird|Rest1],Rest2):-
   (Weird < 97; Weird > 122), !,
   convertWord(Rest1,Rest2).

convertWord([Char|Rest1],[Char|Rest2]):-
   convertWord(Rest1,Rest2).

/*************************************************************************

    File: alphaConversionDRT.pl
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

:- swi_module(alphaConversionDRT,[alphaConvertDRS/2]).

:- swi_use_module(comsemPredicates,[compose/3,
				memberList/2]).


/*========================================================================
   Alpha Conversion (introducing substitutions)
========================================================================*/

alphaConvertDRS(B1,B2):-
	alphaConvertDRS(B1,[]-_,B2).


/*========================================================================
   Alpha Conversion (variables)
========================================================================*/

alphaConvertVar(X,Vars,New):-
   var(X), 
   (
      memberList(sub(Z,Y),Vars),
      X==Z, !,
      New=Y
   ;
      New=X
   ).


/*========================================================================
   Alpha Conversion (DRSs)
========================================================================*/

alphaConvertDRS(X1,Vars-Vars,X2):-
   var(X1), 
   alphaConvertVar(X1,Vars,X2).

alphaConvertDRS(Exp,Vars-Vars,lam(Y,B2)):-
   nonvar(Exp),
   Exp=lam(X,B1),
   alphaConvertDRS(B1,[sub(X,Y)|Vars]-_,B2).

alphaConvertDRS(Exp,Vars-Vars,drs([],[])):-
   nonvar(Exp),
   Exp=drs([],[]).

alphaConvertDRS(Exp,Vars1-Vars2,drs([],[C2|Conds2])):-
   nonvar(Exp),
   Exp=drs([],[C1|Conds1]),
   alphaConvertCondition(C1,Vars1,C2), 
   alphaConvertDRS(drs([],Conds1),Vars1-Vars2,drs([],Conds2)).

alphaConvertDRS(Exp,Vars1-Vars2,drs([New|L2],C2)):-
   nonvar(Exp),
   Exp=drs([Ref|L1],C1),
   alphaConvertDRS(drs(L1,C1),[sub(Ref,New)|Vars1]-Vars2,drs(L2,C2)).

alphaConvertDRS(Exp,Vars1-Vars3,alfa(Type,B3,B4)):-
   nonvar(Exp),
   Exp=alfa(Type,B1,B2),
   alphaConvertDRS(B1,Vars1-Vars2,B3),
   alphaConvertDRS(B2,Vars2-Vars3,B4).

alphaConvertDRS(Exp,Vars1-Vars3,merge(B3,B4)):-
   nonvar(Exp),
   Exp=merge(B1,B2),
   alphaConvertDRS(B1,Vars1-Vars2,B3),
   alphaConvertDRS(B2,Vars2-Vars3,B4).

alphaConvertDRS(Exp,Vars-Vars,app(E3,E4)):-
   nonvar(Exp),
   Exp=app(E1,E2),
   alphaConvertDRS(E1,Vars-_,E3),
   alphaConvertDRS(E2,Vars-_,E4).


/*========================================================================
   Alpha Conversion (DRS-Conditions)
========================================================================*/

alphaConvertCondition(not(B1),Vars,not(B2)):-
   alphaConvertDRS(B1,Vars-_,B2).

alphaConvertCondition(imp(B1,B2),Vars,imp(B3,B4)):-
    alphaConvertDRS(B1,Vars-Vars1,B3),
    alphaConvertDRS(B2,Vars1-_,B4).

alphaConvertCondition(or(B1,B2),Vars,or(B3,B4)):-
    alphaConvertDRS(B1,Vars-_,B3),
    alphaConvertDRS(B2,Vars-_,B4).

alphaConvertCondition(Cond1:F,Vars,Cond2:F):-
    alphaConvertCondition(Cond1,Vars,Cond2).

alphaConvertCondition(Cond1,Vars,Cond2):-
   \+ Cond1 = not(_),
   \+ Cond1 = imp(_,_),
   \+ Cond1 = or(_,_),
   \+ Cond1 = _:_,
   compose(Cond1,Symbol,Args1),
   alphaConvertList(Args1,Vars,Args2),
   compose(Cond2,Symbol,Args2).


/*========================================================================
   Alpha Conversion (listwise)
========================================================================*/

alphaConvertList([],_,[]).

alphaConvertList([X|L1],Vars,[Y|L2]):-
   var(X),
   alphaConvertVar(X,Vars,Y),
   alphaConvertList(L1,Vars,L2).

alphaConvertList([X|L1],Vars,[X|L2]):-
   atom(X),
   alphaConvertList(L1,Vars,L2).

/*************************************************************************

    File: betaConversionDRT.pl
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

:- swi_module(betaConversionDRT,[betaConvert/2]).

:- swi_use_module(comsemPredicates,[compose/3,substitute/4]).

:- swi_use_module(alphaConversionDRT,[alphaConvertDRS/2]).


/*========================================================================
   Beta-Conversion (introducing stack)
========================================================================*/

betaConvert(X,Y):-
   betaConvert(X,Y,[]).


/*========================================================================
   Beta-Conversion (core stuff)
========================================================================*/

betaConvert(X,Y,[]):-
   var(X),
   Y=X.

betaConvert(Expression,Result,Stack):- 
   nonvar(Expression),
   Expression = app(Functor,Argument),
   nonvar(Functor),
   alphaConvertDRS(Functor,Converted),
   betaConvert(Converted,Result,[Argument|Stack]).

betaConvert(Expression,Result,[X|Stack]):-
   nonvar(Expression),
   Expression = lam(X,Formula),
   betaConvert(Formula,Result,Stack).

betaConvert(Formula,Result,[]):-
   nonvar(Formula),
   \+ (Formula = app(X,_), nonvar(X)),
   compose(Formula,Functor,Formulas),
   betaConvertList(Formulas,ResultFormulas),
   compose(Result,Functor,ResultFormulas).


/*========================================================================
   Beta-Convert a list
========================================================================*/

betaConvertList([],[]).

betaConvertList([Formula|Others],[Result|ResultOthers]):-
   betaConvert(Formula,Result),
   betaConvertList(Others,ResultOthers).


/*========================================================================
   Info
========================================================================*/

info2:-
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n> betaConversion.pl, by Patrick Blackburn and Johan Bos               <',[]),
   format('~n>                                                                     <',[]),
   format('~n> ?- betaConvert(F,C).         - beta-convert a formula               <',[]),
   format('~n> ------------------------------------------------------------------- <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

% :- info2.


/*************************************************************************

    File: mergeDRT.pl
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

:- swi_module(mergeDRT,[mergeDrs/2]).

:- swi_use_module(comsemPredicates,[appendLists/3,
                                basicCondition/3]).


/*========================================================================
   DRS-merge
========================================================================*/

mergeDrs(drs(D,C1),drs(D,C2)):-
   mergeDrs(C1,C2).

mergeDrs(lam(X,B1),lam(X,B2)):-
   mergeDrs(B1,B2).

mergeDrs(merge(B1,B2),drs(D3,C3)):-
   mergeDrs(B1,drs(D1,C1)),
   mergeDrs(B2,drs(D2,C2)),
   appendLists(D1,D2,D3),
   appendLists(C1,C2,C3).

mergeDrs([Cond1:F|C1],[Cond2:F|C2]):-
   mergeDrs([Cond1|C1],[Cond2|C2]).

mergeDrs([imp(B1,B2)|C1],[imp(B3,B4)|C2]):-
   mergeDrs(B1,B3),
   mergeDrs(B2,B4),
   mergeDrs(C1,C2).

mergeDrs([or(B1,B2)|C1],[or(B3,B4)|C2]):-
   mergeDrs(B1,B3),
   mergeDrs(B2,B4),
   mergeDrs(C1,C2).

mergeDrs([not(B1)|C1],[not(B2)|C2]):-
   mergeDrs(B1,B2),
   mergeDrs(C1,C2).

mergeDrs([Basic|C1],[Basic|C2]):-
   basicCondition(Basic,_,_),
   mergeDrs(C1,C2).

mergeDrs([],[]).


/*************************************************************************

    File: bindingViolation.pl
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

:- swi_module(bindingViolation,[bindingViolationDrs/1]).

:- swi_use_module(comsemPredicates,[compose/3]).


/*========================================================================
   DRS with no Binding Violation
========================================================================*/

bindingViolationDrs(drs(_,C)):-
   bindingViolationConds(C).

bindingViolationDrs(merge(B1,B2)):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2)
   ).

bindingViolationConds([not(B)|L]):-
   (
      bindingViolationDrs(B), !
   ;
      bindingViolationConds(L)
   ), !.

bindingViolationConds([imp(B1,B2)|L]):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2), !
   ;
      bindingViolationConds(L)
   ), !.


bindingViolationConds([or(B1,B2)|L]):-
   (
      bindingViolationDrs(B1), !
   ;
      bindingViolationDrs(B2), !
   ;
      bindingViolationConds(L)
   ), !.

bindingViolationConds([Basic|L]):-
   (
      Basic = Cond:[ref:no],
      compose(Cond,_,[X,Y]),
      X==Y, !
   ;
      Basic = Cond:[ref:yes],
      compose(Cond,_,[X,Y]),
      \+ X==Y, !
   ;
      bindingViolationConds(L)
   ).
   

/*************************************************************************

    File: freeVarCheck.pl
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

:- swi_module(freeVarCheck,[freeVarCheckDrs/1]).

:- swi_use_module(comsemPredicates,[compose/3,
                                memberList/2,
                                basicCondition/3]).


/*========================================================================
   Free Variable Check (main predicate)
========================================================================*/

freeVarCheckDrs(Drs):-
   freeVarCheckDrs(Drs,[]-_).


/*========================================================================
   Free Variable Check (DRSs)
========================================================================*/

freeVarCheckDrs(drs([X|D],C),L1-L2):-
   freeVarCheckDrs(drs(D,C),[X|L1]-L2).

freeVarCheckDrs(drs([],C),L-L):-
   freeVarCheckConds(C,L).

freeVarCheckDrs(merge(B1,B2),L1-L3):-
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).

freeVarCheckDrs(alfa(_,B1,B2),L1-L3):-
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-L3).


/*========================================================================
   Free Variable Check (List of conditions)
========================================================================*/

freeVarCheckConds([],_).

freeVarCheckConds([X|C],L):-
   freeVarCheckCond(X,L),
   freeVarCheckConds(C,L).


/*========================================================================
   Free Variable Check (Conditions)
========================================================================*/

freeVarCheckCond(X:_,L):- !,
   freeVarCheckCond(X,L).

freeVarCheckCond(not(B),L):-
   freeVarCheckDrs(B,L-_).

freeVarCheckCond(imp(B1,B2),L1):-
   freeVarCheckDrs(B1,L1-L2),
   freeVarCheckDrs(B2,L2-_).

freeVarCheckCond(or(B1,B2),L):-
   freeVarCheckDrs(B1,L-_),
   freeVarCheckDrs(B2,L-_).

freeVarCheckCond(Basic,L):-
   basicCondition(Basic,_,Args),
   checkTerms(Args,L).


/*========================================================================
   Check Terms
========================================================================*/

checkTerms([],_).

checkTerms([X|T],L):-
   var(X),
   memberList(Y,L),
   X==Y,
   checkTerms(T,L).

checkTerms([X|T],L):-
   atom(X),
   checkTerms(T,L).

/*************************************************************************

    File: backgroundKnowledge.pl
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

:- swi_module(backgroundKnowledge,[backgroundKnowledge/2]).

:- swi_use_module(comsemPredicates,[memberList/2,
				basicFormula/1,
				compose/3]).

:- swi_use_module(lexicalKnowledge,[lexicalKnowledge/3]).

:- swi_use_module(worldKnowledge,[worldKnowledge/3]).

:- swi_use_module(situationalKnowledge,[situationalKnowledge/1]).


/*========================================================================
   Declare Dynamic Predicates
========================================================================*/

:- dynamic knowledge/1.


/*========================================================================
   Add Background Knowledge to Formula
========================================================================*/

backgroundKnowledge(Formula1,Formula2):-
   formula2symbols(Formula1,Symbols),
   backgroundKnowledge(Formula1,Symbols,Formula2).


/*========================================================================
   Add Background Knowledge until fixed point is reached
========================================================================*/

backgroundKnowledge(Formula1,Symbols1,Formula3):-
   computeBackgroundKnowledge(Symbols1,Formula2),
   formula2symbols(and(Formula1,Formula2),Symbols2),
   (
      sort(Symbols1,Sorted), %%% No new symbols, hence fixed point
      sort(Symbols2,Sorted), %%% is reached!
      !,
      Formula3=Formula2
   ;
      backgroundKnowledge(and(Formula1,Formula2),Symbols2,Formula3)
   ).


/*========================================================================
   Computing World Knowledge
========================================================================*/

computeBackgroundKnowledge(Symbols,Formula):-
   retractall(knowledge(_)),
   findall(_,(lexicalKnowledge(Symbol,Arity,F),
              memberList(symbol(Symbol,Arity),Symbols),
              assert(knowledge(F))),_),
   findall(_,(worldKnowledge(Symbol,Arity,F),
              memberList(symbol(Symbol,Arity),Symbols),
              assert(knowledge(F))),_),
   findall(_,(situationalKnowledge(F),
              assert(knowledge(F))),_),
   knowledge2formula(Formula).


/*========================================================================
   Put all selected knowledge in one big formula
========================================================================*/

knowledge2formula(F):-
   knowledge(F1),
   retract(knowledge(F1)),
   (
      knowledge(_), !,
      knowledge2formula(F2),
      F=and(F1,F2)
   ; 
      F=F1
   ).
   

/*========================================================================
   Derive all Symbols from a Formula
========================================================================*/

formula2symbols(F,S):-
   formula2symbols(F,[],S).

formula2symbols(X,S,S):-
   var(X), !.

formula2symbols(X,S,[symbol(X,0)|S]):-
   atom(X),
   \+ memberList(symbol(X,0),S), !.

formula2symbols(X,S,S):-
   atom(X),
   memberList(symbol(X,0),S), !.

formula2symbols(some(_,F),S1,S2):- !,
   formula2symbols(F,S1,S2).

formula2symbols(all(_,F),S1,S2):- !,
   formula2symbols(F,S1,S2).

formula2symbols(not(F),S1,S2):- !,
   formula2symbols(F,S1,S2).

formula2symbols(and(F1,F2),S1,S3):- !,
   formula2symbols(F1,S1,S2),
   formula2symbols(F2,S2,S3).

formula2symbols(or(F1,F2),S1,S3):- !,
   formula2symbols(F1,S1,S2),
   formula2symbols(F2,S2,S3).

formula2symbols(imp(F1,F2),S1,S3):- !,
   formula2symbols(F1,S1,S2),
   formula2symbols(F2,S2,S3).

formula2symbols(F,S1,[symbol(Symbol,1)|S2]):- 
   basicFormula(F),
   compose(F,Symbol,[Arg]),
   \+ memberList(symbol(Symbol,1),S1), !,
   formula2symbols(Arg,S1,S2).

formula2symbols(F,S1,S2):- 
   basicFormula(F),
   compose(F,Symbol,[Arg]),
   memberList(symbol(Symbol,1),S1), !,
   formula2symbols(Arg,S1,S2).

formula2symbols(F,S1,[symbol(Symbol,2)|S3]):- 
   basicFormula(F),
   compose(F,Symbol,[Arg1,Arg2]),
   \+ memberList(symbol(Symbol,2),S1), !,
   formula2symbols(Arg1,S1,S2),
   formula2symbols(Arg2,S2,S3).

formula2symbols(F,S1,S3):- 
   basicFormula(F),
   compose(F,Symbol,[Arg1,Arg2]),
   memberList(symbol(Symbol,2),S1), !,
   formula2symbols(Arg1,S1,S2),
   formula2symbols(Arg2,S2,S3).

/*************************************************************************

    File: drs2fol.pl
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

:- swi_module(drs2fol,[drs2fol/2]).

:- swi_use_module(comsemPredicates,[basicFormula/1]).

/*========================================================================
   Translate DRSs into FOL formulas 
========================================================================*/
drs2fol(drs([],[]),true):-!.

drs2fol(drs([],[Cond]),Formula):-
   cond2fol(Cond,Formula).

drs2fol(drs([],[Cond1,Cond2|Conds]),and(Formula1,Formula2)):-
   cond2fol(Cond1,Formula1),
   drs2fol(drs([],[Cond2|Conds]),Formula2).

drs2fol(drs([X|Referents],Conds),some(X,Formula)):-
   drs2fol(drs(Referents,Conds),Formula).


/*========================================================================
   Translate DRS-Conditions into FOL formulas 
========================================================================*/

cond2fol(bim(drs([X|L],Conds),Drs),all(X,Formula)):-
   drs2fol(drs(L,Conds),F1),
   drs2fol(Drs,F2),
   Formula=and(imp(F1,F2),imp(F2,F1)).

cond2fol(not(Drs),not(Formula)):-
   drs2fol(Drs,Formula).

cond2fol(or(Drs1,Drs2),or(Formula1,Formula2)):-
   drs2fol(Drs1,Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(imp(drs([],Conds),Drs2),imp(Formula1,Formula2)):-
   drs2fol(drs([],Conds),Formula1),
   drs2fol(Drs2,Formula2).

cond2fol(imp(drs([X|Referents],Conds),Drs2),all(X,Formula)):-
   cond2fol(imp(drs(Referents,Conds),Drs2),Formula).

cond2fol(Basic,Formula):-
   basicFormula(Basic),
   Formula=Basic.

cond2fol(Basic:_,Formula):-
   basicFormula(Basic),
   Formula=Basic.


/*========================================================================
   Info
========================================================================*/

info3:-
   format('~n> ------------------------------------------------------------------ <',[]),
   format('~n> drs2fol.pl, by Patrick Blackburn and Johan Bos                     <',[]),
   format('~n>                                                                    <',[]),
   format('~n> ?- drs2fol(DRS,FOL).    - translated a DRS into a FOL formula      <',[]),
   format('~n> ------------------------------------------------------------------ <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

% :- info3.




/*************************************************************************

    File: superSubDRT.pl
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

:- swi_module(superSubDRT,[superSubDrs/3]).

:- swi_use_module(mergeDRT,[mergeDrs/2]).


/*========================================================================
   Compute Super- and SubDRS pairs
========================================================================*/

superSubDrs(drs(D,[imp(Sub,_)|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[imp(B,Sub)|C]),Drs-Super,Sub):-
   mergeDrs(merge(merge(drs(D,C),B),Drs),Super).

superSubDrs(drs(D,[imp(B,_)|C]),Drs-Super,Sub):-
   superSubDrs(B,merge(Drs,drs(D,C))-Super,Sub).

superSubDrs(drs(D,[imp(B1,B2)|C]),Drs-Super,Sub):-
   superSubDrs(B2,merge(Drs,merge(drs(D,C),B1))-Super,Sub).

superSubDrs(drs(D,[or(Sub,_)|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[or(_,Sub)|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[or(B,_)|C]),Drs-Super,Sub):-
   superSubDrs(B,merge(Drs,drs(D,C))-Super,Sub).

superSubDrs(drs(D,[or(_,B)|C]),Drs-Super,Sub):-
   superSubDrs(B,merge(Drs,drs(D,C))-Super,Sub).

superSubDrs(drs(D,[not(Sub)|C]),Drs-Super,Sub):-
   mergeDrs(merge(Drs,drs(D,C)),Super).

superSubDrs(drs(D,[not(B)|C]),Drs-Super,Sub):-
   superSubDrs(B,merge(Drs,drs(D,C))-Super,Sub).

superSubDrs(drs(D,[Cond|C]),Drs-Super,Sub):-
   superSubDrs(drs([],C),Drs-B,Sub),
   mergeDrs(merge(drs(D,[Cond]),B),Super).
































 /*************************************************************************

    File: worldKnowledge.pl
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

:- swi_module(worldKnowledge,[worldKnowledge/3]).

/*========================================================================
   Axioms for World Knowledge
========================================================================*/


%% Nothing can have itself, and no object can be part of two different objects
%%
worldKnowledge(have,2,Axiom):- 
   Axiom = and(not(some(X,have(X,X))),all(X,all(Y,
               imp(some(Z,and(object(X),and(object(Y),and(object(Z),
                   and(have(X,Z),have(Y,Z)))))),eq(X,Y))))).



worldKnowledge(husband,1,Axiom):- 
   Axiom = all(A,imp(husband(A),married(A))).

worldKnowledge(wife,1,Axiom):- 
   Axiom = all(A,imp(wife(A),married(A))).

worldKnowledge(of,2,Axiom):- 
   Axiom = all(A,all(B,imp(of(A,B),have(B,A)))).

/*************************************************************************

    File: situationalKnowledge.pl
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

:- swi_module(situationalKnowledge,[situationalKnowledge/1]).

:- dynamic situationalKnowledge/1.

/*========================================================================
   Axioms for Situational Knowledge
========================================================================*/

%% There are at least two different cars
%%
%situationalKnowledge(Axiom):-
%   Axiom = some(X,some(Y,and(car(X),and(car(Y),not(eq(X,Y)))))).

/*************************************************************************

    File: presupDRT.pl
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

:- swi_module(presupDRT,[presupDRT/0,
                     presupDRT/3,
                     presupDRTTestSuite/0,
                     infix/0,
                     prefix/0]).

:- swi_use_module(readLine,[readLine/1]).

:- swi_use_module(comsemPredicates,[prefix/0,
                                infix/0,
                                compose/3,
                                memberList/2,
                                reverseList/2,
                                removeFirst/3,
                                printRepresentations/1]).

:- swi_use_module(betaConversionDRT,[betaConvert/2]).

:- swi_use_module(mergeDRT,[mergeDrs/2]).

:- swi_use_module(presupTestSuite,[discourse/2]).

:- swi_use_module(bindingViolation,[bindingViolationDrs/1]).

:- swi_use_module(freeVarCheck,[freeVarCheckDrs/1]).

:- swi_use_module(sortalCheck,[sortalCheckDrs/1]).

% :- [englishGrammar].

% :- [englishLexicon].

% :- [semLexPresupDRT].

% :- [semRulesDRT].

'^'(V,G):-forall((G),writeln(V)).

/*========================================================================
   Driver Predicates
========================================================================*/

presupDRT:-
   readLine(Discourse),
   presupDRT(Discourse,drs([],[]),DRSs),
   printRepresentations(DRSs).

presupDRT(Text,Old,New):-
   findall(Sem,Drs^(t([sem:Drs],Text,[]),resolveDrs(merge(Old,Drs),Sem)),New),
   \+ New=[].


/*========================================================================
   Test Suite Predicates
========================================================================*/

presupDRTTestSuite:-
   nl, write('>>>>> PRESUP-DRT ON TEST SUITE <<<<<'), nl,
   discourse(Discourse,Readings),
   format('~nDiscourse: ~p (~p readings)',[Discourse,Readings]),
   presupDRT(Discourse,drs([],[]),DRSs),
   printRepresentations(DRSs),
   fail.

presupDRTTestSuite.


/*========================================================================
   Pronoun Resolution
========================================================================*/

resolveDrs(B,R):-
   \+ findAlfaDrs(B,_,_,_,[]-_),
   \+ bindingViolationDrs(B),
   mergeDrs(B,R).

resolveDrs(ADRS,DRS):-
   findAlfaDrs(ADRS,RDRS,alfa(Type,Alfa),Ac,[]-Bi),
   resolveAlfa(Alfa,Type,Ac,Bi,RDRS),
   resolveDrs(RDRS,DRS).


/*========================================================================
   Find First Alfa-DRS (DRSs)
========================================================================*/

findAlfaDrs(alfa(T,B1,B2),alfa(T,R1,B2),Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !. 

findAlfaDrs(alfa(T,B1,B2),merge(A,B2),alfa(T,M1),[a(A)],Bi-Bi):-
   mergeDrs(B1,M1). 

findAlfaDrs(merge(B1,B),merge(R1,B),Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,R1,Alfa,Ac,Bi1-Bi2), !.

findAlfaDrs(merge(B1,B2),merge(R1,R2),Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B2,R2,Alfa,Ac,[r(M1,R1)|Bi1]-Bi2),
   mergeDrs(B1,M1). 

findAlfaDrs(drs(D,C1),merge(A,R),Alfa,[a(A)|Ac],Bi1-Bi2):-
   findAlfaConds(C1,C2,Alfa,Ac,[r(drs(D,C2),R)|Bi1]-Bi2).


/*========================================================================
   Find First Alfa-DRS (DRS-Conditions)
========================================================================*/

findAlfaConds([imp(B1,B)|C],[imp(B2,B)|C],Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([imp(B1,B2)|C],[imp(merge(R1,A),R2)|C],Alfa,[a(A)|Ac],Bi1-Bi2):-
   findAlfaDrs(B2,R2,Alfa,Ac,[r(M1,R1)|Bi1]-Bi2), !,
   mergeDrs(B1,M1).

findAlfaConds([or(B1,B)|C],[or(B2,B)|C],Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([or(B,B1)|C],[or(B,B2)|C],Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([not(B1)|C],[not(B2)|C],Alfa,Ac,Bi1-Bi2):-
   findAlfaDrs(B1,B2,Alfa,Ac,Bi1-Bi2), !.

findAlfaConds([X|C1],[X|C2],Alfa,Ac,Bi1-Bi2):-
   findAlfaConds(C1,C2,Alfa,Ac,Bi1-Bi2).


/*========================================================================
   Resolve Alfa
========================================================================*/

resolveAlfa(Alfa,_,Ac,Bi,B):-
   bindAlfa(Bi,Alfa),
   dontResolve(Ac),
   sortalCheckDrs(B).

resolveAlfa(Alfa,Type,Ac,Bi,B):-
   accommodateAlfa(Ac,Type,Alfa),
   dontResolve(Bi),
   freeVarCheckDrs(B).


/*------------------------------------------------------------------------
   Binding
------------------------------------------------------------------------*/

bindAlfa([r(drs(D2,C2),drs(D3,C3))|P],drs([X|D1],C1)):-
   memberList(X,D2),
   mergeLists(D1,D2,D3),
   mergeLists(C1,C2,C3),
   dontResolve(P).

bindAlfa([r(R,R)|P],Alfa):-
   bindAlfa(P,Alfa).

bindAlfa([a(drs([],[]))|P],Alfa):-
   bindAlfa(P,Alfa).


/*------------------------------------------------------------------------
   Accommodation
------------------------------------------------------------------------*/

accommodateAlfa(P1,nam,Alfa):-
   removeFirst(a(Alfa),P1,P2),
   dontResolve(P2).

accommodateAlfa([a(Alfa)|P],def,Alfa):-
   dontResolve(P).

accommodateAlfa([r(R,R)|P],Type,Alfa):-
   accommodateAlfa(P,Type,Alfa).

accommodateAlfa([a(drs([],[]))|P],def,Alfa):-
   accommodateAlfa(P,def,Alfa).


/*========================================================================
    Do not resolve remaining of projection path
========================================================================*/

dontResolve([]).

dontResolve([a(drs([],[]))|L]):- 
   dontResolve(L).

dontResolve([r(X,X)|L]):- 
   dontResolve(L).


/*========================================================================
   Merge Lists - Check for Duplicates
========================================================================*/

mergeLists([],L,L).

mergeLists([X|R],L1,L2):-
   memberList(Y,L1), X==Y, !,
   mergeLists(R,L1,L2).

mergeLists([X|R],L1,[X|L2]):-
   mergeLists(R,L1,L2).



/*========================================================================
   Info
========================================================================*/

info5:-
   format('~n> ------------------------------------------------------------------ <',[]),
   format('~n> presupDRT.pl, by Patrick Blackburn and Johan Bos                   <',[]),
   format('~n>                                                                    <',[]),
   format('~n> ?- presupDRT.              - parse a typed-in sentence             <',[]),
   format('~n> ?- presupDRT(S,Old,New).   - parse a sentence and return DRS       <',[]),
   format('~n> ?- presupDRTTestSuite.     - run the test suite                    <',[]),
   format('~n> ------------------------------------------------------------------ <',[]),
   format('~n~n',[]).


/*========================================================================
   Display info at start
========================================================================*/

:- info5.

/*************************************************************************

    File: semRulesDRT.pl
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

/*========================================================================
   Semantic Rules
========================================================================*/

combine(t:Drs,[s:Sem]):- 
   betaConvert(Sem,Drs).

combine(t:Drs,[s:S,t:T]):- 
   betaConvert(merge(S,T),Drs).

combine(t:Converted,[q:Sem]):- 
   betaConvert(Sem,Converted).

combine(s:app(A,B),[np:A,vp:B]).
combine(s:app(A,B),[s:A,s:B]).
combine(s:lam(B,drs([],[imp(S,B)])),[if:S]).
combine(s:lam(B,drs([],[or(S,B)])),[either:S]).
combine(s:S,[then:S]).
combine(s:S,[or:S]).

combine(sinv:app(B,app(A,C)),[av:A,np:B,vp:C]).

combine(q:app(A,B),[whnp:A,vp:B]).
combine(q:A,[sinv:A]).

combine(np:app(app(B,A),C),[np:A,coord:B,np:C]).
combine(np:app(A,B),[det:A,n:B]).
combine(np:A,[pn:A]).
combine(np:A,[qnp:A]).

combine(whnp:app(A,B),[det:A,n:B]).
combine(whnp:A,[qnp:A]).

combine(n:app(app(B,A),C),[n:A,coord:B,n:C]).
combine(n:app(A,B),[adj:A,n:B]).
combine(n:A,[noun:A]).
combine(n:app(B,A),[noun:A,nmod:B]).

combine(nmod:A,[pp:A]).
combine(nmod:A,[rc:A]).
combine(nmod:lam(P,app(A,app(B,P))),[pp:A,nmod:B]).

combine(vp:app(app(B,A),C),[vp:A,coord:B,vp:C]).
combine(vp:app(A,B),[av:A,vp:B]).
combine(vp:app(A,B),[cop:A,np:B]).
combine(vp:A,[iv:A]).
combine(vp:app(A,B),[tv:A,np:B]).

combine(pp:app(A,B),[prep:A,np:B]).

combine(rc:app(A,B),[relpro:A,vp:B]).



/*************************************************************************

    File: semLexPresupDRT.pl
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

semLex(det,M):-
   M = [type:indef,
        number:pl,
        sem:lam(U,lam(V,drs([G],
                            [group(G),
                             imp(drs([X],[member(X,G)]),app(U,X)),
                             imp(drs([Y],[member(Y,G)]),app(V,Y))])))].

semLex(det,M):-
   M = [type:uni,
        number:sg,
        sem:lam(U,lam(V,drs([],[imp(merge(drs([X],[]),app(U,X)),app(V,X))])))].

semLex(det,M):-
   M = [type:indef,
        number:sg,
        sem:lam(U,lam(V,merge(merge(drs([X],[]),app(U,X)),app(V,X))))].

semLex(det,M):-
   M = [type:def,
        number:sg,
        sem:lam(U,lam(V,alfa(def,merge(drs([X],[]),app(U,X)),app(V,X))))].

semLex(det,M):-
   M = [type:neg,
        number:sg,
        sem:lam(U,lam(V,drs([],[not(merge(merge(drs([X],[]),app(U,X)),app(V,X)))])))].

semLex(det,M):-
   M = [type:poss(Symbol),
        number:sg,
        sem:lam(U,lam(V,alfa(pro,drs([Y],[Cond:[]]),alfa(def,merge(drs([X],[of(X,Y)]),app(U,X)),app(V,X)))))],
   compose(Cond,Symbol,[Y]).

semLex(pn,M):-
   M = [symbol:Sym,
        sem:lam(P,alfa(nam,drs([X],[eq(X,Sym):[]]),app(P,X)))].

semLex(pro,M):-
   M = [symbol:Sym,
        sem:lam(P,alfa(pro,drs([X],[Cond:[]]),app(P,X)))],
   compose(Cond,Sym,[X]).

semLex(noun,M):-
   M = [symbol:Sym,
        sem:lam(X,drs([],[Cond:[]]))],
   compose(Cond,Sym,[X]).

semLex(iv,M):-
   M = [symbol:Sym,
        sem:lam(X,drs([],[Cond:[]]))],
   compose(Cond,Sym,[X]).

semLex(tv,M):-
   M = [symbol:Sym,
        Ref,
        sem:lam(K,lam(Y,app(K,lam(X,drs([],[Cond:[Ref]])))))],
   compose(Cond,Sym,[Y,X]).

semLex(cop,M):-
   M = [pol:pos,
        sem:lam(K,lam(Y,app(K,lam(X,drs([],[eq(Y,X):[]])))))];
   M = [pol:neg,
        sem:lam(K,lam(Y,drs([],[not(app(K,lam(X,drs([],[eq(Y,X):[]]))))])))].

semLex(relpro,M):-
   M = [sem:lam(P,lam(Q,lam(X,merge(app(P,X),app(Q,X)))))].

semLex(prep,M):-
   M = [symbol:Sym,
        sem:lam(K,lam(P,lam(Y,merge(app(K,lam(X,drs([],[Cond:[]]))),app(P,Y)))))],
   compose(Cond,Sym,[Y,X]).

semLex(adj,M):-
   M = [symbol:Sym,
        sem:lam(P,lam(X,merge(drs([],[Cond:[]]),app(P,X))))],
   compose(Cond,Sym,[X]).

semLex(av,M):-
   M = [pol:neg,
        sem:lam(P,lam(X,drs([],[not(app(P,X))])))];
   M = [pol:pos,
        sem:lam(P,lam(X,app(P,X)))].

semLex(coord,M):-
   M = [type:conj,
        sem:lam(X,lam(Y,lam(P,merge(app(X,P),app(Y,P)))))];  
   M = [type:disj,
        sem:lam(X,lam(Y,lam(P,drs([],[or(app(X,P),app(Y,P))]))))].





/*************************************************************************

    File: englishGrammar.pl
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

/*========================================================================
   Texts
========================================================================*/

t([sem:T])--> 
   s([coord:no,sem:S]),
   {combine(t:T,[s:S])}.

t([sem:T])--> 
   s([coord:no,sem:S1]), 
   t([sem:S2]),
   {combine(t:T,[s:S1,t:S2])}.

t([sem:T])--> 
   s([coord:yes,sem:S]),
   {combine(t:T,[s:S])}.

t([sem:T])--> 
   s([coord:yes,sem:S1]), 
   t([sem:S2]),
   {combine(t:T,[s:S1,t:S2])}.

t([sem:T])--> 
   q([sem:Q]),
   {combine(t:T,[q:Q])}.


/*========================================================================
   Sentences
========================================================================*/

s([coord:no,sem:Sem])--> 
   np([coord:_,num:Num,gap:[],ref:no,sem:NP]), 
   vp([coord:_,inf:fin,num:Num,gap:[],sem:VP]), 
   {combine(s:Sem,[np:NP,vp:VP])}.

s([coord:yes,sem:Sem])--> 
   s([coord:ant,sem:S1]), 
   s([coord:con,sem:S2]), 
   {combine(s:Sem,[s:S1,s:S2])}.

s([coord:yes,sem:Sem])--> 
   s([coord:either,sem:S1]), 
   s([coord:or,sem:S2]), 
   {combine(s:Sem,[s:S1,s:S2])}.

s([coord:ant,sem:Sem])--> 
   [if], 
   s([coord:no,sem:S]),
   {combine(s:Sem,[if:S])}.

s([coord:either,sem:Sem])--> 
   [either], 
   s([coord:no,sem:S]),
   {combine(s:Sem,[either:S])}.

s([coord:con,sem:Sem])--> 
   [then], 
   s([coord:no,sem:S]),
   {combine(s:Sem,[then:S])}.

s([coord:con,sem:Sem])-->
   s([coord:no,sem:S]),
   {combine(s:Sem,[then:S])}.

s([coord:or,sem:Sem])-->
   [or], 
   s([coord:no,sem:S]),
   {combine(s:Sem,[or:S])}.

sinv([gap:G,sem:S])-->
   av([inf:fin,num:Num,sem:Sem]),
   np([coord:_,num:Num,gap:[],ref:no,sem:NP]),
   vp([coord:_,inf:inf,num:Num,gap:G,sem:VP]), 
   {combine(sinv:S,[av:Sem,np:NP,vp:VP])}.


/*========================================================================
   Questions
========================================================================*/

q([sem:Sem])--> 
   whnp([num:Num,sem:NP]), 
   vp([coord:_,inf:fin,num:Num,gap:[],sem:VP]), 
   {combine(q:Sem,[whnp:NP,vp:VP])}.

q([sem:Sem])--> 
   whnp([num:_,sem:NP]), 
   sinv([gap:[np:NP],sem:S]),
   {combine(q:Sem,[sinv:S])}.


/*========================================================================
   Noun Phrases
========================================================================*/

np([coord:no,num:sg,gap:[np:NP],ref:no,sem:NP])--> [].

np([coord:yes,num:pl,gap:[],ref:Ref,sem:NP])--> 
   np([coord:no,num:sg,gap:[],ref:Ref,sem:NP1]), 
   coord([type:conj,sem:C]), 
   np([coord:_,num:_,gap:[],ref:Ref,sem:NP2]), 
   {combine(np:NP,[np:NP1,coord:C,np:NP2])}.

np([coord:yes,num:sg,gap:[],ref:Ref,sem:NP])--> 
   np([coord:no,num:sg,gap:[],ref:Ref,sem:NP1]), 
   coord([type:disj,sem:C]), 
   np([coord:_,num:sg,gap:[],ref:Ref,sem:NP2]), 
   {combine(np:NP,[np:NP1,coord:C,np:NP2])}.

np([coord:no,num:sg,gap:[],ref:no,sem:NP])--> 
   det([mood:decl,type:_,num:Num,sem:Det]), 
   n([coord:_,num:Num,sem:N]), 
   {combine(np:NP,[det:Det,n:N])}.

np([coord:no,num:sg,gap:[],ref:no,sem:NP])--> 
   pn([sem:PN]), 
   {combine(np:NP,[pn:PN])}.

np([coord:no,num:sg,gap:[],ref:Ref,sem:NP])--> 
   pro([ref:Ref,sem:PN]), 
   {combine(np:NP,[pn:PN])}.

np([coord:no,num:sg,gap:[],ref:no,sem:NP])--> 
   qnp([mood:decl,sem:QNP]), 
   {combine(np:NP,[qnp:QNP])}.


/*========================================================================
   WH Noun Phrases
========================================================================*/

whnp([num:sg,sem:NP])--> 
   qnp([mood:int,sem:QNP]), 
   {combine(whnp:NP,[qnp:QNP])}.

whnp([num:sg,sem:NP])--> 
   det([mood:int,type:_,num:_,sem:Det]), 
   n([coord:_,num:_,sem:N]), 
   {combine(whnp:NP,[det:Det,n:N])}.


/*========================================================================
   Nouns
========================================================================*/

n([coord:yes,num:Num,sem:N])--> 
   n([coord:no,num:Num,sem:N1]), 
   coord([type:_,sem:C]),  
   n([coord:_,num:Num,sem:N2]),
   {combine(n:N,[n:N1,coord:C,n:N2])}.

n([coord:C,num:Num,sem:Sem])--> 
   adj([sem:A]), 
   n([coord:C,num:Num,sem:N]), 
   {combine(n:Sem,[adj:A,n:N])}.

n([coord:no,num:Num,sem:N])--> 
   noun([num:Num,sem:Noun]),
   {combine(n:N,[noun:Noun])}.

n([coord:no,num:Num,sem:Sem])--> 
   noun([num:Num,sem:N]), 
   nmod([sem:PP]),
   {combine(n:Sem,[noun:N,nmod:PP])}. 

nmod([sem:N])--> 
   pp([sem:PP]),
   {combine(nmod:N,[pp:PP])}.

nmod([sem:N])--> 
   rc([sem:RC]),
   {combine(nmod:N,[rc:RC])}.

nmod([sem:Sem])--> 
   pp([sem:PP]), 
   nmod([sem:NMod]),
   {combine(nmod:Sem,[pp:PP,nmod:NMod])}.


/*========================================================================
   Verb Phrases
========================================================================*/

vp([coord:yes,inf:Inf,num:Num,gap:[],sem:VP])--> 
   vp([coord:no,inf:Inf,num:Num,gap:[],sem:VP1]), 
   coord([type:_,sem:C]), 
   vp([coord:_,inf:Inf,num:Num,gap:[],sem:VP2]),
   {combine(vp:VP,[vp:VP1,coord:C,vp:VP2])}.

vp([coord:no,inf:Inf,num:Num,gap:[],sem:VP])--> 
   av([inf:Inf,num:Num,sem:Mod]), 
   vp([coord:_,inf:inf,num:Num,gap:[],sem:V2]), 
   {combine(vp:VP,[av:Mod,vp:V2])}.

vp([coord:no,inf:Inf,num:Num,gap:[],sem:VP])--> 
   cop([inf:Inf,num:Num,sem:Cop]), 
   np([coord:_,num:_,gap:[],ref:_,sem:NP]), 
   {combine(vp:VP,[cop:Cop,np:NP])}.

vp([coord:no,inf:Inf,num:Num,gap:[],sem:VP])--> 
   iv([inf:Inf,num:Num,sem:IV]), 
   {combine(vp:VP,[iv:IV])}.

vp([coord:no,inf:I,num:Num,gap:G,sem:VP])-->   
   tv([inf:I,num:Num,ref:Ref,sem:TV]), 
   np([coord:_,num:_,gap:G,ref:Ref,sem:NP]), 
   {combine(vp:VP,[tv:TV,np:NP])}.


/*========================================================================
   Prepositional Phrases
========================================================================*/

pp([sem:PP])--> 
   prep([sem:Prep]), 
   np([coord:_,num:_,gap:[],ref:no,sem:NP]), 
   {combine(pp:PP,[prep:Prep,np:NP])}.


/*========================================================================
   Relative Clauses
========================================================================*/

rc([sem:RC])--> 
   relpro([sem:RP]), 
   vp([coord:_,inf:fin,num:sg,gap:[],sem:VP]), 
   {combine(rc:RC,[relpro:RP,vp:VP])}.


/*========================================================================
   Lexical Rules
========================================================================*/

iv([inf:Inf,num:Num,sem:Sem])--> 
   {lexEntry(iv,[symbol:Sym,syntax:Word,inf:Inf,num:Num])},
   Word,
   {semLex(iv,[symbol:Sym,sem:Sem])}.

tv([inf:Inf,num:Num,ref:Ref,sem:Sem])--> 
   {lexEntry(tv,[symbol:Sym,syntax:Word,inf:Inf,num:Num])},
   Word,
   {semLex(tv,[symbol:Sym,ref:Ref,sem:Sem])}.

cop([inf:Inf,num:Num,sem:Sem])--> 
   {lexEntry(cop,[pol:Pol,syntax:Word,inf:Inf,num:Num])},
   Word,
   {semLex(cop,[pol:Pol,sem:Sem])}.

det([mood:M,type:Type,num:Num,sem:Det])--> 
   {lexEntry(det,[syntax:Word,mood:M,num:Num,type:Type])},
   Word,
   {semLex(det,[type:Type,number:Num,sem:Det])}. 

pn([sem:Sem])--> 
   {lexEntry(pn,[symbol:Sym,syntax:Word])},
   Word,  
   {semLex(pn,[symbol:Sym,sem:Sem])}.

pro([ref:Ref,sem:Sem])--> 
   {lexEntry(pro,[symbol:Sym,ref:Ref,syntax:Word])},
   Word,  
   {semLex(pro,[symbol:Sym,sem:Sem])}.

relpro([sem:Sem])--> 
   {lexEntry(relpro,[syntax:Word])},
   Word,
   {semLex(relpro,[sem:Sem])}.

prep([sem:Sem])--> 
   {lexEntry(prep,[symbol:Sym,syntax:Word])},
   Word,
   {semLex(prep,[symbol:Sym,sem:Sem])}.

adj([sem:Sem])--> 
   {lexEntry(adj,[symbol:Sym,syntax:Word])},
   Word,
   {semLex(adj,[symbol:Sym,sem:Sem])}.

av([inf:Inf,num:Num,sem:Sem])--> 
   {lexEntry(av,[syntax:Word,inf:Inf,num:Num,pol:Pol])},
   Word,
   {semLex(av,[pol:Pol,sem:Sem])}.

coord([type:Type,sem:Sem])--> 
   {lexEntry(coord,[syntax:Word,type:Type])},
   Word, 
   {semLex(coord,[type:Type,sem:Sem])}.

qnp([mood:M,sem:NP])--> 
   {lexEntry(qnp,[symbol:Symbol,syntax:Word,mood:M,type:Type])},
   Word,
   {semLex(qnp,[type:Type,symbol:Symbol,sem:NP])}.

noun([num:_,sem:Sem])--> 
   {lexEntry(noun,[symbol:Sym,syntax:Word])},
   Word,
   {semLex(noun,[symbol:Sym,sem:Sem])}.



/*************************************************************************

    File: englishLexicon.pl
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

/*========================================================================
   Determiners
========================================================================*/

lexEntry(det,[syntax:[every],mood:decl,num:sg,type:uni]).
lexEntry(det,[syntax:[a],mood:decl,num:sg,type:indef]).
lexEntry(det,[syntax:[some],mood:decl,num:pl,type:indef]).
lexEntry(det,[syntax:[no],mood:decl,num:sg,type:neg]).
lexEntry(det,[syntax:[the],mood:decl,num:sg,type:def]).
lexEntry(det,[syntax:[his],mood:decl,num:sg,type:poss(male)]).
lexEntry(det,[syntax:[her],mood:decl,num:sg,type:poss(female)]).
lexEntry(det,[syntax:[its],mood:decl,num:sg,type:poss(neuter)]).

/*========================================================================
   Nouns
========================================================================*/

lexEntry(noun,[symbol:animal,syntax:[animal]]).
lexEntry(noun,[symbol:beverage,syntax:[beverage]]).
lexEntry(noun,[symbol:building,syntax:[building]]).
lexEntry(noun,[symbol:cup,syntax:[cup]]).
lexEntry(noun,[symbol:burger,syntax:[burger]]).
lexEntry(noun,[symbol:boxer,syntax:[boxer]]).
lexEntry(noun,[symbol:boss,syntax:[boss]]).
lexEntry(noun,[symbol:car,syntax:[car]]).
lexEntry(noun,[symbol:chainsaw,syntax:[chainsaw]]).
lexEntry(noun,[symbol:criminal,syntax:[criminal]]).
lexEntry(noun,[symbol:customer,syntax:[customer]]).
lexEntry(noun,[symbol:drug,syntax:[drug]]).
lexEntry(noun,[symbol:episode,syntax:[episode]]).
lexEntry(noun,[symbol:fdshake,syntax:[five,dollar,shake]]).
lexEntry(noun,[symbol:footmassage,syntax:[foot,massage]]).
lexEntry(noun,[symbol:gimp,syntax:[gimp]]).
lexEntry(noun,[symbol:glass,syntax:[glass]]).
lexEntry(noun,[symbol:gun,syntax:[gun]]).
lexEntry(noun,[symbol:hammer,syntax:[hammer]]).
lexEntry(noun,[symbol:hashbar,syntax:[hash,bar]]).
lexEntry(noun,[symbol:person,syntax:[person]]).
lexEntry(noun,[symbol:husband,syntax:[husband]]).
lexEntry(noun,[symbol:joke,syntax:[joke]]).
lexEntry(noun,[symbol:man,syntax:[man]]).
lexEntry(noun,[symbol:needle,syntax:[needle]]).
lexEntry(noun,[symbol:owner,syntax:[owner]]).
lexEntry(noun,[symbol:piercing,syntax:[piercing]]).
lexEntry(noun,[symbol:plant,syntax:[plant]]).
lexEntry(noun,[symbol:qpwc,syntax:[quarter,pounder,with,cheese]]).
lexEntry(noun,[symbol:radio,syntax:[radio]]).
lexEntry(noun,[symbol:restaurant,syntax:[restaurant]]).
lexEntry(noun,[symbol:robber,syntax:[robber]]).
lexEntry(noun,[symbol:suitcase,syntax:[suitcase]]).
lexEntry(noun,[symbol:shotgun,syntax:[shotgun]]).
lexEntry(noun,[symbol:sword,syntax:[sword]]).
lexEntry(noun,[symbol:vehicle,syntax:[vehicle]]).
lexEntry(noun,[symbol:weapon,syntax:[weapon]]).
lexEntry(noun,[symbol:wife,syntax:[wife]]).
lexEntry(noun,[symbol:woman,syntax:[woman]]).


/*========================================================================
   Proper Names
========================================================================*/

lexEntry(pn,[symbol:butch,syntax:[butch]]).
lexEntry(pn,[symbol:esmarelda,syntax:[esmarelda]]).
lexEntry(pn,[symbol:honey_bunny,syntax:[honey,bunny]]).
lexEntry(pn,[symbol:jimmy,syntax:[jimmy]]).
lexEntry(pn,[symbol:jody,syntax:[jody]]).
lexEntry(pn,[symbol:jules,syntax:[jules]]).
lexEntry(pn,[symbol:lance,syntax:[lance]]).
lexEntry(pn,[symbol:marsellus,syntax:[marsellus]]).
lexEntry(pn,[symbol:marsellus,syntax:[marsellus,wallace]]).
lexEntry(pn,[symbol:marvin,syntax:[marvin]]).
lexEntry(pn,[symbol:mia,syntax:[mia]]).
lexEntry(pn,[symbol:mia,syntax:[mia,wallace]]).
lexEntry(pn,[symbol:pumpkin,syntax:[pumpkin]]).
lexEntry(pn,[symbol:thewolf,syntax:[the,wolf]]).
lexEntry(pn,[symbol:vincent,syntax:[vincent]]).
lexEntry(pn,[symbol:vincent,syntax:[vincent,vega]]).
lexEntry(pn,[symbol:yolanda,syntax:[yolanda]]).


/*========================================================================
   Quantified Noun Phrases
========================================================================*/

lexEntry(qnp,[symbol:person,syntax:[who],mood:int,type:wh]).
lexEntry(qnp,[symbol:thing,syntax:[what],mood:int,type:wh]).


/*========================================================================
   Intransitive Verbs
========================================================================*/

lexEntry(iv,[symbol:collapse,syntax:[collapse],inf:inf,num:sg]).
lexEntry(iv,[symbol:collapse,syntax:[collapses],inf:fin,num:sg]).
lexEntry(iv,[symbol:collapse,syntax:[collapse],inf:fin,num:pl]).

lexEntry(iv,[symbol:dance,syntax:[dance],inf:inf,num:sg]).
lexEntry(iv,[symbol:dance,syntax:[dances],inf:fin,num:sg]).
lexEntry(iv,[symbol:dance,syntax:[dance],inf:fin,num:pl]).

lexEntry(iv,[symbol:die,syntax:[die],inf:inf,num:sg]).
lexEntry(iv,[symbol:die,syntax:[dies],inf:fin,num:sg]).
lexEntry(iv,[symbol:die,syntax:[die],inf:fin,num:pl]).

lexEntry(iv,[symbol:growl,syntax:[growl],inf:inf,num:sg]).
lexEntry(iv,[symbol:growl,syntax:[growls],inf:fin,num:sg]).
lexEntry(iv,[symbol:growl,syntax:[growl],inf:fin,num:pl]).

lexEntry(iv,[symbol:playairguitar,syntax:[play,air,guitar],inf:inf,num:sg]).
lexEntry(iv,[symbol:playairguitar,syntax:[plays,air,guitar],inf:fin,num:sg]).
lexEntry(iv,[symbol:playairguitar,syntax:[play,air,guitar],inf:fin,num:pl]).

lexEntry(iv,[symbol:smoke,syntax:[smoke],inf:inf,num:sg]).
lexEntry(iv,[symbol:smoke,syntax:[smokes],inf:fin,num:sg]).
lexEntry(iv,[symbol:smoke,syntax:[smoke],inf:fin,num:pl]).

lexEntry(iv,[symbol:snort,syntax:[snort],inf:inf,num:sg]).
lexEntry(iv,[symbol:snort,syntax:[snorts],inf:fin,num:sg]).
lexEntry(iv,[symbol:snort,syntax:[snort],inf:fin,num:pl]).

lexEntry(iv,[symbol:shriek,syntax:[shriek],inf:inf,num:sg]).
lexEntry(iv,[symbol:shriek,syntax:[shrieks],inf:fin,num:sg]).
lexEntry(iv,[symbol:shriek,syntax:[shriek],inf:fin,num:pl]).

lexEntry(iv,[symbol:walk,syntax:[walk],inf:inf,num:sg]).
lexEntry(iv,[symbol:walk,syntax:[walks],inf:fin,num:sg]).
lexEntry(iv,[symbol:walk,syntax:[walk],inf:fin,num:pl]).


/*========================================================================
   Transitive Verbs
========================================================================*/

lexEntry(tv,[symbol:clean,syntax:[clean],inf:inf,num:sg]).
lexEntry(tv,[symbol:clean,syntax:[cleans],inf:fin,num:sg]).
lexEntry(tv,[symbol:clean,syntax:[clean],inf:fin,num:pl]).

lexEntry(tv,[symbol:drink,syntax:[drink],inf:inf,num:sg]).
lexEntry(tv,[symbol:drink,syntax:[drinks],inf:fin,num:sg]).
lexEntry(tv,[symbol:drink,syntax:[drink],inf:fin,num:pl]).

lexEntry(tv,[symbol:date,syntax:[date],inf:inf,num:sg]).
lexEntry(tv,[symbol:date,syntax:[dates],inf:fin,num:sg]).
lexEntry(tv,[symbol:date,syntax:[date],inf:fin,num:pl]).

lexEntry(tv,[symbol:discard,syntax:[discard],inf:inf,num:sg]).
lexEntry(tv,[symbol:discard,syntax:[discards],inf:fin,num:sg]).
lexEntry(tv,[symbol:discard,syntax:[discard],inf:fin,num:pl]).

lexEntry(tv,[symbol:eat,syntax:[eat],inf:inf,num:sg]).
lexEntry(tv,[symbol:eat,syntax:[eats],inf:fin,num:sg]).
lexEntry(tv,[symbol:eat,syntax:[eat],inf:fin,num:pl]).

lexEntry(tv,[symbol:enjoy,syntax:[enjoy],inf:inf,num:sg]).
lexEntry(tv,[symbol:enjoy,syntax:[enjoys],inf:fin,num:sg]).
lexEntry(tv,[symbol:enjoy,syntax:[enjoy],inf:fin,num:pl]).

lexEntry(tv,[symbol:hate,syntax:[hate],inf:inf,num:sg]).
lexEntry(tv,[symbol:hate,syntax:[hates],inf:fin,num:sg]).
lexEntry(tv,[symbol:hate,syntax:[hate],inf:fin,num:pl]).

lexEntry(tv,[symbol:have,syntax:[have],inf:inf,num:sg]).
lexEntry(tv,[symbol:have,syntax:[has],inf:fin,num:sg]).
lexEntry(tv,[symbol:have,syntax:[have],inf:fin,num:pl]).

lexEntry(tv,[symbol:kill,syntax:[kill],inf:inf,num:sg]).
lexEntry(tv,[symbol:kill,syntax:[kills],inf:fin,num:sg]).
lexEntry(tv,[symbol:kill,syntax:[kill],inf:fin,num:pl]).

lexEntry(tv,[symbol:know,syntax:[know],inf:inf,num:sg]).
lexEntry(tv,[symbol:know,syntax:[knows],inf:fin,num:sg]).
lexEntry(tv,[symbol:know,syntax:[know],inf:fin,num:pl]).

lexEntry(tv,[symbol:like,syntax:[like],inf:inf,num:sg]).
lexEntry(tv,[symbol:like,syntax:[likes],inf:fin,num:sg]).
lexEntry(tv,[symbol:like,syntax:[like],inf:fin,num:pl]).

lexEntry(tv,[symbol:love,syntax:[love],inf:inf,num:sg]).
lexEntry(tv,[symbol:love,syntax:[loves],inf:fin,num:sg]).
lexEntry(tv,[symbol:love,syntax:[love],inf:fin,num:pl]).

lexEntry(tv,[symbol:pickup,syntax:[pick,up],inf:inf,num:sg]).
lexEntry(tv,[symbol:pickup,syntax:[picks,up],inf:fin,num:sg]).
lexEntry(tv,[symbol:pickup,syntax:[pick,up],inf:fin,num:pl]).

lexEntry(tv,[symbol:shoot,syntax:[shot],inf:inf,num:sg]).
lexEntry(tv,[symbol:shoot,syntax:[shot],inf:fin,num:sg]).
lexEntry(tv,[symbol:shoot,syntax:[shoots],inf:fin,num:sg]).
lexEntry(tv,[symbol:shoot,syntax:[shoot],inf:fin,num:pl]).


/*========================================================================
   Copula
========================================================================*/

lexEntry(cop,[pol:pos,syntax:[is],inf:fin,num:sg]).
lexEntry(cop,[pol:neg,syntax:[is,not],inf:fin,num:sg]).
lexEntry(cop,[pol:pos,syntax:[are],inf:fin,num:pl]).
lexEntry(cop,[pol:neg,syntax:[are,not],inf:fin,num:pl]).


/*========================================================================
   Prepositions
========================================================================*/

lexEntry(prep,[symbol:about,syntax:[about]]).
lexEntry(prep,[symbol:in,syntax:[in]]).
lexEntry(prep,[symbol:of,syntax:[of]]).
lexEntry(prep,[symbol:with,syntax:[with]]).


/*========================================================================
   Adjectives
========================================================================*/

lexEntry(adj,[symbol:big,syntax:[big]]).
lexEntry(adj,[symbol:blue,syntax:[blue]]).
lexEntry(adj,[symbol:female,syntax:[female]]).
lexEntry(adj,[symbol:happy,syntax:[happy]]).
lexEntry(adj,[symbol:kahuna,syntax:[kahuna]]).
lexEntry(adj,[symbol:male,syntax:[male]]).
lexEntry(adj,[symbol:married,syntax:[married]]).
lexEntry(adj,[symbol:red,syntax:[red]]).
lexEntry(adj,[symbol:sad,syntax:[sad]]).
lexEntry(adj,[symbol:small,syntax:[small]]).
lexEntry(adj,[symbol:tall,syntax:[tall]]).


/*========================================================================
   Relative Pronouns
========================================================================*/

lexEntry(relpro,[syntax:[who]]).
lexEntry(relpro,[syntax:[that]]).


/*========================================================================
   Coordinations
========================================================================*/

lexEntry(coord,[syntax:[and],type:conj]).
lexEntry(coord,[syntax:[or],type:disj]).


/*========================================================================
   Auxiliary Verbs
========================================================================*/

lexEntry(av,[syntax:[does],inf:fin,num:sg,pol:pos]).
lexEntry(av,[syntax:[does,not],inf:fin,num:sg,pol:neg]).
lexEntry(av,[syntax:[did],inf:fin,num:sg,pol:pos]).
lexEntry(av,[syntax:[did,not],inf:fin,num:sg,pol:neg]).


/*========================================================================
   Pronouns (third person)
========================================================================*/

lexEntry(pro,[symbol:male,  ref:no, syntax:[he]]).
lexEntry(pro,[symbol:male,  ref:no, syntax:[him]]).
lexEntry(pro,[symbol:male,  ref:yes,syntax:[himself]]).
lexEntry(pro,[symbol:female,ref:no, syntax:[she]]).
lexEntry(pro,[symbol:female,ref:yes,syntax:[herself]]).
lexEntry(pro,[symbol:female,ref:no, syntax:[her]]).
lexEntry(pro,[symbol:neuter,ref:no, syntax:[it]]).
lexEntry(pro,[symbol:neuter,ref:yes,syntax:[itself]]).


/*************************************************************************

    File: presupTestSuite.pl
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

:- swi_module(presupTestSuite,[discourse/2]).


/*========================================================================
    Example Discourses
========================================================================*/

discourse([every,man,likes,himself],1).

discourse([no,man,likes,himself],1).

discourse([no,man,likes,herself],0).

discourse([if,a,man,walks,then,he,smokes],1).

discourse([if,a,man,walks,then,she,smokes],0).

discourse([a,man,walks,the,man,smokes],1).

discourse([mia,dances,she,likes,vincent],1).

discourse([mia,dances,she,does,not,like,vincent],1).

discourse([if,vincent,dances,then,mia,dances],1).

discourse([mia,or,vincent,dances],1).

discourse([every,customer,likes,the,five,dollar,shake],1).

discourse([every,man,likes,his,car],2).

discourse([every,man,likes,the,car],3).

discourse([every,man,that,has,a,car,likes,it],1).

discourse([every,man,that,has,a,car,likes,his,car],2).

discourse([if,vincent,has,a,car,then,he,likes,his,car],2).

discourse([mia,walks,mia,smokes],1).


/*************************************************************************

    File: lexicalKnowledge.pl
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

:- swi_module(lexicalKnowledge,[lexicalKnowledge/3]).

/*========================================================================
   Axioms for Lexical Knowledge: Nouns
========================================================================*/

lexicalKnowledge(event,1,Axiom):-
   Axiom = all(A,imp(event(A),thing(A))).

lexicalKnowledge(entity,1,Axiom):- 
   Axiom = all(A,imp(entity(A),thing(A))).

lexicalKnowledge(object,1,Axiom):- 
   Axiom = all(A,imp(object(A),entity(A))).

lexicalKnowledge(organism,1,Axiom):- 
   Axiom = all(A,imp(organism(A),entity(A))).

lexicalKnowledge(food,1,Axiom):- 
   Axiom = all(A,imp(food(A),object(A))).

lexicalKnowledge(artifact,1,Axiom):- 
   Axiom = all(A,imp(artifact(A),object(A))).

lexicalKnowledge(building,1,Axiom):- 
   Axiom = all(A,imp(building(A),artifact(A))).

lexicalKnowledge(instrument,1,Axiom):- 
   Axiom = all(A,imp(instrument(A),artifact(A))).

lexicalKnowledge(animal,1,Axiom):- 
   Axiom = all(A,imp(animal(A),organism(A))).

lexicalKnowledge(person,1,Axiom):- 
   Axiom = all(A,imp(person(A),organism(A))).

lexicalKnowledge(plant,1,Axiom):- 
   Axiom = all(A,imp(plant(A),organism(A))).

lexicalKnowledge(man,1,Axiom):- 
   Axiom = all(A,imp(man(A),person(A))).

lexicalKnowledge(woman,1,Axiom):- 
   Axiom = all(A,imp(woman(A),person(A))).

lexicalKnowledge(beverage,1,Axiom):- 
   Axiom = all(A,imp(beverage(A),food(A))).

lexicalKnowledge(foodstuff,1,Axiom):- 
   Axiom = all(A,imp(foodstuff(A),food(A))).

lexicalKnowledge(container,1,Axiom):- 
   Axiom = all(A,imp(container(A),instrument(A))).

lexicalKnowledge(device,1,Axiom):- 
   Axiom = all(A,imp(device(A),instrument(A))).

lexicalKnowledge(cup,1,Axiom):- 
   Axiom = all(A,imp(cup(A),container(A))).

lexicalKnowledge(glass,1,Axiom):- 
   Axiom = all(A,imp(glass(A),container(A))).

lexicalKnowledge(burger,1,Axiom):- 
   Axiom = all(A,imp(burger(A),foodstuff(A))).

lexicalKnowledge(qpwc,1,Axiom):- 
   Axiom = all(A,imp(qpwc(A),foodstuff(A))).

lexicalKnowledge(boxer,1,Axiom):- 
   Axiom = all(A,imp(boxer(A),person(A))).

lexicalKnowledge(boss,1,Axiom):- 
   Axiom = all(A,imp(boss(A),person(A))).

lexicalKnowledge(criminal,1,Axiom):- 
   Axiom = all(A,imp(criminal(A),person(A))).

lexicalKnowledge(customer,1,Axiom):- 
   Axiom = all(A,imp(customer(A),person(A))).

lexicalKnowledge(owner,1,Axiom):- 
   Axiom = all(A,imp(owner(A),person(A))).

lexicalKnowledge(robber,1,Axiom):- 
   Axiom = all(A,imp(robber(A),person(A))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(vehicle(A),instrument(A))).

lexicalKnowledge(car,1,Axiom):- 
   Axiom = all(A,imp(car(A),vehicle(A))).

lexicalKnowledge(chainsaw,1,Axiom):- 
   Axiom = all(A,imp(chainsaw(A),device(A))).

lexicalKnowledge(drug,1,Axiom):- 
   Axiom = all(A,imp(drug(A),artifact(A))).

lexicalKnowledge(episode,1,Axiom):- 
   Axiom = all(A,imp(episode(A),event(A))).

lexicalKnowledge(footmassage,1,Axiom):- 
   Axiom = all(A,imp(footmassage(A),event(A))).

lexicalKnowledge(fdshake,1,Axiom):- 
   Axiom = all(A,imp(fdshake(A),beverage(A))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(weapon(A),instrument(A))).

lexicalKnowledge(gun,1,Axiom):- 
   Axiom = all(A,imp(gun(A),weapon(A))).

lexicalKnowledge(hammer,1,Axiom):- 
   Axiom = all(A,imp(hammer(A),device(A))).

lexicalKnowledge(hashbar,1,Axiom):- 
   Axiom = all(A,imp(hashbar(A),building(A))).

lexicalKnowledge(restaurant,1,Axiom):- 
   Axiom = all(A,imp(restaurant(A),building(A))).

lexicalKnowledge(husband,1,Axiom):- 
   Axiom = all(A,imp(husband(A),man(A))).

lexicalKnowledge(joke,1,Axiom):- 
   Axiom = all(A,imp(joke(A),event(A))).

lexicalKnowledge(needle,1,Axiom):- 
   Axiom = all(A,imp(needle(A),device(A))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(piercing(A),artifact(A))).

lexicalKnowledge(radio,1,Axiom):- 
   Axiom = all(A,imp(radio(A),instrument(A))).

lexicalKnowledge(suitcase,1,Axiom):- 
   Axiom = all(A,imp(suitcase(A),container(A))).

lexicalKnowledge(shotgun,1,Axiom):- 
   Axiom = all(A,imp(shotgun(A),gun(A))).

lexicalKnowledge(sword,1,Axiom):- 
   Axiom = all(A,imp(sword(A),weapon(A))).

lexicalKnowledge(wife,1,Axiom):- 
   Axiom = all(A,imp(wife(A),woman(A))).

lexicalKnowledge(entity,1,Axiom):- 
   Axiom = all(A,imp(entity(A),not(event(A)))).

lexicalKnowledge(organism,1,Axiom):- 
   Axiom = all(A,imp(organism(A),not(object(A)))).

lexicalKnowledge(artifact,1,Axiom):- 
   Axiom = all(A,imp(artifact(A),not(food(A)))).

lexicalKnowledge(person,1,Axiom):- 
   Axiom = all(A,imp(person(A),not(animal(A)))).

lexicalKnowledge(plant,1,Axiom):- 
   Axiom = all(A,imp(plant(A),not(animal(A)))).

lexicalKnowledge(plant,1,Axiom):- 
   Axiom = all(A,imp(plant(A),not(person(A)))).

lexicalKnowledge(instrument,1,Axiom):- 
   Axiom = all(A,imp(instrument(A),not(building(A)))).

lexicalKnowledge(drug,1,Axiom):- 
   Axiom = all(A,imp(drug(A),not(building(A)))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(piercing(A),not(building(A)))).

lexicalKnowledge(drug,1,Axiom):- 
   Axiom = all(A,imp(drug(A),not(instrument(A)))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(piercing(A),not(instrument(A)))).

lexicalKnowledge(piercing,1,Axiom):- 
   Axiom = all(A,imp(piercing(A),not(drug(A)))).

lexicalKnowledge(woman,1,Axiom):- 
   Axiom = all(A,imp(woman(A),not(man(A)))).

lexicalKnowledge(device,1,Axiom):- 
   Axiom = all(A,imp(device(A),not(radio(A)))).

lexicalKnowledge(container,1,Axiom):- 
   Axiom = all(A,imp(container(A),not(radio(A)))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(vehicle(A),not(radio(A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(weapon(A),not(radio(A)))).

lexicalKnowledge(container,1,Axiom):- 
   Axiom = all(A,imp(container(A),not(device(A)))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(vehicle(A),not(device(A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(weapon(A),not(device(A)))).

lexicalKnowledge(vehicle,1,Axiom):- 
   Axiom = all(A,imp(vehicle(A),not(container(A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(weapon(A),not(container(A)))).

lexicalKnowledge(weapon,1,Axiom):- 
   Axiom = all(A,imp(weapon(A),not(vehicle(A)))).

lexicalKnowledge(beverage,1,Axiom):- 
   Axiom = all(A,imp(beverage(A),not(foodstuff(A)))).

lexicalKnowledge(footmassage,1,Axiom):- 
   Axiom = all(A,imp(footmassage(A),not(episode(A)))).

lexicalKnowledge(joke,1,Axiom):- 
   Axiom = all(A,imp(joke(A),not(episode(A)))).

lexicalKnowledge(joke,1,Axiom):- 
   Axiom = all(A,imp(joke(A),not(footmassage(A)))).

lexicalKnowledge(cup,1,Axiom):- 
   Axiom = all(A,imp(cup(A),not(glass(A)))).

lexicalKnowledge(suitcase,1,Axiom):- 
   Axiom = all(A,imp(suitcase(A),not(glass(A)))).

lexicalKnowledge(suitcase,1,Axiom):- 
   Axiom = all(A,imp(suitcase(A),not(cup(A)))).

lexicalKnowledge(chainsaw,1,Axiom):- 
   Axiom = all(A,imp(chainsaw(A),not(hammer(A)))).

lexicalKnowledge(needle,1,Axiom):- 
   Axiom = all(A,imp(needle(A),not(hammer(A)))).

lexicalKnowledge(needle,1,Axiom):- 
   Axiom = all(A,imp(needle(A),not(chainsaw(A)))).

lexicalKnowledge(unmarried,1,Axiom):- 
   Axiom = all(A,imp(unmarried(A),not(married(A)))).

lexicalKnowledge(burger,1,Axiom):- 
   Axiom = all(A,imp(burger(A),not(qpwc(A)))).

lexicalKnowledge(restaurant,1,Axiom):- 
   Axiom = all(A,imp(restaurant(A),not(hashbar(A)))).

lexicalKnowledge(gun,1,Axiom):- 
   Axiom = all(A,imp(gun(A),not(sword(A)))).

lexicalKnowledge(wife,1,Axiom):- 
   Axiom = all(A,imp(wife(A),married(A))).

lexicalKnowledge(man,1,Axiom):- 
   Axiom = all(A,imp(man(A),male(A))).

lexicalKnowledge(plant,1,Axiom):- 
   Axiom = all(A,imp(plant(A),neuter(A))).

lexicalKnowledge(object,1,Axiom):- 
   Axiom = all(A,imp(object(A),neuter(A))).

lexicalKnowledge(event,1,Axiom):- 
   Axiom = all(A,imp(event(A),neuter(A))).

lexicalKnowledge(woman,1,Axiom):- 
   Axiom = all(A,imp(woman(A),female(A))).

lexicalKnowledge(male,1,Axiom):- 
   Axiom = all(A,imp(male(A),not(female(A)))).

lexicalKnowledge(neuter,1,Axiom):- 
   Axiom = all(A,imp(neuter(A),not(male(A)))).

lexicalKnowledge(female,1,Axiom):- 
   Axiom = all(A,imp(female(A),not(neuter(A)))).


/*========================================================================
   Axioms for Lexical Knowledge: Proper Names
========================================================================*/

lexicalKnowledge(mia,0,Axiom):- 
   Axiom = all(A,imp(eq(A,mia),woman(A))).

lexicalKnowledge(vincent,0,Axiom):- 
   Axiom = all(A,imp(eq(A,vincent),man(A))).


/*========================================================================
   Axioms for Lexical Knowledge: Adjectives
========================================================================*/

lexicalKnowledge(red,1,Axiom):- 
   Axiom = all(A,imp(red(A),not(blue(A)))).

lexicalKnowledge(big,1,Axiom):- 
   Axiom = all(A,imp(big(A),not(small(A)))).

lexicalKnowledge(sad,1,Axiom):- 
   Axiom = all(A,imp(sad(A),not(happy(A)))).


/*========================================================================
   Axioms for Lexical Knowledge: Intransitive Verbs
========================================================================*/

lexicalKnowledge(collapse,1,Axiom):-
   Axiom = all(X,imp(collapse(X),or(person(X),building(X)))).

lexicalKnowledge(dance,1,Axiom):- 
   Axiom = all(X,imp(dance(X),person(X))).

lexicalKnowledge(die,1,Axiom):- 
   Axiom = all(X,imp(die(X),organism(X))).

lexicalKnowledge(growl,1,Axiom):- 
   Axiom = all(X,imp(growl(X),or(animal(X),person(X)))).


/*========================================================================
   Axioms for Lexical Knowledge: Transitive Verbs
========================================================================*/

lexicalKnowledge(clean,2,Axiom):- 
   Axiom = all(X,all(Y,imp(clean(X,Y),and(person(X),artifact(Y))))).

lexicalKnowledge(drink,2,Axiom):-    
   Axiom = all(X,all(Y,imp(drink(X,Y),and(person(X),beverage(Y))))).

lexicalKnowledge(eat,2,Axiom):- 
   Axiom = all(X,all(Y,imp(eat(X,Y),
               and(or(person(X),animal(X)),
                   and(edible(Y),food(Y)))))).

lexicalKnowledge(member,2,Axiom):-
   Axiom = and(all(X,all(G,imp(member(X,G),and(group(G),thing(X))))),
               and(all(X,imp(group(X),not(thing(X)))),
                   all(G,imp(group(G),some(X,and(member(X,G),some(Y,and(member(Y,G),not(eq(X,Y)))))))))).



