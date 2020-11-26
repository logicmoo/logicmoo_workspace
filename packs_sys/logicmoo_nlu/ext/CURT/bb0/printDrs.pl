/*************************************************************************

         name: printDrs.pl 
      version: August 7, 2001
  description: Pretty Print for DRS
       author: Patrick Blackburn & Johan Bos
 
*************************************************************************/

/*=====================================================================
     Module Declaration
=====================================================================*/

:- module(printDrs,[printDrs/1]).

:- ensure_loaded(comsemOperators).

:- use_module(comsemPredicates,[appendLists/3]).


/*=====================================================================
     Counter for discourse referents
=====================================================================*/

:- dynamic counter/1.

counter(0).
   

/*=====================================================================
     Main Predicate
=====================================================================*/

printDrs(Drs):- 
   retract(counter(_)), 
   assert(counter(1)),
   \+ \+ (formatDrs(Drs,Lines,_), printDrsLines(user,Lines)).


/*=====================================================================
     Print a DRS as a list of lines
=====================================================================*/

printDrsLines(_,[]).
printDrsLines(Stream,[Line|Rest]):-
   name(L,Line), 
   nl(Stream), 
   write(Stream,L),
   printDrsLines(Stream,Rest).


/*=====================================================================
     Format DRSs
=====================================================================*/

formatDrs(X,Lines,Length):-
   var(X), !,
   makeConstant(X), 
   nonvar(X),
   formatDrs(X,Lines,Length).

formatDrs(X,Lines,Length):-
   atom(X), 
   name(X,A),
   addSpaces(A,[K1,K2,K3,K4,K5]), 
   Lines=[[32,32,32,32,32],[32,32,32,32,32],[32,32,32,32,32],[K1,K2,K3,K4,K5]],
   Length=4.

formatDrs(drs(Dom,Cond),[[32|Top],Refs2,[124|Line]|CondLines2],Length):-
   formatConds(Cond,[]-CondLines1,0-CondLength),
   formatRefs(Dom,[]-Refs),
   length([_,_|Refs],RefLength),
   (
    RefLength > CondLength, !, Length = RefLength
   ; 
    Length = CondLength 
   ), 
   closeConds(CondLines1,CondLines2,Length),
   Dif is (Length - RefLength) + 1,
   closeLine([124|Refs],Refs2,Dif,[124]),
   formatLine(95,Length,[32]-Top),
   formatLine(45,Length,[124]-Line).

formatDrs(merge(Drs1,Drs2),Lines3,Length):-
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 7,
   combineLines2(Lines1,Lines2,Lines3,' (+) ',M),
   Length is N1 + N2 + 7.

formatDrs(alfa(_,_,Drs1,Drs2),Lines3,Length):-
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 7,
   combineLines2(Lines1,Lines2,Lines3,' alfa',M),
   Length is N1 + N2 + 7.

formatDrs(lambda(Arg,Drs),Lines3,Length):-!,
   makeConstant(Arg),
   Lines1 = [[32],[32],[32],[32]], 
   formatDrs(Drs,Lines2,N2),
   M = 7,
   name(Arg,Code1),
   appendLists(Code1,[46],Code2),   
   name(Lambda,[76,32|Code2]),
   combineLines2(Lines1,Lines2,Lines3,Lambda,M),
   Length is N2 + M.

formatDrs(Drs,Lines,Length):-
   formatAction(Drs,Lines,Length).


/*=====================================================================
     Format Discourse Referents
=====================================================================*/

formatRefs([],L-L).

formatRefs([Ref|Rest],In-Out):-
   makeConstant(Ref), 
   name(Ref,Code),
   formatRefs(Rest,In-Temp),
   appendLists(Temp,[32|Code],Out).


/*=====================================================================
     Make Discourse Referents atomic
=====================================================================*/

makeConstant(X):- atom(X),!.

makeConstant(X):- 
   var(X),
   retract(counter(N)),
   name(N,Number), 
   name(X,[120|Number]),
   M is N+1,
   assert(counter(M)).


/*=====================================================================
     Format a Line
=====================================================================*/

formatLine(_,1,L-L):-!.

formatLine(Code,N,In-[Code|Out]):-
   M is N - 1, formatLine(Code,M,In-Out).


/*=====================================================================
     Formatting Conditions
=====================================================================*/

formatConds([],L-L,N-N).

formatConds([Drs1 > Drs2|Rest],L1-L2,N0-N4):-!,
   formatConds(Rest,L1-Lines,N0-N3),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 7,
   combineLines(Lines1,Lines2,Lines3,' ==> ',M),
   appendLists(Lines3,Lines,L2),
   Length is N1 + N2 + 10,
   (Length > N3, !, N4 = Length; N4 = N3).

formatConds([Drs1 v Drs2|Rest],L1-L2,N0-N4):-!,
   formatConds(Rest,L1-Lines,N0-N3),
   formatDrs(Drs1,Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 7,
   combineLines(Lines1,Lines2,Lines3,'  V  ',M),
   appendLists(Lines3,Lines,L2),
   Length is N1 + N2 + 10,
   (Length > N3, !, N4 = Length; N4 = N3).

formatConds([question(X,Drs1,Drs2)|Rest],L1-L2,N0-N4):-!,
   Drs1=drs(D,C),
   formatConds(Rest,L1-Lines,N0-N3),
   formatDrs(drs([X|D],C),Lines1,N1),
   formatDrs(Drs2,Lines2,N2),
   M is N1 + 7,
   combineLines(Lines1,Lines2,Lines3,' <?> ',M),
   appendLists(Lines3,Lines,L2),
   Length is N1 + N2 + 10,
   (Length > N3, !, N4 = Length; N4 = N3).

formatConds([~ Drs|Rest],L1-L2,N0-N3):-!,
   formatConds(Rest,L1-Lines,N0-N1),
   formatDrs(Drs,[A,B,C,D|Lines1],N2),
   combineLines([],Lines1,Lines2,6),
   appendLists([[124,32,32,32,32,32,32|A],[124,32,32,32,32,32,32|B],[124,32,32,95,95,32,32|C],[124,32,32,32,32,124,32|D]|Lines2],Lines,L2),
   Length is N2 + 9,
   (Length > N1, !, N3 = Length; N3 = N1).

formatConds([A=B|Rest],In-[[124,32|Line]|Out],N0-N2):-!,
   formatConds(Rest,In-Out,N0-N1),
   makeConstant(A),
   name(A,L1), 
   makeConstant(B),
   name(B,L2),
   appendLists(L1,[32,61,32|L2],Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).

formatConds([Basic|Rest],In-[[124,32|Line]|Out],N0-N2):-
   formatConds(Rest,In-Out,N0-N1),
   formatBasic(Basic,Line),
   length([_,_,_|Line],Length),
   (Length > N1, !, N2 is Length; N2 = N1).


/*=====================================================================
     Formatting Basic Condition
=====================================================================*/

formatBasic(Basic,Line):-
   Basic =.. [uds,[]], !,
   name(uds,F), 
   appendLists(F,[58,91,93],Line).

formatBasic(Basic,Line):-
   Basic =.. [uds,[Arg1]], !,
   makeConstant(Arg1),
   name(uds,F),
   name(Arg1,A),
   appendLists(A,[93],T1),
   appendLists(F,[58,91|T1],Line).

formatBasic(Basic,Line):-
   Basic =.. [uds,[Arg1,Arg2]], !,
   name(uds,F),
   makeConstant(Arg1),
   name(Arg1,A1),
   appendLists(F,[58,91|A1],T1),
   makeConstant(Arg2),
   name(Arg2,A2),
   appendLists(T1,[44|A2],T2),
   appendLists(T2,[93],Line).

   
formatBasic(Basic,Line):-
   Basic =.. [Functor,Arg],
   name(Functor,F),
   makeConstant(Arg),   
   name(Arg,A),
   appendLists(F,[40|A],T),
   appendLists(T,[41],Line).
   
formatBasic(Basic,Line):-
   Basic =.. [Functor,Arg1,Arg2],
   name(Functor,F),
   makeConstant(Arg1),
   name(Arg1,A1),
   makeConstant(Arg2),
   name(Arg2,A2),
   appendLists(F,[40|A1],T1),
   appendLists(T1,[44|A2],T2),
   appendLists(T2,[41],Line).
 
formatBasic(Basic,Line):-
   Basic =.. [Functor,Arg1,Arg2,Arg3],
   name(Functor,F),
   makeConstant(Arg1),
   name(Arg1,A1),
   makeConstant(Arg2),
   name(Arg2,A2),
   makeConstant(Arg3),
   name(Arg3,A3),
   appendLists(F,[40|A1],T1),
   appendLists(T1,[44|A2],T2),
   appendLists(T2,[44|A3],T3),
   appendLists(T3,[41],Line).

formatBasic(Basic,Line):-
   Basic =.. [Functor,Arg1,Arg2,Arg3,Arg4],
   name(Functor,F),
   makeConstant(Arg1),
   name(Arg1,A1),
   makeConstant(Arg2),
   name(Arg2,A2),
   makeConstant(Arg3),
   name(Arg3,A3),
   makeConstant(Arg4),
   name(Arg4,A4),
   appendLists(F,[40|A1],T1),
   appendLists(T1,[44|A2],T2),
   appendLists(T2,[44|A3],T3),
   appendLists(T3,[44|A4],T4),
   appendLists(T4,[41],Line).


/*=====================================================================
     Combine two lines
=====================================================================*/

combineLines([A1,B1,C1,D1|Rest1],[A2,B2,C2,D2|Rest2],Result,Op,N):-
   combineLines([A1,B1,C1],[A2,B2,C2],Firsts,N),
   name(Op,Code),
   appendLists(Code,D2,T),
   appendLists([124,32|D1],T,D),
   combineLines(Rest1,Rest2,Rest,N),
   appendLists(Firsts,[D|Rest],Result).

combineLines([],[],[],_).
combineLines([],[A2|Rest2],[A|Rest],N):-
   closeLine([124],A1,N,[]),
   appendLists(A1,A2,A),
   combineLines([],Rest2,Rest,N).
combineLines([A1|Rest1],[],[[124,32|A1]|Rest],N):-
   combineLines(Rest1,[],Rest,N).
combineLines([A1|Rest1],[A2|Rest2],[A|Rest],N):-
   appendLists([124,32|A1],[32,32,32,32,32|A2],A),
   combineLines(Rest1,Rest2,Rest,N).

combineLines2([A1,B1,C1,D1|Rest1],[A2,B2,C2,D2|Rest2],Result,Op,N):-
   combineLines2([A1,B1,C1],[A2,B2,C2],Firsts,N),
   name(Op,Code),
   appendLists(Code,D2,T),
   appendLists([32|D1],T,D),
   combineLines2(Rest1,Rest2,Rest,N),
   appendLists(Firsts,[D|Rest],Result).

combineLines2([],[],[],_).
combineLines2([],[A2|Rest2],[A|Rest],N):-
   closeLine([],A1,N,[]),
   appendLists(A1,A2,A),
   combineLines2([],Rest2,Rest,N).
combineLines2([A1|Rest1],[],[[32|A1]|Rest],N):-
   combineLines2(Rest1,[],Rest,N).
combineLines2([A1|Rest1],[A2|Rest2],[A|Rest],N):-
   appendLists([32|A1],[32,32,32,32,32|A2],A),
   combineLines2(Rest1,Rest2,Rest,N).


/*=====================================================================
     Close Conditions (add '|')
=====================================================================*/

closeConds([],[[124|Bottom]],Length):-
   formatLine(95,Length,[124]-Bottom).

closeConds([Line|Rest1],[New|Rest2],Length):-
   length(Line,L),
   R is Length - L,
   closeLine(Line,New,R,[124]),
   closeConds(Rest1,Rest2,Length).

closeLine(Line,New,N,Accu):- N < 1, !, appendLists(Line,Accu,New).
closeLine(Line,New,N,Accu):- M is N - 1, closeLine(Line,New,M,[32|Accu]).


/*=====================================================================
     Add Spaces in a Line
=====================================================================*/

addSpaces([A,B],[32,32,32,A,B]).
addSpaces([A,B,C],[32,32,A,B,C]).
addSpaces([A,B,C,D],[32,A,B,C,D]).
addSpaces([A,B,C,D,E],[A,B,C,D,E]).

