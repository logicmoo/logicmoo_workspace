/*************************************************************************

         name: drsTools.pl (Volume 2, Chapter 2)
      version: January 10, 2003
  description: Additional predicates for working with DRSs
       author: Christof Rumpf
 
*************************************************************************/

:- module(drsTools,[exDRS/2,
		    checkDRS/1,
		    ppDRS/1]).

:- use_module(drs2fol,[drs2fol/2]).		   

:- ensure_loaded(comsemOperators).

:- dynamic(exDRS/2).

% ppDRS(+DRS)
% Pretty print DRSs.

ppDRS(DRS):- \+ \+ (numbervars(DRS,0,_), ppDRS(DRS,0)).

ppDRS(drs(Refs,Conds),T):- !,
	ppl(T),
	ppRefs(Refs,T),
	ppl(T),
	ppConds(Conds,T),
	ppl(T).
	
ppl(T):- tab(T), write('-----------'), nl. 

ppRefs(Refs,T):- tab(T), ppRefs(Refs), nl.
ppRefs([]).
ppRefs([R|Rs]):- write(R), tab(1), ppRefs(Rs).

ppConds([],_):- !.
ppConds([DRS1 > DRS2|Cs],T):- !,
	T1 is T+3,
	ppDRS(DRS1,T1),
	tab(T1), write('==>'), nl,
	ppDRS(DRS2,T1),
	ppConds(Cs,T).
ppConds([DRS1 v DRS2|Cs],T):- !,
	T1 is T+3,
	ppDRS(DRS1,T1),
	tab(T1), write('OR'), nl,
	ppDRS(DRS2,T1),
	ppConds(Cs,T).
ppConds([~DRS|Cs],T):- !,
	T1 is T+3,
	tab(T1), write('NOT'), nl,
	ppDRS(DRS,T1),
	ppConds(Cs,T).
ppConds([C|Cs],T):-
	tab(T), write(C), nl,
	ppConds(Cs,T).
	
% checkDRS(?Integer)
% Pretty print example DRS and display fol transformation.

checkDRS(N):-
	exDRS(N,DRS),
	drs2fol(DRS,FOL),
	numbervars(FOL,0,_),
	ppDRS(DRS),
	nl, write(FOL), nl, nl.

% exDRS(?Integer, ?DRS)
% Example DRSs.

exDRS(1,drs([],[drs([X],[woman(X)]) > drs([Y],[boxer(Y),love(X,Y)])])).
exDRS(2,drs([Y],[drs([X],[woman(X)]) > drs([],[boxer(Y),love(X,Y)])])).
exDRS(3,drs([],[drs([X,Y],[woman(X)]) > drs([],[boxer(Y),love(X,Y)])])).
exDRS(4,drs([X,Y],[drs([],[woman(X)]) > drs([],[boxer(Y),love(X,Y)])])).
exDRS(5,drs([],[drs([X,Y],[man(X),big_cahuna_burger(Y),eat(X,Y)]) > drs([V,W],[enjoy(V,W),V=X,W=Y])])).
exDRS(6,merge(merge(merge(drs([A],[]),drs([],[woman(A),female(A)])),drs([],[snort(A)])),alfa(B,nonrefl,female(B),drs([],[collapse(B)])))).


