/*************************************************************************

         name: matchDRT.pl
      version: Feb 15, 1999
  description: Matching for DRSs, including consistency check
      authors: Patrick Blackburn & Johan Bos

*************************************************************************/

:- module(matchDRT,[matchDrs/4]).

:- use_module(comsemPredicates,[memberList/2,compose/3]),
   use_module(semOntology,[consistent/2]).

/*========================================================================
   Partial Match
========================================================================*/

matchDrs(X,drs(D1,C1),drs(D2,C2),drs(D3,C3)):-
	memberList(X,D2),
	mergeLists(D1,D2,D3),
	mergeLists(C1,C2,C3),
	consistentConditions(X,C3).

consistentConditions(X,Conds):-
   \+ (
	  memberList(Cond1,Conds),
	  memberList(Cond2,Conds), \+ Cond1=Cond2,
	  compose(Cond1,Symbol1,[Y]), Y==X,
	  compose(Cond2,Symbol2,[Z]), Z==X,
	  \+ consistent(Symbol1,Symbol2)
      ).
	  
/*========================================================================
   Merging of Lists
========================================================================*/

mergeLists([],L,L).

mergeLists([X|R],L1,L2):-
	memberList(Y,L1),
	X==Y, !,
	mergeLists(R,L1,L2).

mergeLists([X|R],L1,[X|L2]):-
	mergeLists(R,L1,L2).


