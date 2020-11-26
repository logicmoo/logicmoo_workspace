/**********************************************************************

         name: acDRT.pl (Volume 2, Chapter 5)
      version: July 30, 2001
  description: Presupposition Projection with acceptability constraints
      authors: Patrick Blackburn & Johan Bos

**********************************************************************/

:- module(acDRT,[consistentDRT/0,
		 informativeDRT/0,
		 localDRT/0]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[memberList/2,
				printRepresentations/1]).

:- use_module(presupDRT,[presupDRT/2,presupDRT/3]).

:- use_module(acceptabilityConstraints,[consistent/2,
					noFreeVars/1,
					informative/2,
				        localConsistent/1,
				        localInformative/1]).

:- use_module(printDrs,[printDrs/1]).


/*========================================================================
   Consistency
========================================================================*/

consistentDRT:-
   format('~nEnter discourse:',[]),
   readLine(Discourse),
   presupDRT(Discourse,DRSs),
   consistentReadings(DRSs,_).


/*========================================================================
   Informativity
========================================================================*/

informativeDRT:-
   format('~nEnter old discourse:',[]),
   readLine(OldDiscourse),
   presupDRT(OldDiscourse,OldDRSs),
   consistentReadings(OldDRSs,OldConsistentDRSs),
   format('~n~nEnter new discourse:',[]),
   readLine(NewDiscourse),
   setof(New,Old^(memberList(Old,OldConsistentDRSs),presupDRT(NewDiscourse,Old,New)),NewDRSs),
   consistentReadings(NewDRSs,NewConsistentDRSs),
   setof((Old,New),(memberList(Old,OldDRSs),memberList(New,NewConsistentDRSs)),OldNew),
   informativeReadings(OldNew,_).


/*========================================================================
   Local Informativity and Consistency
========================================================================*/

localDRT:-
   format('~nEnter discourse:',[]),
   readLine(Discourse),
   presupDRT(Discourse,DRSs),
   localReadings(DRSs).


/*========================================================================
   Select Consistent Readings 
========================================================================*/

consistentReadings([],[]).

consistentReadings([Drs|Readings1],Readings3):-
	format('~n~nDRS: ',[]),
	printDrs(Drs), nl,
	(
	    noFreeVars(Drs), !,
	    (
		consistent(Drs,Model), !,
		format('~n* Consistent *~nModel: ~p',[Model]),
		Readings3=[Drs|Readings2]
	    ;
		format('~n* Inconsistent *',[]),
		Readings3=Readings2
	    )
	;
	    format('~nContains free variables',[]),
	    Readings3=Readings2
	),
	consistentReadings(Readings1,Readings2).



/*========================================================================
   Select Informative Readings 
========================================================================*/

informativeReadings([],[]).

informativeReadings([(Old,New)|Readings1],Readings3):-
	(   
	    informative(Old,New), !,
	    format('~n* Informative *',[]),
	    Readings3=[New|Readings2]
	;   
	    format('~n* Uninformative *',[]),
	    Readings3=Readings2
	),
	informativeReadings(Readings1,Readings2).




/*========================================================================
   Select Local Consistent and Informative Readings 
========================================================================*/

localReadings([]).

localReadings([Drs|Readings]):-
	format('~n~nDRS: ',[]),
	printDrs(Drs), nl,
	(
	    noFreeVars(Drs),
	    consistent(Drs,_), !,
	    format('~n* Consistent *~n',[]),
	    (	
		localInformative(Drs), !,
		format('~n* Locally Informative *~n',[])
	    ;	
		format('~n* Locally Uninformative *~n',[])
	    ),
	    (	
		localConsistent(Drs), !,
		format('~n* Locally Consistent *~n',[])
	    ;	
		format('~n* Locally Inconsistent *~n',[])
	    )
	;
	    format('~nContains free variables or inconsistent~n',[])
	),
	localReadings(Readings).




