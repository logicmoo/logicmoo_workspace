/***********************************************************************

 
         name: database_agenda.pl
  description: Converts actions/queries to SQL commands
               calling the files sqlinsert.pl and sqlselect.pl
	       Commands: insertDB, deleteSQL, selectDB, countSQL
       author: Rebecca Jonson

***************************************************************************/


:- module( database_agenda, [insertDB/3, consultDB/4, countSQL/4, deleteSQL/3, selectDB/4] ).
:- use_module(sqlinsert).
:- use_module(sqlselect).
:- use_module(library(system)).
:- use_module(library(charsio)).
:- use_module(library(lists)).
:- use_module(mysql).
:- use_module(library(terms)).


/*=====================================================================
     insertDB( +Table, +Values, -Answer )
     -- Is supposed to return Answer if insertion succeeds
=======================================================================*/

insertDB(appointments,set(Values),Answer):-
	insertDB2(appointments,set(Values),Answer).


/*=====================================================================
     selectDB( +Table, +Query, Values,-Answer )
     -- Returns an Answer to a Query given
        background Values (returns answer "empty" if nothing found)
=======================================================================*/

selectDB(appointments,[],set(Values),[]).
%%%What time is the EVENT? (Date/Event)--> Time
selectDB(appointments,time, set(Values),Ans):-
        print_query(appt_date,[appointments],Values,Ans).

%%What do I have a DATE/TIME
selectDB(appointments,event, set(Values),Ans):-
        print_query(text,[appointments],Values,Ans).

selectDB(appointments,[X|VAL], set(Values),[Ans1|Ans2]):-
        print_query(X,[appointments],Values, Ans1),
	selectDB(appointments,VAL, set(Values),Ans2).

selectDB(appointments,VAL, set(Values),Ans):-
        print_query(VAL,[appointments],Values,Ans).	

/*=====================================================================
     countDB(-Table,-Column, -Value, -Answer )
     -- Returns (if it succeeds) a Number of the Amount of units having
     a value as Value in a specific column
     VALUES1:X<<Y --> X LIKE "Y%"
     VALUES2:X<<Y --> X LIKe "%Y"
     VALUES3:X=Y --> X = "Y"	     
=======================================================================*/

countSQL(appointments, Column, set(Values),Ans):-
	countLIKEDB(appointments,Column,set(Values),Ans).

 
/*=====================================================================
     deleteSQL( +Table, +Values, -Answer )
     -- Deletes a row in table given
        background Values
=======================================================================*/

deleteSQL(appointments,set(Values),Ans):-
	 deleteDB(appointments,set(Values),Ans).




%%%What am I up to this week? (Date1 - Date2 --> Events + Times
%%%Am I booked on fridays? (Date1-DateN,) --> Yes/No
%%%Change meeting on friday to saturday UPDATE





