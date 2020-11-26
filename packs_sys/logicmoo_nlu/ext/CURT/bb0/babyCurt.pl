/*************************************************************************

         name: babyCurt.pl (Volume 1, Chapter 6)
      version: April 17, 2001
  description: Curt (baby version for First-Order Logic)
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(curt,[curt/0]).

:- ensure_loaded(comsemOperators).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[memberList/2,
				printRepresentations/1]).

:- use_module(kellerStorage,[kellerStorage/2]).


/*========================================================================
   Start Curt
========================================================================*/

curt:- 
   curtTalk([],run).


/*========================================================================
   Control
========================================================================*/

curtTalk(_,quit).

curtTalk(OldReadings,run):-
   readLine(Input),
   curtUpdate(Input,OldReadings-NewReadings,CurtsMoves,State), !,
   curtOutput(CurtsMoves),
   curtTalk(NewReadings,State).


/*========================================================================
   Curt Help
========================================================================*/

curtHelp:-
	nl, write('bye: no more talking'),
	nl, write('readings: prints current readings'),
	nl, write('new: starts a new discourse'),
	nl.


/*========================================================================
   Curt's output
========================================================================*/

curtOutput([]).

curtOutput([Move|Moves]):-
	realizeMove(Move,Output),
	format('~n~nCurt: ~p~n',[Output]),
	curtOutput(Moves).


/*========================================================================
   Curt's Moves
========================================================================*/

realizeMove(clarify,'Want to tell me something?').

realizeMove(bye,'Bye bye!').

realizeMove(accept,'OK.').

realizeMove(noparse,'What?').


/*========================================================================
   Update Curt's Information State
========================================================================*/

curtUpdate([],R-R,[clarify],run):- !.

curtUpdate([bye],R-R,[bye],quit):- !.

curtUpdate([new],_-[],[],run):- !.

curtUpdate([help],R-R,[],run):- !,
	curtHelp.

curtUpdate([readings],Readings-Readings,[],run):- !,
	printRepresentations(Readings).

curtUpdate(Input,OldReadings-NewReadings,[accept],run):-
	kellerStorage(Input,Readings), !,
	\+ Readings = [],
	findall(
		Formula,
		(
		    memberList(New,Readings),
 		    (
		     memberList(Old,OldReadings),
		     Formula=(New&Old)
		    ;
		     OldReadings=[],
		     Formula=New
		    )
		),
		NewReadings
	       ).

curtUpdate(_,R-R,[noparse],run).


