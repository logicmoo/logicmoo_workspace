
 
/*************************************************************************

        name: infostate.pl
     version: January 7, 2000
 description: IS Type definition
      author: Staffan Larsson, Peter Bohlin
 
*************************************************************************/

:- module(infostate, [infostate_variable_of_type/2]).




/*----------------------------------------------------------------------
     dis -- dynamic infostate
----------------------------------------------------------------------*/

infostate_variable_of_type( is, IS ) :-
	IS = record( [ private : Private,
		       shared : Shared ] ), 
	Shared = record( [ com : set( proposition ), 
			   qud : stack( question ),
			   actions : stack( action ),
			   lu : LU ] ), 
	Private = record( [ agenda: stack( action ), 
			    plan : stackset( action ), 
			    bel : set( proposition ), 
			    tmp : Shared ] ),
	LU = record( [ speaker : speaker,
		       moves : assocset( move, bool ) ] ).
