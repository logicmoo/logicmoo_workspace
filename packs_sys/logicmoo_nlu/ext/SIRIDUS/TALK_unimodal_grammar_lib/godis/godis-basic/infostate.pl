
 
/*************************************************************************

        name: infostate.pl
 description: IS Type definition
 
*************************************************************************/

:- module(infostate, [infostate_variable_of_type/2]).




/*----------------------------------------------------------------------
     dis -- dynamic infostate
----------------------------------------------------------------------*/

infostate_variable_of_type( is, IS ) :-
    IS = record( [ private : Private,
		   shared : Shared ] ), 
    Shared = record( [ com : set( proposition ), 
		       qud : stackset( question ),
		       lu : LU ] ), 
    Private = record( [ agenda: stack(action), 
			plan : stack( action ), 
			bel : set( proposition ) ] ),
    LU = record( [ speaker : speaker,
		   moves : set( move ) ] ).
