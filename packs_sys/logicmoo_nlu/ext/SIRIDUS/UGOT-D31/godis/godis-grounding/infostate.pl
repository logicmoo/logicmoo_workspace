
 
/*************************************************************************

        name: infostate.pl
 description: IS Type definition for IBiS2
 
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
		       pm : set( move ),
		       lu : LU
		       ] ), 
    Private = record( [ agenda: oqueue( action),
			plan : stackset( action ), 
			bel : set( proposition ),
			tmp : TMP,
			nim : oqueue( move ) ] ),
    LU = record( [ speaker : speaker,
		   moves : set( move ) ] ),
    TMP = record( [ com : set( proposition ), 
		    qud : stackset( question ),
		    agenda : oqueue( action ),
		    plan : stackset( action ) ] ).

