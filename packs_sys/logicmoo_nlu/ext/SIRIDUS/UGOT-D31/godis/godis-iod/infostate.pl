
 
/*************************************************************************

        name: infostate.pl
 description: IS Type definition
 
*************************************************************************/

:- module(infostate, [infostate_variable_of_type/2]).




infostate_variable_of_type( is, IS ) :-
    IS = record( [ private : Private,
		   shared : Shared ] ), 
    Shared = record( [ com : set( proposition ), 
		       issues: stackset( question ),
		       qud : stackset( question ),
		       pm : set( move ),
		       lu : LU ] ), 
    Private = record( [ agenda: oqueue( action),
			plan : stackset( action ), 
			bel : set( proposition ),
			tmp : record( [ usr : TMP, sys: TMP ] ),
			nim : oqueue( pair( speaker, move ) ) ] ),
    LU = record( [ speaker : speaker,
		   moves : set( move ) ] ),
    TMP = record( [ com : set( proposition ), 
		    qud : stackset( question ),
		    issues : stackset( question ),
		    agenda : oqueue( action ),
		    plan : stackset( action ) ] ).
