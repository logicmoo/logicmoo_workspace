
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
    LU = record( [ speaker : participant,
		   moves : set( move ) ] ).
