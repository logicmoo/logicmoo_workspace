
 
/*************************************************************************

        name: indites_datatypes.pl 
     version: 
 description: Additional GoDiS datatypes
      author: Peter Bohlin, Staffan Larsson
 
*************************************************************************/

:- multifile is_type/1, of_type/2, empty_object/2, operation/4, condition/3.





/*----------------------------------------------------------------------
     dmove -- dialogue move
----------------------------------------------------------------------*/

of_type( goodbye, move ).
of_type( greet, move ).
of_type( wait, move ) .
of_type( answer(Q,R), move ) :-
	of_type( Q, question ), 
	of_type( R, proposition ) .
of_type( answer(R), move ) :-
	of_type( R, proposition ) . % WRONG!!!

of_type( ask(Q), move ) :-
	of_type( Q, question ) .
% requestRepeat ???

operation( _, reduce( R ), Q, P ):-
	reduce( Q, R, P ).
	

/*----------------------------------------------------------------------
     action
----------------------------------------------------------------------*/

is_type( action ).

of_type( quit, action ).
of_type( greet, action ).
of_type( respond(Q), action ) :-
	of_type( Q, question ).
of_type( raise(Q), action ) :-
	of_type( Q, question ).


/*----------------------------------------------------------------------
     question
----------------------------------------------------------------------*/

is_type( question ).

of_type( X^P, question ) :-
	var( X ), 
	of_type( P, proposition ) .


/*----------------------------------------------------------------------
     proposition
----------------------------------------------------------------------*/

is_type( proposition ).

of_type( _=_, proposition ).
of_type( P, proposition ):-atom(P).

