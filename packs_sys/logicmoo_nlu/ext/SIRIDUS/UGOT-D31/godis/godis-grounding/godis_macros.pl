/*************************************************************************

        name: ibis_macros.pl 
 description: Condition and operation macros for IBiS2
 
*************************************************************************/

:- module( macros, [macro_cond/2, macro_op/2]).      

macro_op(dummy,dummy).


macro_cond( q_raising_icm(Move),
	    [ % icm:und is q-raising...
	      ( Move = icm:und*int:_ )  ] ).


macro_cond( q_raising_action(Move),
	    [ % icm:und is q-raising...
	      ( Move = icm:und*int:_ )
	    or ( Move= raise(_) or Move = findout(_) )
	    ] ).

macro_cond( q_raising_move(Move),
	    [ % icm:und is q-raising...
	      ( Move = icm:und*int:_ ) 
	    or ( Move = ask( _ ) )
	    ] ).
