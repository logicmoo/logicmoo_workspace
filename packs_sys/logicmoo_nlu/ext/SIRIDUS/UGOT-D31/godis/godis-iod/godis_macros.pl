/*************************************************************************

        name: godis_macros.pl 
 description: Condition and operation macros for GoDiS-IOD/AOD
 
*************************************************************************/

:- module( macros, [macro_cond/2, macro_op/2]).      

alias( tsq, tmp/sys/qud ).

macro_op(dummy,dummy).


macro_cond( q_raising_icm(Move),
	    [ % icm:und is q-raising...
	      ( Move = icm:und*Polarity:_*Content ) and
	    ( not ( Content = (not _) and Polarity=pos ) ) ] ).


macro_cond( q_raising_action(Move),
	    [ % icm:und is q-raising...
	      ( Move = icm:und*Polarity:_*Content  and
	    ( not ( Content = (not _) and Polarity=pos ) ) )
	      or ( Move= raise(_) or Move = findout(_) )
	    ] ).

macro_cond( q_raising_move(Move),
	    [ % icm:und is q-raising...
	      ( Move = icm:und*Polarity:_*Content  and
	    ( not ( Content = (not _) and Polarity=pos ) ) ) 
	      or ( Move = ask( _ ) )
	    ] ).

