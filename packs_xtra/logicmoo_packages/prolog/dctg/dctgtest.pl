/*  DCTGTEST.PL  */

/*  Shelved on 21st August 1990.  */


/*
    This file contains a demonstration DCTG grammar and a predicate for
    demonstrating it. The grammar, taken from ..., parses lists
    representing binary fractions into their values. Call go/0 to
    demonstrate it.

    You will need the predicate **(X+,Y+), which raises X to the power Y
    (2nd clause for bit, below).

    To use this file, call
        ?- dctg_reconsult( 'DCTGTEST.PL' ).
    then call
        ?- go.
*/


bit ::= [0]
<:>
bitval( 0, _ ).

bit ::= [1]
<:>
bitval( V,Scale ) ::- V is **(2,Scale).


bitstring ::= []
<:>
length(0)
&&
value(0,_).

bitstring ::= bit^^B, bitstring^^B1
<:>
length( Length ) ::-
    B1 ^^ length(Length1),
    Length is Length1 + 1
&&
value( Value, ScaleB ) ::-
    B ^^ bitval( VB, ScaleB ),
    S1 is ScaleB - 1,
    B1 ^^ value( V1, S1 ),
    Value is VB + V1.


number ::= bitstring ^^ B, fraction ^^ F
<:>
value(V) ::-
    B ^^ length(Length),
    S is Length-1,
    B ^^ value( VB, S ),
    F ^^ fractional_value( VF ),
    V is VB + VF.


fraction ::= ['.'], bitstring ^^ B
<:>
fractional_value( V ) ::-
    S is -1,
    B ^^ value( V, S ).

fraction ::= []
<:>
fractional_value(0).


test( L, V ) :-
    write( 'LIST ' ), write( L ), nl,
    number( Tree, L, [] ),
/*    write( 'TREE ' ), write( Tree ), nl, */
    Tree ^^ value( V ),
    write( 'VALUE ' ), write( V ), nl, nl.


go :-
    test( [0], _ ),
    test( [1], _ ),
    test( [1,0,1,1,1,0], _ ),
    test( [1,'.',1], _ ),
    test( [1,'.',0,1,1,0,1], _ ).
