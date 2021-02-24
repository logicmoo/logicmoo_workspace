/*-------------------------------------------------------------------------*/
/* Benchmark (Finite Domain)                                               */
/*                                                                         */
/* Name           : digit8.pl                                              */
/* Title          : particular 8 digit number                              */
/* Original Source: Daniel Diaz - INRIA France                             */
/* Adapted by     : Daniel Diaz for GNU Prolog                             */
/* Date           : October 1993                                           */
/*                                                                         */
/* Find the 8 digit number N such that:                                    */
/*                                                                         */
/*    - N is a square                                                      */
/*    - if we put a 1 in front of the decimal notation of N then it is     */
/*      still a square                                                     */
/*                                                                         */
/* Solution:                                                               */
/*  [N,X,M,Y]                                                              */
/*  [23765625,4875,123765625,11125]                                        */
/*  [56250000,7500,156250000,12500]                                        */
/*-------------------------------------------------------------------------*/

go:-
    statistics(runtime,_),
    (digit8(L), 
     write(L), nl,
     fail
     ;
     write('No more solutions'), nl),
    statistics(runtime,[_,Y]),
    write('time : '), write(Y), nl.

digit8(L):-
    L=[N,X,M,Y],
    N #>= 10000000, 
    N#=<99999999,
    X**2 #= N,
    100000000+N #= M,
    Y**2 #= M,
    labeling_ff(L).



