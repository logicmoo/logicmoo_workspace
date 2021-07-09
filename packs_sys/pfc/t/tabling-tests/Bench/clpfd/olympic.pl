%   File   : olympic.pl
%   Author : Neng-Fa ZHOU
%   Date   : 1993
%   Purpose: solve a puzzle taken from Olympic Arithmetic Contest
/***********************************************************************
   Given ten variables with the following configuration:

               X7   X8   X9   X10

                  X4   X5   X6

                     X2   X1             

                        X1

  We already know that X1 is equal to 3 and want to assign each variable
  with a different integer from {1,2,...,10} such that for any three
  variables 
                      Xi   Xj

                         Xk
  the following constraint is satisfied:

                    |Xi-Xj| = Xk
***********************************************************************/

top:-
    vars_cs(Vars),
    labeling(Vars).
%    write(Vars).


go:-
    cputime(S),
    top,
    cputime(E),
    T is E-S,
    write(T), write(' milliseconds').

vars_cs(Vars):-
    Vars=[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10],
    in(Vars,1..10),
    alldifferent(Vars),
    X1#=3,
    minus(X2,X3,X1),
    minus(X4,X5,X2),
    minus(X5,X6,X3),
    minus(X7,X8,X4),
    minus(X8,X9,X5),
    minus(X9,X10,X6).

minus(X,Y,Z):-
    X-Y#=Z.
minus(X,Y,Z):-
    Y-X#=Z.

mylabeling([]).
mylabeling([V|Vs]):-
    indomain(V),
    write(V),write(' '),
    (V=2->write('from here'),c_START_TRACE;true),
    mylabeling(Vs).




