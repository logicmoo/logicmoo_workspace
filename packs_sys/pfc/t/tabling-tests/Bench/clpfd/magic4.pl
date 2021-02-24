%   File   : magic4.pl
%   Author : Neng-Fa ZHOU
%   Date   : 1992
%   Purpose: solve the magic square puzzle for a 4*4 board

%   solution = [1,2,15,16,12,14,3,5,13,7,10,4,8,11,6,9]

top:-
    vars_constraints(Vars),
    labeling(Vars).
%    write(Vars).

go:-
    statistics(runtime,[Start|_]),
    top,
    statistics(runtime,[End|_]),
    T is End-Start,
    write('execution time is '),write(T), write(milliseconds),nl.


vars_constraints(Vars):-
    Vars=[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16],
    Vars in 1..16,
    alldifferent(Vars),

    X1+X2+X3+X4#=34,
    X5+X6+X7+X8#=34,
    X9+X10+X11+X12#=34,
    X13+X14+X15+X16#=34,

    X1+X5+X9+X13#=34,
    X2+X6+X10+X14#=34,
    X3+X7+X11+X15#=34,
    X4+X8+X12+X16#=34,

    X1+X6+X11+X16#=34,
    X4+X7+X10+X13#=34.



    
