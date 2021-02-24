%   File   : zebra.pl
%   Author : Neng-Fa ZHOU
%   Date   : 1992
%   Purpose: solve the five-house puzzle in CLP(FD)

top:-
    vars_constraints(Vars),
    labeling(Vars).
%    write(Vars),nl.

go:-
    statistics(runtime,[Start|_]),
    top,
    statistics(runtime,[End|_]),
    T is End-Start,
    write('execution time is '),write(T), write(milliseconds).



vars_constraints(Vars):-
    Vars=[N1,N2,N3,N4,N5,
	   C1,C2,C3,C4,C5,
	   P1,P2,P3,P4,P5,
	   A1,A2,A3,A4,A5,
	   D1,D2,D3,D4,D5],
    Vars in 1..5,
    alldifferent([C1,C2,C3,C4,C5]),
    alldifferent([P1,P2,P3,P4,P5]),
    alldifferent([N1,N2,N3,N4,N5]),
    alldifferent([A1,A2,A3,A4,A5]),
    alldifferent([D1,D2,D3,D4,D5]),
    N1#=C2,
    N2#=A1,
    N3#=P1,
    N4#=D3,
    N5#=1,
    D5#=3,
    P3#=D1,
    C1#=D4,
    P5#=A4,
    P2#=C3,
    C1#=C5+1,
    plusorminus(A3,P4,1),
    plusorminus(A5,P2,1),
    plusorminus(N5,C4,1).

plusorminus(X,Y,C):-
    X#=Y-C.
plusorminus(X,Y,C):-
    X#=Y+C.

mylabeling([]).
mylabeling([V|Vs]):-
    myindomain(V),
    write(V),write(' '),
    mylabeling(Vs).

