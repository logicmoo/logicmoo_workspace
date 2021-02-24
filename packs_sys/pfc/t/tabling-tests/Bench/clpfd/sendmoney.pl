top:-
    vars_constraints(Vars),
    labeling(Vars).
%    write(Vars).

go :-
    statistics(runtime,[Start|_]),
    top,
    statistics(runtime,[End|_]),
    T is End-Start,
    write('%execution time ='), write(T), write(' milliseconds'),nl.

vars_constraints(Vars):-
    Vars:=[S,E,N,D,M,O,R,Y],
    Vars in 0..9,
    alldifferent([S,E,N,D,M,O,R,Y]),
    S#\=0,
    M#\=0,
    1000*S+100*E+10*N+D+1000*M+100*O+10*R+E#=10000*M+1000*O+100*N+10*E+Y.

mylabeling([]).
mylabeling([V|Vs]):-
    indomain(V),
    write(V),write(' '),nl,
    mylabeling(Vs).

