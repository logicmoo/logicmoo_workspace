/* 
    Revised by Neng-Fa ZHOU
    taken from "Constraint Satisfaction in LP" by P. Van Hentenryck
    S=(s0,s1,...,sn-1) is a magic sequence if there are si occurrences of i
    for i=0,...,n-1.
*/

go:-
    statistics(runtime,[Start|_]),
    top,
    statistics(runtime,[End|_]),
    T is End-Start,
    write('execution time is '),write(T), write(milliseconds),nl.

top:-
    is_top(9).

is_top(N):-
    constrs(N,L),
    labeling(L),
    write(L),
    nl.

constrs(N,L):-
    functor(S,f,N),
    S=..[_|L],
    L in 0..N,
    occurrences(L,0,L),
    sum(L,0,N).

occurrences([],N,L).
occurrences([X|Xs],N,L):-
    freeze(X,occur(X,N,L)), %call occur(X,N,L) when X is nonvar
    N1 is N+1,
    occurrences(Xs,N1,L).
    
occur(0,Val,L):-
    outof(Val,L).
occur(N,Val,[Val|L]):-
    N > 0,
    N1 is N-1,
    occur(N1,Val,L).
occur(N,Val,[X|L]):-
    N > 0,
    Val #\= X,
    occur(N,Val,L).

outof(X,[]).
outof(X,[Y|Ys]):-
    X #\= Y,
    outof(X,Ys).

sum([],S,N):-S #= N.
sum([X|Xs],S,N):-
    sum(Xs,S+X,N).


