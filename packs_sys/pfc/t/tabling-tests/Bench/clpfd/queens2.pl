%   File   : queens.pl
%   Author : Neng-Fa ZHOU
%   Date   : 1998
%   Purpose: A linear-constraint program for solving the N-queens problem 


top:-
    N=25,
    is_top(N).

go:-
    write('N=?'),read(N),queens(N).

queens(N):-
    statistics(runtime,[Start|_]),
    is_top(N),
    statistics(runtime,[End|_]),
    T is End-Start,
    write('%execution time ='), write(T), write(' milliseconds'),nl.

is_top(N):-
    make_list(N,List),
    domain(List,1,N),
    make_list12(List,0,List1,List2),
    constrain_queens(List,List1,List2),
    labeling(List).
%    write(List).

make_list(0,[]):-!.
make_list(N,[_|Rest]):-
    N1 is N-1,
    make_list(N1,Rest).

make_list12([],I,List1,List2):-List1=[],List2=[].
make_list12([Q|Qs],I,List1,List2):-
    List1=[Q1|Rest1],
    List2=[Q2|Rest2],
    newDomainVar(Q1),
    newDomainVar(Q2),
    Q1 #= Q+I,
    Q2 #= Q-I,
    I1 is I+1,
    make_list12(Qs,I1,Rest1,Rest2).

newDomainVar(X):-
    domain(X,-9999,99999).

constrain_queens(List,List1,List2):-
    all_different(List),
    all_different(List1),
    all_different(List2).
