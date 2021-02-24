%   File   : queens.pl
%   Author : Neng-Fa ZHOU
%   Date   : 2000
%   Purpose: A linear-space program for solving the N-queens problem 


top:-
    N=96,
    once(N).

go:-
    write('N=?'),read(N),queens(N).

queens(N):-
    statistics(runtime,[Start|_]),
    once(N),
    statistics(runtime,[End|_]),
    T is End-Start,
    write('%execution time ='), write(T), write(' milliseconds'),nl.

once(N):-
    fd_vector_min_max(0,N), % set the size of bit vectors
    make_list(N,List),
    domain(List,1,N),
    constrain_queens(List,[]),
    labeling_ff(List),
    write(List).

make_list(0,[]):-!.
make_list(N,[_|Rest]):-
    N1 is N-1,
    make_list(N1,Rest).

constrain_queens([],Left).
constrain_queens([Q|Qs],Left):-
    constrain_queen(Q,Left,Qs),
    constrain_queens(Qs,[Q|Left]).
    
% delay the constraint until Q is instantiated
delay constrain_queen(Q,Left,Qs):-var(Q) : {ins(Q)}.
constrain_queen(Q,Left,Right):-true :
    exclude_positions(Q,1,Left),
    exclude_positions(Q,1,Right).

exclude_positions(Q0,N,[]).
exclude_positions(Q0,N,[Q|Qs]):-
    R1 is Q0-N,
    R2 is Q0+N,
    domain_set_false(Q,Q0), % not in the same row
    domain_set_false(Q,R1), % not in the same diagonal
    domain_set_false(Q,R2), % not in the same diagonal
    N1 is N+1,
    exclude_positions(Q0,N1,Qs).

