:-table reach/2.

go:-
    cputime(Start),
    top,
    cputime(End),
    T is End-Start,
    write('TIME:'),write(T),nl.

main:-top.

top:-
    reach(X,Y),
%    write(r(X,Y)),nl,
    fail.
top.

reach(X,Y):-edge(X,Y).
reach(X,Y):-reach(X,Z),reach(Z,Y).

:-['edge.pl'].
