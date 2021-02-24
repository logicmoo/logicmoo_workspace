:-table sg/2,edge/2.

go:-
    cputime(Start),
    top,
    cputime(End),
    T is End-Start,
    write('TIME:'),write(T),nl.

main:-top.

top:-
    sg(X,Y),fail.
top.

test:-
    findall(sg(X,Y),sg(X,Y),Bag),
    sort(Bag,SortBag),
    write(SortBag),nl.


sg(X,X).
sg(X,Y):-edge(X,XX),sg(XX,YY),edge(Y,YY).

:-['sg_edge.pl'].
