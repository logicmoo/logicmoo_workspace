
:- module(tcl,[]).
% :- topl((go/0, main/0)).
:- table(reach/2).

cputime(X):- statistics(cputime,X).

/*


NEW SWI Tabling asserta vs recorda
TIME:9.638810966000001
TIME:13.412892943000001

OLD SWI Tabling asserta vs recorda
TIME:8.806049874
TIME:13.26628324 
*/


tgo:-
    (cputime(Start)),
    ttop,
    cputime(End),
    T is End-Start,
    write('TIME:'),write(T),nl.

main:-ttop.

ttop:-
    reach(X,Y),
    % write(X->Y),
    fail.
ttop.

reach(X,Y):-edge(X,Y).
reach(X,Y):-reach(X,Z),edge(Z,Y).

:-['sg_edge.pl'].

:- time(tgo),!.
