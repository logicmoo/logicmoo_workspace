:- module(qu_vitor, [run/2]).

:- use_module(library(andorra/andorra)).

:- determinate(snint(A),nonvar(A)).
:- determinate(safe(_,_,C),nonvar(C)).
%:- determinate(solve(_A,B,_C),(B ?\=[_|_])).
:- determinate(solve(_,B,_),( nonvar(B), term(B,[1]) ?\= square(8,_))).
:- determinate(not_threatened(_,_,_,_),true).
:- determinate(get_solutions(_,_),true).
:- determinate(newsquare(A,_),nonvar(A)).

go(X):- run(8,X).

snint(1).
snint(2).
snint(3).
snint(4).
snint(5).
snint(6).
snint(7).
snint(8).


size(8).

safe(_,_,[]).
safe(A,B,[square(C,D)|E]) :-
        not_threatened(C,D,A,B),
        safe(A,B,E).

solve(_,[square(8,B)|C],D) :-
        !,
        D=[square(8,B)|C].
solve(A,B,C) :-
        newsquare(B,D),
        solve(A,[D|B],C).

not_threatened(A,B,C,D) :-
%        A\==C,
        B\==D,
        E is A-B,
        F is C-D,
        F\==E,
        G is A+B,
        H is C+D,
        G\==H.

get_solutions(A,B) :-
        solve(A,[],B).

run(A,B) :-
        get_solutions(A,B).

newsquare([],square(1,A)) :-
        snint(A).
newsquare([square(A,B)|C],square(D,E)) :-
        D is A+1,
        snint(E),
        not_threatened(A,B,D,E),
        safe(D,E,C).


/*---------------------------
-S 0 no det code no cut

265460 millisecons
reductions: 366605
fail:       16541
total:      383146
(92 solustions)

-S 0 no det code with cut
258350 millisecons
reductions: 349714
fail:       15805
total:      365519

-S 0

102820 millisecons
reductions: 153207
fail:       13756
total:      166963

-S 1

82950 millisecons
reductions: 157183
fail:       13760
total:      170943

-S 4

68640 millisecons
reductions: 172321
fail:       13851
total:      186172

-M 2

54480 millisecons
reductions: 153052
fail:       13755
total:      166807

-M 3

36880 millisecons
reductions: 152592
fail:       13754
total:      166346
-----------------------------
after delet the first \==

andorra:
107890 millisecons
reductions: 153179
fail:       13756
total:      166935

prolog: 
251520 millisecons
reductions: 309432
fail:       15805
total:      325237

------------------------------*/
