%   File   : queens.pl
%   Author : Neng-Fa ZHOU
%   Date   : 1992
%   Purpose: solve N-queens problem with CLP(FD)


top:-
    N=25,
    make_list(N,List),
    List in 1..N,
    constrain_queens(List),
    labeling_ff(List),
    write(List).

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
    List in 1..N,
    constrain_queens(List),
    labelingff(List),
    write(List),nl.

constrain_queens([]).
constrain_queens([X|Y]):-
    safe(X,Y,1),
    constrain_queens(Y).

safe(_,[],_).
safe(X,[Y|T],K):-
    noattack(X,Y,K),
    K1 is K+1,
    safe(X,T,K1).

noattack(X,Y,K):-
    X #\= Y,
    X+K #\= Y,
    X-K #\= Y.

make_list(0,[]):-!.
make_list(N,[_|Rest]):-
    N1 is N-1,
    make_list(N1,Rest).

mylabeling_ff([]).
mylabeling_ff([X|Xs]):-
    integer(X),!,
    mylabeling_ff(Xs).
mylabeling_ff(Xs):-    
    my_select_min(Xs,Var),
    indomain(Var),
    mylabeling_ff(Xs).

my_select_min([Var|Vars],BestVar):-
    integer(Var),!,
    my_select_min(Vars,BestVar).
my_select_min([Var|Vars],BestVar):-
    fd_size(Var,Size),
    my_select_min(Vars,Var,Size,BestVar).

my_select_min([],Var,Size,BestVar):-BestVar=Var.
my_select_min([Var|Vars],CurVar,CurSize,BestVar):-
    integer(Var),!,
    my_select_min(Vars,CurVar,CurSize,BestVar).
my_select_min([Var|Vars],CurVar,CurSize,BestVar):-
    fd_size(Var,Size),
    (Size>=CurSize->CurSize1 is CurSize, CurVar1=CurVar;
     CurSize1 is Size, CurVar1 = Var),
    my_select_min(Vars,CurVar1,CurSize1,BestVar).
