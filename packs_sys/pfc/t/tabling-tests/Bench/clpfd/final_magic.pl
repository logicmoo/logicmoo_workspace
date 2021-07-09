all(Goal) :-
    setval(ct,0),
    statistics(runtime,_),
    call(Goal),
    writeln(Goal),
    incval(ct),
    fail.
all(_) :-
    write('No. of solutions: '), getval(ct,Ct), writeln(Ct),
    statistics(runtime,[_,Y]),
    write('time : '), writeln(Y).


%:- lib(fd).

magic(N, L):-
        length(L,N),
        N1 is N-1,
        L:: 0..N1,
        constraints(L, L, 0, N, N),
        labelingff(L).

constraints([], _, _, S0, S1) :- S0=0, S1=0.
constraints([X|Xs], L, I, S, S2):-
        sum(L, I, X1),
        X1#=X,
        I1 is I+1,
        S1 #>= 0,
        S1 #= S-X,                              % redundant constraint 1
        c_0(I, X, S2, S3),
        constraints(Xs, L, I1, S1, S3).

c_0(0, _X, S0, S1) :- !, S0=S1.
c_0(I, X, S0, S1) :- I*X+S1#=S0.

sum([], _, 0).
sum([X|Xs], I, S) :-
%        '#='(X,I,B),
        (X#=I) #<=> B,
        S=B+S1,
        sum(Xs, I, S1).

magic_labeling([First|Rest]) :-
    dvar_domain_list(First,DomList),
    rmember(First,DomList),
    labelingff(Rest).
/*
labelingff([]) :- !.
labelingff(List) :-
    deleteff(Var,List,Rest),
    indomain(Var),
    labelingff(Rest).
*/
rmember(X,[_|T]) :-
    rmember(X,T).
rmember(X,[X|_]).

setval(X,Value):-
    global_set(X,Value).

getval(X,Value):-
    global_get(X,Value).

incval(X):-
    global_get(X,OldVal),
    NewVal is OldVal+1,
    global_set(X,NewVal).









