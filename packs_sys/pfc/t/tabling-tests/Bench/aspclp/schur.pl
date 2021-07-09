go:-
    statistics(runtime,_),
    top,
    statistics(runtime,[_,Y]),
    write('time : '), write(Y), nl.
    
top:-
    schur(110,5).

schur(N,P) :-
    length(List,N),   %% List = [X1...Xn], Xi is the part to which i belongs
    domain(List,1,P), %% there are P parts 
    constraints(List, N),
    (once(labeling([ff],List)),
     dump(List,1) ;
     write('No solution'), nl).

constraints(List, N) :-
    recursion(List,1,1,N).


recursion(_,M,_,N):- 
    M > N, !.
recursion(List,I,J,N):-  
    I + J > N,!,
    I1 is I + 1,
    recursion(List,I1,1,N).
recursion(List,I,J,N):-  
    I > J,!,
    J1 is J + 1,
    recursion(List,I,J1,N).
recursion(List,I,J,N) :-
     K is I + J,
    nth(I,List,BlockI),
    nth(J,List,BlockJ),
    nth(K,List,BlockK),
    (BlockI #= BlockJ) #=> (BlockK #\= BlockI),
    J1 is J+1,
    recursion(List,I,J1,N).
    
%%%%%%%%%%%%%%%%%

dump([],_).
dump([A|B],N) :-
    format("~q -> part ~q~n",[N,A]),
    N1 is N+1,
    dump(B,N1).


nth(1,[A|_],A) :- !.
nth(I,[_|R],A) :-
   I1 is I - 1,
   nth(I1,R,A).

