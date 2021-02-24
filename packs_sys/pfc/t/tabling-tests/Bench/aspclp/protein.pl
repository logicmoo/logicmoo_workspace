fold(Primary):-
    statistics(runtime,_),
    pf(Primary),
    statistics(runtime,[_,Y]),
    write('time : '), write(Y), nl.

go:-
    statistics(runtime,_),
    top,
    statistics(runtime,[_,Y]),
    write('time : '), write(Y), nl.

top:-
    protein(Id,Primary),
    length(Primary,N),
    pf(Primary).

protein(1,[p,p,h,p,p,h,h,p,p,h,p,h,h,h,h,h,p,p,h]).

pf(Primary ) :-
       constrain(Primary,Tertiary,Energy),
/*
    frozen(Constrs),
    (member(C,Constrs),
     write(C),nl,
     fail;
     true),
*/
       fd_minimize(labelingff(Tertiary), Energy),
%       fd_minimize(labeling_ff_count_backtrack(Tertiary), Energy),
    format("Primary:~w,Tertiary:~w,Energy:~w~n",[Primary,Tertiary,Energy]),
       write('Energy: '),write(Energy),nl,
       write(Tertiary),nl.
pf(_) :-
       write('no solutions'),nl.
       
constrain(Primary,Tertiary,Energy) :-
       length(Primary,N),
       M is 2*N,
       Min is N - integer(sqrt(N)), 
       Max is N + integer(sqrt(N)),
       length(Tertiary,M),
       domain(Tertiary,Min,Max),
       starting_point(Tertiary,N),
       avoid_self_loops(Tertiary),
       next_constraints(Tertiary),
       energy_constraint(Primary,Tertiary,Energy).

starting_point([N,N,N,N1|_],N) :-
      N1 is N + 1.
      
avoid_self_loops(Tertiary):-
       positions_to_integers(Tertiary, ListaInteri),
       all_different(ListaInteri).

positions_to_integers([X,Y|R], [I|S]):-
       I  #= X*100+Y,
       positions_to_integers(R,S).
positions_to_integers([],[]).

next_constraints([_,_]).
next_constraints([X1,Y1,X2,Y2|C]) :-
       next(X1,Y1,X2,Y2),
       next_constraints([X2,Y2|C]).

next(X1,Y1,X2,Y2):-
       domain([Dx,Dy],0,1),
       Dx #= abs(X1-X2),
       Dy #= abs(Y1-Y2),
       Dx + Dy #= 1.

energy_constraint([_],_,0).
energy_constraint([A,B|Primary],[XA,YA,XB,YB|Tertiary],E) :-
       energy_contribution_of_A(0,A,XA,YA,Primary,Tertiary,E1),
       energy_constraint([B|Primary],[XB,YB|Tertiary],E2),
       E #= E1 + E2.

energy_contribution_of_A(_,_,_,_,[],[],0).
energy_contribution_of_A(0,A,XA,YA,[_|Primary],[_,_|Tertiary],E):-
       energy_contribution_of_A(1,A,XA,YA,Primary,Tertiary,E).
energy_contribution_of_A(1,A,XA,YA,[B|Primary],[XB,YB|Tertiary],E):-
       energy(A,XA,YA,B,XB,YB,C),
       energy_contribution_of_A(0,A,XA,YA,Primary,Tertiary,E1),
       E #= E1 + C.

energy(h,XA,YA,h,XB,YB,C) :-
       C in -1..0,
       DX #= abs(XA - XB),
       DY #= abs(YA - YB),
       1 #= DX + DY  #<=> C #= -1.
energy(h,_,_,p,_,_,0).
energy(p,_,_,h,_,_,0).
energy(p,_,_,p,_,_,0).
