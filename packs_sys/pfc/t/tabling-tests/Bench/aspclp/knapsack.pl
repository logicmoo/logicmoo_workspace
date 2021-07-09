go:-
    statistics(runtime,_),
    top,
    statistics(runtime,[_,Y]),
    write('time : '), write(Y), nl.

top:-
    knapsack(1023,1525).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

thedata([2,4, 8,16,32,64,128,256,512,1024],
            [2,5,11,23,47,95,191,383,767,1535]).
            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
knapsack(Space,Profit) :-
    thedata(Weights,Costs),
    length(Weights,N),
    length(Vars,N),
    domain(Vars,0,Space),
    scalar_product(Weights,Vars,#=<,Space),
    scalar_product(Costs,Vars,#>=,Profit),
    labeling([ff],Vars),!,
    write(Vars),nl.
knapsack(_,_) :-
    write('No solutions.'),nl.
          
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
runAll :-
    p(S,P), 
    write('Space='),write(S),write('\t Profit='),write(P),nl,nl,
    knapsack(S,P),
    fail.   

p(255, 374).
p(255, 375).
p(511, 757).
p(511, 758).
p(1023,1524).
p(1023,1525).
p(2047,3059).
p(2047,3060).


