%% domain description
%% States = [ State0, ..., State_NTime]
%% Each State is [X1,...,XNBlocks] where Xi in [0,NBlocks]
%% (Xi = j means that block i is on block j. 0 is the table)
%% Actions = [ [ Block, ToBlock], ... ] (a list of NTime pairs) 
%% Assume NBlocks > 1  

go:-
    statistics(runtime,_),
    top,
    statistics(runtime,[_,Y]),
    write('time : '), write(Y), nl.
    
top:-
    go(7,40).

go(M,N):-
    planning(M,N).

planning( NBlocks, NTime) :-
    init_domains(NBlocks,NTime, States),
    initial_state(States),
    final_state(States),
    init_actions(NBlocks,NTime,Actions),
    forward(Actions,States),
    no_rep(Actions),
    action_properties(Actions,States),
    term_variables(Actions,Vars),
    labeling(Vars).
planning( _, NTime ) :-
    write('No solution With '),write(NTime),
    write(' Steps'),nl.

    
init_domains(NBlocks,NTime,States) :-
    T1 is NTime+1,
    length(States,T1),
    init_domains1(NBlocks,States).
init_domains1(_,[]).
init_domains1(N,[State|States]) :-
    length(State,N),
    init_domains1(N,States),
    domain(State,0,N), 
    count(0, State, '#=<', 3).

%%%%%%% Initial State: [0,1,2,3,...,N-1]

initial_state( [State|_] ) :-
    increasing_list(State).
    
%%%%% Final State: Two columns. One of even and one of odd
%%%%% numbers: [0,0,1,2,3,..N-2]

final_state( States ) :-
   append(_,[[0 |FS]],States),
   increasing_list(FS).

%%%%%

init_actions(_,0,[]) :- !.   
init_actions(N,T,[[Block,To_Block]|Acts]) :-
    T1 is T - 1,
    Block in 1..N, %%% Table cannot be moved 
    To_Block in 0..N, %%% Blocks can go on table and on blocks,  
    (Block #< To_Block #=> To_Block #= 0), %%% on Bigger blocks. 
    Block #\= To_Block ,   %%% A Block cannot go on itself 
    init_actions(N,T1,Acts).
 
%%%%%

forward([],_).
forward([[Block,To_Block]|B],[CurrState, NextState |Rest]) :-
    element(Block,NextState,To_Block), %%% [...,Xi,...] Block i can go on Xi     
    is_clear(CurrState,Block),         %%% if both are clear  
    is_clear(CurrState,To_Block),
    element(Block,CurrState,Old),
    Old #\= To_Block,
    forward(B,[NextState|Rest]).

is_clear([],_).
is_clear([A|B],X) :-
    (X #\= 0 #=> A #\= X),  
    is_clear(B,X).

no_rep([_]).
no_rep([[X1,_],[X2,Y2]|Rest]) :-
     X1 #\= X2,
     no_rep([[X2,Y2]|Rest]).
     

action_properties([],_).
action_properties([ [Block,_To_Block] | Rest],[ CurrState,NextState | States ]) :-    
    inertia(1, Block, CurrState, NextState),
    action_properties(Rest, [NextState | States ]).

inertia(_,_, [],[]).
inertia(N, X, [A|B],[C|D]) :-
    N1 is N+1,
    inertia(N1, X, B, D),
    (X #\= N #=> A #= C). 
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% AUX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   
increasing_list(List) :-
   sequence(List,0).
sequence([],_).
sequence([N|R],N) :-
    M is N+1, sequence(R,M).

stampa([S],[]) :-
     write(S),nl.
stampa([S|R],[A|B]) :-
     write(S),write('  '),
     write(A),nl,
     stampa(R,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
