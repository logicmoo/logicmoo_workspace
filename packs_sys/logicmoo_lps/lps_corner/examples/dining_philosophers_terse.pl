
:- expects_dialect(lps).

% this must be the FIRST directoive in the file:
% :- relax_untimed_literals.  % uncomment this to let implicit time vars in rules and composite events stay unbound; 


% First, "environmental" information
initial_state([available(fork(0)),available(fork(1)),available(fork(2)),available(fork(3)),available(fork(4))]).

% a little hack assuming adjacent/3 enumerates our philosophers
observe(Desires,2) :- findall(time_to_eat(Phi),adjacent(_,Phi,_),Desires).

maxTime(10).



% Now for the agent/simulation:
if time_to_eat(philosopher(N)) during [_T1,T2] 
then 
    dine(philosopher(N)) during [T3,_T4].

adjacent(fork(1),philosopher(1),fork(2)).
adjacent(fork(3),philosopher(3),fork(4)).
adjacent(fork(0),philosopher(0),fork(1)).
adjacent(fork(2),philosopher(2),fork(3)).
adjacent(fork(4),philosopher(4),fork(0)).


event(time_to_eat(_)).
action(think(_)).
action(pickup_forks(_,_,_)).
action(eat(_)).
action(putdown_forks(_,_,_)).
fluent(available(_)).

dine(philosopher(N)) if
    think(philosopher(N)) during _,
    adjacent(F1,philosopher(N),F2),
    pickup_forks(F1,philosopher(N),F2) to T2,
    eat(philosopher(N)) from T2 to T3,
    putdown_forks(F1,philosopher(N),F2) from T3.

pickup_forks(F1,philosopher(_N),_F2) terminates available(F1).
pickup_forks(_F1,philosopher(_N),F2) terminates available(F2).

putdown_forks(F1,philosopher(_N),_F2) initiates available(F1).
putdown_forks(_F1,philosopher(_N),F2) initiates available(F2).

false  
	pickup_forks(F1,philosopher(_N),_F2), not available(F1).
false  
	pickup_forks(_F1,philosopher(_N),F2), not available(F2).
false  
	pickup_forks(_F1,philosopher(_N),F), pickup_forks(F,philosopher(_K),_F2).

