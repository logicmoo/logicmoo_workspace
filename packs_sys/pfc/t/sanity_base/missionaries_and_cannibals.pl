% to run the code in SWI-Prolog, do
%        ?- ['missionaries_and_cannibals.pl'].
%
% Represent state as `world(CL,ML,B,CR,MR)`

start(world(3,3,left,0,0)).

goal(world(0,0,right,3,3)).

legal(CL,ML,CR,MR) :-
	% is this state a legal one?
	ML>=0, CL>=0, MR>=0, CR>=0,
	(ML>=CL ; ML=0),
	(MR>=CR ; MR=0).

% Possible moves:
move(world(CL,ML,left,CR,MR),world(CL,ML2,right,CR,MR2)):-
	% Two missionaries cross left to right.
	MR2 is MR+2,
	ML2 is ML-2,
	legal(CL,ML2,CR,MR2).

move(world(CL,ML,left,CR,MR),world(CL2,ML,right,CR2,MR)):-
	% Two cannibals cross left to right.
	CR2 is CR+2,
	CL2 is CL-2,
	legal(CL2,ML,CR2,MR).

move(world(CL,ML,left,CR,MR),world(CL2,ML2,right,CR2,MR2)):-
	%  One missionary and one cannibal cross left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	MR2 is MR+1,
	ML2 is ML-1,
	legal(CL2,ML2,CR2,MR2).

move(world(CL,ML,left,CR,MR),world(CL,ML2,right,CR,MR2)):-
	% One missionary crosses left to right.
	MR2 is MR+1,
	ML2 is ML-1,
	legal(CL,ML2,CR,MR2).

move(world(CL,ML,left,CR,MR),world(CL2,ML,right,CR2,MR)):-
	% One cannibal crosses left to right.
	CR2 is CR+1,
	CL2 is CL-1,
	legal(CL2,ML,CR2,MR).

move(world(CL,ML,right,CR,MR),world(CL,ML2,left,CR,MR2)):-
	% Two missionaries cross right to left.
	MR2 is MR-2,
	ML2 is ML+2,
	legal(CL,ML2,CR,MR2).

move(world(CL,ML,right,CR,MR),world(CL2,ML,left,CR2,MR)):-
	% Two cannibals cross right to left.
	CR2 is CR-2,
	CL2 is CL+2,
	legal(CL2,ML,CR2,MR).

move(world(CL,ML,right,CR,MR),world(CL2,ML2,left,CR2,MR2)):-
	%  One missionary and one cannibal cross right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	MR2 is MR-1,
	ML2 is ML+1,
	legal(CL2,ML2,CR2,MR2).

move(world(CL,ML,right,CR,MR),world(CL,ML2,left,CR,MR2)):-
	% One missionary crosses right to left.
	MR2 is MR-1,
	ML2 is ML+1,
	legal(CL,ML2,CR,MR2).

move(world(CL,ML,right,CR,MR),world(CL2,ML,left,CR2,MR)):-
	% One cannibal crosses right to left.
	CR2 is CR-1,
	CL2 is CL+1,
	legal(CL2,ML,CR2,MR).


% Recursive call to solve the problem
path(world(CL1,ML1,B1,CR1,MR1),world(CL2,ML2,B2,CR2,MR2),Explored,MovesList) :- 
   move(world(CL1,ML1,B1,CR1,MR1),world(CL3,ML3,B3,CR3,MR3)), 
   not(member(world(CL3,ML3,B3,CR3,MR3),Explored)),
   path(world(CL3,ML3,B3,CR3,MR3),world(CL2,ML2,B2,CR2,MR2),[world(CL3,ML3,B3,CR3,MR3)|Explored],
    [ [ world(CL3,ML3,B3,CR3,MR3), world(CL1,ML1,B1,CR1,MR1)] | MovesList ]).

% Solution found
path(world(CL,ML,B,CR,MR),world(CL,ML,B,CR,MR),_,MovesList):- 
	output(MovesList).

% Printing
output([]) :- nl. 
output([[A,B]|MovesList]) :- 
	output(MovesList), 
   	write(B), write(' -> '), write(A), nl.

% Find the solution for the missionaries and cannibals problem
do_test :- 
   path(world(3,3,left,0,0),world(0,0,right,3,3),[world(3,3,left,0,0)],_).

:- use_module(library(statistics)).
:- time(do_test).

/*

dmiles@gitlab:/opt/logicmoo_workspace/packs_sys/pfc/t/sanity_base# swipl -f missionaries_and_cannibals.pl

world(3,3,left,0,0) -> world(1,3,right,2,0)
world(1,3,right,2,0) -> world(2,3,left,1,0)
world(2,3,left,1,0) -> world(0,3,right,3,0)
world(0,3,right,3,0) -> world(1,3,left,2,0)
world(1,3,left,2,0) -> world(1,1,right,2,2)
world(1,1,right,2,2) -> world(2,2,left,1,1)
world(2,2,left,1,1) -> world(2,0,right,1,3)
world(2,0,right,1,3) -> world(3,0,left,0,3)
world(3,0,left,0,3) -> world(1,0,right,2,3)
world(1,0,right,2,3) -> world(1,1,left,2,2)
world(1,1,left,2,2) -> world(0,0,right,3,3)
% 10,253 inferences, 0.009 CPU in 0.009 seconds (100% CPU, 1101537 Lips)

*/


