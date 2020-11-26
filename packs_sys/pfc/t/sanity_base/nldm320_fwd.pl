

% swipl  % 396,814,768 inferences, 62.415 CPU in 62.434 seconds (100% CPU, 6357653 Lips)
% XSB 5 seconds
% dra-intrerp 

%:- use_module(library(dra)).
%:- dra_table(tc/2).

% 205,135 inferences, 0.025 CPU in 0.025 seconds (100% CPU, 8122756 Lips)
%tc(X,Y):- between(1,320,X),between(1,320,Y).

% 819,855 inferences, 0.091 CPU in 0.091 seconds (100% CPU, 9032752 Lips)
%tc(X,Y):- between(1,640,X),between(1,640,Y).

% 4,097,935 inferences, 0.455 CPU in 0.455 seconds (100% CPU, 9009341 Lips)
%tc(X,Y):- between(1,1280,X),between(1,1280,Y).
write_ln(X):- nl,nl,write(X),nl,nl.
:- use_module(library(statistics)).

assert_adj(N):-
  retractall(adj(_,1)),
  assert_1adj(N),
  assertz(adj(N,1)).

assert_1adj(1):- !.
assert_1adj(N):- 
    Next is N-1,
    asserta(adj(Next,N)),
    assert_1adj(Next).

% 4.320962062 = 128
:- assert_adj(10).
% 37.451337628 = 256

:- use_module(library(pfc)).
:- cls.
/*
:- table(tc/2).
tc(X,Y):- adj(X,Y).
tc(X,Z):- tc(X,Y),tc(Y,Z).
*/

:- ain(( adj(X,Y) ==> tc(X,Y))).
:- ain(( (tc(X,Y),tc(Y,Z)) ==> tc(X,Z))).

:- predicate_property(adj(_,_),number_of_clauses(CC)),write_ln(adj=CC).
:- predicate_property(tc(_,_),number_of_clauses(CC)),write_ln(tc=CC).


:- statistics(cputime,X),assert(load_time(X)).
:- time(findall(_,tc(_X,_Y),L)),!,length(L,N),write_ln(length_was(N)).
:- statistics(cputime,X),retract(load_time(LT)), Time is X-LT,write_ln(time_was(Time)).

