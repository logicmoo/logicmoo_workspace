
% This file contains tests for optimal solutions. 
% The optimal solution is calculated with dfs (and can be theoreticly prooved
% for hanoi). The file will comment if the tests run non-optimal, or does not
% provide a solution at all. The running time is capped at 3 seconds (can be 
% changed in common2.pl, get_solutions predicate). Test by using mutants of the files,
% see the commented lines in the beginning.

% Note that only admissible heuristics for a-star will guarantee to always find optimal
% solutions. Admissible means that the heuristic never overestimates the distances.
% Only h_max (and h_0) are admissible, but h_diff is admissible under certain assumptions 
% that are fullfilled here (that each non-satisfied action requires at least one action 
% to fulfill). h_add is not admissible, which you can easily check by running the tests.

% In this version, the revisiting-bug has _not_ been fixed yet, so all heurisitics will
% return non-optimal solutions.
                                                            
% Note that h_max is _not_ monotonic, which means that it has to revisit nodes to get the 
% optimal solutions.
 
% Some tests are commented out, because they require long running times. If you are patient
% and change the maximum running time, they can be commented in. 
:- use_module(library(plunit)).

:-[readFile, parseProblem, parseDomain, common, common2].
%:- ['a-star', forward, h_add].
%:- ['a-star', forward, h_0].
:- ['a-star', forward, h_max].
%:- ['a-star', forward, h_diff].

:- begin_tests(test_hanoi).

test(hanoi_test2) :-
        get_solution('test/hanoi/domain-hanoi.pddl', 'test/hanoi/hanoi_test2.pddl', X),
        (X =:= 2;
%         print('Hanoi2 passed'), nl; 
        (nl, print(' ===> '), print('Hanoi2 failed, expected 2, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(hanoi3) :-
        get_solution('test/hanoi/domain-hanoi.pddl', 'test/hanoi/hanoi3.pddl', X),
        (X =:= 7;
%         print('Hanoi3 passed'), nl; 
        (nl, print(' ===> '), print('Hanoi3 failed, expected 7, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(hanoi4) :-
        get_solution('test/hanoi/domain-hanoi.pddl', 'test/hanoi/hanoi4.pddl', X),
        (X =:= 15;
%         print('Hanoi4 passed'), nl; 
        (nl, print(' ===> '), print('Hanoi4 failed, expected 15, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

% h_max fails here
test(hanoi5) :-
        get_solution('test/hanoi/domain-hanoi.pddl', 'test/hanoi/hanoi5.pddl', X),
        (X =:= 31;
%         print('Hanoi5 passed'), nl; 
        (nl, print(' ===> '), print('Hanoi5 failed, expected 31, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

% h_add fails here              
%test(hanoi6) :-
%        get_solution('test/hanoi/domain-hanoi.pddl', 'test/hanoi/hanoi6.pddl', X),
%        (X =:= 63;
%%         print('Hanoi63 passed'), nl; 
%        (nl, print(' ===> '), print('Hanoi6 failed, expected 63, computed: '), print(X), 
%         print(' <==='), nl, nl, fail)), !.

:- end_tests(test_hanoi).

:- begin_tests(shuffling).

test(shuffling-1) :-
        get_solution('test/shuffling/shuffling_domain.pddl', 'test/shuffling/shuffling-1.pddl', X),
        (X =:= 1;
%       print('Shuffling-1 passed'), nl; 
        (nl, print(' ===> '), print('Shuffling-1 failed, expected 1, computed: '), print(X), 
        print(' <==='), nl, nl, fail)), !.

test(shuffling-2) :-
        get_solution('test/shuffling/shuffling_domain.pddl', 'test/shuffling/shuffling-2.pddl', X),
        (X =:= 1;
%       print('Shuffling-2 passed'), nl; 
        (nl, print(' ===> '), print('Shuffling-2 failed, expected 1, computed: '), print(X), 
        print(' <==='), nl, nl, fail)), !.

test(shuffling-3) :-
        get_solution('test/shuffling/shuffling_domain.pddl', 'test/shuffling/shuffling-3.pddl', X),
        (X =:= 1;
%       print('Shuffling-3 passed'), nl; 
        (nl, print(' ===> '), print('Shuffling-3 failed, expected 1, computed: '), print(X), 
        print(' <==='), nl, nl, fail)), !.

test(shuffling-4) :-
        get_solution('test/shuffling/shuffling_domain.pddl', 'test/shuffling/shuffling-4.pddl', X),
        (X =:= 2;
%       print('Shuffling-4 passed'), nl; 
        (nl, print(' ===> '), print('Shuffling-4 failed, expected 1, computed: '), print(X), 
        print(' <==='), nl, nl, fail)), !.

test(shuffling-5) :-
        get_solution('test/shuffling/shuffling_domain.pddl', 'test/shuffling/shuffling-5.pddl', X),
        (X =:= 1;
%       print('Shuffling-5 passed'), nl; 
        (nl, print(' ===> '), print('Shuffling-5 failed, expected 1, computed: '), print(X), 
        print(' <==='), nl, nl, fail)), !.
        
:- end_tests(shuffling).

:- begin_tests(sliding).

test(sliding2x2-1) :-
        get_solution('test/sliding/sliding_domain.pddl', 'test/sliding/sliding2x2-1.pddl', X),
        (X =:= 1;
%         print('Sliding2x2-1 passed'), nl; 
        (nl, print(' ===> '), print('Sliding2x2-1 failed, expected 1, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(sliding2x2-2) :-
        get_solution('test/sliding/sliding_domain.pddl', 'test/sliding/sliding2x2-2.pddl', X),
        (X =:= 3;
%         print('Sliding2x2-2 passed'), nl; 
        (nl, print(' ===> '), print('Sliding2x2-2 failed, expected 3, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(sliding2x2-3) :-
        get_solution('test/sliding/sliding_domain.pddl', 'test/sliding/sliding2x2-3.pddl', X),
        (X =:= 4;
%         print('Sliding2x2-3 passed'), nl; 
        (nl, print(' ===> '), print('Sliding2x2-3 failed, expected 4, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(sliding2x2-4) :-
        get_solution('test/sliding/sliding_domain.pddl', 'test/sliding/sliding2x2-4.pddl', X),
        (X =:= 6;
%         print('Sliding2x2-6 passed'), nl; 
        (nl, print(' ===> '), print('Sliding2x2-4 failed, expected 6, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(sliding3x3-1) :-
        get_solution('test/sliding/sliding_domain.pddl', 'test/sliding/sliding3x3-1.pddl', X),
        (X =:= 1;
%         print('Sliding3x3-1 passed'), nl; 
        (nl, print(' ===> '), print('Sliding3x3-1 failed, expected 1, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(sliding3x3-2) :-
        get_solution('test/sliding/sliding_domain.pddl', 'test/sliding/sliding3x3-2.pddl', X),
        (X =:= 6;
%         print('Sliding3x3-2 passed'), nl; 
        (nl, print(' ===> '), print('Sliding3x3-2 failed, expected 6, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.
        
test(sliding3x3-3) :-
        get_solution('test/sliding/sliding_domain.pddl', 'test/sliding/sliding3x3-3.pddl', X),
        (X =:= 9;
%         print('Sliding3x3-3 passed'), nl; 
        (nl, print(' ===> '), print('Sliding3x3-3 failed, expected 9, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(sliding4x4-1) :-
        get_solution('test/sliding/sliding_domain.pddl', 'test/sliding/sliding4x4-1.pddl', X),
        (X =:= 1;
%         print('Sliding4x4-1 passed'), nl; 
        (nl, print(' ===> '), print('Sliding4x4-1 failed, expected 1, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.
        
test(sliding4x4-2) :-
        get_solution('test/sliding/sliding_domain.pddl', 'test/sliding/sliding4x4-2.pddl', X),
        (X =:= 3;
%         print('Sliding4x4-2 passed'), nl; 
        (nl, print(' ===> '), print('Sliding4x4-2 failed, expected 3, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

:- end_tests(sliding).

:- begin_tests(gripper).

test(gripper2) :-
        get_solution('test/gripper/domain-gripper.pddl', 'test/gripper/gripper2.pddl', X),
        (X =:= 5;
%         print('Gripper2 passed'), nl; 
        (nl, print(' ===> '), print('Gripper2 failed, expected 5, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.   

test(gripper4) :-
        get_solution('test/gripper/domain-gripper.pddl', 'test/gripper/gripper4.pddl', X),
        (X =:= 11;
%         print('Gripper4 passed'), nl; 
        (nl, print(' ===> '), print('Gripper4 failed, expected 11, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

%test(gripper6) :-
%        get_solution('test/gripper/domain-gripper.pddl', 'test/gripper/gripper6.pddl', X),
%        (X =:= 17;
%%         print('Gripper6 passed'), nl; 
%        (nl, print(' ===> '), print('Gripper6 failed, expected 17, computed: '), print(X), 
%         print(' <==='), nl, nl, fail)), !.

:- end_tests(gripper). 

:- begin_tests(blocks).

test(blocks-03-0) :-
        get_solution('test/blocks/domain-blocks.pddl', 'test/blocks/blocks-03-0.pddl', X),
        (X =:= 4;
%         print('Blocks-03-0 passed'), nl; 
        (nl, print(' ===> '), print('Blocks-03-0 failed, expected 4, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(blocks-04-0) :-
        get_solution('test/blocks/domain-blocks.pddl', 'test/blocks/blocks-04-0.pddl', X),
        (X =:= 6;
%         print('Blocks-04-0 passed'), nl; 
        (nl, print(' ===> '), print('Blocks-04-0 failed, expected 6, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.
        
test(blocks-04-1) :-
        get_solution('test/blocks/domain-blocks.pddl', 'test/blocks/blocks-04-1.pddl', X),
        (X =:= 10;
%         print('Blocks-04-1 passed'), nl; 
        (nl, print(' ===> '), print('Blocks-04-1 failed, expected 10, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.
        
test(blocks-04-2) :-
        get_solution('test/blocks/domain-blocks.pddl', 'test/blocks/blocks-04-2.pddl', X),
        (X =:= 6;
%         print('Blocks-04-2 passed'), nl; 
        (nl, print(' ===> '), print('Blocks-04-2 failed, expected 6, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(blocks-05-0) :-
        get_solution('test/blocks/domain-blocks.pddl', 'test/blocks/blocks-05-0.pddl', X),
        (X =:= 12;
%         print('Blocks-05-0 passed'), nl; 
        (nl, print(' ===> '), print('Blocks-05-0 failed, expected 12, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(blocks-05-1) :-
        get_solution('test/blocks/domain-blocks.pddl', 'test/blocks/blocks-05-1.pddl', X),
        (X =:= 10;
%         print('Blocks-05-1 passed'), nl; 
        (nl, print(' ===> '), print('Blocks-05-1 failed, expected 10, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.
        
test(blocks-05-2) :-
        get_solution('test/blocks/domain-blocks.pddl', 'test/blocks/blocks-05-2.pddl', X),
        (X =:= 16;
%         print('Blocks-05-2 passed'), nl; 
        (nl, print(' ===> '), print('Blocks-05-2 failed, expected 16, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

%test(blocks-06-0) :-
%        get_solution('test/blocks/domain-blocks.pddl', 'test/blocks/blocks-06-0.pddl', X),
%        (X =:= 12;
%%         print('Blocks-06-0 passed'), nl; 
%        (nl, print(' ===> '), print('Blocks-06-0 failed, expected 12, computed: '), print(X), 
%         print(' <==='), nl, nl, fail)), !.
        
        
:- end_tests(blocks).

:- begin_tests(elevators).

test(elevator-01-0) :-
        get_solution('test/elevators/domain-elevators.pddl', 'test/elevators/elevators-01-0.pddl', X),
        (X =:= 4;
%         print('Elevator-01-0 passed'), nl; 
        (nl, print(' ===> '), print('Elevator-01-0 failed, expected 4, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(elevator-01-4) :-
        get_solution('test/elevators/domain-elevators.pddl', 'test/elevators/elevators-01-4.pddl', X),
        (X =:= 4;
%         print('Elevator-01-4 passed'), nl; 
        (nl, print(' ===> '), print('Elevator-01-4 failed, expected 4, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(elevator-02-0) :-
        get_solution('test/elevators/domain-elevators.pddl', 'test/elevators/elevators-02-0.pddl', X),
        (X =:= 7;
%         print('Elevator-02-0 passed'), nl; 
        (nl, print(' ===> '), print('Elevator-02-0 failed, expected 4, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(elevator-02-4) :-
        get_solution('test/elevators/domain-elevators.pddl', 'test/elevators/elevators-02-4.pddl', X),
        (X =:= 7;
%         print('Elevator-02-4 passed'), nl; 
        (nl, print(' ===> '), print('Elevator-02-4 failed, expected 7, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(elevator-03-0) :-
        get_solution('test/elevators/domain-elevators.pddl', 'test/elevators/elevators-03-0.pddl', X),
        (X =:= 10;
%         print('Elevator-03-0 passed'), nl; 
        (nl, print(' ===> '), print('Elevator-03-0 failed, expected 10, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

test(elevator-03-1) :-
        get_solution('test/elevators/domain-elevators.pddl', 'test/elevators/elevators-03-1.pddl', X),
        (X =:= 11;
%         print('Elevator-03-1 passed'), nl; 
        (nl, print(' ===> '), print('Elevator-03-1 failed, expected 11, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

% h_add fails here
test(elevator-03-2) :-
        get_solution('test/elevators/domain-elevators.pddl', 'test/elevators/elevators-03-2.pddl', X),
        (X =:= 10;
%         print('Elevator-03-2 passed'), nl; 
        (nl, print(' ===> '), print('Elevator-03-2 failed, expected 10, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.
                
test(elevator-03-3) :-
        get_solution('test/elevators/domain-elevators.pddl', 'test/elevators/elevators-03-3.pddl', X),
        (X =:= 10;
%         print('Elevator-03-3 passed'), nl; 
        (nl, print(' ===> '), print('Elevator-03-3 failed, expected 10, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

% h_add fails here
test(elevator-03-4) :-
        get_solution('test/elevators/domain-elevators.pddl', 'test/elevators/elevators-03-4.pddl', X),
        (X =:= 10;
%         print('Elevator-03-4 passed'), nl; 
        (nl, print(' ===> '), print('Elevator-03-4 failed, expected 10, computed: '), print(X), 
         print(' <==='), nl, nl, fail)), !.

%test(elevator-04-0) :-
%        get_solution('test/elevators/domain-elevators.pddl', 'test/elevators/elevators-04-0.pddl', X),
%        (X =:= 14;
%%         print('Elevator-04-0 passed'), nl; 
%        (nl, print(' ===> '), print('Elevator-04-0 failed, expected 14, computed: '), print(X), 
%         print(' <==='), nl, nl, fail)), !.
%
%test(elevator-04-4) :-
%        get_solution('test/elevators/domain-elevators.pddl', 'test/elevators/elevators-04-4.pddl', X),
%        (X =:= 15;
%%         print('Elevator-04-4 passed'), nl; 
%        (nl, print(' ===> '), print('Elevator-04-4 failed, expected 14, computed: '), print(X), 
%         print(' <==='), nl, nl, fail)), !.

:- end_tests(elevators).

:- run_tests.