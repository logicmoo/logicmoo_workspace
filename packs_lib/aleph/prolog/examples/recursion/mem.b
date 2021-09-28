% Simple illustration of the learning of recursive predicates
%       in Aleph
% To run do the following:
%       a. Load Aleph
%       b. read_all(mem).
%       c. induce.

% :- modeh(*,mem(+any,+list)).
% :- modeb(*,mem(+any,+list)).
% :- modeb(1,((+list) = ([-any|-list]))).

:- mode(*,mem(+any,+list)).
:- mode(1,((+list) = ([-any|-list]))).

:- set(i,3).
:- set(noise,0).


:- determination(mem/2,mem/2).
:- determination(mem/2,'='/2).

