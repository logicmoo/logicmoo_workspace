% Simple illustration of using Aleph to do incremental learning
% To run do the following:
%       a. Load Aleph
%       b. read_all(mem).
%       c. induce_incremental.
% After that, just follow the menus on screen.

% :- modeh(*,mem(+any,+list)).
% :- modeb(*,mem(+any,+list)).
% :- modeb(1,((+list) = ([-any|-list]))).

:- mode(*,mem(+any,+list)).
:- mode(1,((+list) = ([-any|-list]))).

:- set(i,3).
:- set(noise,0).
:- set(print,1).


:- determination(mem/2,mem/2).
:- determination(mem/2,'='/2).

