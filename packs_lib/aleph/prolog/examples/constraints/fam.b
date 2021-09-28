% Simple illustration of the extraction of integrity constraints
%       by Aleph
% To run do the following:
%       a. Load Aleph
%       b. read_all(fam).
%       c. induce_constraints.
% This will learn a set of (possibly redundant) constraints that hold
% in the background knowledge. The procedure is similar to that used
% by DeRaedt et al in Claudien. Constraints that are ``nearly true''
% can be obtained by changing the noise parameter.

:- modeh(1,false).

:- modeb(*,human(-person)).

:- modeb(1,male(+person)).
:- modeb(1,female(+person)).
:- modeb(1,not(male(+person))).
:- modeb(1,not(female(+person))).

:- determination(false/0,human/1).
:- determination(false/0,male/1).
:- determination(false/0,female/1).
:- determination(false/0,(not)/1).

:- set(noise,0).


male('Fred').
female('Wilma').
human('Fred').
human('Wilma').
