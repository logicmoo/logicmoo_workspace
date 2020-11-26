% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% ezrecg.pl [Chapter  4] A simple topdown recognizer
%
% DCG precursor, also to illustrate use of difference lists
% to avoid explicit concatenation
%
% rules
s(X,Z)   :- np(X,Y), vp(Y,Z).
vp(X,Z)  :- v(X,Z).
vp(X,Z)  :- v(X,Y), np(Y,Z).
%
% lexicon
%
np(['Dr. Chan'|X],X).
np(['MediCenter'|X],X).
np([nurses|X],X).
np([patients|X],X).
v([died|X],X).
v([employed|X],X).
%
% examples
%
test1 :- s([patients,died],[]).
test2 :- s(['MediCenter',employed,nurses],[]).
