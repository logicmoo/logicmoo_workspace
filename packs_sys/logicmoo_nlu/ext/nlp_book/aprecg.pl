% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% aprecg.pl [Chapter  4] Encoding a PSG in Prolog
%
% simple topdown recognizer/generator
%
?-reconsult('library.pl').
%
% rules
s(Z)   :- np(X), vp(Y),
          append(X,Y,Z).
vp(Z)  :- v(X), np(Y),
          append(X,Y,Z).
vp(Z)  :- v(Z).
%
% lexicon
%
np(['Dr. Chan']).
np(['MediCenter']).
np([nurses]).
np([patients]).
v([died]).
v([employed]).
%
% examples
%
test1 :- s([patients,died]).
test2 :- s(['MediCenter',employed,nurses]).
