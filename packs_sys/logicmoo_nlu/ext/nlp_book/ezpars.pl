% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% ezpars.pl [Chapter  4] A simple top-down parser
%
% rules
s([s,NP,VP],X,Z)   :- np(NP,X,Y), vp(VP,Y,Z).
vp([v,V],X,Z)  :- v(V,X,Z).
vp([v,V,NP],X,Z)  :- v(V,X,Y), np(NP,Y,Z).
%
% lexicon
%
np([np,'Dr. Chan'],['Dr. Chan'|X],X).
np([np,'MediCenter'],['MediCenter'|X],X).
np([np,nurses],[nurses|X],X).
np([np,patients],[patients|X],X).
v([v,dies],[died|X],X).
v([v,employed],[employed|X],X).
%
% examples
%
test1 :- s(Parse,[patients,died],[]),write(Parse),nl.
test2 :- s(Parse,['MediCenter',employed,nurses],[]),write(Parse),nl.
