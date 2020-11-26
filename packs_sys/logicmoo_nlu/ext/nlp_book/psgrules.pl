% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% psgrules.pl [Chapter  4] An example set of CF-PSG rules
%
% DCG style format
%
:- op(255,xfx,--->).
%
initial(s).		% used by chart parsers
%
s   ---> (np, vp).
np  ---> (det, nb).
nb  ---> n.
nb  ---> (n, rel).
rel ---> (wh, vp).
vp  ---> iv.
vp  ---> (tv, np).
vp  ---> (dv, np, pp).
vp  ---> (sv, s).
pp  ---> (p, np).
%
