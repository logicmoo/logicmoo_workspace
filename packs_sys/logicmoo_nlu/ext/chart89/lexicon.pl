% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% lexicon.pl [Chapter  4] An example lexicon
%
% ?- op(255,xfx,--->).
?- op(1050, xfx, --->).
%
np  ---> [kim].
np  ---> [sandy].
np  ---> [lee].
np  ---> [bread].
det ---> [a].
det ---> [the].
det ---> [her].
n   ---> [consumer].
n   ---> [duck].
n   ---> [man].
n   ---> [woman].
wh  ---> [who].
wh  ---> [that].
p   ---> [to].
iv  ---> [died].
iv  ---> [ate].
tv  ---> [ate].
tv  ---> [saw].
tv  ---> [gave].
dv  ---> [gave].
dv  ---> [handed].
sv  ---> [knew].
%
% following items only used by FSTNs (at present)
%
bv  ---> [is].
bv  ---> [was].
cnj ---> [and].
cnj ---> [or].
adj ---> [happy].
adj ---> [stupid].
mod ---> [very].
adv ---> [often].
adv ---> [always].
adv ---> [sometimes].
