% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% fstnarcs.pl [Chapter  2] An example finite state transition network
%
% ENGLISH1
%
initial(1).
final(9).
arc(1,3,np).
arc(1,2,det).
arc(2,3,n).
arc(3,4,bv).
arc(4,5,adv).
arc(4,5,'#').
arc(5,6,det).
arc(5,7,det).
arc(5,8,'#').
arc(6,7,adj).
arc(6,6,mod).
arc(7,9,n).
arc(8,9,adj).
arc(8,8,mod).
arc(9,4,cnj).
arc(9,1,cnj).

word(np,kim).
word(np,sandy).
word(np,lee).
word(det,a).
word(det,the).
word(det,her).
word(n,consumer).
word(n,man).
word(n,woman).
word(bv,is).
word(bv,was).
word(cnj,and).
word(cnj,or).
word(adj,happy).
word(adj,stupid).
word(mod,very).
word(adv,often).
word(adv,always).
word(adv,sometimes).
