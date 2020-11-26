% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% rtnarcs.pl [Chapter  3] A simple recursive transition network
%
% the S network
%
initial(0,s).
final(2,s).
%
arc(0,1,np,s).
arc(1,2,vp,s).
%
% the NP network
%
initial(0,np).
final(2,np).
%
arc(0,1,det,np).
arc(1,2,n,np).
arc(2,3,wh,np).
arc(3,2,vp,np).
%
% the VP network
%
initial(0,vp).
final(1,vp).
final(2,vp).
%
arc(0,1,v,vp).
arc(1,2,np,vp).
arc(1,3,that,vp).
arc(3,2,s,vp).

word(n,woman).
word(n,house).
word(n,table).
word(n,mouse).
word(n,man).
word(np,'Mayumi').
word(np,'Maria').
word(np,'Washington').
word(np,'John').
word(np,'Mary').
word(det,a).
word(det,the).
word(det,that).
word(v,sees).
word(v,hits).
word(v,sings).
word(v,lacks).
word(v,saw).
word(wh,who).
word(wh,which).
word(wh,that).
%
