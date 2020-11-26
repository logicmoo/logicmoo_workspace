% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% rtrans.pl [Chapter  3] Pushdown transduction
%
?- reconsult('library.pl').
%
% version of rtnpars.pl that takes:
%  first string, second string, start node, network
% transduce(Net,Node,String1,Left1,String2,Left2)
%       Net: the network in which the traversal takes place
%       Node: the node to start from
%       String1: the first tape
%       Left1: the portion of the first tape "left over"
%       String2: the second tape
%       Left2: the portion of the second tape "left over"

transduce(Net,Node,X1,X1,X2,X2) :-
    final(Net,Node).
transduce(Net,Node_1,X1,Z1,X2,Z2) :-
   arc(Node_1,Node_2,Label,Net),
%  dbgwrite(arc(Node_1,Node_2,Label,Net),Node_1),
   traverse2(Label,X1,Y1,X2,Y2),
   transduce(Net,Node_2,Y1,Z1,Y2,Z2).
%
traverse2(['#',Word],X1,X1,[Word|X2],X2).
traverse2([Word,'#'],[Word|X1],X1,X2,X2).
traverse2([Word_1,Word_2],[Word_1|X1],X1,[Word_2|X2],X2) :-
   not(special(Word_1)), not(special(Word_2)).
traverse2(Category,String_1,Left_1,String_2,Left_2) :-
   word(Category,Word_pair),
   traverse2(Word_pair,String_1,Left_1,String_2,Left_2).
traverse2(Subnet,String_1,Left_1,String_2,Left_2) :-
   initial(Subnet,Node),
   transduce(Subnet,Node,String_1,Left_1,String_2,Left_2).
traverse2('#',X1,X1,X2,X2).
%
test(Sentence_1,Sentence_2) :-
   traverse2(s,Sentence_1,[],Sentence_2,[]),
   write(Sentence_1), nl,
   write(Sentence_2), nl.

special('#').
%
% French translation example
%
% S network
%
initial(s,0).
final(s,2).
arc(0,1,np,s).
arc(1,2,vp,s).

% NP network
%
initial(np,0).
final(np,2).
arc(0,1,det_femn,np).
arc(1,2,n_femn,np).
arc(0,4,det_masc,np).
arc(4,2,n_masc,np).
arc(2,3,wh,np).
arc(3,2,vp,np).

% VP network
%
initial(vp,0).
final(vp,1).
final(vp,2).
arc(0,1,v,vp).
arc(1,2,np,vp).
arc(1,3,[that,que],vp).
arc(3,2,s,vp).
word(n_masc,[man,homme]).
word(n_masc,[horse,cheval]).
word(n_femn,[house,maison]).
word(n_femn,[table,table]).
word(np,[john,jean]).
word(np,[mary,marie]).
word(np,[jean,jeanne]).
word(det_masc,[a,un]).
word(det_masc,[the,le]).
word(det_masc,[this,ce]).
word(det_femn,[a,une]).
word(det_femn,[the,la]).
word(det_femn,[this,cette]).
word(v,[sees,voit]).
word(v,[hits,frappe]).
word(v,[sings,chante]).
word(v,[lacks,manque]).
word(wh,[who,qui]).
word(wh,[which,qui]).
word(wh,[that,qui]).

test1 :-
  test([john,sings],French).
test2 :-
  test(English,[marie,voit,un,cheval]).
test3 :-
  test([jean,sees,a,house,that,lacks,a,table],French).
test4 :-
  test([the,man,who,sings,sees,that,this,house,lacks,a,table],French).
