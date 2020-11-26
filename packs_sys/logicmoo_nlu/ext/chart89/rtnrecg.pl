% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% rtnrecg.pl [Chapter  3] A recursive transition network recognizer
%
% uses difference lists
%
?- reconsult('library.pl').
?- reconsult('rtnarcs.pl').
?- reconsult('examples.pl').
%
% recognize(Net,Node,WordList,Left)
%      Net: the name of the network to be traversed
%      Node:  the name of the node you wish to start from
%      WordList:  list of words which you want to test
%      Left: the list of words left over after the traversal
recognize(Net,Node,X,X) :-
   final(Node,Net).
recognize(Net,Node_1,X,Z) :-
   arc(Node_1,Node_2,Label,Net),
   dbgwrite(arc(Node_1,Node_2,Label,Net),Node_1),
   traverse(Label,X,Y),
   recognize(Net,Node_2,Y,Z).

% traverse(Label,WordList,Left)
%      Label: an arc label specifying a kind of test
%      WordList:  list of words which you want to test
%      Left: the list of words left over after the traversal of the arc
traverse(Word,[Word|X],X) :- not(special(Word)).
traverse(Category,[Word|X],X) :-
   word(Category,Word).
traverse(Net,String,Left) :-
   initial(Node,Net),
   recognize(Net,Node,String,Left).
%
traverse('#',X,X).
%
test(Sentence) :-
  traverse(s,Sentence,[]).

special('#').
special(Category) :- word(Category,_).
