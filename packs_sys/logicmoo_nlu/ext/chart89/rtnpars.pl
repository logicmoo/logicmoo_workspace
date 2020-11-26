% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
%
% rtnpars.pl [Chapter  3] A recursive transition network parser
%
?- reconsult('library.pl').
?- reconsult('rtnarcs.pl').
?- reconsult('examples.pl').
%
parse(Net,Node,X,X,[]) :-
   final(Node,Net).
parse(Net,Node_1,X,Z,[ParseXY|ParsesYZ]) :-
  arc(Node_1,Node_2,Label,Net),
  dbgwrite(arc(Node_1,Node_2,Label,Net),Node_1),
  traverse(Label,X,Y,ParseXY),
  parse(Net,Node_2,Y,Z,ParsesYZ).
%
traverse(Word,[Word|X],X,[Word]) :- not(special(Word)).
traverse(Category,[Word|X],X,[Category,Word]) :-
   word(Category,Word).
traverse(Net,String,StringLeft,[Net|Parses]) :-
   initial(Node,Net),
   parse(Net,Node,String,StringLeft,Parses).
traverse('#',X,X,[]).
%
test(String) :-
   traverse(s,String,[],Parse),
   write(Parse),
   nl.

special('#').
special(Category) :- word(Category,_).
