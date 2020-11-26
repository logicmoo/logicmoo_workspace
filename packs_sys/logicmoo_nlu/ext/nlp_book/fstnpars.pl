% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% fstnpars.pl [Chapter  2] A finite state transition network parser
%
% The "parse" is simply a list of the nodes visited and arcs traversed (the path).
%
?- reconsult('examples.pl').
?- reconsult('library.pl').
?- reconsult('fstnarcs.pl').
%
parse(Node,[],[Node]) :-
   final(Node).
parse(Node_1,String,[Node_1,Label|Path]) :-
   arc(Node_1,Node_2,Label),
   traverse(Label,String,NewString),
   parse(Node_2,NewString,Path).

test(Words) :-
   initial(Node),
   parse(Node,Words,Path),
   write(Path), nl.

traverse(Label,[Label|Words],Words) :-
    not(special(Label)).
traverse(Label,[Word|Words],Words) :-
    word(Label,Word).
traverse('#',String,String).

special('#').
special(Category) :- word(Category,_).
