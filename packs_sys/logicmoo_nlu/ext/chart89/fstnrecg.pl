% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% fstnrecg.pl [Chapter  2] A finite state transition network recognizer
%
% As it stands, with the example in fstnarcs.pl, this will
% generate, though it gets stuck in a boring loop
%
% The network for this program can be found in the file 'fstnarcs.pl'
% If you want to try this program on different networks, just edit
% the 'fstnarcs.pl' file or change the name of the file reconsulted
% below.
%
% The 'examples.pl' file contains some ready-made test clauses that
% will work with this program - see testf1, testf2 and testf3.
%
?- reconsult('examples.pl').
?- reconsult('library.pl').
?- reconsult('fstnarcs.pl').
%
% recognize(Node,Wordlist)
%      WordList:  list of words which you want to test - these words
%                 must be enclosed in square brackets and separated
%                 by commas
%      Node:  number of the node you wish to start from
%
% This clause succeeds if WordList can be recognized by the fstn in the
% database starting from the node 'Node' and ending in a final state.
% The 'dbgwrite' clauses will write out which nodes are being tried,
% but will not otherwise effect the predicate.
%
recognize(Node,[]) :-
    final(Node).
recognize(Node_1,String) :-
    arc(Node_1,Node_2,Label),
    traverse(Label,String,NewString),
%   dbgwrite(arc(Node_1,Node_2,Label), Node_1),
    recognize(Node_2,NewString).

traverse(Label,[Label|Words],Words) :-
    not(special(Label)).
traverse(Label,[Word|Words],Words) :-
    word(Label,Word).
traverse('#',String,String).

special('#').
special(Category) :- word(Category,_).
%
%
% test(WordList)
%      WordList:  List of words, as above
%
% This clause succeeds if the fstn in the database can recognize
% the string starting from an initial node.
%
test(Words) :-
    initial(Node),
    recognize(Node,Words).
%
generate :-
   test(X),
   write(X), nl,
   fail.
