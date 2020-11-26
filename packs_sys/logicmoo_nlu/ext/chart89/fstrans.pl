% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%   Example code from the book "Natural Language Processing in Prolog"  %
%                      published by Addison Wesley                      %
%        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
%
% fstrans.pl [Chapter  2] A very simple finite state transducer
%
initial(1).
final(5).

arc(1,2,'WHERE').
arc(2,3,'BE').
arc(3,4,'FDET').
arc(4,5,'FNOUN').
arc(3,6,'MDET').
arc(6,5,'MNOUN').

word('WHERE',[where,ou]).
word('BE',[is,est]).
word('FDET',[the,la]).
word('MDET',[the,le]).
word('FNOUN',[exit,sortie]).
word('FNOUN',[shop,boutique]).
word('FNOUN',[toilet,toilette]).
word('MNOUN',[policeman,gendarme]).

transduce(Node,[],[]) :-
   final(Node).
transduce(Node_1,String1,String2) :-
   arc(Node_1,Node_2,Label),
   traverse2(Label,String1,NewString1,String2,NewString2),
   transduce(Node_2,NewString1,NewString2).

traverse2([Word1,Word2],[Word1|RestString1],RestString1,[Word2|RestString2],RestString2) :-
   not(special(Word1)), not(special(Word2)).
traverse2(Abbrev,String1,NewString1,String2,NewString2) :-
   word(Abbrev,NewLabel),
   traverse2(NewLabel,String1,NewString1,String2,NewString2).
traverse2(['#',Word2],String1,String1,[Word2|RestString2],RestString2).
traverse2([Word1,'#'],[Word1|RestString1],RestString1,String2,String2).
traverse2('#',String1,String1,String2,String2).

test(String_A) :-
   initial(Node),
   transduce(Node,String_A,String_B),
   write(String_B),
   nl.

special('#').
%
test :-
  write('Input:  '),write([where,is,the,shop]),nl,
  write('Output: '),test([where,is,the,shop]),
  write('Input:  '),write([where,is,the,policeman]),nl,
  write('Output: '),test([where,is,the,policeman]).
