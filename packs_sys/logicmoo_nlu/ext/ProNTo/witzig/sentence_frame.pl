% sentence_frame.pl 
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 05-06-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia

% short program to experiment with the files wn_fr.pl and wn_sen.pl


:- ensure_loaded('wn_s.pl').
:- ensure_loaded('wn_fr.pl').
:- ensure_loaded('wn_sen.pl').

% sentence_frame(+Verb)
% Take an atom as argument, which should be a verb, and print out a sentence
% on the screen that shows the context in which the verb usually occurs

sentence_frame(Verb) :- 	s(Num,W_Num,Verb,v,_,_), fr(Num,F_Num,W_Num),
			 	sen(F_Num,String_1,String_2),
				write(String_1), write(Verb), write(String_2), nl.

sentence_frame(Verb) :-		s(Num,_,Verb,v,_,_), fr(Num,F_Num,0),
			 	sen(F_Num,String_1,String_2),
				write(String_1), write(Verb), write(String_2), nl.

