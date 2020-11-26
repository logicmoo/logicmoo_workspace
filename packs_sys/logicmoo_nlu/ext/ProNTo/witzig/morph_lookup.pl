% morph_lookup.pl 
% 
% ProNTo - Prolog Natural Language Toolkit
% Accessing WordNet from Prolog
%
% author: Sarah Witzig
% date created: 05-06-2003
% date last modified: 05-07-2003
% notes: Artificial Intelligence Center, University of Georgia
%

:- ensure_loaded('wn_s_convert.pl').

% Input is a list of morphological analyzed words, basically the output
% of Jason Schlachter's ProNTo morphological analyser. 
% It looks up if a word or list of words is in WordNet, succeeds if yes,
% fails if no. 

% morph_atoms_lookup(+Morph)
% Look up one morphological analyzed word like [[hard,-er]] or a list
% of all possibilities of a morphological analyzes of a word, e.g.
% [[harder],[harde,-r],[hard,-er]]. 
% Succeed if it exists, fail otherwise.

morph_atoms_lookup(Morph) :- morph_bag_lookup([Morph]).

% morph_bag_lookup(+Morph)
% Lookup a sentence that has been morphological analyzed, e.g.
% [[[he]],[[walked],[walke,-d],[walk,-ed]]].

morph_bag_lookup([]).
morph_bag_lookup([[[XF|_]|_]|Rest]) :- 		s([XF,A|_],_,_,_,_,_), \+atom(A), 
						morph_bag_lookup(Rest).

morph_bag_lookup([[XF|RestPos]|Rest]) :-	\+ s([XF|_],_,_,_,_,_), 
						morph_bag_lookup([RestPos|Rest]).

% morph_atoms_lookup(+Morph,-Result)
% Look up one morphological analyzed word and return a list.
% If the word was found in WordNet return a list [Morph,Synset_ID,W_Num,Category].
% If the word can not be found return [not,a,word].

morph_atoms_lookup(Morph,Result) :- morph_bag_lookup([Morph],Result).

% morph_bag_lookup(+Morph,-Result)
% Look up a sentence that has been morphological analyzed and return a list 
% of lists. Each of the inner lists should either have information on
% the word that has been identified in WordNet or be the list [not,a,word].

morph_bag_lookup([],[]).
morph_bag_lookup([[[XF|_]|_]|Rest],[[XF,Num,W_Num,C]|RestReturn]) :- 	
							s([XF,A|_],Num,W_Num,C,_,_), \+atom(A), 
							morph_bag_lookup(Rest,RestReturn).
	
morph_bag_lookup([[XF|RestPos]|Rest],RestReturn) :-	\+ s([XF|_],_,_,_,_,_),
							morph_bag_lookup([RestPos|Rest],RestReturn).

morph_bag_lookup([[]|Rest],[[not,a,word]|RestReturn]) :- morph_bag_lookup(Rest,RestReturn).

