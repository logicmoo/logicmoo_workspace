/****************************************************
% pronto_morph_spelling_rules.pl
% Author: Jason Schlachter (ai@uga.edu)(www.arches.uga.edu/~ai)

% Released: May 8th, 2003
% Artificial Intelligence Center (www.ai.uga.edu)
% ***see pronto_morph.pdf for documentation

% Morphological Analyzer to be used with
% ProNTo (Prolog Natural Language Toolkit),
% created at the Artificial Intelligence Center
% of The University of Georgia
****************************************************/




% *******************************************************
% COMMENT OUT SPELLING RULES THAT YOU DO NOT WANT TO USE!
% *******************************************************
% split_suffix(+Characters,-Root,-Suffix)
%
%  Splits a word into root and suffix.
%  Must line up with the end of the word
%  (that's the job of find_suffix, which calls it).
%  Fails if there is no suffix.
%  Instantiates Category to the syntactic category
%  to which this suffix attaches.
% *******************************************************

split_suffix( X , [] , S ):-
	suffix(	X , S ).

% Suffixes with doubled consonants in root
split_suffix([z,z,e,s],[z],-s).  % "quizzes", "whizzes"
split_suffix([s,s,e,s],[s],-s).  % "gasses" (verb), "crosses"
% TOO VAGUE????
split_suffix([V,C,C,V2|Rest],[V,C],Suffix) :-
   vowel(V), \+ vowel(C), vowel(V2), suffix([V2|Rest],Suffix).

% y changing to i and -s changing to -es simultaneously
split_suffix([C,i,e,s],[C,y],-s) :- \+ vowel(C).

% y changes to i after consonant, before suffix beg. w vowel
split_suffix([C,i,X|Rest],[C,y],Suffix) :-
   \+ vowel(C), \+ (X = i), suffix([_|Rest],Suffix).

% -es = -s after s (etc.)
split_suffix([s,h,e,s],[s,h],-s).
split_suffix([c,h,e,s],[c,h],-s).
split_suffix([s,e,s],[s],-s).
split_suffix([z,e,s],[z],-s).
split_suffix([x,e,s],[x],-s).

% verb spelling rules
split_suffix([e,n],[e],-en).         % (ex. forgiven --> forgive)

% -est = superlative inflection
split_suffix( [ e , s , t ] , [ e ] , -est ). % example is "finest"
split_suffix( [ C , C , e , s , t ] , [ C ] , -est ) :-  % example is "reddest"
	\+ vowel( C ).

% should this be here??
split_suffix( [ i , e , s , t ] , [ y ] , -est ). % example is "craziest"

% -er = comparative inflection
split_suffix( [ C , C , e , r ] , [ C ] , -er ) :-  % example is "redder"
	\+ vowel( C ).
split_suffix( [i , e , r ] , [ y ] , -er ). % example is "crazier"

% Final e drops out before a suffix beg. with a vowel
split_suffix([C,V|Rest],[C,e],Suffix) :-
   \+ vowel(C), vowel(V), suffix([V|Rest],Suffix).


% spelling rules for English words
% that have foriegn or scientific origins
%
split_suffix([l,v,e,s],[l,f],-pl).
split_suffix([e,a,u,x],[e,a,u],-pl).
split_suffix([i],[u,s],-pl).
split_suffix([i,a],[i,u,m],-pl).     
split_suffix([a],[u,m],-pl).         % (ex. antra --> antrum)
split_suffix([a,e],[a],-pl).         % (ex. amoebae --> amoeba)
split_suffix([s,e,s],[s,i,s],-pl).   % (ex. amniocenteses --> amniocenteses)
split_suffix([x,e,s],[x,i,s],-pl).   % (ex. apomixes apomixis)
split_suffix([i,c,e,s],[e,x],-pl).   % (ex. apices apex)
split_suffix([i,c,e,s],[i,x],-pl).   % (ex. appendices appendix)
split_suffix([i,m],[i],-pl).	     % (ex. ashkenazim ashkenazi)
split_suffix([e,s],[],-pl).          % (ex. banjoes --> banjo)
split_suffix([i,n,a],[e,n],-pl).     % (ex. praenomina --> praenomen)
split_suffix([i,a],[e],-pl).	     % (ex. qualia --> quale)
split_suffix([a,t,a],[a],-pl).       % (ex. trymata --> tryma)
split_suffix([i,a],[i,o,n],-pl).     % (ex. acromia --> acromion) 
split_suffix([a],[o,n],-pl).         % (ex. entera --> enteron)
split_suffix([v,e,s],[f,e],-pl).     % (ex. knives --> knife)
split_suffix([i],[o], -pl).          % (ex. maestri --> maestro)
split_suffix([f,e,e,t],[f,o,o,t],-pl).     % (ex. blackfeet --> blackfoot)
split_suffix([a,e],[e],-pl).               % (ex. phylae --> phyle) 
split_suffix([i],[],-pl).		   % (ex. pirogi --> pirog)
split_suffix([e,s],[i,s],-pl).             % (ex. vermes --> vermis)

