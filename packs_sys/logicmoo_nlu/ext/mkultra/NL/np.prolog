%%%                  Noun Phrases

%% np(?Meaning, ?Case, Agreement, ?Gap)
%  Noun phrases

:- randomizable np//4.
np(NP, _C, third:Number, nogap) --> 
   det(N1^NP), n(Number, N1).
np(NP, _C, third:Number, nogap) --> proper_noun(Number, NP).
np(NP, Case, Agreement, nogap) --> pronoun(Case, Agreement, NP).
np((X^S)^S, _C, _Agreement, np(X)) --> [].

