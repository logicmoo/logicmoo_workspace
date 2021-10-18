
:- ensure_loaded(library(dcg_hash/dcg_hashexp)).
%:- clear_dcgs.

sentence --> noun_phrase, verb_phrase.
noun_phrase --> determiner, noun.
verb_phrase --> verb, noun_phrase.

determiner --> [the].
determiner --> [a].
noun --> [cat].
noun --> [mouse].
verb --> [chases].

dcg_test(DCG,Input):- phrase(DCG,Input,[]).

:- dcg_test(sentence,[the,cat,chaese,the,mouse]).

:- abolish(sentence//0).
:- abolish(sentence,2).
  

sentence --> noun_phrase(N), verb_phrase(N).

noun_phrase(N) --> determiner(N), noun(N).
verb_phrase(N) --> verb(N), noun_phrase(_).

determiner(_) --> [the].
determiner(singular) --> [a].
determiner(plural) --> [].

noun(singular) --> [cat].
noun(plural) --> [cats].
noun(singular) --> [mouse].
noun(plural) --> [mice].

verb(singular) --> [chases].
verb(plural) --> [chase].

:- dcg_test(sentence,[the,cat,chases,the,mouse]).


letter(X) --> [X], 
  {
    member(X, [a,b,c])
  }.

:- dcg_test(letter(a),[a]).
:- dcg_test(letter(X),[a]), X = a.
:- dcg_test((letter(X),letter(X)),[a,a]).

palin --> [].
palin --> letter(_).
palin --> letter(X), palin, letter(X).

:- dcg_test(palin,[a,a,a]).
:- dcg_test(palin,[b,a,a,a,b]).


