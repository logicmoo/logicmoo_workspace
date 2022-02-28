% relpron( that ).
% relpron( who  ).
% relpron( whom ).

pronoun_word('I', subject, first, singular, $speaker).
pronoun_word(me, object, first, singular, $speaker).
pronoun_word(my, genitive, first, singular, $speaker).
pronoun_word(you, subject, second, singular, $addressee).
pronoun_word(you, object, second, singular, $addressee).
pronoun_word(your, genitive, second, singular, $addressee).
pronoun_word(we, subject, first, plural, $dialog_group).
pronoun_word(us, object, first, plural, $dialog_group).
pronoun_word(our, genitive, first, plural, $dialog_group).

demonstrative_pronoun(this).
demonstrative_pronoun(that).

:- register_all_lexical_items([D], demonstrative_pronoun(D)).

here_there_adverb(here).
here_there_adverb(there).

:- register_all_lexical_items([D], here_there_adverb(D)).


:- randomizable noun/3.

:- randomizable whpron/2.
whpron( who, person  ).
%whpron( whom, person ).
whpron( what, entity ).

% :- randomizable det/2.
% det( every, (X^S1)^(X^S2)^   all(X,S1,S2) ).
% det( a,     (X^S1)^(X^S2)^exists(X,S1,S2)  ).
% det( some,  (X^S1)^(X^S2)^exists(X,S1,S2)  ).

:- register_all_lexical_items([P], whpron(P, _)).
:- register_all_lexical_items([P], pronoun_word(P,_,_,_,_)).

/*
:- randomizable det/2.
det( every, (X^S1)^(X^S2)^   all(X,S1,S2) ).
det( a,     (X^S1)^(X^S2)^exists(X,S1,S2)  ).
det( some,  (X^S1)^(X^S2)^exists(X,S1,S2)  ).

:- randomizable n/3.
n( author,     authors,     X^author(X)     ).
n( book,       books,       X^book(X)       ).
n( professor,  professors,  X^professor(X)  ).
n( program,    programs,    X^program(X)    ).
n( programmer, programmers, X^programmer(X) ).
n( student,    students,    X^student(X)    ).

:- randomizable pn/2.
proper_noun( begriffsschrift, begriffsschrift ).
proper_noun( bertrand,        bertrand        ).
proper_noun( bill,            bill            ).
proper_noun( gottlob,         gottlob         ).
proper_noun( lunar,           lunar           ).
proper_noun( principia,       principia       ).
proper_noun( shrdlu,          shrdlu          ).
proper_noun( terry,           terry           ).

*/