
%% relpron( that ).
%% relpron( who  ).
%% relpron( whom ).

pronoun('I', subject, first, singular, $speaker).
pronoun(me, object, first, singular, $speaker).
pronoun(you, _, second, singular, $speaker).

:- randomizable whpron/1.
whpron( who  ).
whpron( whom ).
whpron( what ).

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
