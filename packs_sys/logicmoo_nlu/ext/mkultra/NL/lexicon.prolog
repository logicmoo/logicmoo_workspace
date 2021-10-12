%% relpron( that ).
%% relpron( who  ).
%% relpron( whom ).

pronoun_word('I', subject, first, singular, $speaker).
pronoun_word(me, object, first, singular, $speaker).
pronoun_word(my, genitive, first, singular, $speaker).
pronoun_word(you, subject, second, singular, $addressee).
pronoun_word(you, object, second, singular, $addressee).
pronoun_word(your, genitive, second, singular, $addressee).
pronoun_word(we, subject, first, plural, $dialog_group).
pronoun_word(us, object, first, plural, $dialog_group).
pronoun_word(our, genitive, first, plural, $dialog_group).

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

