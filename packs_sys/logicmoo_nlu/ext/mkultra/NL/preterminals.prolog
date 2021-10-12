det(LF) --> [D], {det(D, LF)}.

:- randomizable n//2.
n(singular, LF)   --> [N], {noun(N, _, LF)}.
n(plural, LF)   --> [N], {noun(_, N, LF)}.

proper_noun(singular, (E^S)^S) --> [PN], {proper_noun(PN, E)}.

pronoun(Case, Person:Number, (E^S)^S) --> [PN], {pronoun(PN, Case, Person, Number, E)}.

%relpron --> [RP], {relpron(RP)}.
whpron --> [WH], {whpron(WH)}.

%%
%% Verb conjugations
%%

:- randomizable iv//4.
%                                                      Base TPS Past PastP PresP LF
iv(simple, third:singular, LF, present) --> [IV], { intransitive_verb(_,   IV, _,   _,    _,    LF) }.
iv(simple, Agreement,      LF, present) --> [IV], { intransitive_verb(IV,  _,  _,   _,    _,    LF),
						    Agreement \= third:singular }.
iv(simple, _Agreement,      LF, past)   -->  [IV], { intransitive_verb(_,  _,  IV,  _,    _,    LF) }.
iv(simple, _Agreement,      LF, future) -->  [IV], { intransitive_verb(IV, _,  _,   _,    _,    LF) }.
% Used only in the construction X does not BASEFORM.
iv(base, _Agreement,      LF, present) -->  [IV], { intransitive_verb(IV, _,  _,   _,    _,    LF) }.
iv(past_participle, _Agreement,      LF, _Tense) -->  [IV], { intransitive_verb(_,  _,  _,   IV,   _,    LF) }.
iv(present_participle, _Agreement,   LF, _Tense) -->  [IV], { intransitive_verb(_,  _,  _,   _,    IV,   LF) }.

:- randomizable tv//4.
%                                                      Base TPS Past PastP PresP LF
tv(simple, third:singular, LF, present) --> [TV], { transitive_verb(_,   TV, _,   _,    _,    LF) }.
tv(simple, Agreement,      LF, present) --> [TV], { transitive_verb(TV,  _,  _,   _,    _,    LF),
						    Agreement \= third:singular }.
tv(simple, _Agreement,      LF, past)   -->  [TV], { transitive_verb(_,  _,  TV,  _,    _,    LF) }.
tv(simple, _Agreement,      LF, future) -->  [TV], { transitive_verb(TV, _,  _,   _,    _,    LF) }.
% Used only in the construction X does not BASEFORM.
tv(base, _Agreement,      LF, present) -->  [TV], { transitive_verb(TV, _,  _,   _,    _,    LF) }.
tv(past_participle, _Agreement,      LF, _Tense) -->  [TV], { transitive_verb(_,  _,  _,   TV,   _,    LF) }.
tv(present_participle, _Agreement,   LF, _Tense) -->  [TV], { transitive_verb(_,  _,  _,   _,    TV,   LF) }.

