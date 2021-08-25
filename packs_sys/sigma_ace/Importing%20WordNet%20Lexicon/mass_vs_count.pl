% File to determine whether nouns are count or mass nouns.

% Of course it is not a strict dichotomy but we want to correctly
% classify most that clearly one or the other. For those that can
% be either we want to err on the side of saying they are mass nouns
% since the worse that will happen is that we will allow an article
% to be dropped that should not be.

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement
	
% can_be_mass_noun(+Word,+Noun_Synset_ID,+SUMO_Noun,-Countability) sets Countability to 'mass' if the noun
% could be a mass noun then its determiner may be omitted, but does not have to be. As there is no
% clear line between mass and count nouns this attempts to err on the side of including nouns as
% possibly 'mass' unless it is clear they are more likely 'count'.

% Most mass nouns are substances like 'gas', 'air', and 'water'. Foods can also be treated
% as mass nouns

can_be_mass_noun(Word,Noun_Synset_ID,SUMO_Noun,mass) :- is_subclass_or_equal(SUMO_Noun,'Substance'),!.

can_be_mass_noun(Word,Noun_Synset_ID,SUMO_Noun,mass) :- is_subclass_or_equal(SUMO_Noun,'Food'),!.

can_be_mass_noun(Word,Noun_Synset_ID,SUMO_Noun,mass) :- can_be_a_mass_noun(Word),!.

% Exception list, words that do not make sense in a phrase like "One ____, two _____, etc."
% but do fit in a phrase like "Some __________".
can_be_a_mass_noun(money).
can_be_a_mass_noun(furniture).
can_be_a_mass_noun(data).
can_be_a_mass_noun(life).
can_be_a_mass_noun(beauty).
can_be_a_mass_noun(truth).
can_be_a_mass_noun(crime).
can_be_a_mass_noun(law).
can_be_a_mass_noun(education).


% The default is that words are considered to be count nouns.
can_be_mass_noun(Word,Noun_Synset_ID,SUMO_Noun,count).

% examples:

% can_be_mass_noun(money,109639711,'CurrencyMeasure',Countability),write(Countability),nl.   returns Countability = mass
% can_be_mass_noun(toy,103524895,'Artifact',Countability),write(Countability),nl.   returns Countability = count
% can_be_mass_noun(red,103879230,'ColorAttribute',Countability),write(Countability),nl.   returns Countability = count
% can_be_mass_noun(blood,104197156,'Blood',Countability),write(Countability),nl.   returns Countability = mass
% can_be_mass_noun(water,110650211,'Water',Countability),write(Countability),nl.   returns Countability = mass
% can_be_mass_noun(life,110060845,'SubjectiveAssessmentAttribute',Countability),write(Countability),nl.   returns Countability = mass
% can_be_mass_noun(ball,102240791,'Artifact',Countability),write(Countability),nl.   returns Countability = count
% can_be_mass_noun(airplane,102174460,'TransportationDevice',Countability),write(Countability),nl.   returns Countability = count
% can_be_mass_noun(tank,103468818,'[TransportationDevice, Weapon]',Countability),write(Countability),nl.  returns Countability = count