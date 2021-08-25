% File to determine if WordNet nouns are [person, time, or object] according to the ACE categorizations
% needed to interpret adverbial prepositions such as 'on', 'at', and 'in' and to answer WH-questions.

% celt_noun_type(+Sumo,+Synset,-Type)
% Sumo is the name of a Sumo concept.
% Synset is the WordNet synset ID.
% Type is one of [person, time, or object].

:- style_check(-singleton).      % no singletons warnings
:- style_check(-discontiguous).  % allows more creative predicate placement

:-write('Loading code to infer noun types [person, object, time]...'),nl.

% SUMO

% sumo('Human',synonymousExternalConcept,'person',100004123 ,noun,
%     "a human being; 'there was too much for one person to do'").

% sumo('TimeMeasure',synonymousExternalConcept,'time',100015594 ,noun,
%     "the continuum of experience in which events pass from the future through the present to the past").

% sumo('Physical',synonymousExternalConcept,'entity',100001740 ,noun,
%      "anything having existence (living or nonliving)").

% WordNet

% Synset 110875931 = [time_unit, unit_of_time]
%              (a unit for measuring time periods)
% Synset 110843624 = [time_period, period, period_of_time, amount_of_time]
%              (an indefinite length of time; "a time period of 30 years"; "hastened the period of his recovery")
% Synset 110856821 = [clock_time, time]
%              (the time as given by a clock; "do you know what time it is?"; "the time is 10 o'clock")
% Synset 105432762 = [happening, occurrence, natural_event]
%              (an event that happens)



% WordNet Only Rules

% Synset id 100004123 = [person, individual, someone, somebody,
%                        mortal, human, soul] (a human being; "there was too much for one person to do")

celt_noun_type_from_WN(Sumo,Synset,person) :-
	is_hypernym_in(Synset,[100004123]),!.

% Synset id 110875931 = [time_unit, unit_of_time] (a unit for measuring time periods)
% Synset id 110843624 = [time_period, period, period_of_time, amount_of_time] (an indefinite length of time;
%                        "a time period of 30 years"; "hastened the period of his recovery")
% Synset id 110856821 = [clock_time, time] (the time as given by a clock; "do you know what time it is?"; "the time is 10 o'clock")
% Synset id 105432762 = [happening, occurrence, natural_event] (an event that happens)

celt_noun_type_from_WN(Sumo,Synset,time) :-
	is_hypernym_in(Synset,[110875931,110843624,110856821,105432762]),!.

celt_noun_type_from_WN(Sumo,Synset,object).



% SUMO Only rules

celt_noun_type_from_SUMO(Sumo,Synset,time) :-
 	member(Class,['TimeMeasure','Process']),
 	is_subclass_or_equal(Sumo,Class),!.

celt_noun_type_from_SUMO(Sumo,Synset,person) :-
 	member(Class,['Human','OccupationalRole','SocialRole']),
 	is_subclass_or_equal(Sumo,Class),!.

celt_noun_type_from_SUMO(Sumo,Synset,person) :- is_subclass_or_equal(Sumo,'Human'),!.

celt_noun_type_from_SUMO(Sumo,Synset,object).

% wordnet_noun_type(+WN_noun,-Type) determines the type [person, time, or object]
% for a WordNet noun (e.g., 'box').

wordnet_noun_type(WN_noun,Type) :- noun_for_lexicon(WN_noun,Gloss,Sumo,Synset),celt_noun_type_from_SUMO(Sumo,Synset,Type),!.
wordnet_noun_type(WN_noun,unknown_noun_type) :- !.
